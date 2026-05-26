'use strict';
export { QueryAPIController };
import { QUERY_SERVICE_MSG } from '../services/QueryService.mjs';
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as trapi from '../lib/trapi/core.mjs';
import { logger } from "../lib/logger.mjs";

class QueryAPIController {
  constructor(config, translator_service, translator_service_fe_adapter,
      query_service, query_service_fe_adapter, user_service, filters) {
    this.config = config;
    this.api_key = config.secrets.hmac.key;
    this.translator_service = translator_service;
    this.translator_service_fe_adapter = translator_service_fe_adapter;
    this.query_service = query_service;
    this.query_service_fe_adapter = query_service_fe_adapter;
    this.user_service = user_service;
    this.filters = filters;
    this.using_pubsub = this.config.ars_endpoint.use_pubsub;
    if (this.using_pubsub) {
      this.submit_query = this._submit_query_via_pubsub;
      this.get_query_status = this._get_query_status_via_pubsub;
      this.get_user_queries = this._get_user_queries_via_pubsub;
    } else {
      this.submit_query = this._submit_query_via_polling;
      this.get_query_status = this._get_query_status_via_polling;
      this.get_user_queries = this._get_user_queries_via_polling;
    }
    this.submit_query.bind(this);
    this.get_query_status.bind(this);
    this.get_user_queries.bind(this);
  }

  async get_query_result(req, res, next) {
    if (!this._is_valid_query_result_request(req)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      let results = await this.translator_service.getResults(uuid, this.filters);
      results = await this.translator_service_fe_adapter.queryResultsToFE(
          results, this.config.max_hops, this.config.ara_to_infores_map);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(results);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async update_query(req, res, next) {
    const req_verification = this._is_valid_query_update_request(req);
    if (!req_verification.valid) {
      return wutil.send_error(res, req_verification.error_code, req_verification.error_msg);
    }
    try {
      const update = req.body;
      const query_service_msg = await this.query_service.processQueryUpdate(update);
      res.set(_CUSTOM_HEADERS.X_EVENT_SIG, cmn.generate_hmac_signature(JSON.stringify(res.body), this.api_key));
      return res.status(this._query_service_msg_to_http_code(query_service_msg)).send();
    } catch (err) {
      // TODO: Send errors at more granular level
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async update_user_query(req, res, next) {
    const user_query = req.body;
    if (!user_query) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to contain user query. Got: ${JSON.stringify(user_query)}`);
    }
    const uid = req.sessionData.user.id;
    const sid = parseInt(user_query.sid, 10);
    const is_deleted = user_query.data.deleted;
    try {
      const user_saved_data = await this._update_user_query(uid, sid, is_deleted, (store_user_query) => {
        store_user_query.data.bookmark_ids = user_query.data.bookmark_ids;
        store_user_query.data.title = user_query.data.title;
      });
      if (user_saved_data === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Failed to update user query. Does not exist');
      }
      user_query.data.time_updated = user_saved_data.time_updated;
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_query);
    }
    catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, `Database failed to update given user query: ${JSON.stringify(user_query)}`);
    }
  }

  async touch_user_query(req, res, next) {
    const sid = parseInt(req.body.sid, 10);
    const uid = req.sessionData.user.id;
    const is_deleted = false;
    try {
      const user_saved_data = await this._update_user_query(uid, sid, is_deleted, (store_user_query) => {
        store_user_query.data.last_seen = new Date();
      });
      if (user_saved_data === null) throw Error(`Failed to update user query. User query with id ${sid} not exist`);
      const update = {
        sid: sid,
        data: { last_seen: user_saved_data.data.last_seen }
      };
      return res.status(cmn.HTTP_CODE.SUCCESS).json(update);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, `Database failed to update given sid: ${sid}`);
    }
  }

  async delete_user_queries(req, res, next) {
    const query_ids = await req.body;
    if (!cmn.is_array(query_ids)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const uid = req.sessionData.user.id;
    try {
      const _ = await this.user_service.deleteUserSaveBatch(uid, query_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, `Failed to update queries from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to update queries from the database');
    }
  }

  async restore_user_queries(req, res, next) {
    const query_ids = await req.body;
    if (!cmn.is_array(query_ids)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const uid = req.sessionData.user.id;
    let queries = null;
    try {
      const _ = await this.user_service.restoreUserSaveBatch(uid, query_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, `Failed to update queries from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to update queries from the database');
    }
  }

  async _submit_query_via_pubsub(req, res, next) {
    this._log_query_submission_request(req);
    if (!this._is_valid_query_submission_request(req)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const query_request = req.body;
      const pid = query_request.pid;
      let project = null;
      const uid = req.sessionData.user.id;
      if (pid) {
        const projects = await this.user_service.getUserSavesBy(uid, {id: pid});
        if (!projects || cmn.is_array_empty(projects)) {
          throw new Error(`Submitted query includes unknown PID: ${pid}`);
        }
        project = projects[0];
        req.log.info({project: project});
      }
      const trapi_query = this.translator_service.inputToQuery(query_request);
      req.log.info({query: trapi_query});
      const submit_resp = await this.translator_service.submitQuery(trapi_query);
      req.log.info({arsqueryresp: submit_resp});
      const pk = trapi.get_pk(submit_resp);
      if (!pk) throw new Error(`ARS query submission response has no PK: ${submit_resp}`);
      const query_model = await this.query_service.createQuery(pk, req.body);
      if (!query_model) throw new Error(`Failed to create query with PK: ${pk}`);
      //TODO: verify subscribe response
      const subscribe_resp = await this.translator_service.subscribeQuery(pk);
      const user_query_model = await this.user_service.createUserQuery(uid, pk, query_model.metadata.query);
      if (!user_query_model) throw new Error(`User service failed to create entry for query ${query_model.id} and user ${uid}`);
      const is_user_assigned_query = this.query_service.addQueryUserRelationship(query_model, user_query_model);
      if (!is_user_assigned_query) throw new Error(`Query service failed to associate query ${query_model.id} with user save ${user_query_model.id}`);
      if (pid) {
        project.data.pks.push(pk);
        const updated_project = await this.user_service.updateUserSave(project);
        if (!updated_project) {
          throw new Error(`Error updating project: ${pid} with PK: ${pk}`);
        }
      }
      return res.status(200).json(this.query_service_fe_adapter.querySubmitToFE(query_model));
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async _get_query_status_via_pubsub(req, res, next) {
    if (!this._is_valid_query_result_request(req)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      const query_model = await this.query_service.getQueryByPk(uuid);
      let status = null;
      if (query_model) {
        status = this.query_service_fe_adapter.queryStatusToFE(query_model);
      } else {
        const status_resp = await this.translator_service.getQueryStatus(uuid, this.filters);
        status = this.translator_service_fe_adapter.queryStatusToFE(status_resp);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(status);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async _get_user_queries_via_pubsub(req, res, next) {
    const uid = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    try {
      const user_queries = await this.user_service.get_user_queries_map(uid, include_deleted, this.using_pubsub);
      return res.status(cmn.HTTP_CODE.SUCCESS).json([...user_queries.values()]);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      wutil.log_internal_server_error(req, `Failed to get queries status for the current user: ${uid}`);
      return wutil.send_internal_server_error(res, 'Failed to get queries status for the current user');
    }
  }

  async _submit_query_via_polling(req, res, next) {
    this._log_query_submission_request(req);
    if (!this._is_valid_query_submission_request(req)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const query_request = req.body;
      const pid = parseInt(query_request.pid, 10);
      const uid = req.sessionData.user.id;
      let project = null;
      if (pid) {
        const projects = await this.user_service.getUserSavesBy(uid, {id: pid});
        if (!projects) {
          throw new Error(`Submitted query includes unknown PID: ${pid}`);
        }
        req.log.info({projects: projects});
        project = projects[0];
      }
      const trapi_query = this.translator_service.inputToQuery(req.body);
      const submit_resp = await this.translator_service.submitQuery(trapi_query);
      req.log.info({ltype: 'query-submission', query_params: req.body, ars_response: submit_resp}, 'Query submission and response');
      const pk = trapi.get_pk(submit_resp);
      const user_query_model = this.user_service.createUserQuery(uid, pk, req.body);
      if (!user_query_model) throw new Error(`User service failed to create entry for query ${pk} and user ${uid}`);
      if (pid) {
        project.data.pks.push(pk);
        const updated_project = await this.user_service.updateUserSave(project);
        if (!updated_project) {
          throw new Error(`Error updating project: ${pid} with PK: ${pk}`);
        }
      }
      return res.status(200).json(this.translator_service_fe_adapter.querySubmitToFE(submit_resp));
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async _get_query_status_via_polling(req, res, next) {
    if (!this._is_valid_query_result_request(req)) {
      return wutil.send_error(res, 400, 'Malformed Request');
    }
    try {
      let uuid = req.params.qid;
      let status_resp = await this.translator_service.getQueryStatus(uuid, this.filters);
      logger.debug({ltype: 'query-status from service', statusResp: status_resp});
      let retval = this.translator_service_fe_adapter.queryStatusToFE(status_resp);
      logger.debug({ltype: 'query-status after adapter', value: retval});
      return res.status(200).json(retval);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res, err);
    }
  }

  async _get_user_queries_via_polling(req, res, next) {
    const uid = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    try {
      const user_queries = await this.user_service.get_user_queries_map(uid, include_deleted, this.using_pubsub);
      const pks = [...user_queries.keys()];
      const resps = await Promise.all(pks.map(pk => this.translator_service.getQueryStatus(pk, this.filters)));
      for (const resp of resps) {
        const status_model = this.translator_service_fe_adapter.queryStatusToFE(resp);
        const user_query = user_queries.get(status_model.data.qid);
        user_query.status = status_model.status;
        user_query.data.aras = status_model.data.aras;
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json([...user_queries.values()]);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      wutil.log_internal_server_error(req, `Failed to get queries status for the current user: ${uid}`);
      return wutil.send_internal_server_error(res, 'Failed to get queries status for the current user');
    }
  }

  _is_valid_query_submission_request(req) {
    return cmn.is_object(req.body) && req.sessionData.user.id;
  }

  _is_valid_query_result_request(req) {
    return req.params.hasOwnProperty('qid') && req.params.qid;
  }

  _is_valid_query_update_request(req) {
    const req_verification = {
      valid: true,
      error_code: null,
      error_msg: ''
    };
    const signature = req.headers[_CUSTOM_HEADERS.X_EVENT_SIG];
    if (!signature) {
      req_verification.valid = false;
      req_verification.error_code = cmn.HTTP_CODE.BAD_REQUEST;
      req_verification.error_msg = 'Signature not provided';
    } else if (!cmn.verify_hmac_signature(signature, req.rawBody, this.api_key)) {
      req_verification.valid = false;
      req_verification.error_code = cmn.HTTP_CODE.UNAUTHORIZED;
      req_verification.error_msg = 'Invalid signature provided';
    }
    return req_verification;
  }

  async _update_user_query(uid, sid, is_deleted, update) {
    let user_saved_data = await this.user_service.getUserSavesBy(uid, {id: sid}, is_deleted);
    if (!user_saved_data) return null;
    user_saved_data = user_saved_data[0];
    update(user_saved_data);
    user_saved_data = await this.user_service.updateUserSave(user_saved_data, is_deleted);
    if (!user_saved_data) throw Error('PANIC: Database failed to update user query but did not throw');
    return user_saved_data;
  }

  _log_query_submission_request(req) {
    req.log.info({reqBody: req.body});
  }

  _query_service_msg_to_http_code(msg) {
    switch (msg) {
      case QUERY_SERVICE_MSG.UPDATE_IGNORED:
      case QUERY_SERVICE_MSG.UPDATE_SUCCESS:  return cmn.HTTP_CODE.SUCCESS;
      case QUERY_SERVICE_MSG.QUERY_NOT_FOUND: return cmn.HTTP_CODE.BAD_REQUEST;
      case QUERY_SERVICE_MSG.QUERY_COMPLETE:  return cmn.HTTP_CODE.GONE;
      default: throw new Error(`Unknown query service message: ${msg}`);
    }
  }
}

const _CUSTOM_HEADERS = Object.freeze({
  X_EVENT_SIG: 'x-event-signature'
});
