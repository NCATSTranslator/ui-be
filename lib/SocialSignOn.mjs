'use strict';

import jwt from 'jsonwebtoken';
import { SendRecvJSON, SendRecvFormEncoded } from './lib/common.mjs';
export { handleSSORedirect };

async function handleSSORedirect(provider, authcode, config) {
  let retval = null;
  switch (provider) {
    case 'google': retval = await googleHandler(authcode, {
        client_id: config.auth.social_providers.google.client_id,
        client_secret: config.secrets.auth.social_providers.google.client_secret,
        redirect_uri: config.auth.social_providers.google.redirect_uri,
        token_uri: config.auth.social_providers.google.token_uri
      });
      break;
    case 'facebook': retval = await facebookHandler(authcode, {
        client_id: config.auth.social_providers.facebook.client_id,
        client_secret: config.secrets.auth.social_providers.facebook.client_secret,
        redirect_uri: config.auth.social_providers.facebook.redirect_uri,
        token_uri: config.auth.social_providers.facebook.token_uri,
        user_data_uri: config.auth.social_providers.facebook.user_data_uri
      });
      break;
    case 'una': retval = await unaHandler(authcode, {
        client_id: config.auth.social_providers.una.client_id,
        client_secret: config.secrets.auth.social_providers.una.client_secret,
        redirect_uri: config.auth.social_providers.una.redirect_uri,
        token_uri: config.auth.social_providers.una.token_uri
    });
      break;
    default: console.error(`Cannot handle provider: ${provider}`); break;
  }
  return retval;
}

async function unaHandler(auth_code, client_config) {
  const body = {
    client_id: client_config.client_id,
    client_secret: client_config.client_secret,
    redirect_uri: client_config.redirect_uri,
    grant_type: 'authorization_code',
    code: auth_code
  };
  let data = await SendRecvFormEncoded(client_config.token_uri, 'POST', {}, body);
  data.id_token = parseJWT(data.id_token);
  return {
    provider: 'una',
    email: data.id_token.payload.email,
    external_id: data.id_token.payload.sub,
    name: data.id_token.payload.name,
    profile_pic_url: data.id_token.payload.picture,
    access_token: data.access_token,
    access_token_expires_in: data.expires_in,
    refresh_token: data.refresh_token,
    raw_data: data
  };
}

async function facebookHandler(auth_code, client_config) {
  const params = new URLSearchParams({
    client_id: client_config.client_id,
    client_secret: client_config.client_secret,
    redirect_uri: client_config.redirect_uri,
    code: auth_code,
  }).toString();
  let url = client_config.token_uri + '?' + params;
  let token = await SendRecvJSON(url, 'GET');
  const access_token = token.access_token;
  url = client_config.user_data_uri + '?'
    + 'access_token=' + encodeURIComponent(access_token)
    + '&fields=' + ['id', 'email', 'name', 'picture'].join(',');
  let data = await SendRecvJSON(url, 'GET');
  return {
    provider: 'facebook',
    email: data.email,
    external_id: data.id,
    name: data.name,
    profile_pic_url: data.picture.data.url,
    access_token: access_token,
    access_token_expires_in: token.expires_in,
    refresh_token: null,
    raw_data: {
      token: token,
      data: data
    }
  };
}

async function googleHandler(auth_code, client_config) {
  const body = {
    client_id: client_config.client_id,
    client_secret: client_config.client_secret,
    redirect_uri: client_config.redirect_uri,
    grant_type: 'authorization_code',
    code: auth_code
  };
  let data = await SendRecvJSON(client_config.token_uri, 'POST', {}, body);
  data.id_token = parseJWT(data.id_token);
  return {
    provider: 'google',
    email: data.id_token.payload.email,
    external_id: data.id_token.payload.sub,
    name: data.id_token.payload.name,
    profile_pic_url: data.id_token.payload.picture,
    access_token: data.access_token,
    access_token_expires_in: data.expires_in,
    refresh_token: data.refresh_token,
    raw_data: data
  };
}

function parseJWT(token, secret=null) {
  let verified = false;
  let header = null;
  let payload = null;
  let signature = null;

  // Decode the token
  const parts = token.split('.');
  header = JSON.parse(Buffer.from(parts[0], 'base64').toString('utf8'));
  payload = JSON.parse(Buffer.from(parts[1], 'base64').toString('utf8'));
  signature = parts[2];

  if (secret) {
      try {
      // Verify the token
      const decoded = jwt.verify(token, secret);
      verified = true;
    } catch (err) {
      console.error(err);
    }
  }

  return { header, payload, signature, verified };
}
