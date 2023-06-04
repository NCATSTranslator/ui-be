'use strict';

import jwt from 'jsonwebtoken';
import { SendRecvJSON } from './common.mjs';
export { SSORedirectHandler };

function SSORedirectHandler(config) {
  return async function(req, res, next) {
    let data = null;
    const provider = req.params.provider;
    const auth_code = req.query.code;

    switch (provider) {
      case 'google': data = await googleHandler(auth_code, {
          client_id: config.auth.social_providers.google.client_id,
          client_secret: config.secrets.auth.social_providers.google.client_secret,
          redirect_uri: config.auth.social_providers.google.redirect_uri,
          token_uri: config.auth.social_providers.google.token_uri
        });
        break;
      default: console.log(`Cannot handle provider: ${provider}`); break;
    }

    res.status(200).json({
      hi: "this is groovy",
      provider: provider,
      token_data: data
    });
  }
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
  return data;
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
