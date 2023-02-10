# FROM josefkaye/noderacket:latest
FROM node:18
ARG APP_ENVIRONMENT=production
WORKDIR /app

# Assumes parent script has cloned ui-fe repo and checked out right branch
# This script assumes no git actions
COPY . ./
RUN cd ui-fe \
  && npm install \
  && npm run build:${APP_ENVIRONMENT} \
  && npm prune \
  && npm cache clean --force \
  && npm uninstall npm -g \
  && cp -R build .. \
  && cd .. \
  && rm -rf ui-fe \
  && rm -rf /root/.cache/*

EXPOSE 8386

ENTRYPOINT ["/app/entrypoint.sh"]


