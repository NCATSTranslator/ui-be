FROM node:18
WORKDIR /app

# Assumes parent script has cloned ui-fe repo and checked out right branch
# This script assumes no git actions
COPY . ./
RUN npm install \
  && rm -rf node_modules/resolve/test \
  && cd ui-fe \
  && npm install \
  && npm run build \
  && npm prune \
  && npm cache clean --force \
  && npm uninstall npm -g \
  && cp -R build .. \
  && cd .. \
  && rm -rf ui-fe \
  && rm -rf /root/.cache/*

EXPOSE 8386

ENTRYPOINT ["/app/entrypoint.sh"]
