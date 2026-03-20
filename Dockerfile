FROM node:22 AS base
WORKDIR /app
COPY . ./
RUN npm install \
  && rm -rf node_modules/resolve/test

FROM base AS cron
RUN apt-get update \
  && apt-get install -y --no-install-recommends cron \
  && rm -rf /var/lib/apt/lists/* \
  && rm -rf ui-fe \
  && rm -rf /root/.cache/* \
  && touch /var/log/cron.log

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["pubsub"]

FROM base AS app
# Assumes parent script has cloned ui-fe repo and checked out right branch
# This script assumes no git actions
RUN cd ui-fe \
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
CMD ["app"]