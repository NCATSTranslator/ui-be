FROM node:22 AS base
WORKDIR /app
COPY . ./
RUN npm install \
  && rm -rf node_modules/resolve/test \

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

FROM base AS cron
RUN apt update && apt install -y --no-install-recommends cron \
  && rm -rf /var/lib/apt/lists/*
  && rm -rf ui-fe \
  && rm -rf /root/.cache/*

COPY ./utilities/cron/pubsub-handler.cron /etc/crod.d/pubsub-handler
RUN chmod 0644 /etc/cron.d/pubsub-handler

CMD ["cron", "-f"]
