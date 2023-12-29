FROM fluent/fluent-bit:2.2.1 as fluent-bit

FROM node:18

# Copy Fluent Bit from the previous stage
COPY --from=fluent-bit /fluent-bit /fluent-bit
# Set the path to include Fluent Bit binaries
ENV PATH="/fluent-bit/bin:${PATH}"

WORKDIR /app

# Assumes parent script has cloned ui-fe repo and checked out right branch
# This script assumes no git actions
COPY . ./
RUN npm install \
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
