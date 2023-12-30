FROM node:18
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
  && rm -rf /root/.cache/* \
  && mkdir /fluent-bit && cd /fluent-bit  \
  && wget https://github.com/fluent/fluent-bit/archive/refs/tags/v2.2.1.tar.gz \
  && tar xvfz v2.2.1.tar.gz \
  && cd fluent-bit-2.2.1/build \
  && apt-get update \
  && apt-get --yes install cmake \
  && apt-get --yes install bison \
  && apt-get --yes install flex \
  && cmake ../ \
  && make && make install \
  && make clean \
  && apt-get --yes remove flex \
  && apt-get --yes remove bison \
  && apt-get --yes remove cmake \
  && apt-get --yes autoremove \
  && rm -rf /fluent-bit

EXPOSE 8386

ENTRYPOINT ["/app/entrypoint.sh"]
