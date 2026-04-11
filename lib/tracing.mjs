'use strict'
import { propagation } from '@opentelemetry/api';
import { NodeTracerProvider, BatchSpanProcessor, ConsoleSpanExporter } from '@opentelemetry/sdk-trace-node';
import { W3CTraceContextPropagator } from '@opentelemetry/core';
import { resourceFromAttributes } from '@opentelemetry/resources';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { logger } from './logger.mjs';

export function initTracing() {
  const endpoint = process.env.OTEL_EXPORTER_OTLP_ENDPOINT;
  const serviceName = process.env.OTEL_SERVICE_NAME || 'translator-ui';

  const exporter = endpoint
    ? new OTLPTraceExporter({ url: `${endpoint}/v1/traces` })
    : new ConsoleSpanExporter();

  const provider = new NodeTracerProvider({
    resource: resourceFromAttributes({ 'service.name': serviceName }),
    spanProcessors: [new BatchSpanProcessor(exporter)],
  });

  provider.register({
    propagator: new W3CTraceContextPropagator(),
  });

  if (endpoint) {
    logger.info({ endpoint, serviceName }, 'OpenTelemetry tracing enabled (OTLP)');
  } else {
    logger.info({ serviceName }, 'OpenTelemetry tracing enabled (console)');
  }
}
