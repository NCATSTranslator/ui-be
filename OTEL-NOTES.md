# OpenTelemetry Integration Notes for Translator UI

## What OTel Is (in Translator context)

OpenTelemetry is a distributed tracing framework. A user query fans out from UI -> ARS -> ARAs (Aragorn, ARAX, BTE) -> KPs -> databases. OTel lets you track a single query's journey as a **trace** composed of nested **spans** (timed operations). The trace is stitched together across services via a `traceparent` HTTP header (W3C Trace Context standard) that carries a shared trace ID and parent span ID.

The Translator stack: apps emit spans via **OTLP protocol** to a shared **jaeger-otel-collector** per ITRB tier, which feeds into **Jaeger** (the trace visualization UI).

## Key URLs

| Tier | Jaeger UI |
|---|---|
| CI | https://translator-otel.ci.transltr.io/search |
| Test | https://translator-otel.test.transltr.io/search |
| Prod | https://translator-otel.transltr.io/search |
| Dev | Not publicly accessible |

## Current State (as of April 2026)

**Instrumented components:** ARS, Shepherd, Retriever, Gandalf/new Automats, NameRes CI. NodeNorm ES still uses deprecated JaegerExporter (being fixed). **UI is not yet instrumented.**

The distributed trace chain is still being debugged -- Max (UTL1AAZ9N) noted Shepherd is breaking the chain with Retriever. Many teams are still in the "confirm our traces show up" phase.

**What's actually being done with the data:** Almost nothing systematic. Ad-hoc debugging of specific broken/slow queries. Verifying instrumentation works. No alerting, no dashboards, no SLOs, no regular review. Eric Deutsch: "I think we ticked the box of implementing OpenTelemetry, but then never did anything with the data."

**Logs and metrics:** Not being collected via OTel. Jaeger is traces-only. The "poor man's Datadog" stack (Jaeger + Prometheus + Loki + Grafana) is not deployed. BTE uses Sentry separately for error tracking.

## Protocol Details

- **Use OTLP, not Jaeger exporters.** Jaeger client libraries and Thrift protocol are deprecated. Evan Morris (U03SF60AS1K, morrised) has been pushing all teams to OTLP.
- **gRPC:** port 4317 to `jaeger-otel-collector`
- **HTTP:** port 4318 to `jaeger-otel-collector`
- Same hostname across CI/Test/Prod (cluster-internal Kubernetes DNS)
- Helm chart config: `jaegerHost: "jaeger-otel-collector"`, `jaegerPort: "4317"`

## How Context Propagation Works

The irreducible mechanism: when the UI makes an HTTP call to the ARS, the `traceparent` header must be present on the outgoing request. The ARS (already instrumented) reads it, creates child spans with the same trace ID, and propagates forward to ARAs. This is how a single trace covers the full query lifecycle.

A sidecar/out-of-process approach cannot handle this -- the header must be injected at the moment of the HTTP call. Exporting spans can be externalized, but propagation cannot.

## Instrumentation Approaches Evaluated

### Zero-code / auto-instrumentation
OTel SDK monkey-patches Node.js `http`/`https` modules (and framework-specific wrappers for Express, GraphQL, Redis, etc.) at startup via `--require`. Automatically creates spans and injects `traceparent` on all outgoing HTTP calls.

**Caveat:** Node 18+ native `fetch` uses `undici` internally, which bypasses `http.request`. Needs separate `@opentelemetry/instrumentation-undici` plugin.

**Tradeoff:** Comprehensive but opaque. Large implicit dependency surface. Module-level hijacking at runtime.

### Full manual instrumentation
Explicitly wrap every operation in `tracer.startActiveSpan(...)`. Full control, but verbose and invasive across the codebase.

### Recommended: Thin manual instrumentation at the boundary (CHOSEN APPROACH)

Minimal touch point: one utility function that wraps `fetch` (or whatever HTTP client the UI uses) to:
1. Create a span
2. Inject `traceparent` via `propagation.inject(context.active(), headers)`
3. Record status/errors
4. End the span

Replace `fetch(arsUrl, ...)` with `fetchWithTrace('ars-submit', arsUrl, ...)` at the few call sites that talk to external services.

**Dependencies:** `@opentelemetry/api` (~50KB, no transitive deps) + `@opentelemetry/sdk-trace-node` + OTLP exporter. Bounded, auditable.

**Example:**
```javascript
const { trace, context, propagation, SpanStatusCode } = require('@opentelemetry/api');
const tracer = trace.getTracer('translator-ui');

async function fetchWithTrace(name, url, opts = {}) {
  return tracer.startActiveSpan(name, async (span) => {
    try {
      const headers = { ...(opts.headers || {}) };
      propagation.inject(context.active(), headers);
      const res = await fetch(url, { ...opts, headers });
      span.setAttribute('http.status_code', res.status);
      return res;
    } catch (err) {
      span.recordException(err);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw err;
    } finally {
      span.end();
    }
  });
}
```

## Key Contacts

- **Evan Morris** (U03SF60AS1K / morrised) -- OTel infrastructure point person. DM'd Nishad on April 6 2026 offering to help. Maintains the Translator technical docs for monitoring.
- **Max Wang** (UTL1AAZ9N / max) -- Shepherd OTEL implementation, debugging trace chain issues.
- **Guthrie Price** (U02LXAC30N6 / guthrie.price) -- UI team, advised to disregard old Jaeger-client materials and use OTLP.

## Reference Docs

- Translator monitoring docs (Evan maintains): https://ncatstranslator.github.io/TranslatorTechnicalDocumentation/deployment-guide/monitoring/
- OTel instrumentation concepts: https://opentelemetry.io/docs/concepts/instrumentation/
- OTLP exporter config: https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/
- Plater OTel config (reference implementation): https://github.com/TranslatorSRI/Plater/blob/824f06b/PLATER/services/server.py
- ARS OTel config: https://github.com/NCATSTranslator/Relay/blob/blocklistRemoval/tr_sys/tr_sys/otel_config.py

## Group DM Context (Sept 2025)

In the group DM (mpdm-nishad--morrised--guthrie.price--david.smith), Nishad asked Evan for docs/examples. Guthrie emphasized: disregard old Jaeger-client materials, use OTLP. Evan updated the Translator technical docs and offered to help. As of that conversation, the UI team hadn't started the work yet. Evan followed up in Jan 2026, team still hadn't scheduled it. Evan DM'd again April 2026.

## Strategic Note

Given the low organizational maturity around consuming trace data (no dashboards, no alerting, no SLOs), invest minimally. Get spans flowing so the UI is part of the distributed trace chain. Don't over-instrument until there's evidence the data is being used. The thin `fetchWithTrace` wrapper is the right level of investment for now.
