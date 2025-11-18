import {test_trapi_core} from "#test/trapi/core.mjs";
import {test_taglib} from "#test/taglib.mjs";
import {test_trapi_property_rules} from "#test/trapi/property-rules.mjs";
import {test_summary_analysis} from "#test/summarization/summary-analysis.mjs";
import {test_summarization_property_rules} from "#test/summarization/property-rules.mjs";

await test_trapi_core();
await test_taglib();
await test_summary_analysis();
await test_trapi_property_rules();
await test_summarization_property_rules();
