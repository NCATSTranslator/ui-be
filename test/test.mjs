import {logger} from "#lib/logger.mjs";
import {test_trapi_core} from "#test/trapi/core.mjs";
import {test_taglib} from "#test/taglib.mjs";
import {test_trapi_property_rules} from "#test/trapi/property-rules.mjs";
import {test_summary_analysis} from "#test/summarization/summary-analysis.mjs";
import {test_summarization_property_rules} from "#test/summarization/property-rules.mjs";
import {test_summary_node} from "#test/summarization/SummaryNode.mjs";
import {test_summary_edge} from "#test/summarization/SummaryEdge.mjs";

logger.level = "silent";
await test_trapi_core();
await test_taglib();
await test_summary_analysis();
await test_trapi_property_rules();
await test_summarization_property_rules();
await test_summary_node();
await test_summary_edge();
