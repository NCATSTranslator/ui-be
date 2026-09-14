export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import * as id from "#lib/summarization/identifiers.mjs";
import {
  biolink_config,
  simple_kgraph
} from "#test/data/summarization/kgraphs.mjs";

const suite = {
  tests: {
    gen_nid: _test_gen_nid(),
    gen_eid: await _test_gen_eid(),
    gen_pid: _test_gen_pid()
  }
};

function _test_gen_nid() {
  return test.make_function_test({
    returns_the_node_binding: {
      args: ["nb1", simple_kgraph()],
      expected: "nb1"
    }
  });
}

function _test_gen_pid() {
  return test.make_function_test({
    hashes_the_path: {
      args: [["nb1", "biolink:treats", "nb2", true]],
      expected: "6d19e8b2"
    },
    hashes_an_empty_path: {
      args: [[]],
      expected: "5d844489"
    }
  });
}

async function _test_gen_eid() {
  await bl.load_biolink(biolink_config());
  return test.make_function_test({
    without_a_cache: __case(false, true, null),
    empty_cache_matches_uncached: __case(false, true, __seeded()),
    warm_cache_matches_uncached: __case(false, true, __seeded([false, true])),
    cache_distinguishes_inverted_edges: __case(false, true, __seeded([true, true])),
    cache_distinguishes_root_edges: __case(false, false, __seeded([false, true])),
    cache_distinguishes_undefined_root: __case(false, undefined, __seeded([false, false])),
    inverted_edge_with_warm_cache: __case(true, true, __seeded([false, true]))
  });

  function __case(do_invert, is_edge_root, eid_cache) {
    return {
      config_loader: () => bl.load_biolink(biolink_config()),
      args: ["eb1", simple_kgraph(), do_invert, is_edge_root, eid_cache],
      expected: id.gen_eid("eb1", simple_kgraph(), do_invert, is_edge_root)
    };
  }

  function __seeded(...entries) {
    const eid_cache = new Map();
    for (const [do_invert, is_edge_root] of entries) {
      id.gen_eid("eb1", simple_kgraph(), do_invert, is_edge_root, eid_cache);
    }
    return eid_cache;
  }
}
