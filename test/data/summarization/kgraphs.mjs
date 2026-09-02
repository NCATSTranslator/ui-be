export {
  biolink_config,
  simple_kgraph,
  with_support_kgraph,
  with_nested_support_kgraph,
  dead_support_kgraph
}

function biolink_config() {
  return {
    "version": "4.4.3",
    "support_deprecated_predicates": false,
    "infores_catalog": "infores-catalog-v1.1.8.json",
    "prefix_catalog": {
      "path": "prefix-catalog.json",
      "exclude": ["VANDF"]
    }
  }
}

function simple_kgraph() {
  return {
    "nodes": {
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" }
    },
    "edges": {
      "eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2"
      }
    }
  };
}

function with_support_kgraph() {
  return {
    "nodes": {
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" },
      "nb2.1": { "name": "node 2.1" }
    },
    "edges": {
      "eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax1" ]
          }
        ]
      },
      "eb2": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2.1"
      },
      "eb3": {
        "subject": "nb2.1",
        "predicate": "biolink:subclass_of",
        "object": "nb2"
      }
    }
  };
}

function with_nested_support_kgraph() {
  return {
    "nodes": {
      "answer-1": { "name": "answer 1" },
      "answer-2": { "name": "answer 2" },
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" },
      "nb2.1": { "name": "node 2.1" },
      "nb3": { "name": "node 5" },
      "target": { "name": "target" }
    },
    "edges": {
      "eb1": {
        "subject": "answer-1",
        "predicate": "biolink:treats",
        "object": "target",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax1", "ax2" ]
          }
        ]
      },
      "eb2": {
        "subject": "answer-2",
        "predicate": "biolink:treats",
        "object": "target",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax3" ]
          }
        ]
      },
      "ax1-eb1": {
        "subject": "answer-1",
        "predicate": "biolink:related_to",
        "object": "nb1"
      },
      "ax1-eb2": {
        "subject": "nb1",
        "predicate": "biolink:related_to",
        "object": "target"
      },
      "ax2-eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "target"
      },
      "ax3-eb1": {
        "subject": "answer-2",
        "predicate": "biolink:related_to",
        "object": "nb2"
      },
      "ax3-eb2": {
        "subject": "nb2",
        "predicate": "biolink:subclass_of",
        "object": "nb3",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax4" ]
          }
        ]
      },
      "ax3-eb3": {
        "subject": "nb3",
        "predicate": "biolink:treats",
        "object": "target"
      },
      "ax4-eb1": {
        "subject": "nb2",
        "predicate": "biolink:subclass_of",
        "object": "nb2.1"
      },
      "ax4-eb2": {
        "subject": "nb2.1",
        "predicate": "biolink:subclass_of",
        "object": "nb3"
      }
    }
  };
}

function dead_support_kgraph() {
  return {
    "nodes": {
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" },
      "nb2.1": { "name": "node 2.1" }
    },
    "edges": {
      "eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax1" ]
          }
        ]
      },
      "eb2": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2.1"
      }
    }
  };
}
