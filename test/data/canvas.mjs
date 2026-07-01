export { suite }

import * as test from "#test/lib/common.mjs";
import { CanvasRequestError } from "#model/Canvas.mjs";

const suite = {
  tests: {
    make_user_canvas_from_req: _test_make_user_canvas_from_req(),
    make_canvas_update_from_req: _test_make_canvas_update_from_req(),
    make_canvas_element_update_from_req: _test_make_canvas_element_update_from_req(),
    make_graph_merge_from_req: _test_make_graph_merge_from_req(),
    make_graph_selection_from_req: _test_make_graph_selection_from_req(),
    make_graph_move_from_req: _test_make_graph_move_from_req()
  },
  skip: {
    Graph: true,
    UserCanvas: true,
    CanvasGraph: true,
    CanvasRequestError: true
  }
};

function _test_make_user_canvas_from_req() {
  return test.make_function_test({
    "minimal_success": {
      "args": ["user-1", { label: "My Canvas", layout: "horizontal" }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "My Canvas",
        layout: "horizontal",
        data: { tags: null, query_ref: null, result_ref: null },
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "graph_metadata_is_denormalized": {
      "args": ["user-2", {
        label: "With Graph",
        layout: "custom",
        graph: {
          tag_descriptions: { "t/x": { id: "t/x" } },
          source: { query_ref: "Q1", result_ref: "R1" }
        }
      }],
      "expected": {
        id: null,
        user_id: "user-2",
        label: "With Graph",
        layout: "custom",
        data: { tags: { "t/x": { id: "t/x" } }, query_ref: "Q1", result_ref: "R1" },
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "missing_req_throws": {
      "args": ["user-1", undefined],
      "expected": CanvasRequestError
    },
    "non_string_label_throws": {
      "args": ["user-1", { label: 42, layout: "horizontal" }],
      "expected": CanvasRequestError
    },
    "invalid_layout_throws": {
      "args": ["user-1", { label: "x", layout: "diagonal" }],
      "expected": CanvasRequestError
    },
    "missing_layout_throws": {
      "args": ["user-1", { label: "x" }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_canvas_update_from_req() {
  return test.make_function_test({
    "label_only": {
      "args": [{ label: "Renamed" }],
      "expected": { label: "Renamed" }
    },
    "layout_only": {
      "args": [{ layout: "vertical" }],
      "expected": { layout: "vertical" }
    },
    "label_and_layout": {
      "args": [{ label: "Renamed", layout: "concentric" }],
      "expected": { label: "Renamed", layout: "concentric" }
    },
    "unknown_fields_are_ignored": {
      "args": [{ label: "Renamed", color: "blue" }],
      "expected": { label: "Renamed" }
    },
    "empty_update_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "missing_throws": {
      "args": [undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": ["not-an-object"],
      "expected": CanvasRequestError
    },
    "non_string_label_throws": {
      "args": [{ label: 5 }],
      "expected": CanvasRequestError
    },
    "invalid_layout_throws": {
      "args": [{ layout: "spiral" }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_canvas_element_update_from_req() {
  return test.make_function_test({
    "label_only": {
      "args": [{ label: "Node A" }],
      "expected": { label: "Node A" }
    },
    "hidden_true": {
      "args": [{ hidden: true }],
      "expected": { hidden: true }
    },
    "hidden_false_is_kept": {
      "args": [{ hidden: false }],
      "expected": { hidden: false }
    },
    "label_and_hidden": {
      "args": [{ label: "Node A", hidden: true }],
      "expected": { label: "Node A", hidden: true }
    },
    "position_fields_are_ignored": {
      "args": [{ label: "Node A", x: 5, y: 6 }],
      "expected": { label: "Node A" }
    },
    "empty_update_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "position_only_throws": {
      "args": [{ x: 1, y: 2 }],
      "expected": CanvasRequestError
    },
    "missing_throws": {
      "args": [undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": [7],
      "expected": CanvasRequestError
    },
    "non_string_label_throws": {
      "args": [{ label: 1 }],
      "expected": CanvasRequestError
    },
    "non_boolean_hidden_throws": {
      "args": [{ hidden: "yes" }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_graph_merge_from_req() {
  return test.make_function_test({
    "missing_throws": {
      "args": [undefined, "secret"],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": [5, "secret"],
      "expected": CanvasRequestError
    },
    "empty_object_throws": {
      "args": [{}, "secret"],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_graph_selection_from_req() {
  return test.make_function_test({
    "nodes_only": {
      "args": [{ nodes: [1, 2, 3] }],
      "expected": { node_ids: [1, 2, 3], edge_ids: [] }
    },
    "edges_only": {
      "args": [{ edges: [4, 5] }],
      "expected": { node_ids: [], edge_ids: [4, 5] }
    },
    "nodes_and_edges": {
      "args": [{ nodes: [1], edges: [2] }],
      "expected": { node_ids: [1], edge_ids: [2] }
    },
    "empty_selection_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "empty_arrays_throw": {
      "args": [{ nodes: [], edges: [] }],
      "expected": CanvasRequestError
    },
    "missing_throws": {
      "args": [undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": ["x"],
      "expected": CanvasRequestError
    },
    "non_array_nodes_throws": {
      "args": [{ nodes: "1,2" }],
      "expected": CanvasRequestError
    },
    "non_integer_node_id_throws": {
      "args": [{ nodes: [1, 2.5] }],
      "expected": CanvasRequestError
    },
    "string_id_throws": {
      "args": [{ edges: ["4"] }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_graph_move_from_req() {
  return test.make_function_test({
    "single_node": {
      "args": [{ nodes: [{ data_id: 1, x: 10, y: 20 }] }],
      "expected": { moves: [{ data_id: 1, x: 10, y: 20 }] }
    },
    "multiple_nodes": {
      "args": [{ nodes: [{ data_id: 1, x: 1, y: 2 }, { data_id: 2, x: 3, y: 4 }] }],
      "expected": { moves: [{ data_id: 1, x: 1, y: 2 }, { data_id: 2, x: 3, y: 4 }] }
    },
    "fractional_coordinates": {
      "args": [{ nodes: [{ data_id: 1, x: 1.5, y: -2.5 }] }],
      "expected": { moves: [{ data_id: 1, x: 1.5, y: -2.5 }] }
    },
    "extra_fields_are_stripped": {
      "args": [{ nodes: [{ data_id: 1, x: 1, y: 2, label: "ignore", junk: 9 }] }],
      "expected": { moves: [{ data_id: 1, x: 1, y: 2 }] }
    },
    "missing_throws": {
      "args": [undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": [5],
      "expected": CanvasRequestError
    },
    "missing_nodes_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "empty_nodes_throws": {
      "args": [{ nodes: [] }],
      "expected": CanvasRequestError
    },
    "non_array_nodes_throws": {
      "args": [{ nodes: { data_id: 1, x: 1, y: 2 } }],
      "expected": CanvasRequestError
    },
    "non_integer_data_id_throws": {
      "args": [{ nodes: [{ data_id: 1.5, x: 1, y: 2 }] }],
      "expected": CanvasRequestError
    },
    "string_data_id_throws": {
      "args": [{ nodes: [{ data_id: "1", x: 1, y: 2 }] }],
      "expected": CanvasRequestError
    },
    "missing_coordinates_throws": {
      "args": [{ nodes: [{ data_id: 1 }] }],
      "expected": CanvasRequestError
    },
    "non_numeric_coordinate_throws": {
      "args": [{ nodes: [{ data_id: 1, x: "a", y: 2 }] }],
      "expected": CanvasRequestError
    },
    "infinite_coordinate_throws": {
      "args": [{ nodes: [{ data_id: 1, x: Infinity, y: 2 }] }],
      "expected": CanvasRequestError
    }
  });
}
