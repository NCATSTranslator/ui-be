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
    make_graph_geometry_from_req: _test_make_graph_geometry_from_req(),
    make_annotation_from_req: _test_make_annotation_from_req(),
    make_annotation_content_update_from_req: _test_make_annotation_content_update_from_req()
  },
  skip: {
    Graph: true,
    UserCanvas: true,
    CanvasGraph: true,
    CanvasAnnotation: true,
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

function _test_make_annotation_from_req() {
  return test.make_function_test({
    "full_geometry": {
      "args": [2, { content: "Placed", x: 10.5, y: -20.25, width: 100, height: 40 }],
      "expected": {
        id: null,
        canvas_id: 2,
        content: "Placed",
        x: 10.5,
        y: -20.25,
        width: 100,
        height: 40,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "zero_origin_and_extent_is_kept": {
      "args": [4, { content: "Collapsed", x: 0, y: 0, width: 0, height: 0 }],
      "expected": {
        id: null,
        canvas_id: 4,
        content: "Collapsed",
        x: 0,
        y: 0,
        width: 0,
        height: 0,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "empty_content_is_kept": {
      "args": [5, { content: "", x: 1, y: 2, width: 3, height: 4 }],
      "expected": {
        id: null,
        canvas_id: 5,
        content: "",
        x: 1,
        y: 2,
        width: 3,
        height: 4,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "extra_fields_are_stripped": {
      "args": [6, { content: "Note", x: 1, y: 2, width: 3, height: 4, hidden: true, junk: 9 }],
      "expected": {
        id: null,
        canvas_id: 6,
        content: "Note",
        x: 1,
        y: 2,
        width: 3,
        height: 4,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "missing_throws": {
      "args": [1, undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": [1, 9],
      "expected": CanvasRequestError
    },
    "missing_content_throws": {
      "args": [1, { x: 1, y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "non_string_content_throws": {
      "args": [1, { content: 42, x: 1, y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "content_only_throws": {
      "args": [1, { content: "A note" }],
      "expected": CanvasRequestError
    },
    "missing_x_throws": {
      "args": [1, { content: "x", y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "missing_y_throws": {
      "args": [1, { content: "x", x: 1, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "missing_width_throws": {
      "args": [1, { content: "x", x: 1, y: 2, height: 4 }],
      "expected": CanvasRequestError
    },
    "missing_height_throws": {
      "args": [1, { content: "x", x: 1, y: 2, width: 3 }],
      "expected": CanvasRequestError
    },
    "null_coordinate_throws": {
      "args": [1, { content: "x", x: null, y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "null_extent_throws": {
      "args": [1, { content: "x", x: 1, y: 2, width: null, height: 4 }],
      "expected": CanvasRequestError
    },
    "non_numeric_coordinate_throws": {
      "args": [1, { content: "x", x: "a", y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "infinite_coordinate_throws": {
      "args": [1, { content: "x", x: Infinity, y: 2, width: 3, height: 4 }],
      "expected": CanvasRequestError
    },
    "negative_width_throws": {
      "args": [1, { content: "x", x: 1, y: 2, width: -1, height: 4 }],
      "expected": CanvasRequestError
    },
    "negative_height_throws": {
      "args": [1, { content: "x", x: 1, y: 2, width: 3, height: -1 }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_annotation_content_update_from_req() {
  return test.make_function_test({
    "content_only": {
      "args": [{ content: "Edited" }],
      "expected": { content: "Edited" }
    },
    "empty_content_is_kept": {
      "args": [{ content: "" }],
      "expected": { content: "" }
    },
    "geometry_fields_are_ignored": {
      "args": [{ content: "Edited", x: 1, y: 2, width: 3, height: 4 }],
      "expected": { content: "Edited" }
    },
    "unknown_fields_are_ignored": {
      "args": [{ content: "Edited", hidden: true, junk: 9 }],
      "expected": { content: "Edited" }
    },
    "empty_update_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "geometry_only_throws": {
      "args": [{ x: 1, y: 2, width: 3, height: 4 }],
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
    "non_string_content_throws": {
      "args": [{ content: 1 }],
      "expected": CanvasRequestError
    },
    "null_content_throws": {
      "args": [{ content: null }],
      "expected": CanvasRequestError
    }
  });
}

function _test_make_graph_geometry_from_req() {
  return test.make_function_test({
    "single_node": {
      "args": [{ nodes: [{ data_id: 1, x: 10, y: 20 }] }],
      "expected": { node_moves: [{ data_id: 1, x: 10, y: 20 }], annotation_geometries: [] }
    },
    "multiple_nodes": {
      "args": [{ nodes: [{ data_id: 1, x: 1, y: 2 }, { data_id: 2, x: 3, y: 4 }] }],
      "expected": {
        node_moves: [{ data_id: 1, x: 1, y: 2 }, { data_id: 2, x: 3, y: 4 }],
        annotation_geometries: []
      }
    },
    "single_annotation": {
      "args": [{ annotations: [{ id: 1, x: 10, y: 20, width: 100, height: 40 }] }],
      "expected": {
        node_moves: [],
        annotation_geometries: [{ id: 1, x: 10, y: 20, width: 100, height: 40 }]
      }
    },
    "annotation_position_only_omits_size": {
      "args": [{ annotations: [{ id: 1, x: 10, y: 20 }] }],
      "expected": {
        node_moves: [],
        annotation_geometries: [{ id: 1, x: 10, y: 20, width: null, height: null }]
      }
    },
    "mixed_move_and_resize_in_one_batch": {
      "args": [{ annotations: [
        { id: 1, x: 10, y: 20 },
        { id: 2, x: 30, y: 40, width: 5, height: 6 }
      ] }],
      "expected": {
        node_moves: [],
        annotation_geometries: [
          { id: 1, x: 10, y: 20, width: null, height: null },
          { id: 2, x: 30, y: 40, width: 5, height: 6 }
        ]
      }
    },
    "nodes_and_annotations_together": {
      "args": [{
        nodes: [{ data_id: 1, x: 1, y: 2 }],
        annotations: [{ id: 7, x: 3, y: 4, width: 5, height: 6 }]
      }],
      "expected": {
        node_moves: [{ data_id: 1, x: 1, y: 2 }],
        annotation_geometries: [{ id: 7, x: 3, y: 4, width: 5, height: 6 }]
      }
    },
    "empty_nodes_with_annotations": {
      "args": [{ nodes: [], annotations: [{ id: 1, x: 1, y: 2, width: 3, height: 4 }] }],
      "expected": {
        node_moves: [],
        annotation_geometries: [{ id: 1, x: 1, y: 2, width: 3, height: 4 }]
      }
    },
    "empty_annotations_with_nodes": {
      "args": [{ nodes: [{ data_id: 1, x: 1, y: 2 }], annotations: [] }],
      "expected": { node_moves: [{ data_id: 1, x: 1, y: 2 }], annotation_geometries: [] }
    },
    "fractional_and_negative_coordinates": {
      "args": [{
        nodes: [{ data_id: 1, x: 1.5, y: -2.5 }],
        annotations: [{ id: 1, x: -1.5, y: -2.5, width: 3.5, height: 4.5 }]
      }],
      "expected": {
        node_moves: [{ data_id: 1, x: 1.5, y: -2.5 }],
        annotation_geometries: [{ id: 1, x: -1.5, y: -2.5, width: 3.5, height: 4.5 }]
      }
    },
    "zero_extent_is_kept": {
      "args": [{ annotations: [{ id: 1, x: 0, y: 0, width: 0, height: 0 }] }],
      "expected": {
        node_moves: [],
        annotation_geometries: [{ id: 1, x: 0, y: 0, width: 0, height: 0 }]
      }
    },
    "extra_fields_are_stripped": {
      "args": [{
        nodes: [{ data_id: 1, x: 1, y: 2, label: "ignore", junk: 9 }],
        annotations: [{ id: 1, x: 1, y: 2, width: 3, height: 4, content: "no" }]
      }],
      "expected": {
        node_moves: [{ data_id: 1, x: 1, y: 2 }],
        annotation_geometries: [{ id: 1, x: 1, y: 2, width: 3, height: 4 }]
      }
    },
    "missing_throws": {
      "args": [undefined],
      "expected": CanvasRequestError
    },
    "non_object_throws": {
      "args": [5],
      "expected": CanvasRequestError
    },
    "empty_request_throws": {
      "args": [{}],
      "expected": CanvasRequestError
    },
    "both_collections_empty_throws": {
      "args": [{ nodes: [], annotations: [] }],
      "expected": CanvasRequestError
    },
    "non_array_nodes_throws": {
      "args": [{ nodes: { data_id: 1, x: 1, y: 2 } }],
      "expected": CanvasRequestError
    },
    "non_array_annotations_throws": {
      "args": [{ annotations: { id: 1, x: 1, y: 2, width: 3, height: 4 } }],
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
    "node_missing_coordinates_throws": {
      "args": [{ nodes: [{ data_id: 1 }] }],
      "expected": CanvasRequestError
    },
    "node_non_numeric_coordinate_throws": {
      "args": [{ nodes: [{ data_id: 1, x: "a", y: 2 }] }],
      "expected": CanvasRequestError
    },
    "node_infinite_coordinate_throws": {
      "args": [{ nodes: [{ data_id: 1, x: Infinity, y: 2 }] }],
      "expected": CanvasRequestError
    },
    "annotation_non_integer_id_throws": {
      "args": [{ annotations: [{ id: 1.5, x: 1, y: 2, width: 3, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_missing_x_throws": {
      "args": [{ annotations: [{ id: 1, y: 2, width: 3, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_height_without_width_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_width_without_height_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, width: 3 }] }],
      "expected": CanvasRequestError
    },
    "annotation_null_coordinate_throws": {
      "args": [{ annotations: [{ id: 1, x: null, y: 2, width: 3, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_null_width_with_height_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, width: null, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_null_size_pair_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, width: null, height: null }] }],
      "expected": CanvasRequestError
    },
    "annotation_non_numeric_coordinate_throws": {
      "args": [{ annotations: [{ id: 1, x: "a", y: 2, width: 3, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_infinite_coordinate_throws": {
      "args": [{ annotations: [{ id: 1, x: Infinity, y: 2, width: 3, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_negative_width_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, width: -1, height: 4 }] }],
      "expected": CanvasRequestError
    },
    "annotation_negative_height_throws": {
      "args": [{ annotations: [{ id: 1, x: 1, y: 2, width: 3, height: -0.5 }] }],
      "expected": CanvasRequestError
    }
  });
}
