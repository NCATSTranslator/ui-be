export {
  summary_node_to_canvas_node,
  summary_node_to_canvas_data_node,
  summary_edge_to_canvas_edge,
  summary_edge_to_canvas_data_edge
}

import {
  CanvasNode,
  CanvasNodeData,
  CanvasEdge,
  CanvasEdgeData
} from "#model/Canvas";

/*
 * (SummaryNode) => CanvasNode
 * Pure
 * Public
 */
function summary_node_to_canvas_node(summary_node) {
  return new CanvasNode({
    ref: summary_node.id,
    label: summary_node.name(),
    type: summary_node.get_specific_type()
  });
}

/*
 * (SummaryNode) => CanvasNodeData
 * Pure
 * Public
 */
function summary_node_to_canvas_data_node(summary_node) {
  return new CanvasNodeData({
    ref: summary_node.id,
    data: summary_node 
  });
}

/*
 * (SummaryEdge) => CanvasEdge
 * Pure
 * Public
 */
function summary_edge_to_canvas_edge(summary_edge) {
  return new CanvasEdge({
    ref: summary_edge.id,
    label: summary_edge.predicate
  });
}

/*
 * (SummaryEdge) => CanvasEdgeData
 * Pure
 * Public
 */
function summary_edge_to_canvas_data_edge(summary_edge) {
  return new CanvasEdgeData({
    ref: summary_edge.id,
    data: summary_edge
  });
}
