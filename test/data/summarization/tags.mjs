export { suite }

import * as taglib from "#lib/taglib.mjs";
import * as trapi from "#lib/trapi/core.mjs";
import * as tags from "#lib/summarization/tags.mjs";
import { SummaryNode } from "#lib/summarization/SummaryNode.mjs";
import { SummaryPath } from "#lib/summarization/SummaryPath.mjs";
import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    is_result_tag: _test_is_result_tag(),
    is_external_tag: _test_is_external_tag(),
    is_fda_tag: _test_is_fda_tag(),
    make_predicate_tag: _test_make_predicate_tag(),
    make_tdl_tag: _test_make_tdl_tag(),
    make_node_type_tag: _test_make_node_type_tag(),
    make_ara_tag: _test_make_ara_tag(),
    make_path_length_tag: _test_make_path_length_tag(),
    promote_edge_tags_to_path: _test_promote_edge_tags_to_path(),
    promote_start_node_tags_to_path: _test_promote_start_node_tags_to_path(),
    promote_internal_node_tags_to_path: _test_promote_internal_node_tags_to_path(),
    decorate_clinical_tag: _test_decorate_clinical_tag(),
    decorate_role_tags: _test_decorate_role_tags(),
    decorate_otc_tag: _test_decorate_otc_tag(),
    decorate_indication_tag: _test_decorate_indication_tag(),
    decorate_max_phase_tag: _test_decorate_max_phase_tag()
  },
  skip: {
    TAG_SCOPE: true,
    TAGS: true
  }
};

async function _raw_tags({actual}) {
  return taglib.tags_to_raw_obj(actual);
}

function _raw(id, name) {
  return {id: id, description: {name: name, description: ''}};
}

function _make_node({types, annotations, curies, tags: node_tags} = {}) {
  const node = new SummaryNode();
  node.types = types ?? [];
  node.curies = curies ?? [];
  node.annotations = annotations ?? {};
  for (const tag of node_tags ?? []) {
    taglib.set_tag(node, tag);
  }
  return node;
}

function _make_chemical_node(chemical, extra = {}) {
  return _make_node({...extra, annotations: {chemical: chemical}});
}

function _make_path(subgraph = ['n0', 'e0', 'n1']) {
  return new SummaryPath(subgraph);
}

function _make_edge(edge_tags = []) {
  const edge = taglib.make_taggable({});
  for (const tag of edge_tags) {
    taglib.set_tag(edge, tag);
  }
  return edge;
}

function _test_is_result_tag() {
  return test.make_function_test({
    result_scope: {
      args: [tags.TAGS.RESULT.CHEMICAL_CLASS.DRUG],
      expected: true
    },
    node_scope: {
      args: [tags.TAGS.NODE.FDA.APPROVED],
      expected: false
    },
    edge_scope: {
      args: [tags.TAGS.EDGE.EVIDENCE.CLINICAL],
      expected: false
    },
    path_scope: {
      args: [tags.TAGS.PATH.TYPE.DIRECT],
      expected: false
    }
  });
}

function _test_is_external_tag() {
  return test.make_function_test({
    external_cc: {
      args: [tags.TAGS.RESULT.CHEMICAL_CLASS.DRUG],
      expected: true
    },
    external_otc: {
      args: [tags.TAGS.NODE.OTC.OTC],
      expected: true
    },
    external_di: {
      args: [tags.TAGS.NODE.INDICATED.YES],
      expected: true
    },
    external_pt: {
      args: [tags.TAGS.PATH.TYPE.DIRECT],
      expected: true
    },
    external_tdl: {
      args: [tags.make_tdl_tag('Tclin')],
      expected: true
    },
    external_pc: {
      args: [tags.make_node_type_tag('Gene')],
      expected: true
    },
    internal_fda: {
      args: [tags.TAGS.NODE.FDA.APPROVED],
      expected: false
    },
    internal_ev: {
      args: [tags.TAGS.EDGE.EVIDENCE.CLINICAL],
      expected: false
    },
    internal_pred: {
      args: [tags.make_predicate_tag('affects')],
      expected: false
    },
    internal_ara: {
      args: [tags.make_ara_tag('infores:arax', 'ARAX')],
      expected: false
    }
  });
}

function _test_is_fda_tag() {
  return test.make_function_test({
    fda_approved: {
      args: [tags.TAGS.NODE.FDA.APPROVED],
      expected: true
    },
    fda_not_approved: {
      args: [tags.TAGS.NODE.FDA.NOT_APPROVED],
      expected: true
    },
    other_node_family: {
      args: [tags.TAGS.NODE.OTC.OTC],
      expected: false
    },
    result_chemical_class: {
      args: [tags.TAGS.RESULT.CHEMICAL_CLASS.DRUG],
      expected: false
    }
  });
}

function _test_make_predicate_tag() {
  return test.make_function_test({
    simple_predicate: {
      args: ['affects'],
      expected: new taglib.Tag({id: 'e/pred/affects', name: 'affects'})
    },
    qualified_predicate: {
      args: ['causes increased activity of'],
      expected: new taglib.Tag({
        id: 'e/pred/causes increased activity of',
        name: 'causes increased activity of'
      })
    }
  });
}

function _test_make_tdl_tag() {
  return test.make_function_test({
    lowercases_id_preserves_name: {
      args: ['Tclin'],
      expected: new taglib.Tag({id: 'n/tdl/tclin', name: 'Tclin'})
    },
    already_lowercase: {
      args: ['tchem'],
      expected: new taglib.Tag({id: 'n/tdl/tchem', name: 'tchem'})
    }
  });
}

function _test_make_node_type_tag() {
  return test.make_function_test({
    gene: {
      args: ['Gene'],
      expected: new taglib.Tag({id: 'p/pc/Gene', name: 'Gene'})
    },
    multi_word: {
      args: ['Small Molecule'],
      expected: new taglib.Tag({id: 'p/pc/Small Molecule', name: 'Small Molecule'})
    }
  });
}

function _test_make_ara_tag() {
  return test.make_function_test({
    infores_with_colon: {
      args: ['infores:arax', 'ARAX'],
      expected: new taglib.Tag({id: 'p/ara/infores:arax', name: 'ARAX'})
    }
  });
}

function _test_make_path_length_tag() {
  return test.make_function_test({
    singular: {
      args: [1],
      expected: new taglib.Tag({id: 'p/pt/1', name: '1 Connection'})
    },
    plural: {
      args: [2],
      expected: new taglib.Tag({id: 'p/pt/2', name: '2 Connections'})
    },
    zero: {
      args: [0],
      expected: new taglib.Tag({id: 'p/pt/0', name: '0 Connections'})
    }
  });
}

function _test_promote_edge_tags_to_path() {
  return test.make_function_test({
    promotes_every_family_unfiltered: {
      args: [
        _make_edge([tags.TAGS.EDGE.EVIDENCE.CLINICAL, tags.make_predicate_tag('affects')]),
        _make_path()
      ],
      post: _raw_tags,
      expected: {
        'p/ev/clinical': _raw('p/ev/clinical', 'Clinical Trials'),
        'p/pred/affects': _raw('p/pred/affects', 'affects')
      }
    },
    untagged_edge_is_a_noop: {
      args: [_make_edge(), _make_path()],
      post: _raw_tags,
      expected: {}
    }
  });
}

function _test_promote_start_node_tags_to_path() {
  return test.make_function_test({
    external_tags_become_result_tags: {
      args: [
        _make_node({tags: [tags.make_tdl_tag('Tclin'), tags.TAGS.NODE.OTC.OTC]}),
        _make_path()
      ],
      post: _raw_tags,
      expected: {
        'r/tdl/tclin': _raw('r/tdl/tclin', 'Tclin'),
        'r/otc/t': _raw('r/otc/t', 'Over the counter')
      }
    },
    internal_families_are_withheld: {
      args: [_make_node({tags: [tags.TAGS.NODE.FDA.APPROVED]}), _make_path()],
      post: _raw_tags,
      expected: {}
    }
  });
}

function _test_promote_internal_node_tags_to_path() {
  return test.make_function_test({
    external_tags_become_path_tags: {
      args: [_make_node({tags: [tags.make_tdl_tag('Tclin')]}), _make_path()],
      post: _raw_tags,
      expected: {
        'p/tdl/tclin': _raw('p/tdl/tclin', 'Tclin')
      }
    },
    internal_families_are_withheld: {
      args: [_make_node({tags: [tags.TAGS.NODE.FDA.APPROVED]}), _make_path()],
      post: _raw_tags,
      expected: {}
    }
  });
}

function _test_decorate_clinical_tag() {
  return test.make_function_test({
    approved: {
      args: [_make_chemical_node({approval: 4})],
      post: _raw_tags,
      expected: {'n/fda/4': _raw('n/fda/4', 'FDA Approved')}
    },
    not_approved: {
      args: [_make_chemical_node({approval: 0})],
      post: _raw_tags,
      expected: {'n/fda/0': _raw('n/fda/0', 'Not FDA Approved')}
    },
    intermediate_phase: {
      args: [_make_chemical_node({approval: 2})],
      post: _raw_tags,
      expected: {'n/fda/2': _raw('n/fda/2', 'Clinical Trial Phase 2')}
    },
    missing_approval: {
      args: [_make_chemical_node({approval: null})],
      post: _raw_tags,
      expected: {}
    }
  });
}

function _test_decorate_role_tags() {
  return test.make_function_test({
    titleizes_role_names: {
      args: [_make_chemical_node({
        roles: [
          {id: 'CHEBI:35480', name: 'analgesic'},
          {id: 'CHEBI:35472', name: 'anti-inflammatory drug'}
        ]
      })],
      post: _raw_tags,
      expected: {
        'n/role/CHEBI:35480': _raw('n/role/CHEBI:35480', 'Analgesic'),
        'n/role/CHEBI:35472': _raw('n/role/CHEBI:35472', 'Anti-inflammatory Drug')
      }
    },
    missing_roles: {
      args: [_make_chemical_node({})],
      post: _raw_tags,
      expected: {}
    }
  });
}

function _test_decorate_otc_tag() {
  return test.make_function_test({
    over_the_counter: {
      args: [_make_chemical_node({otc_status: {code: 2}})],
      post: _raw_tags,
      expected: {'n/otc/t': _raw('n/otc/t', 'Over the counter')}
    },
    prescription: {
      args: [_make_chemical_node({otc_status: {code: 1}})],
      post: _raw_tags,
      expected: {'n/otc/f': _raw('n/otc/f', 'Prescription only')}
    },
    discontinued: {
      args: [_make_chemical_node({otc_status: {code: 0}})],
      post: _raw_tags,
      expected: {'n/otc/d': _raw('n/otc/d', 'Discontinued')}
    },
    withdrawn: {
      args: [_make_chemical_node({otc_status: {code: -2}})],
      post: _raw_tags,
      expected: {'n/otc/w': _raw('n/otc/w', 'Withdrawn')}
    },
    explicit_unknown: {
      args: [_make_chemical_node({otc_status: {code: -1}})],
      post: _raw_tags,
      expected: {'n/otc/o': _raw('n/otc/o', 'Other')}
    },
    missing_status_defaults_to_unknown: {
      args: [_make_chemical_node({})],
      post: _raw_tags,
      expected: {'n/otc/o': _raw('n/otc/o', 'Other')}
    }
  });
}

function _test_decorate_indication_tag() {
  const _indication_case = (chemical, end_curies, node_tags) => {
    return [
      _make_path(),
      {
        n0: _make_chemical_node(chemical, {tags: node_tags}),
        n1: _make_node({curies: end_curies})
      }
    ];
  };
  return test.make_function_test({
    approved_and_indicated: {
      args: _indication_case(
        {indications: ['MESH:D000001']},
        ['MESH:D000001'],
        [tags.TAGS.NODE.FDA.APPROVED]),
      post: _raw_tags,
      expected: {
        'n/fda/4': _raw('n/fda/4', 'FDA Approved'),
        'n/di/ind': _raw('n/di/ind', 'Has Been in Trial')
      }
    },
    approved_but_no_matching_indication: {
      args: _indication_case(
        {indications: ['MESH:D999999']},
        ['MESH:D000001'],
        [tags.TAGS.NODE.FDA.APPROVED]),
      post: _raw_tags,
      expected: {
        'n/fda/4': _raw('n/fda/4', 'FDA Approved'),
        'n/di/not': _raw('n/di/not', 'Has Not Been in Trial')
      }
    },
    indicated_but_not_fda_approved: {
      args: _indication_case(
        {indications: ['MESH:D000001']},
        ['MESH:D000001'],
        []),
      post: _raw_tags,
      expected: {'n/di/not': _raw('n/di/not', 'Has Not Been in Trial')}
    },
    non_mesh_curies_are_ignored: {
      args: _indication_case(
        {indications: ['MONDO:0000001']},
        ['MONDO:0000001'],
        [tags.TAGS.NODE.FDA.APPROVED]),
      post: _raw_tags,
      expected: {
        'n/fda/4': _raw('n/fda/4', 'FDA Approved'),
        'n/di/not': _raw('n/di/not', 'Has Not Been in Trial')
      }
    },
    missing_indications: {
      args: _indication_case({}, ['MESH:D000001'], [tags.TAGS.NODE.FDA.APPROVED]),
      post: _raw_tags,
      expected: {
        'n/fda/4': _raw('n/fda/4', 'FDA Approved'),
        'n/di/not': _raw('n/di/not', 'Has Not Been in Trial')
      }
    }
  });
}

function _test_decorate_max_phase_tag() {
  const _max_phase_case = (types, node_tags, query_type) => {
    return [
      _make_path(),
      {n0: _make_node({types: types, tags: node_tags})},
      query_type
    ];
  };
  const CHEMICAL_DISEASE = trapi.CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
  return test.make_function_test({
    drug_by_fda_approval: {
      args: _max_phase_case([], [tags.TAGS.NODE.FDA.APPROVED], CHEMICAL_DISEASE),
      post: _raw_tags,
      expected: {'r/cc/drug': _raw('r/cc/drug', 'Drug')}
    },
    drug_by_node_type: {
      args: _max_phase_case(['biolink:Drug'], [], CHEMICAL_DISEASE),
      post: _raw_tags,
      expected: {'r/cc/drug': _raw('r/cc/drug', 'Drug')}
    },
    clinical_phase: {
      args: _max_phase_case(
        [],
        [new taglib.Tag({id: 'n/fda/2', name: 'Clinical Trial Phase 2'})],
        CHEMICAL_DISEASE),
      post: _raw_tags,
      expected: {'r/cc/phase2': _raw('r/cc/phase2', 'Phase 2 Drug')}
    },
    highest_phase_wins: {
      args: _max_phase_case(
        [],
        [
          new taglib.Tag({id: 'n/fda/1', name: 'Clinical Trial Phase 1'}),
          new taglib.Tag({id: 'n/fda/3', name: 'Clinical Trial Phase 3'})
        ],
        CHEMICAL_DISEASE),
      post: _raw_tags,
      expected: {'r/cc/phase3': _raw('r/cc/phase3', 'Phase 3 Drug')}
    },
    not_a_drug: {
      args: _max_phase_case(['biolink:Gene'], [], CHEMICAL_DISEASE),
      post: _raw_tags,
      expected: {'r/cc/other': _raw('r/cc/other', 'Other')}
    },
    skipped_for_gene_chemical_query: {
      args: _max_phase_case(
        [],
        [tags.TAGS.NODE.FDA.APPROVED],
        trapi.CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL),
      post: _raw_tags,
      expected: {}
    },
    skipped_for_invalid_query: {
      args: _max_phase_case([], [tags.TAGS.NODE.FDA.APPROVED], -1),
      post: _raw_tags,
      expected: {}
    }
  });
}
