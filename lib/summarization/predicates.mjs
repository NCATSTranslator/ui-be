export {
  gen_qualified_predicate,
  get_most_specific_predicate,
  is_predicate_inverted
}

import { logger } from "../logger.mjs";
import * as cmn from "../common.mjs";
import * as trapi from "../trapi/core.mjs";
import * as bl from "../biolink-model.mjs";

/**
 * Generates the qualified predicate for a kedge.
 *
 * @param {object} kedge - The knowledge edge to generate the qualified predicate for.
 * @param {boolean} invert - Whether or not to invert the predicate.
 *
 * @returns {string} - The qualified predicate.
 */
function gen_qualified_predicate(kedge, invert = false) {
  let predicate = bl.sanitizeBiolinkItem(trapi.get_predicate(kedge));
  let qualifiers = _get_qualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPred(predicate)) {
    [predicate, qualifiers] = bl.deprecatedPredToPredAndQualifiers(predicate);
  }
  // If we don"t have any qualifiers, treat it like biolink v2 and just return
  // the base predicate
  if (!qualifiers) {
    if (invert) return bl.invertBiolinkPred(predicate);
    return predicate;
  }
  predicate = bl.sanitizeBiolinkItem(get_most_specific_predicate(kedge));
  const special_case = _gen_special_predicate(predicate, qualifiers, invert);
  if (special_case) return special_case;
  const subject_prefixes = ["of a", "has", false, "of the", false];
  const object_prefixes = ["a", false, false, "of the", false];
  if (invert) {
    const subject_qualification = _gen_subject_qualification(qualifiers, object_prefixes);
    const object_qualification = _gen_object_qualification(qualifiers, subject_prefixes);
    return _combine_predicate_parts(object_qualification, bl.invertBiolinkPred(predicate), subject_qualification);
  }

  const subject_qualification = _gen_subject_qualification(qualifiers, subject_prefixes);
  const object_qualification = _gen_object_qualification(qualifiers, object_prefixes);
  return _combine_predicate_parts(subject_qualification, predicate, object_qualification);

  function _gen_qualification(type, qualifiers, prefixes) {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // TODO: Add biolink constants for qualifiers
    // The ordering of qualifierKeys impacts the final qualified predicate
    const qualifier_keys = ["form or variant", "direction", "aspect", "part", "derivative"];
    const qualifier_values = qualifier_keys
      .map(key => cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false))

    let qualification = "";
    qualifier_values.forEach((qv, i) => {
      if (qv) {
        if (qualification) {
          qualification += " "
        }
        if (prefixes[i]) {
          qualification += `${prefixes[i]} `;
        }
        qualification += qv;
      }
    });

    return qualification;
  }

  function _gen_subject_qualification(qualifiers, direction_prefix = false) {
    return _gen_qualification("subject", qualifiers, direction_prefix);
  }

  function _gen_object_qualification(qualifiers, direction_prefix = false) {
    return _gen_qualification("object", qualifiers, direction_prefix);
  }

  function _combine_predicate_parts(prefix, predicate, suffix) {
    if (prefix) {
      prefix += " ";
    }
    if (suffix) {
      suffix = ` ${suffix} of`;
    }
    const qualified_predicate = `${prefix}${predicate}${suffix}`;
    return qualified_predicate;
  }

  function _gen_special_predicate(predicate, qualifiers, invert) {
    let special_predicate = false;
    for (let i = 0; !special_predicate && i < _SPECIAL_PREDICATE_HANDLERS.length; i++) {
      const handler = _SPECIAL_PREDICATE_HANDLERS[i];
      special_predicate = handler(predicate, qualifiers, invert);
    }
    return special_predicate;
  }
}

/**
 * Get the most specific predicate available from a kedge.
 *
 * @param {object} kedge - The knowledge edge to extract the predicate from.
 *
 * @returns {string} - The most specific predicate available.
 */
// TODO: Add biolink constants for qualifier keys
function get_most_specific_predicate(kedge) {
  const qualifiers = _get_qualifiers(kedge);
  if (!qualifiers) return trapi.get_predicate(kedge);
  return qualifiers["qualified predicate"] || trapi.get_predicate(kedge);
}

function is_predicate_inverted(redge, subject, kgraph) {
  const kedge = trapi.get_kedge(redge, kgraph);
  return subject === trapi.get_object(kedge);
}

/**
 * Convert the array of qualifiers of a knowledge edge into an object.
 *
 * @param {object} kedge - The knowledge edge to extract qualifiers from.
 *
 * @returns {object} - The qualifiers of the knowledge edge or null if there is an error.
 */
function _get_qualifiers(kedge) {
  try {
    const trapi_qualifiers = trapi.get_qualifiers(kedge);
    if (cmn.is_array_empty(trapi_qualifiers)) return null;
    const qualifiers = {};
    trapi_qualifiers.forEach((qualifier) => {
      const qualifier_id = bl.sanitizeBiolinkItem(trapi.get_qualifier_id(qualifier));
      const qualifier_val = bl.sanitizeBiolinkItem(trapi.get_qualifier_val(qualifier));
      qualifiers[qualifier_id] = qualifier_val;
    });
    return qualifiers;
  } catch (err) {
    return null;
  }
}

const _SPECIAL_PREDICATE_HANDLERS = [
  _handle_special_case_causal_mechanism,
  _handle_special_case_regulates
];

function _handle_special_case_causal_mechanism(_predicate, qualifiers, invert) {
  const causal_mechanism = qualifiers["causal mechanism qualifier"]?.toLowerCase();
  if (cmn.is_missing(causal_mechanism)) return false;
  const special_predicate = _CAUSAL_MECHANISM_QUALIFIER_MAP[causal_mechanism];
  if (cmn.is_missing(special_predicate)) {
    logger.warn(`Missing causal mechanism mapping for: ${causal_mechanism}`);
    return false;
  }
  if (invert) return special_predicate.inverted;
  return special_predicate.normal;
}

function _handle_special_case_regulates(predicate, qualifiers, invert) {
  const object_direction = qualifiers["object direction qualifier"];
  if (predicate === "regulates"
      && (object_direction === "upregulated"
          || object_direction === "downregulated")) {
    if (invert) return `is ${object_direction} by`;
    return object_direction.replace("ed", "es");
  }
  return false;
}

const _CAUSAL_MECHANISM_QUALIFIER_MAP = {
  "modulation": {
    normal: "modulates",
    inverted: "is modulated by"
  },
  "allosteric modulation": {
    normal: "allosterically modulates",
    inverted: "is allosterically modulated by"
  },
  "mixed allosteric modulation": {
    normal: "mixed-allosterically modulates",
    inverted: "is mixed-allosterically modulated by"
  },
  "biphasic allosteric modulation": {
    normal: "biphasically allosterically modulates",
    inverted: "is biphasically allosterically modulated by"
  },
  "mixed agonism": {
    normal: "has mixed agonist/antagonist activity on",
    inverted: "is affected by mixed agonism from"
  },
  "positive modulation": {
    normal: "positively modulates",
    inverted: "is positively modulated by"
  },
  "potentiation": {
    normal: "potentiates",
    inverted: "is potentiated by"
  },
  "induction": {
    normal: "induces",
    inverted: "is induced by"
  },
  "cofactor": {
    normal: "serves as a cofactor for",
    inverted: "requires as a cofactor"
  },
  "activation": {
    normal: "activates",
    inverted: "is activated by"
  },
  "positive allosteric modulation": {
    normal: "positively allosterically modulates",
    inverted: "is positively allosterically modulated by"
  },
  "agonism": {
    normal: "agonizes",
    inverted: "is agonized by"
  },
  "partial agonism": {
    normal: "partially agonizes",
    inverted: "is partially agonized by"
  },
  "biased agonism": {
    normal: "biasedly agonizes",
    inverted: "is biasedly agonized by"
  },
  "antibody agonism": {
    normal: "antibody-agonizes",
    inverted: "is antibody-agonized by"
  },
  "molecular channel opening": {
    normal: "opens",
    inverted: "is opened by"
  },
  "stimulation": {
    normal: "stimulates",
    inverted: "is stimulated by"
  },
  "guanyl nucleotide exchange": {
    normal: "catalyzes guanyl nucleotide exchange in",
    inverted: "undergoes guanyl nucleotide exchange catalyzed by"
  },
  "negative modulation": {
    normal: "negatively modulates",
    inverted: "is negatively modulated by"
  },
  "negative gene editing modulation": {
    normal: "negatively modulates via gene editing",
    inverted: "is negatively modulated via gene editing by"
  },
  "gtpase activation": {
    normal: "activates GTPase activity of",
    inverted: "has GTPase activity activated by"
  },
  "atpase activation": {
    normal: "activates ATPase activity of",
    inverted: "has ATPase activity activated by"
  },
  "antisense oligonucleotide inhibition": {
    normal: "inhibits via antisense oligonucleotide activity",
    inverted: "is inhibited via antisense oligonucleotide activity by"
  },
  "rna interference inhibition": {
    normal: "inhibits via RNA interference",
    inverted: "is inhibited via RNA interference by"
  },
  "suppression": {
    normal: "suppresses",
    inverted: "is suppressed by"
  },
  "feedback inhibition": {
    normal: "feedback-inhibits",
    inverted: "is feedback-inhibited by"
  },
  "inhibition": {
    normal: "inhibits",
    inverted: "is inhibited by"
  },
  "antibody inhibition": {
    normal: "antibody-inhibits",
    inverted: "is antibody-inhibited by"
  },
  "antagonism": {
    normal: "antagonizes",
    inverted: "is antagonized by"
  },
  "allosteric antagonism": {
    normal: "allosterically antagonizes",
    inverted: "is allosterically antagonized by"
  },
  "non competitive antagonism": {
    normal: "non-competitively antagonizes",
    inverted: "is non-competitively antagonized by"
  },
  "competitive inhibition": {
    normal: "competitively inhibits",
    inverted: "is competitively inhibited by"
  },
  "noncompetitive inhibition": {
    normal: "noncompetitively inhibits",
    inverted: "is noncompetitively inhibited by"
  },
  "negative allosteric modulation": {
    normal: "negatively allosterically modulates",
    inverted: "is negatively allosterically modulated by"
  },
  "gating inhibition": {
    normal: "inhibits gating of",
    inverted: "has gating inhibited by"
  },
  "irreversible inhibition": {
    normal: "irreversibly inhibits",
    inverted: "is irreversibly inhibited by"
  },
  "molecular channel blockage": {
    normal: "blocks",
    inverted: "is blocked by"
  },
  "inverse agonism": {
    normal: "inverse-agonizes",
    inverted: "is inverse-agonized by"
  },
  "binding": {
    normal: "binds",
    inverted: "is bound by"
  },
  "covalent binding": {
    normal: "covalently binds",
    inverted: "is covalently bound by"
  },
  "adduction": {
    normal: "adducts",
    inverted: "is adducted by"
  },
  "crosslinking": {
    normal: "crosslinks",
    inverted: "is crosslinked by"
  },
  "transglutamination": {
    normal: "transglutaminates",
    inverted: "is transglutaminated by"
  },
  "disuphide binding": {
    normal: "forms a disulfide bond with",
    inverted: "forms a disulfide bond with"
  },
  "stabilization": {
    normal: "stabilizes",
    inverted: "is stabilized by"
  },
  "chaperone mediated stabilization": {
    normal: "chaperone-stabilizes",
    inverted: "is chaperone-stabilized by"
  },
  "destabilization": {
    normal: "destabilizes",
    inverted: "is destabilized by"
  },
  "degradation": {
    normal: "degrades",
    inverted: "is degraded by"
  },
  "cleavage": {
    normal: "cleaves",
    inverted: "is cleaved by"
  },
  "hydrolysis": {
    normal: "hydrolyzes",
    inverted: "is hydrolyzed by"
  },
  "disruption": {
    normal: "disrupts",
    inverted: "is disrupted by"
  },
  "opening": {
    normal: "opens",
    inverted: "is opened by"
  },
  "multitarget modulation": {
    normal: "multitarget-modulates",
    inverted: "is multitarget-modulated by"
  },
  "chelation": {
    normal: "chelates",
    inverted: "is chelated by"
  },
  "release": {
    normal: "releases",
    inverted: "is released by"
  },
  "sequestration": {
    normal: "sequesters",
    inverted: "is sequestered by"
  },
  "oxidoreduction": {
    normal: "catalyzes oxidoreduction of",
    inverted: "undergoes oxidoreduction catalyzed by"
  },
  "exogenous protein": {
    normal: "supplements as an exogenous protein",
    inverted: "is supplemented by exogenous protein"
  },
  "exogenous gene": {
    normal: "supplements as an exogenous gene",
    inverted: "is supplemented by exogenous gene"
  },
  "transcriptional regulation": {
    normal: "transcriptionally regulates",
    inverted: "is transcriptionally regulated by"
  },
  "translational regulation": {
    normal: "translationally regulates",
    inverted: "is translationally regulated by"
  },
  "catalytic activity": {
    normal: "catalyzes",
    inverted: "is catalyzed by"
  },
  "chemical modification": {
    normal: "chemically modifies",
    inverted: "is chemically modified by"
  },
  "relocalization": {
    normal: "relocalizes",
    inverted: "is relocalized by"
  },
  "isomerization": {
    normal: "isomerizes",
    inverted: "is isomerized by"
  },
  "signaling mediated control": {
    normal: "controls via signaling",
    inverted: "is controlled via signaling by"
  },
  "immune system modulation": {
    normal: "immune-modulates",
    inverted: "is immune-modulated by"
  },
  "vaccine antigen": {
    normal: "acts as a vaccine antigen for",
    inverted: "is targeted by vaccine antigen"
  },
  "post transcriptional regulation": {
    normal: "post-transcriptionally regulates",
    inverted: "is post-transcriptionally regulated by"
  },
  "molecular modification": {
    normal: "molecularly modifies",
    inverted: "is molecularly modified by"
  },
  "phosphorylation": {
    normal: "phosphorylates",
    inverted: "is phosphorylated by"
  },
  "dephosphorylation": {
    normal: "dephosphorylates",
    inverted: "is dephosphorylated by"
  },
  "neddylation": {
    normal: "neddylates",
    inverted: "is neddylated by"
  },
  "deneddylation": {
    normal: "deneddylates",
    inverted: "is deneddylated by"
  },
  "lipidation": {
    normal: "lipidates",
    inverted: "is lipidated by"
  },
  "palmitoylation": {
    normal: "palmitoylates",
    inverted: "is palmitoylated by"
  },
  "myristoylation": {
    normal: "myristoylates",
    inverted: "is myristoylated by"
  },
  "tyrosination": {
    normal: "tyrosinates",
    inverted: "is tyrosinated by"
  },
  "carboxylation": {
    normal: "carboxylates",
    inverted: "is carboxylated by"
  },
  "ubiquitination": {
    normal: "ubiquitinates",
    inverted: "is ubiquitinated by"
  },
  "monoubiquitination": {
    normal: "monoubiquitinates",
    inverted: "is monoubiquitinated by"
  },
  "polyubiquitination": {
    normal: "polyubiquitinates",
    inverted: "is polyubiquitinated by"
  },
  "deubiquitination": {
    normal: "deubiquitinates",
    inverted: "is deubiquitinated by"
  },
  "sulfation": {
    normal: "sulfates",
    inverted: "is sulfated by"
  },
  "reduction": {
    normal: "reduces",
    inverted: "is reduced by"
  },
  "oxidation": {
    normal: "oxidizes",
    inverted: "is oxidized by"
  },
  "acetylation": {
    normal: "acetylates",
    inverted: "is acetylated by"
  },
  "deacetylation": {
    normal: "deacetylates",
    inverted: "is deacetylated by"
  },
  "glycosylation": {
    normal: "glycosylates",
    inverted: "is glycosylated by"
  },
  "deglycosylation": {
    normal: "deglycosylates",
    inverted: "is deglycosylated by"
  },
  "methylation": {
    normal: "methylates",
    inverted: "is methylated by"
  },
  "trimethylation": {
    normal: "trimethylates",
    inverted: "is trimethylated by"
  },
  "demethylation": {
    normal: "demethylates",
    inverted: "is demethylated by"
  },
  "sumoylation": {
    normal: "sumoylates",
    inverted: "is sumoylated by"
  },
  "desumoylation": {
    normal: "desumoylates",
    inverted: "is desumoylated by"
  },
  "ADP-ribosylation": {
    normal: "ADP-ribosylates",
    inverted: "is ADP-ribosylated by"
  },
  "de-ADP-ribosylation": {
    normal: "de-ADP-ribosylates",
    inverted: "is de-ADP-ribosylated by"
  },
  "ampylation": {
    normal: "AMPylates",
    inverted: "is AMPylated by"
  },
  "hydroxylation": {
    normal: "hydroxylates",
    inverted: "is hydroxylated by"
  },
  "s nitrosylation": {
    normal: "S-nitrosylates",
    inverted: "is S-nitrosylated by"
  }
}
