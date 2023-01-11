'use strict'

import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';
import * as evd from '../evidence.mjs';
import * as trapi from '../trapi.mjs';

function assertInterface(obj, keys)
{
  assert.equal(Object.keys(obj).length, keys.length);
  keys.forEach((key) =>
    {
      assert.ok(cmn.jsonHasKey(obj, key));
    });
}

function assertArrayStructure(arr, vals)
{
  assert.equal(arr.length, vals.length);
  vals.forEach((val, i) =>
    {
      assert.equal(arr[i], val);
    });
}

function assertArrayValues(actual, expected)
{
  const ac = [...actual].sort();
  const ec = [...expected].sort();

  assert.deepStrictEqual(ac, ec);
}

function assertPathStructure(path, nodes, edges)
{
  assert.equal(path.length % 2, 1);

  let nodeCount = 0;
  let edgeCount = 0;
  let edge;
  let even = true;
  path.forEach((objKey, i) => {
    if (even && cmn.jsonHasKey(nodes, objKey))
    {
      nodeCount += 1;
    }
    else if (!even && cmn.jsonHasKey(edges, objKey))
    {
      edge = cmn.jsonGet(edges, objKey);
      assert.equal(edge.subject, path[i-1]);
      assert.equal(edge.object, path[i+1]);
      edgeCount += 1;
    }
    else {
      assert.fail(`The object is not in the correct position\n\tposition: ${i}\n\tobject: ${obj}`);
    }

    even = !even;
  });

  assert.equal(nodeCount - 1, edgeCount);
}

function assertPathEndpoints(path, result)
{
  assert.equal(path[0], result.subject);
  assert.equal(path[path.length-1], result.object);
}

function assertAraCounts(objs, expectedCounts)
{
  assertArrayValues(objs.map((o) => { return o.aras.length; }), expectedCounts);
}

describe('makeMetadataObject', () =>
  {
    it('Should return a metadata object when given a qid and agents', () =>
      {
        const mo = trapi.makeMetadataObject('123', ['a', 'b']);
        assert.deepEqual(mo, {'qid': '123', 'aras': ['a', 'b']});
      });

    it('Should throw if not given a qid or agents', () =>
      {
        assert.throws(() => { return trapi.makeMetadataObject('123'); });
        assert.throws(() => { return trapi.makeMetadataObject(undefined, ['a']); });
      });

    it('Should throw if the qid is not a string', () =>
      {
        assert.throws(() => { return trapi.makeMetadataObject(123, ['a']); });
      });

    it('Should throw if the agents are not an array', () =>
      {
        assert.throws(() => { return trapi.makeMetadatObject('123', 6); });
      });
  });

describe('queryToCreativeQuery', () =>
  {
    it('Should generate a valid TRAPI creative query given a valid query object', () =>
      {
        const curie = 'AWESOME:123';
        const diseaseQuery = {
          'message': {
            'query_graph': {
              'nodes': {
                'drug': {
                  'categories': ['biolink:ChemicalEntity']
                },
                'disease': {
                  'ids': [curie],
                  'categories': ['biolink:Disease']
                }
              },
              'edges': {
                't_edge': {
                  'subject': 'drug',
                  'object': 'disease',
                  'predicates': ['biolink:treats'],
                  'knowledge_type': 'inferred'
                }
              }
            }
          }
        };
        const diseaseObject = {'type': 'disease', 'curie': curie, 'direction': null};
        assert.deepEqual(trapi.queryToCreativeQuery(diseaseObject), diseaseQuery);

        const geneQuery = {
          "message": {
            "query_graph": {
              "nodes": {
                "gene": {
                  "categories": ["biolink:Gene"],
                  "ids": [curie]
                },
                "chemical": {
                  "categories": ["biolink:ChemicalEntity"]
                }
              },
              "edges": {
                "t_edge": {
                  "object": "gene",
                  "subject": "chemical",
                  "predicates": ["biolink:affects"],
                  "knowledge_type": "inferred",
                  "qualifier_constraints": [
                    {
                      "qualifier_set": [
                        {
                          "qualifier_type_id": "biolink:object_aspect_qualifier",
                          "qualifier_value": "activity_or_abundance"
                        },
                        {
                          "qualifier_type_id": "biolink:object_direction_qualifier",
                          "qualifier_value": "increased"
                        }
                      ]
                    }
                  ]
                }
              }
            }
          }
        };
        const geneObject = {'type': 'gene', 'curie': curie, 'direction': 'increased'};
        assert.deepEqual(trapi.queryToCreativeQuery(geneObject), geneQuery);

      });

    it('Should throw if the query object is malformed', () =>
      {
        assert.throws(() => { return trapi.queryToCreativeQuery({}); });
        assert.throws(() => { return trapi.queryToCreativeQuery({'abc': 'AWESOME:123'}); });
        assert.throws(() => { return trapi.queryToCreativeQuery({'disease': 'AWESOME:123'}); });
        assert.throws(() => { return trapi.queryToCreativeQuery({'type': 'disease', 'curie': 'AWESOME:123'}); });
        assert.throws(() => { return trapi.queryToCreativeQuery({'type': 'disease', 'direction': 'increased'}); });
        assert.throws(() => { return trapi.queryToCreativeQuery({'curie': 'AWESOME:123', 'direction': 'increased'}); });
      });

    it('Should throw if the query object is not an object', () =>
      {
        assert.throws(() => { return trapi.queryToCreativeQuery(undefined); });
        assert.throws(() => { return trapi.queryToCreativeQuery('AWESOME:123'); });
        assert.throws(() => { return trapi.queryToCreativeQuery(['disease', 'AWESOME:123']); });
        assert.throws(() => { return trapi.queryToCreativeQuery('{"disease": "AWESOME:123"}'); });
      });

    it('Should throw if the query object does not have a valid type', () =>
      {
        assert.throws(() => { return trapi.queryToCreativeQuery({'type': 'fruit', 'curie': 'Apple', 'direction': 'red'}); });
      });
  });

describe('creativeAnswersToSummary', () =>
  {
    it('Should return an empty summary for an empty answers array', async () =>
      {
        const emptySummary = await cmn.readJson('test/data/trapi/empty-summary.json');
        assert.deepEqual(trapi.creativeAnswersToSummary('AWESOME:123', []), emptySummary);
      });

    // Under test
    // * The summary has the correct structure
    // * The result has 1 path with the correct structure
    // * The node rules and edge rules populate the nodes and edges objects
    // * That the inverse edge is generated
    // * Publications are aggregated and populated
    // * All objects are marked with the correct ARA
    describe('Processing one result with one path', () =>
      {
        let onePathResult;
        let qid;
        let summary;
        let expectedDrugName;
        before(async () =>
          {
            qid = 'AWESOME:123';
            onePathResult = await cmn.readJson('test/data/trapi/one-result-one-path.json');;
            summary = trapi.creativeAnswersToSummary(qid, onePathResult);
            expectedDrugName = 'simvastatin';
          });

        it('Should return a summary object that adheres to the summary interface' , () =>
          {
            assertInterface(summary, ['meta', 'results', 'paths', 'nodes', 'edges', 'publications']);
          });

        describe('The summary object', () =>
          {
            it('Should have a metadata object that adheres to the metadata interface', () =>
              {
                assertInterface(summary.meta, ['qid', 'aras']);
              });

            it('Should have 1 result', () =>
              {
                assert.equal(summary.results.length, 1);
              });

            it('Should have 1 path', () =>
              {
                assert.equal(Object.keys(summary.paths).length, 1);
              });

            it('Should have 2 nodes', () =>
              {
                assert.equal(Object.keys(summary.nodes).length, 2);
              });

            it('Should have 2 edges', () =>
              {
                assert.equal(Object.keys(summary.edges).length, 2);
              });

            it('Should have 7 publications', () =>
              {
                assert.equal(Object.keys(summary.publications).length, 7);
              });

            describe('The metadata object', () =>
              {
                let metadata;
                before(() => { metadata = summary.meta; });

                it('Should have the supplied QID', () =>
                  {
                    assert.equal(metadata.qid, qid);
                  });

                it('Should have a single agent reporting named "test"', () =>
                  {
                    assertArrayStructure(metadata.aras, ['test']);
                  });
              });

            describe('The result', () =>
              {
                let result;
                before(() => { result = summary.results[0]; });

                it('Should have a single path that matches the summary path key', () =>
                  {
                    assert.equal(result.paths.length, 1);
                    assert.ok(cmn.jsonHasKey(summary.paths, result.paths[0]));
                  });

                it('Should have a subject that is in the summary nodes', () =>
                  {
                    assert.ok(cmn.jsonHasKey(summary.nodes, result.subject));
                  });

                it('Should have an object that is in the summary nodes', () =>
                  {
                    assert.ok(cmn.jsonHasKey(summary.nodes, result.object));
                  });

                it('Should have the drug named simvastatin', () =>
                  {
                    assert.equal(result.drug_name, expectedDrugName);
                  });
              });

            describe('The path', () =>
              {
                let aras;
                let path;
                before(() =>
                  {
                    const paths = summary.paths;
                    const pathObj = paths[Object.keys(paths)[0]];
                    aras = pathObj.aras;
                    path = pathObj.subgraph;
                  });

                it('Should be reported by a single ARA named "test"', () =>
                  {
                    assertArrayStructure(aras, ['test']);
                  });

                it('Should have a length of 3', () =>
                  {
                    assert.equal(path.length, 3);
                  });

                it('Should alternate nodes and edges starting and ending with a node', () =>
                  {
                    assertPathStructure(path, summary.nodes, summary.edges);
                  });

                it('Should be have the first and last nodes be the subject and object of the result', () =>
                  {
                    assertPathEndpoints(path, summary.results[0]);
                  });
              });

            describe('The nodes object', () =>
              {
                let nodes;
                before(() => {
                  nodes = Object.values(summary.nodes);
                });

                it('Should have all nodes adhere to the node interface', () =>
                  {
                    nodes.forEach((node) =>
                      {
                        assertInterface(node, ['names', 'aras', 'types', 'curies', 'fda_info',
                                               'descriptions', 'synonyms', 'same_as', 'iri_types']);
                      });
                  });

                describe('The drug object', () =>
                  {
                    let drugCurie;
                    let drug;
                    let expectedType;
                    before(() => {
                      drugCurie = 'PUBCHEM.COMPOUND:54454';
                      drug = summary.nodes[drugCurie];
                      expectedType = bl.tagBiolink('SmallMolecule');
                    });

                    it('Should have 1 ARA', () =>
                      {
                        assertArrayStructure(drug.aras, ['test']);
                      });

                    it('Should have 1 name', () =>
                      {
                        assertArrayStructure(drug.names, [expectedDrugName]);
                      });

                    it('Should have 1 type', () =>
                      {
                        assertArrayStructure(drug.types, [bl.tagBiolink('SmallMolecule')]);
                      });

                    it('Should have 1 curie', () =>
                      {
                        assertArrayStructure(drug.curies, [drugCurie]);
                      });

                    it('Should have the expected FDA info object', () =>
                      {
                        assertInterface(drug.fda_info, ['highest_fda_approval_status']);
                        assert.equal(drug.fda_info.highest_fda_approval_status, 'FDA Approval');
                      });

                    it('Should have 1 description', () =>
                      {
                        assertArrayStructure(drug.descriptions, ['Example']);
                      });

                    it('Should have 3 synonyms', () =>
                      {
                        assertArrayStructure(drug.synonyms, ['A', 'B', 'C']);
                      });

                    it('Should have 1 like nodes', () =>
                      {
                        assertArrayStructure(drug.same_as, ['A']);
                      });

                    it('Should have 1 IRI types', () =>
                      {
                        assertArrayStructure(drug.iri_types, ['A']);
                      });
                  });

                describe('The disease object', () =>
                  {
                    let diseaseCurie;
                    let disease;
                    let expectedType;
                    before(() => {
                      diseaseCurie = 'HP:0003124';
                      disease = summary.nodes[diseaseCurie];
                      expectedType = bl.tagBiolink('Disease');
                    });

                    it('Should have 1 ARA', () =>
                      {
                        assertArrayStructure(disease.aras, ['test']);
                      });

                    it('Should have 1 name', () =>
                      {
                        assertArrayStructure(disease.names, [diseaseCurie]);
                      });

                    it('Should have 1 type', () =>
                      {
                        assertArrayStructure(disease.types, [expectedType]);
                      });

                    it('Should have 1 curie', () =>
                      {
                        assertArrayStructure(disease.curies, [diseaseCurie]);
                      });

                    it('Should have a null FDA info', () =>
                      {
                        assert.equal(disease.fda_info, null);
                      });

                    it('Should have no descriptions', () =>
                      {
                        assert.equal(disease.descriptions.length, 0);
                      });

                    it('Should have no synonyms', () =>
                      {
                        assert.equal(disease.synonyms.length, 0);
                      });

                    it('Should have no like nodes', () =>
                      {
                        assert.equal(disease.same_as.length, 0);
                      });

                    it('Should have no IRI types', () =>
                      {
                        assert.equal(disease.iri_types.length, 0);
                      });
                  });
              });

            describe('The edges object', () =>
              {
                let edges;
                let e1;
                let e2;
                before(() => {
                  edges = Object.values(summary.edges);
                  e1 = edges[0];
                  e2 = edges[1];
                });

                it('Should have all edges adhere to the edge interface', () =>
                  {
                    edges.forEach((edge) =>
                      {
                        assertInterface(edge, ['predicate', 'iri_types', 'aras', 'subject', 'object', 'publications']);
                      });
                  });

                it('Should have the 2 edges be inverses', () =>
                  {
                    assert.equal(e1.predicate, bl.invertBiolinkPredicate(e2.predicate));
                    assert.deepEqual(e1.iri_types, e2.iri_types);
                    assert.deepEqual(e1.aras, e2.aras);
                    assert.equal(e1.subject, e2.object);
                    assert.equal(e1.object, e2.subject);
                    assert.deepEqual(e1.publications, e2.publications);
                  });

                it('Should only have edges with biolink compliant predicate', () =>
                  {
                    edges.forEach((edge) =>
                      {
                        assert.ok(bl.isBiolinkPredicate(edge.predicate));
                      });
                  });

                it('Should have the first edge contain a single ara of "test"', () =>
                  {
                    assert.equal(e1.aras.length, 1);
                    assert.equal(e1.aras[0], 'test');
                  });

                it('Should have the first edge have subjects and objects in the summary nodes object', () =>
                  {
                    assert.ok(cmn.jsonHasKey(summary.nodes, e1.subject));
                    assert.ok(cmn.jsonHasKey(summary.nodes, e1.object));
                  });

                it('Should have the first edge contain 7 valid publications', () =>
                  {
                    assert.equal(e1.publications.length, 7);
                    e1.publications.forEach((p) =>
                      {
                        assert.ok(evd.isValidId(p));
                      });
                  });

              });

            describe('The publications structure', () =>
              {
                let publications;
                before(() => { publications = Object.values(summary.publications); });

                it('Should have all publications adhere to the publications interface', () =>
                  {
                    publications.forEach((p) =>
                      {
                        assertInterface(p, ['type', 'url', 'snippet', 'pubdate']);
                      });
                  });

                it('Should have 3 PMIDs, 2 NCTs, and 2 DOIs', () =>
                  {
                    const pubTypes = {
                      'PMID': 0,
                      'NCT':  0,
                      'DOI':  0
                    };
                    publications.forEach((p) =>
                      {
                        pubTypes[p.type] += 1;
                      });

                    assert.equal(pubTypes.PMID, 3);
                    assert.equal(pubTypes.NCT, 2);
                    assert.equal(pubTypes.DOI, 2);
                  });

                it('Should have 2 publications with a sentence and publication date', () =>
                  {
                    const expectedPmids = {
                      'PMID:11152376': {
                        'snippet': 'A',
                        'pubdate': '2001 Jan'
                      },
                      'PMID:11691537': {
                        'snippet': 'B',
                        'pubdate': '2001 Nov 01'
                      }
                    };

                    const ps = summary.publications;
                    let expectedSentence;
                    let expectedPubdate;

                    Object.keys(ps).forEach((k) =>
                      {
                        if (cmn.jsonHasKey(expectedPmids, k))
                        {
                          expectedSentence = expectedPmids[k].snippet;
                          expectedPubdate = expectedPmids[k].pubdate;
                        }
                        else
                        {
                          expectedSentence = null;
                          expectedPubdate = null;
                        }

                        assert.ok(ps[k].url !== null);
                        assert.equal(expectedSentence, ps[k].snippet);
                        assert.equal(expectedPubdate, ps[k].pubdate);
                      });
                  });
              });
          });
      });

    // Under test
    // * Multiple paths are generated from one result
    // * Paths are not generated for path lengths greater than the max
    // * Nodes are always generated
    // * Edges are generated for missing non-critical information
    // * Edges are not generated for missing critical information
    describe('Processing one result with multiple paths', () =>
      {
        let multiPathResult;
        let summary;
        before(async () =>
          {
            const qid = 'AWESOME:123';
            multiPathResult = await cmn.readJson('test/data/trapi/one-result-multi-path.json');;
            summary = trapi.creativeAnswersToSummary(qid, multiPathResult);
          });

        describe('The summary object', () =>
          {
            it('Should have 1 result', () =>
              {
                assert.equal(summary.results.length, 1);
              });

            it('Should have 3 paths', () =>
              {
                assert.equal(Object.keys(summary.paths).length, 3);
              });

            it('Should have 6 nodes', () =>
              {
                assert.equal(Object.keys(summary.nodes).length, 6);
              });

            it('Should have 16 edges', () =>
              {
                assert.equal(Object.keys(summary.edges).length, 16);
              });

            describe('The result', () =>
              {
                let result;
                before(() => { result = summary.results[0]; });

                it('Should have 3 paths that match the summary path keys', () =>
                  {
                    assert.equal(result.paths.length, 3);
                    result.paths.forEach((pathKey) =>
                      {
                        assert.ok(cmn.jsonHasKey(summary.paths, pathKey));
                      });
                  });
              });

            describe('The paths object', () =>
              {
                let paths;
                before(() =>
                  {
                    const pathObjs = Object.values(summary.paths);
                    paths = pathObjs.map((obj) => { return obj.subgraph; });
                  });

                it('Should have all paths be the proper length', () =>
                  {
                    const pathLengths = paths.map((p) => { return p.length; });
                    pathLengths.sort((a, b) => { return a - b; });
                    assert.deepStrictEqual(pathLengths, [3, 5, 7]);
                  });

                it('Should have all paths have the proper structure', () =>
                  {
                    paths.forEach((p) =>
                      {
                        assertPathStructure(p, summary.nodes, summary.edges);
                      });
                  });

                it('Should have all paths link to the result', () =>
                  {
                    paths.forEach((p) =>
                      {
                        assertPathEndpoints(p, summary.results[0]);
                      });
                  });
              });
          });
      });

    // Under test
    // Multiple answer results are included in the summary
    // Repeats of the same information across multiple answers is not included
    // Extra information for the same result, node, etc. previously processed is added to the summary
    describe('Processing multiple answers', () =>
      {
        let multiAnswers;
        let summary;
        before(async () =>
          {
            const qid = 'AWESOME:123';
            multiAnswers = await cmn.readJson('test/data/trapi/multi-answers.json');
            summary = trapi.creativeAnswersToSummary(qid, multiAnswers);
          });

        describe('The summary object', () =>
          {
            it('Should have 3 results', () =>
              {
                assert.equal(summary.results.length, 3);
              });

            it('Should have 3 paths', () =>
              {
                assert.equal(Object.keys(summary.paths).length, 3);
              });

            it('Should have 6 nodes', () =>
              {
                assert.equal(Object.keys(summary.nodes).length, 6);
              });

            it('Should have 6 edges', () =>
              {
                assert.equal(Object.keys(summary.edges).length, 6);
              });

            it('Should have 2 publications', () =>
              {
                assert.equal(Object.keys(summary.publications).length, 2);
              });

            describe('The metadata object', () =>
              {
                let metadata;
                before(() => { metadata = summary.meta; });

                it('Should have 2 responding ARAs', () =>
                  {
                    assert.equal(metadata.aras.length, 2);
                    assert.ok(metadata.aras.includes('test1'));
                    assert.ok(metadata.aras.includes('test2'));
                  });
              });

            describe('The results object', () =>
              {
                let results;
                before(() => { results = summary.results; });

                it('Should contain the 3 expected results', () =>
                  {
                    const actualDrugNames = results.map((res) => { return res.drug_name; });
                    const expectedDrugNames = ['panacea', 'DrugA', 'drug1'];
                    assertArrayValues(actualDrugNames, expectedDrugNames);

                    const actualSubjects = results.map((res) => { return res.subject; });
                    const expectedSubjects = ['panacea', 'nc1', 'drug1'];
                    assertArrayValues(actualSubjects, expectedSubjects);

                    const actualObjects = results.map((res) => { return res.object; });
                    const expectedObjects = ['everything', 'nc2', 'disease1'];
                    assertArrayValues(actualObjects, expectedObjects);

                    results.forEach((res) =>
                      {
                        assert.equal(res.paths.length, 1);
                      });

                    results.map((res) => { return res.paths; }).forEach((p) =>
                      {
                        assert.ok(cmn.jsonHasKey(summary.paths, p));
                      });
                  });
              });

            describe('The paths object', () =>
              {
                let paths;
                before(() => { paths = Object.values(summary.paths); });

                it('Should have 1 path with 2 contributing ARAs', () =>
                  {
                    assertAraCounts(paths, [1, 1, 2]);
                  });
              });

            describe('The nodes object', () =>
              {
                let nodes;
                before(() => { nodes = Object.values(summary.nodes); });

                it('Should have 2 nodes with 2 contributing ARAs', () =>
                  {
                    assertAraCounts(nodes, [1, 1, 1, 1, 2, 2]);
                  });

                describe('The merged node', () =>
                  {
                    let node;
                    before(() => { node = cmn.jsonGet(summary.nodes, 'nc1'); });

                    it('Should have 2 contributing ARAs', () =>
                      {
                        assert.equal(node.aras.length, 2);
                        assertArrayValues(node.aras, ['test1', 'test2']);
                      });

                    it('Should have 1 name', () =>
                      {
                        assert.equal(node.names.length, 1);
                        assert.equal(node.names[0], 'DrugA');
                      });

                    it('Should have 2 types', () =>
                      {
                        assert.equal(node.types.length, 2);
                        assertArrayValues(node.types, ['biolink:SmallMolecule', 'biolink:NamedEntity']);
                      });

                    it('Should have 3 curies', () =>
                      {
                        assert.equal(node.curies.length, 3);
                        assertArrayValues(node.curies, ['TEST:001', 'TEST:002', 'TEST:003']);
                      });

                    it('Should have no FDA info', () =>
                      {
                        assert.equal(node.fda_info, null);
                      });

                    it('Should have 1 description', () =>
                      {
                        assert.equal(node.descriptions.length, 1);
                        assert.equal(node.descriptions[0], 'A made up drug');
                      });

                    it('Should have 1 synonyms', () =>
                      {
                        assert.equal(node.synonyms.length, 1);
                        assert.equal(node.synonyms[0], 'DrugX');
                      });

                    it('Should have 2 same_as values', () =>
                      {
                        assert.equal(node.same_as.length, 2);
                        assertArrayValues(node.same_as, ['DrugZ', 'DrugY']);
                      });

                    it('Should have 1 iri_type', () =>
                      {
                        assert.equal(node.iri_types.length, 1);
                        assert.equal(node.iri_types[0], 'TEST:007');
                      });
                  });
              });

            describe('The edges object', () =>
              {
                let edges;
                before(() => { edges = Object.values(summary.edges); });

                it('Should have 2 edges with 2 contributing ARAs', () =>
                  {
                    assertAraCounts(edges, [1, 1, 1, 1, 2, 2]);
                  });

                describe('The merged edge', () =>
                  {
                    let edge;
                    before(() =>
                      {
                        edge = edges.filter((e) =>
                          {
                            return e.aras.length === 2 && e.predicate === 'treats';
                          })[0];
                      });

                    it('Should have the appropriate ARAs', () =>
                      {
                        assertArrayValues(edge.aras, ['test1', 'test2']);
                      });

                    it('Should have the correct subject', () =>
                      {
                        assert.equal(edge.subject, 'nc1');
                      });

                    it('Should have the correct object', () =>
                      {
                        assert.equal(edge.object, 'nc2');
                      });

                    it('Should have 2 publications', () =>
                      {
                        assert.equal(edge.publications.length, 2);
                        assertArrayValues(edge.publications, ['PMID:123', 'PMID:124']);
                      });

                    it('Should have 2 iri_types', () =>
                      {
                        assert.equal(edge.iri_types.length, 2);
                        assertArrayValues(edge.iri_types, ['ETEST:001', 'ETEST:002']);
                      });
                  });
              });

            describe('The publications object', () =>
              {
                let publications;
                before(() => { publications = summary.publications; });

                it('Should have the expected publication data', () =>
                  {
                    const expectedPubs = {
                      'PMID:123': {
                        'snippet': '123',
                        'pubdate': 'ABC',
                        'type': 'PMID',
                        'url': 'https://pubmed.ncbi.nlm.nih.gov/123'
                      },
                      'PMID:124': {
                        'snippet': '124',
                        'pubdate': 'ABD',
                        'type': 'PMID',
                        'url': 'https://pubmed.ncbi.nlm.nih.gov/124'
                      }
                    };

                    assert.deepStrictEqual(summary.publications, expectedPubs);
                  });
              });
          });
      });
  });
