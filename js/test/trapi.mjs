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

describe('diseaseToCreativeQuery', () =>
  {
    it('Should generate a valid TRAPI creative query given a disease object', () =>
      {
        const curie = 'AWESOME:123';
        const query = {
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
                'treats': {
                  'subject': 'drug',
                  'object': 'disease',
                  'predicates': ['biolink:treats'],
                  'knowledge_type': 'inferred'
                }
              }
            }
          }
        };
        const diseaseObject = {'disease': curie};

        assert.deepEqual(trapi.diseaseToCreativeQuery(diseaseObject), query);
      });

    it('Should throw if the disease object is malformed', () =>
      {
        assert.throws(() => { return trapi.diseaseToCreativeQuery({}); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery({'abc': 'AWESOME:123'}); });
      });

    it('Should throw if the disease object is not an object', () =>
      {
        assert.throws(() => { return trapi.diseaseToCreativeQuery(undefined); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery('AWESOME:123'); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery(['disease', 'AWESOME:123']); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery('{"disease": "AWESOME:123"}'); });
      });
  });

describe('creativeAnswersToSummary', () =>
  {
    it('Should return an empty summary for an empty answers array', async () =>
      {
        const emptySummary = await cmn.readJson('test/data/trapi/empty-summary.json');
        assert.deepEqual(trapi.creativeAnswersToSummary('AWESOME:123', []), emptySummary);
      });

    describe('Processing a single one hop result', () =>
      {
        let oneHopResult;
        let qid;
        let summary;
        let expectedDrugName;
        before(async () =>
          {
            qid = 'AWESOME:123';
            oneHopResult = await cmn.readJson('test/data/trapi/one-result-one-hop.json');;
            summary = trapi.creativeAnswersToSummary(qid, oneHopResult);
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

                it('Should have 2 nodes and 1 edge that are in the summary nodes and edges', () =>
                  {
                    assert.equal(path.length, 3);
                    let nodeCount = 0;
                    let edgeCount = 0;
                    path.forEach((obj) => {
                      if (cmn.jsonHasKey(summary.nodes, obj))
                      {
                        nodeCount += 1;
                      }
                      else if (cmn.jsonHasKey(summary.edges, obj))
                      {
                        edgeCount += 1;
                      }
                      else
                      {
                        assert.fail(`An object in a path was not part of the summary: ${obj}`);
                      }
                    });

                    assert.equal(nodeCount, 2);
                    assert.equal(edgeCount, 1);
                  });

                it('Should alternate nodes and edges starting and ending with a node', () =>
                  {
                    let even = true;
                    path.forEach((obj) => {
                      if (even && !cmn.jsonHasKey(summary.nodes, obj))
                      {
                        assert.fail(`The object should be a node. Got: ${obj}`);
                      }
                      else if (!even && !cmn.jsonHasKey(summary.edges, obj))
                      {
                        assert.fail(`The object should be an edge. Got: ${obj}`);
                      }
                      even = !even;
                    });

                    assert.equal(path.length % 2, 1);
                  });

                it('Should have the first node be the subject of the result', () =>
                  {
                    assert.equal(path[0], summary.results[0].subject);
                  });

                it('Should have the last node be the object of the the result', () =>
                  {
                    assert.equal(path[path.length-1], summary.results[0].object);
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
                      console.log(drug);
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
                        assertInterface(edge, ['predicates', 'iri_type', 'aras', 'subject', 'object', 'publications']);
                      });
                  });

                it('Should have the 2 edges be inverses', () =>
                  {
                    assert.equal(e1.predicates[0], bl.invertBiolinkPredicate(e2.predicates[0]));
                    assert.deepEqual(e1.iri_type, e2.iri_type);
                    assert.deepEqual(e1.aras, e2.aras);
                    assert.equal(e1.subject, e2.object);
                    assert.equal(e1.object, e2.subject);
                    assert.deepEqual(e1.publications, e2.publications);
                  });

                it('Should only have edges with biolink compliant predicates', () =>
                  {
                    edges.forEach((edge) =>
                      {
                        edge.predicates.forEach((predicate) =>
                          {
                            assert.ok(bl.isBiolinkPredicate(predicate));
                          });
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
                        'sentence': 'A',
                        'pubdate': '2001 Jan'
                      },
                      'PMID:11691537': {
                        'sentence': 'B',
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
                          expectedSentence = ps[k].sentence;
                          expectedPubdate = ps[k].pubdate;
                        }
                        else
                        {
                          expectedSentence = null;
                          expectedPubdate = null;
                        }

                        assert.ok(ps[k].url !== null);
                        assert.equal(expectedSentence, ps[k].sentence);
                        assert.equal(expectedPubdate, ps[k].pubdate);
                      });
                  });
              });
          });
      });
  });
