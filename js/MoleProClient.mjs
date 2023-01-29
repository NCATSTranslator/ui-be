'use strict';

import { SendRecvJSON } from "./common.mjs";

export { MoleProClient };

class MoleProClient {
    constructor(origin, queryPath) {
        this.origin = origin;
        this.queryPath = queryPath;
        this.queryURL = `${origin}${queryPath}`;
    }

    static createKGFromNodeIds(attributes, nodeIds) {
        let nodes = {};
        nodeIds.forEach(e => { nodes[e] = {}; });
        let retval = {
            submitter: 'annotate_nodes',
            workflow: [ {
                id: 'annotate_nodes',
                parameters: { attributes: attributes }
            }],
            message: {
                knowledge_graph: {
                    edges: {},
                    nodes: nodes
                }
            }
        };
        return retval;
    }

    async annotateGraph(kg) {
        let res = await SendRecvJSON(this.queryURL, 'POST', {}, kg);
        return res;
    }
}
/*
let c = new MoleProClient('https://molepro-trapi.ci.transltr.io', '/molepro/trapi/v1.3/query');

let kg = MoleProClient.createKGFromNodeIds(['ChEMBL:atc_classification'], ["ATC:A10AB05","CHEMBL.COMPOUND:CHEMBL1201743","CHEMBL.COMPOUND:CHEMBL1566","CHEMBL.COMPOUND:CHEMBL1697838","CHEMBL.COMPOUND:CHEMBL1779710","CHEMBL.COMPOUND:CHEMBL1929387","CHEMBL.COMPOUND:CHEMBL2018096","CHEMBL.COMPOUND:CHEMBL2103841","CHEMBL.COMPOUND:CHEMBL2104391","CHEMBL.COMPOUND:CHEMBL2107869","CHEMBL.COMPOUND:CHEMBL2108336","CHEMBL.COMPOUND:CHEMBL2147777","CHEMBL.COMPOUND:CHEMBL249263","CHEMBL.COMPOUND:CHEMBL3322001","CHEMBL.COMPOUND:CHEMBL3585580","CHEMBL.COMPOUND:CHEMBL3707235","CHEMBL.COMPOUND:CHEMBL383634","CHEMBL.COMPOUND:CHEMBL39736","CHEMBL.COMPOUND:CHEMBL408","CHEMBL.COMPOUND:CHEMBL427216","CHEMBL.COMPOUND:CHEMBL429910","CHEMBL.COMPOUND:CHEMBL4303637","CHEMBL.COMPOUND:CHEMBL448570","CHEMBL.COMPOUND:CHEMBL476960","CHEMBL.COMPOUND:CHEMBL529888","FMA:241992","FMA:264827","FMA:264829","FMA:61799","FMA:67243","FMA:82743","FMA:83365","MESH:D000078790","MESH:D015398","MONDO:0001076","MONDO:0001336","MONDO:0001830","MONDO:0002050","MONDO:0002177","MONDO:0002254","MONDO:0002277","MONDO:0002909","MONDO:0004781","MONDO:0004790","MONDO:0004946","MONDO:0004955","MONDO:0004995","MONDO:0005010","MONDO:0005015","MONDO:0005044","MONDO:0005066","MONDO:0005068","MONDO:0005137","MONDO:0005147","MONDO:0005148","MONDO:0005154","MONDO:0005240","MONDO:0005347","MONDO:0005406","MONDO:0005827","MONDO:0007035","MONDO:0007147","MONDO:0008487","MONDO:0011122","MONDO:0012819","MONDO:0013209","MONDO:0019182","UMLS:C0243077","UMLS:C0277785","UMLS:C0699748","UMLS:C4279587","UniProtKB:P01133","UniProtKB:P01303","UniProtKB:P01375","UniProtKB:P04278","UniProtKB:P04746","UniProtKB:P05019","UniProtKB:P05121","UniProtKB:P05556","UniProtKB:P06213","UniProtKB:P06276","UniProtKB:P07550","UniProtKB:P08069","UniProtKB:P08575","UniProtKB:P08588","UniProtKB:P09038","UniProtKB:P09681","UniProtKB:P10253","UniProtKB:P11166","UniProtKB:P11474","UniProtKB:P13501","UniProtKB:P13866","UniProtKB:P14672","UniProtKB:P15692","UniProtKB:P18031","UniProtKB:P19021","UniProtKB:P19320","UniProtKB:P19793","UniProtKB:P20366","UniProtKB:P23443","UniProtKB:P24385","UniProtKB:P27361","UniProtKB:P27487","UniProtKB:P27986","UniProtKB:P31639","UniProtKB:P35222","UniProtKB:P35568","UniProtKB:P36956","UniProtKB:P37231","UniProtKB:P40763","UniProtKB:P43220","UniProtKB:P48048","UniProtKB:P48357","UniProtKB:P49327","UniProtKB:P49407","UniProtKB:P62508","UniProtKB:P98170","UniProtKB:Q03181","UniProtKB:Q06124","UniProtKB:Q07325","UniProtKB:Q07869","UniProtKB:Q09428","UniProtKB:Q12778","UniProtKB:Q12809","UniProtKB:Q13133","UniProtKB:Q14654","UniProtKB:Q15842","UniProtKB:Q15848","UniProtKB:Q16236","UniProtKB:Q6V1X1","UniProtKB:Q86TI2","UniProtKB:Q96EB6","UniProtKB:Q9NY91","UniProtKB:Q9UBK2","UniProtKB:Q9Y4H2"]);
let res = await c.annotateGraph(kg);
export { c, kg, res };
*/
