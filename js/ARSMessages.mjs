'use strict';

export {isAcceptedQuery, pk} ;

function isAcceptedQuery(msg) {
    return msg.fields.code === 200 || msg.fields.code === 202;
}

function pk(msg) {
    return msg.pk;
}
