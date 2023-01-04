'use strict';

export {isRunningQuery, pk} ;

function isRunningQuery(msg) {
    return msg.fields.code === 200 || msg.fields.code === 202;
}

function pk(msg) {
    return msg.pk;
}
