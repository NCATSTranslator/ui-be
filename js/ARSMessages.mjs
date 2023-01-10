'use strict';

export { isAcceptedQuery, msgId, parentQueryRunning,parentQueryDone }

function isAcceptedQuery(msg) {
    return msg.fields.code === 200 || msg.fields.code === 202;
}

function msgId(msg) {
    if (msg.hasOwnProperty('message')) {
        return msg.message;
    } else if (msg.hasOwnProperty('pk')) {
        return msg.pk;
    } else {
        return null;
    }
}

function parentQueryRunning(msg) {
    msg.status === 'Running';
}

function parentQueryDone(msg) {
    msg.status === 'Done';
}
