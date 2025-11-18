export {
  make_taggable,
  set_tag,
  has_tag,
  get_tags,
  merge_tags,
  Tag,
  TAG_ID
}

import * as cmn from './common.mjs';

function make_taggable(obj) {
  if (!cmn.is_object(obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'make_taggable', `Expected object, got: ${JSON.stringify(obj)}`);
  }
  if (_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'make_taggable', `Object was already made taggable: ${JSON.stringify(obj)}`);
  }
  obj[TAG_ID] = new _Tags();
  return obj;
}

function set_tag(obj, tag) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'set_tag', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  obj[TAG_ID].set_tag(tag);
  return obj;
}


function has_tag(obj, tag) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'has_tags', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  return obj[TAG_ID].has_tag(tag);
}

function get_tags(obj) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'get_tags', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  return obj[TAG_ID].tags();
}

function merge_tags({from_obj, to_obj, overwrite, selector}) {
  if (!_is_taggable(from_obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'merge_tags', `from_obj is not taggable: ${JSON.stringify(from_obj)}`);
  }
  if (!_is_taggable(to_obj)) {
    throw new cmn.DeveloperError('lib/taglib.mjs', 'merge_tags', `to_obj is not taggable: ${JSON.stringify(to_obj)}`);
  }
  if (overwrite !== true) {
    if (selector) {
      selector = (tag) => !has_tag(from_obj, tag) && selector(tag);
    } else {
      selector = (tag) => !has_tag(from_obj, tag);
    }
  }
  if (selector) {
    for (const tag of get_tags(from_obj)) {
      if (selector(tag)) {
        set_tag(to_obj, tag);
      }
    }
  } else {
    for (const tag of get_tags(from_obj)) {
      set_tag(to_obj, tag);
    }
  }
  return to_obj;
}

class Tag {
  constructor({id, name, description}) {
    if (cmn.is_missing(id)) {
      throw new cmn.DeveloperError('lib/taglib.mjs', 'Tag', 'Creating a tag requires an ID');
    }
    this.id = id;
    this.description = Object.freeze({
      name: name ?? '',
      description: description ?? ''
    });
    Object.freeze(this);
  }
}

const TAG_ID = Symbol('__tags__');

function _is_taggable(obj) {
  return obj[TAG_ID] !== undefined;
}

class _Tags {
  constructor() {
    this._data = {};
  }

  tags() {
    return Object.values(this._data);
  }

  tag_ids() {
    return Object.keys(this._data);
  }

  has_tag(tag) {
    return this._data[tag.id] !== undefined;
  }

  set_tag(tag) {
    this._data[tag.id] = tag;
    return this;
  }
}
