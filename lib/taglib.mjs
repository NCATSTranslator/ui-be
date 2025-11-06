export {
  make_taggable,
  has_tag,
  get_tags,
  set_tag,
  merge_tags,
  Tag
}

import * as cmn from './common.mjs';

function make_taggable(obj) {
  if (!cmn.is_object(obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'make_taggable', `Expected object, got: ${obj}`);
  }
  if (_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'make_taggable', `Object was already made taggable: ${obj}`);
  }
  obj.__tags__ = new _Tags();
  return obj;
}

function has_tag(obj, tag) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'has_tags', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  return obj.__tags__.has_tag(tag);
}

function get_tags(obj) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'get_tags', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  return obj.__tags__.tags();
}

function set_tag(obj, tag) {
  if (!_is_taggable(obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'set_tag', `obj is not taggable: ${JSON.stringify(obj)}`);
  }
  return obj.__tags__.set_tag(tag);
}

function merge_tags({from_obj, to_obj, overwrite, selector}) {
  if (!_is_taggable(from_obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'merge_tags', `from_obj is not taggable: ${JSON.stringify(from_obj)}`);
  }
  if (!_is_taggable(to_obj)) {
    throw new cmn.DeveloperError('lib/tags.mjs', 'merge_tags', `to_obj is not taggable: ${JSON.stringify(to_obj)}`);
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
      throw new cmn.DeveloperError('lib/tags.mjs', 'Tag', 'Creating a tag requires an ID');
    }
    this.id = id;
    this.description = Object.freeze({
      name: name ?? '',
      description: description ?? ''
    });
    Object.freeze(this);
  }
}

function _is_taggable(obj) {
  return obj.__tags__ !== undefined;
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
    this._data[tag.label] = tag;
    return this;
  }
}

