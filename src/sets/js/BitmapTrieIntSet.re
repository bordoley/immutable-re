/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
/* FIXME: I'm fairly certain the BitmapTrie functions can be changed to properly sort IntSet */

[%%bs.raw {|
var Curry = require("bs-platform/lib/js/curry.js");
var Sequence = require("../../core/Sequence.js");
var BitmapTrie = require("../../utils/BitmapTrie.js");
var CopyOnWriteArray = require("../../indexed/CopyOnWriteArray.js");
|}];

type t; /* null | number | [bitmap, owner, ...values(t)] */

[@bs.val] external contains: (int, int, t) => bool = "";
[%%bs.raw{|
function contains(_depth, value, _set) {
  while(true) {
    var set = _set;
    var depth = _depth;
    if (set == null) {
      return false;
    } else if (typeof set === "number") {
      return value === set;
    } else {
      var bitmap = set[0];
      var bit = BitmapTrie.bitPos(value, depth);
      var index = BitmapTrie.index(bitmap, bit);
      if (BitmapTrie.containsNode(bitmap, bit)) {
        _set = set[index + 2];
        _depth = depth + 1 | 0;
        continue ;
      } else {
        return false;
      }
    }
  };
}|}];

let empty: t = [%bs.raw{| null |}];

[@bs.val] external reduceWhile : (
      ('acc, t) => bool,
      ('acc, t) => 'acc,
      ('acc, int) => bool,
      ('acc, int) => 'acc,
      'acc,
      t
    ) => 'acc = "";
[%%bs.raw{|
function reduceWhile(levelPredicate, levelReducer, predicate, f, acc, set) {
  if (set == null) {
    return acc;
  } else if (typeof set === "number") {
    if (Curry._2(predicate, acc, set)) {
      return Curry._2(f, acc, set);
    } else {
      return acc;
    }
  } else {
    return CopyOnWriteArray.reduce(2, levelPredicate, levelReducer, acc, set);
  }
}|}];

let reduce =
    (~while_ as predicate: ('acc, int) => bool, f: ('acc, int) => 'acc, acc: 'acc, set: t)
    : 'acc =>
  if (predicate === Functions.alwaysTrue2) {
    let rec levelReducer = (acc, node) =>
      reduceWhile(Functions.alwaysTrue2, levelReducer, Functions.alwaysTrue2, f, acc, node);
    levelReducer(acc, set)
  } else {
    let shouldContinue = ref(true);
    let predicate = (acc, v) =>
      if (shouldContinue^) {
        let result = predicate(acc, v);
        shouldContinue := result;
        result
      } else {
        false
      };
    let levelPredicate = (_, _) => shouldContinue^;
    let rec levelReducer = (acc, node) =>
      reduceWhile(levelPredicate, levelReducer, predicate, f, acc, node);
    levelReducer(acc, set)
  };

[@bs.val] external toSequence: t => Sequence.t(int) = "";
[%%bs.raw{|
function toSequence(set) {
  if (set == null) {
    return Sequence.empty();
  } else if (typeof set === "number") {
    return Sequence.$$return(set);
  } else {
    return Sequence.flatMap(toSequence, CopyOnWriteArray.toSequence(2, set));
  }
}|}];

type updateLevelNode = (Transient.Owner.t, int, t, t) => t;

[@bs.val] external updateLevelNodePersistent : updateLevelNode = "";
[%%bs.raw{|
function updateLevelNodePersistent(_, index, childNode, set) {
  return CopyOnWriteArray.update(index+2, childNode, set);
}|}];

[@bs.val] external updateLevelNodeTransient: updateLevelNode = "";
[%%bs.raw{|
function updateLevelNodeTransient(owner, index, childNode, set) {
  if (set[1] === owner) {
    set[index+2] = childNode;
    return set;
  } else {
    return updateLevelNodePersistent(_, index, childNode, set)
  }
}|}];

[@bs.val] external add: (updateLevelNode, Transient.Owner.t, int, int, t) => t = "";
[%%bs.raw{|
function add(_updateLevelNode, owner, depth, value, _set) {
  while(true) {
    var set = _set;
    var updateLevelNode = _updateLevelNode;
    if (set == null) {
      return value;
    } else if (typeof set === "number") {
      var entryValue = set;
      if (value === entryValue) {
        return set;
      } else {
        var bitmap = BitmapTrie.bitPos(entryValue, depth);
        _set = [bitmap, owner, entryValue]
        _updateLevelNode = updateLevelNodeTransient;
        continue ;

      }
    } else {
      var bitmap$1 = set[0];
      var bit = BitmapTrie.bitPos(value, depth);
      var index = BitmapTrie.index(bitmap$1, bit);
      if (BitmapTrie.containsNode(bitmap$1, bit)) {
        var childNode = set[index+2];
        var newChildNode = add(updateLevelNode, owner, depth + 1 | 0, value, childNode);
        if (childNode === newChildNode) {
          return set;
        } else {
          return Curry._4(updateLevelNode, owner, index, newChildNode, set);
        }
      } else {
        var newSet = CopyOnWriteArray.insertAt(index+2, value, set);
        newSet[0] = bitmap$1 | bit;
        newSet[1] = owner;
        return newSet;
      }
    }
  };
}|}];

[@bs.val] external remove: (updateLevelNode, Transient.Owner.t, int, int, t) => t = "";
[%%bs.raw{|
function remove(updateLevelNode, owner, depth, value, set) {
  if (set == null) {
    return set;
  } else if (typeof set === "number") {
    if (value === set) {
      return null;
    } else {
      return set;
    }
  } else {
    var bitmap = set[0];
    var bit = BitmapTrie.bitPos(value, depth);
    var index = BitmapTrie.index(bitmap, bit);
    if (BitmapTrie.containsNode(bitmap, bit)) {
      var childNode = set[index + 2];
      var newChildNode = remove(updateLevelNode, owner, depth + 1 | 0, value, childNode);
      if (newChildNode === childNode) {
        return set;
      } else if (newChildNode) {
        return Curry._4(updateLevelNode, owner, index, newChildNode, set);
      } else {
        var newSet = CopyOnWriteArray.removeAt(index+2, set);
        newSet[0] = bitmap ^ bit;
        newSet[1] = owner;
        if (CopyOnWriteArray.count(newSet) > 2) {
          return newSet;
        } else {
          return null;
        }
      }
    } else {
      return set;
    }
  }
}|}];

let add = add;
let contains = contains;
let remove = remove;
let toSequence = toSequence;
let updateLevelNodePersistent = updateLevelNodePersistent;
let updateLevelNodeTransient = updateLevelNodeTransient;
