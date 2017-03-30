/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = array 'a;

let count (arr: t 'a): int => Array.length arr;

let addFirst (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 1 count;
  retval
};

let addLast (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let empty (): (t 'a) => [||];

let getOrRaiseFlipped (arr: t 'a) (index: int): 'a =>
  arr.(index);

let get (index: int) (arr: t 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count arr) index (getOrRaiseFlipped arr);

let getOrRaise (index: int) (arr: t 'a): 'a => arr.(index);

let first (arr: t 'a): (option 'a) => get 0 arr;

let firstOrRaise (arr: t 'a): 'a => getOrRaise 0 arr;

let lastIndexOrRaise (arr: t 'a): int => {
  let lastIndex = count arr - 1;
  if (lastIndex >= 0) lastIndex
  else failwith "empty";
};

let last (arr: t 'a): (option 'a) => {
  let lastIndex = count arr - 1;
  if (lastIndex >= 0) (get lastIndex arr)
  else None;
};

let lastOrRaise (arr: t 'a): 'a => {
  let lastIndex = count arr - 1;
  arr.(lastIndex)
};

let init = Array.init;

let insertAt (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange (count + 1) index;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 index;
  Array.blit arr index retval (index + 1) (count - index);

  retval;
};

let isEmpty (arr: t 'a): bool => (count arr) === 0;

let isNotEmpty (arr: t 'a): bool => (count arr) !== 0;

let ofUnsafe (arr: array 'a): (t 'a) => arr;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index < arrCount) {
      let next = arr.(index);

      if (predicate acc next) {
        let acc = f acc arr.(index);
        loop acc (index + 1);
      }
      else acc
    }
    else acc;

  loop acc 0;
};

let reduceWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index < arrCount) {
      let next = arr.(index);

      if (predicate acc index next) {
        let acc = f acc index next;
        loop acc (index + 1);
      }
      else acc;
    }
    else acc;

  loop acc 0;
};

let reduceRight
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index >= 0) {
      let next = arr.(index);

      if (predicate acc next) {
        let acc = f acc arr.(index);
        loop acc (index - 1);
      }
      else acc
    }
    else acc;

  loop acc (arrCount - 1);
};

let reduceRightWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrLastIndex = lastIndexOrRaise arr;
  let rec loop acc index =>
    if (index >= 0) {
      let next = arr.(index);

      if (predicate acc index next) {
        let acc = f acc index next;
        loop acc (index - 1);
      }
      else acc
    }
    else acc;

  loop acc arrLastIndex;
};

let removeLastOrRaise (arr: t 'a): (t 'a) => {
  let count = count arr;

  if (count === 0) (failwith "Array is empty")
  else if (count === 1) [||]
  else Array.sub arr 0 (count - 1);
};

let removeAt (index: int) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let newLength = count - 1;
  let anyItem = arr.(0);

  let retval = Array.make newLength anyItem;
  Array.blit arr 0 retval 0 index;
  Array.blit arr (index + 1) retval index (newLength - index);

  retval
};

let removeFirstOrRaise (arr: t 'a): (t 'a) =>
  removeAt 0 arr;

let skip (startIndex: int) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  if (startIndex < 0) (failwith "startIndex is < 0")
  else if (startIndex >= arrCount) [||]
  else if (startIndex === 0) arr
  else {
    let newCount = arrCount - startIndex;
    Array.sub arr startIndex newCount;
  };
};

let take (newCount: int) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  if (newCount < 0) (failwith "count is < 0")
  else if (newCount >= arrCount) arr
  else if (newCount === 0) [||]
  else Array.sub arr 0 newCount;
};

let toIterable (arr: t 'a): (Iterable.t 'a) =>
  if (isEmpty arr) (Iterable.empty ())
  else { reduce: fun predicate f acc => reduce while_::predicate f acc arr };

let toIterableRight (arr: t 'a): (Iterable.t 'a) =>
  if (isEmpty arr) (Iterable.empty ())
  else { reduce: fun predicate f acc => reduceRight while_::predicate f acc arr };

let toKeyedIterable (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) (KeyedIterable.empty ())
  else { reduce: fun predicate f acc => reduceWithIndex while_::predicate f acc arr };

let toKeyedIterableRight (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) (KeyedIterable.empty ())
  else { reduce: fun predicate f acc => reduceRightWithIndex while_::predicate f acc arr };

let toSequenceRight (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) (Sequence.empty ())
  else {
    let rec loop index => fun () =>
      if (index < 0) Sequence.Completed
      else Sequence.Next arr.(index) (loop (index - 1));
    loop (count arr - 1);
  };

let toSequence (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) (Sequence.empty ())
  else {
    let arrCount = count arr;
    let rec loop index => fun () =>
      if (index < arrCount) (Sequence.Next arr.(index) (loop (index + 1)))
      else Sequence.Completed;
    loop 0;
  };

let toSequenceWithIndex (arr: t 'a): (Sequence.t (int, 'a)) => {
  let arrCount = count arr;
  let rec loop index => fun () =>
    if (index < arrCount) (Sequence.Next (index, arr.(index)) (loop (index + 1)))
    else Sequence.Completed;
  loop 0;
};

let toCollection (set: t 'a): (Collection.t 'a) => {
  count: count set,
  iterable: fun () => toIterable set,
  sequence: fun () => toSequence set,
};

let update (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  Preconditions.failIfOutOfRange arrCount index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};

let updateWith (index: int) (f: 'a => 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = f arr.(index);
  clone
};

let toMap (arr: t 'a): (ImmMap.t int 'a) => {
  containsKey: fun index => index >= 0 && index < count arr,
  count: count arr,
  get: fun i => get i arr,
  getOrRaise: fun index => getOrRaise index arr,
  keyedIterator: fun () => toKeyedIterable arr,
  sequence: fun () => toSequenceWithIndex arr,
};

let module ReducerRight = Iterable.Reducer.Make1 {
  type nonrec t 'a = t 'a;

  let reduce = reduceRight;
  let toIterable = toIterableRight;
};

let module Reducer = Iterable.Reducer.Make1 {
  type nonrec t 'a = t 'a;

  let reduce = reduce;
  let toIterable = toIterable;
};