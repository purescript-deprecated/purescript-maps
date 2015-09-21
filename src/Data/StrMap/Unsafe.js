/* global exports */
"use strict";

// module Data.StrMap.Unsafe

function safeKey(x) { return "$" + x; }

exports.unsafeIndex = function (m) {
  return function (k) {
    return m[safeKey(k)];
  };
};
