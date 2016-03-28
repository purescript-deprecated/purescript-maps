/* global exports */
"use strict";

// module Data.StrMap.ST

function safeKey(x) { return "$" + x; }

exports["new"] = function () {
  return {};
};

exports.peekImpl = function (just) {
  return function (nothing) {
    return function (m) {
      return function (k) {
        return function () {
          return m.hasOwnProperty(safeKey(k)) ? just(m[safeKey(k)]) : nothing;
        };
      };
    };
  };
};

exports.poke = function (m) {
  return function (k) {
    return function (v) {
      return function () {
        m[safeKey(k)] = v;
        return m;
      };
    };
  };
};

exports["delete"] = function (m) {
  return function (k) {
    return function () {
      delete m[safeKey(k)];
      return m;
    };
  };
};
