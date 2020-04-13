"use stricts";


exports.mkListenerUnit = function (fn) {
  return function () {
    return function () {
      return fn();
    }
  }
};

exports.mkListenerOne = function (fn) {
  return function () {
    return function (one) {
      return fn(one)();
    }
  }
};

exports.mkListenerTwo = function (fn) {
  return function () {
    return function (one, two) {
        return fn(one)(two)();
    }
  }
};
