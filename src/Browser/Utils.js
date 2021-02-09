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

exports.mkListenerThree = function (fn) {
  return function () {
    return function (one, two, three) {
        return fn(one)(two)(three)();
    }
  }
};

exports["unsafeLog'"] = function (data) {
  console.log(">> this is unsafe:");
  console.log(data);
  return data;
};

exports["unsafeLog"] = function (data) {
  return function() {
    exports["unsafeLog'"](data);
  };
};
