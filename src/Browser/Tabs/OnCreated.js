"use stricts";

exports.mkListener = function (fn) {
  return function () {
    return function (event) {
      return fn(event)();
    }
  }
};

exports.addListener = function (listener) {
  return function () {
    browser.tabs.onCreated.addListener(listener);
  }
}

exports.removeListener = function (listener) {
  return function () {
    return browser.tabs.onCreated.removeListener(listener);
  }
}
