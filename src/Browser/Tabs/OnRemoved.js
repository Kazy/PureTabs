"use stricts";

exports.addListener = function (listener) {
  return function () {
    browser.tabs.onRemoved.addListener(listener);
  }
}

exports.removeListener = function (listener) {
  return function () {
    return browser.tabs.onRemoved.removeListener(listener);
  }
}
