"use stricts";

exports.addListener = function (listener) {
  return function () {
    browser.windows.onRemoved.addListener(listener);
  }
}

exports.removeListener = function (listener) {
  return function () {
    return browser.windows.onRemoved.removeListener(listener);
  }
}

