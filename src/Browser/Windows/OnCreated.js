"use stricts";

exports.addListener = function (listener) {
  return function () {
    browser.windows.onCreated.addListener(listener);
  }
}

exports.removeListener = function (listener) {
  return function () {
    return browser.windows.onCreated.removeListener(listener);
  }
}


