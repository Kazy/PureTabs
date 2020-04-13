"use stricts";

exports.addListenerImpl = function (listener) {
  return function () {
    browser.tabs.onCreated.addListener(listener);
  }
}

exports.removeListener = function (listener) {
  return function () {
    return browser.tabs.onCreated.removeListener(listener);
  }
}
