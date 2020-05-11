"use strict";

exports["addListener'"] = function (listener) {
  return function() {
    return browser.tabs.onActivated.addListener(listener);
  };
};

exports["removeListener"] = function (listener) {
  return function() {
    return browser.tabs.onActivated.removeListener(listener);
  };
};
