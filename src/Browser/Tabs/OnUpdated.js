"use strict";

exports["addListener'"] = function (listener) {
  return function() {
    return browser.tabs.onUpdated.addListener(listener, {});
  };
};

exports["removeListener'"] = function (listener) {
  return function() {
    return browser.tabs.onUpdated.removeListener(listener);
  };
};
