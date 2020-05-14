"use strict";

exports["addListener"] = function (listener) {
  return function () {
    return browser.tabs.onMoved.addListener(listener);
  };
};

exports["removeListener"] = function (listener) {
  return function() {
    return browser.tabs.onMoved.removeListener(listener);
  };
};


