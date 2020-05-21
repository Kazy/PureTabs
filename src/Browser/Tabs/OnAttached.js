"use strict";

exports["addListener"] = function (lst) {
  return function () {
    return browser.tabs.onAttached.addListener(lst);
  };
};

exports["removeListener"] = function (lst) {
  return function () {
    return browser.tabs.onAttached.removeListener(lst);
  };
};
