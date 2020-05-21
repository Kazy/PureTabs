"use strict";

exports["addListener"] = function (lst) {
  return function () {
    return browser.tabs.onDetached.addListener(lst);
  };
};

exports["removeListener"] = function (lst) {
  return function () {
    return browser.tabs.onDetached.removeListener(lst);
  };
};
