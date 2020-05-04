"use strict";

exports.queryImpl = function () {
  return browser.tabs.query({});
};


exports["remove'"] = function (tabs) {
  return function () {
    return browser.tabs.remove(tabs);
  };
};
