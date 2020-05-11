"use strict";

exports.queryImpl = function () {
  return browser.tabs.query({});
};


exports["remove'"] = function (tabs) {
  return function () {
    return browser.tabs.remove(tabs);
  };
};

exports["update'"] = function () {
  return function (updateProperties) {
    return function (tabId) {
      return function () {
        return browser.tabs.update(tabId, updateProperties);
      };
    }
  };
};
