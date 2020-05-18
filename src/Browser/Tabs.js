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
    };
  };
};

exports["moveTab"] = function (tabIds) {
  return function (moveProperties) {
    return function () {
      return browser.tabs.move(tabIds, moveProperties);
    };
  };
};

exports["createTab"] = function (union) {
  return function (createProperties) {
    return function () {
      return browser.tabs.create(createProperties);
    };
  };
};
