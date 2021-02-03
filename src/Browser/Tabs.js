"use strict";

exports.queryImpl = function () {
  return browser.tabs.query({});
};

exports["browserRemove'"] = function (tabs) {
  return function () {
    return browser.tabs.remove(tabs);
  };
};

exports["browserUpdate'"] = function () {
  return function (updateProperties) {
    return function (tabId) {
      return function () {
        return browser.tabs.update(tabId, updateProperties);
      };
    };
  };
};

exports["browserMoveTab"] = function (tabIds) {
  return function (moveProperties) {
    return function () {
      return browser.tabs.move(tabIds, moveProperties);
    };
  };
};

exports["browserCreateTab"] = function (union) {
  return function (createProperties) {
    return function () {
      return browser.tabs.create(createProperties);
    };
  };
};
