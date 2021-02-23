"use strict";


exports["setTabValueImpl"] = function(tabId, key, value) {
  return browser.sessions.setTabValue(tabId, key, value);
};

exports["removeTabValueImpl"] = function(tabId, key) {
  return browser.sessions.removeTabValue(tabId, key);
};

exports["getTabValueImpl"] = function(Just, Nothing, tabId, key) {
    return browser.sessions.getTabValue(tabId, key).then(val => {
      if (val === undefined) return Nothing;
      else return Just(val);
    });
};


exports["setWindowValueImpl"] = function(windowId, key, value) {
  return browser.sessions.setWindowValue(windowId, key, value);
};

exports["removeWindowValueImpl"] = function(windowId, key) {
  return browser.sessions.removeWindowValue(windowId, key);
};

exports["getWindowValueImpl"] = function(windowId, key) {
  return browser.sessions.getWindowValue(windowId, key);
};
