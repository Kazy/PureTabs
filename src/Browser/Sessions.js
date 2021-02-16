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
