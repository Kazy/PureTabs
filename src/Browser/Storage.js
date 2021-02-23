"use strict";

exports["storageLocalGetImpl"] = function(key) {
  return browser.storage.local.get(key).then(obj => {
    return obj[key];
  });
}

exports["storageLocalSetImpl"] = function(key, value) {
  return browser.storage.local.set({[key]: value});
}
