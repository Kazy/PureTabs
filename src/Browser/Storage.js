"use strict";

exports["storageLocalGetImpl"] = function(key, Just, Nothing) {
  return browser.storage.local.get(key).then(obj => {
    if (obj === undefined || obj[key] === undefined) {
      return Nothing;
    } else {
      return Just(obj[key]);
    }
  });
}

exports["storageLocalSetImpl"] = function(key, value) {
  return browser.storage.local.set({[key]: value});
}
