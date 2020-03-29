"use strict";

exports.connect = function () {
    return browser.runtime.connect({name: name});
}

exports.postMessage = function (port) {
  return function (message) {
    return function () {
      port.postMessage(message);
    }
  }
}

exports.onConnectAddListener = function (fn) {
  return function () {
    return browser.runtime.onConnect.addListener(p => {
      fn(p)();
    })
  }
}

exports.onMessageAddListener = function (port) {
  return function (fn) {
    return function () {
      return port.onMessage.addListener(m => fn(m)());
    }
  }
}
