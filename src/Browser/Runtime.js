"use strict";


exports.connect = function () {
    return browser.runtime.connect();
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
    return browser.runtime.onConnect.addListener(fn)
  }
}

exports.portOnDisconnect = function(port) {
  return function (fn) {
    return function () {
      return port.onDisconnect.addListener(fn)
    }
  }
}

exports.onMessageAddListener = function (port) {
  return function (fn) {
    return function () {
      return port.onMessage.addListener(fn);
    }
  }
}

exports.onMessageRemoveListener = function (port) {
  return function (fn) {
    return function () {
      return port.onMessage.removeListener(fn);
    }
  }
}

exports.portEquality = function (p1) {
  return function (p2) {
    return p1 === p2
  }
}


exports.portHasError = function(port) {
  return function () {
    return port.error != null;
  };
};
