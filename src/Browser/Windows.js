"use strict";

exports.getCurrentImpl = function () {
  return browser.windows.getCurrent()
};
