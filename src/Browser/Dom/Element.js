"use strict";


exports["scrollIntoView"] = function(elem) {
  return function() {
    elem.scrollIntoView({
      behavior: "smooth",
      block: "nearest"
    });
  };
};
