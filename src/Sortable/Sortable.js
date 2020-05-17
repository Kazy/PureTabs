"use strict";

var Sortable = require("../../node_modules/sortablejs/Sortable.min.js");

const optionsEventField = [
  "onChoose",
  "onUnchoose",
  "onStart",
  "onEnd",
  "onAdd",
  "onUpdate",
  "onSort",
  "onRemove",
  "onFilter",
  "onClone",
  "onChange",
];
const optionsEffectField = optionsEventField.concat(["onMove"]);

exports["create'"] = function (options, el, parseEvent) {

  const optionsCopy = Object.assign({}, options);
  for (const field of optionsEffectField) {
    if (field in optionsCopy) {
      let func = optionsCopy[field];
      if (optionsEventField.includes(field)) {
        func = parseEvent(func);
      }
      optionsCopy[field] = unEffect(func);
    }
  }

  return function () {
    return Sortable.create(el, optionsCopy);
  };
};

exports.isTrue = function (b) {
  return b === true;
};
exports.isFalse = function (b) {
  return b === false;
};
exports.isClone = function (b) {
  return b === "clone";
};
const unEffect = function (f) {
  return function (event) {
    console.log(event);
    return f(event)();
  };
};
