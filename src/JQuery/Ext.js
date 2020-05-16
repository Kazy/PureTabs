"use strict";

exports.after = function(ob) {
    return function(ob1) {
        return function() {
            ob1.after(ob);
        };
    };
};

exports.prepend = function(ob) {
    return function(ob1) {
        return function() {
            ob1.prepend(ob);
        };
    };
};

