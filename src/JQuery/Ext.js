"use strict";

exports.after = function(ob) {
    return function(ob1) {
        return function() {
            ob1.after(ob);
        };
    };
};

