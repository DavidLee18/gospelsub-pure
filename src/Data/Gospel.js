"use strict";

exports.parseIntImpl = function (just) {
    return function (nothing) {
        return function (rawString) {
            const num = parseInt(rawString, 10);
            return Number.isInteger(num) ? just(num) : nothing;
        }
    }
}