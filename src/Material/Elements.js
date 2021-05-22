"use strict";

exports.toSelectedDetailImpl = function (Just) {
    return function(Nothing) {
        return function(event) {
            const detail = event.detail;
            const i = detail.index;
            return i instanceof Set && typeof detail.diff !== 'undefined'
            ? { index: Array.from(i), diff: Just(detail.diff) }
            : { index: Array.of(i), diff: Nothing };
        }
    }
}