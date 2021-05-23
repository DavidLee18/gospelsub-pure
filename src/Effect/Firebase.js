"use strict";

exports.firestore = function () {
    return firebase.firestore();
}

exports.doc = function (path) {
    return function (fs) {
        return function () {
            return fs.doc(path);
        }
    }
}

exports.readDocPromise = function (Just) {
    return function (Nothing) {
        return function (docRef) {
            return function () {
                return docRef.get().then(snapshot => snapshot.exists ? Just(snapshot.data()) : Nothing);
            }
        }
    }
}

exports.collection = function (path) {
    return function (fs) {
        return function () {
            return fs.collection(path);
        }
    }
}

exports.addToCollectionPromise = function(data) {
    return function(colRef) {
        return function() {
            return colRef.add(data);
        }
    }
}

exports.readCollectionPromise = function(colRef) {
    return function() {
        return colRef.get().then(snapshot => snapshot.docs.filter(doc => doc.exists).map(doc => doc.data()));
    }
}