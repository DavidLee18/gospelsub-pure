"use strict";

exports.firestore = function () {
    return firebase.firestore();
}

exports.doc = function (fs) {
    return function (path) {
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

exports.collection = function (fs) {
    return function (path) {
        return function () {
            return fs.collection(path);
        }
    }
}

exports.addToCollectionPromise = function(colRef) {
    return function(data) {
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