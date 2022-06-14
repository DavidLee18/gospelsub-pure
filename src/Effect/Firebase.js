"use strict";

export function firestore () {
    return firebase.firestore();
}

export function doc (path) {
    return function (fs) {
        return function () {
            return fs.doc(path);
        }
    }
}

export function readDocPromise (Just) {
    return function (Nothing) {
        return function (docRef) {
            return function () {
                return docRef.get().then(snapshot => snapshot.exists ? Just(snapshot.data()) : Nothing);
            }
        }
    }
}

export function collection (path) {
    return function (fs) {
        return function () {
            return fs.collection(path);
        }
    }
}

export function addToCollectionPromise(data) {
    return function(colRef) {
        return function() {
            return colRef.add(data);
        }
    }
}

export function readCollectionPromise(colRef) {
    return function() {
        return colRef.get().then(snapshot => snapshot.docs.filter(doc => doc.exists).map(doc => doc.data()));
    }
}