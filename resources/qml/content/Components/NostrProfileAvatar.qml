import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

NostrImage {
    id: root

    property string npub: ""
    property bool isRobohashFallback: false

    Layout.preferredWidth: 36
    Layout.preferredHeight: 36
    Layout.alignment: Qt.AlignVCenter

    width: 36
    height: 36

    Component.onCompleted: {
        if (url && url !== "") {
            resolveUrl(url)
            return
        }

        if (npub && npub !== "") {
            var robohashUrl = "https://robohash.org/" + npub + ".png?size=50x50"
            isRobohashFallback = true
            image.source = "qrc:/icons/person.svg"
            resolveUrl(robohashUrl)
        }
    }

    onUrlChanged: {
        if (url && url !== "") {
            image.source = root.fallbackImage
            resolveUrl(url)
            return
        }

        if (npub && npub !== "") {
            var robohashUrl = "https://robohash.org/" + npub + ".png?size=50x50"
            isRobohashFallback = true
            image.source = "qrc:/icons/person.svg"
            resolveUrl(robohashUrl)
        }
    }

    onNpubChanged: {
        if (url && url !== "") {
            image.source = root.fallbackImage
            resolveUrl(url)
            return
        }

        if (npub && npub !== "") {
            var robohashUrl = "https://robohash.org/" + npub + ".png?size=50x50"
            isRobohashFallback = true
            image.source = "qrc:/icons/person.svg"
            resolveUrl(robohashUrl)
        }
    }

    function onImageFailed() {
        if (isRobohashFallback) {
            image.source = "qrc:/icons/person.svg"
            status = "failed"
            return
        }

        if (npub && npub !== "") {
            var robohashUrl = "https://robohash.org/" + npub + ".png?size=50x50"
            isRobohashFallback = true
            image.source = "qrc:/icons/person.svg"
            resolveUrl(robohashUrl)
        }
    }
}