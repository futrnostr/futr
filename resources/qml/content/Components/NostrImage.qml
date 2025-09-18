import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: root

    property string url: ""
    property alias image: image
    property string status: "idle" // "idle" | "checking" | "caching" | "ready" | "failed"
    property string fallbackImage: "qrc:/icons/person.svg"
    property string currentUrl: ""

    Image {
        id: image
        anchors.fill: parent
        fillMode: Image.PreserveAspectFit
        cache: false
        asynchronous: true
        source: root.fallbackImage

        onStatusChanged: {
            if (image.status === Image.Error) {
                image.source = root.fallbackImage
            }
        }
    }

    Component.onCompleted: {
        if (url && url !== "") {
            resolveUrl(url)
        }
    }

    onUrlChanged: {
        if (url && url !== "") {
            image.source = root.fallbackImage
            resolveUrl(url)
        }
    }

    Component.onDestruction: {
        mediaPeekCompleted.disconnect(mediaPeekCallback)
        mediaCacheCompleted.disconnect(mediaCacheCallback)
    }

    function resolveUrl(targetUrl) {
        if (!targetUrl || targetUrl === "") {
            status = "idle"
            image.source = root.fallbackImage
            return
        }

        currentUrl = targetUrl
        var downloadStatus = hasDownload(targetUrl) || []

        if (downloadStatus && downloadStatus.length === 2) {
            var cacheFile = downloadStatus[0]
            var mimeType = downloadStatus[1]

            if (mimeType.indexOf("image/") === 0) {
                status = "ready"
                image.source = cacheFile
                return
            }
        }

        status = "checking"
        mediaPeekCompleted.connect(mediaPeekCallback)
        peekMimeType(targetUrl)
    }

    function mediaPeekCallback(url, mime) {
        if (url !== currentUrl) {
            return
        }

        if (mime.indexOf("image/") === 0 || mime.indexOf("video/") === 0) {
            status = "caching"
            mediaCacheCompleted.connect(mediaCacheCallback)
            cacheMedia(url)
        } else {
            status = "failed"
            image.source = root.fallbackImage
            onImageFailed()
        }
    }

    function mediaCacheCallback(url, success, pathOrError) {
        if (url !== currentUrl) {
            return
        }

        if (success) {
            status = "ready"
            image.source = pathOrError
        } else {
            status = "failed"
            image.source = root.fallbackImage
            onImageFailed()
        }
    }

    // Override this function to handle image failure
    function onImageFailed() {
    }
}