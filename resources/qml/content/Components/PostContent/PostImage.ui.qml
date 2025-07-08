import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Image {
    id: postImage

    property string value
    property string original

    source: value
    cache: false
    asynchronous: true
    fillMode: Image.PreserveAspectFit
    height: sourceSize.height > 0 ? sourceSize.height * (width / sourceSize.width) : 200

    onImageClicked: function(url) {
        stackView.push(imageViewerComponent, {"imageSource": url, "original": original})
    }

    Text {
        id: errorText
        anchors.centerIn: parent
        visible: postImage.status === Image.Error
        text: "Failed to load image"
        color: Material.accent
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: clickable ? Qt.PointingHandCursor : Qt.ArrowCursor
        onClicked: {
            if (clickable) {
                imageClicked(value, original)
            }
        }
    }

    property bool clickable: true
    signal imageClicked(string url, string original)
}
