import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls.Material 2.15

Image {
    id: postImage

    property string value
    property string original

    source: value
    cache: false
    asynchronous: true
    fillMode: Image.PreserveAspectFit
    Layout.fillWidth: true
    Layout.preferredHeight: sourceSize.height > 0 ? sourceSize.height * (width / sourceSize.width) : 200

    onImageClicked: function(url) {
        stackView.push(imageViewerComponent, {"imageSource": url, "original": original})
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
