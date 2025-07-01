import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls.Material 2.15

Image {
    id: postImage

    property string value
    property string original

    fillMode: Image.PreserveAspectFit
    Layout.fillWidth: true
    Layout.preferredHeight: sourceSize.height > 0 ? sourceSize.height * (width / sourceSize.width) : 200

    onImageClicked: function(url) {
        stackView.push(imageViewerComponent, {"imageSource": url, "original": original})
    }

    Rectangle {
        anchors.centerIn: parent
        visible: postImage.status === Image.Error
        color: "transparent"
        width: errorText.width + 20
        height: errorText.height + 10

        Text {
            id: errorText
            anchors.centerIn: parent
            text: "Failed to load image"
            color: Material.accent
        }
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: clickable ? Qt.PointingHandCursor : Qt.ArrowCursor
        onClicked: {
            if (clickable) {
                imageClicked(value)
            }
        }
    }

    property bool clickable: true
    signal imageClicked(string url)
}
