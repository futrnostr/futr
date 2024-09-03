import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Rectangle {
    id: root
    required property string proof
    required property string value
    required property string icon
    required property string link

    visible: proof.length > 0

    height: 10
    width: content.implicitWidth - 20

    color: "transparent"

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor

        onClicked: {
            Qt.openUrlExternally(link)
        }
    }

    Row {
        spacing: 5

        Image {
            source: icon
            width: 10
            height: 10
            anchors.verticalCenter: parent.verticalCenter
        }

        Text {
            text: value
            color: "blue"
            anchors.verticalCenter: parent.verticalCenter
        }
    }
}
