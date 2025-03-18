import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15

import Futr 1.0

Rectangle {
    id: root
    width: notificationText.width + 40
    height: 40
    radius: Constants.radius_l
    color: Material.accent
    opacity: 0
    z: 999999

    property alias text: notificationText.text

    Text {
        id: notificationText
        anchors.centerIn: parent
        text: ""
        color: "white"
        font.bold: true
    }

    Behavior on opacity {
        NumberAnimation { duration: 300 }
    }

    Timer {
        id: closeTimer
        interval: 3000
        running: root.opacity > 0
        onTriggered: root.opacity = 0
    }

    function show(message) {
        text = message
        opacity = 1.0
    }
}
