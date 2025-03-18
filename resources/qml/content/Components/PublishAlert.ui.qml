import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import Futr 1.0
import Components 1.0

Rectangle {
    id: root

    property var publishStatus
    property int timeout: 5000 // 5 seconds

    height: layout.implicitHeight + 16
    radius: Constants.radius_s

    color: Qt.rgba(Material.backgroundColor.r, Material.backgroundColor.g, Material.backgroundColor.b, 0.8)

    ColumnLayout {
        id: layout
        anchors.fill: parent
        anchors.margins: 8
        spacing: Constants.radius_s

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            Text {
                text: "Publishing: " + publishStatus.eventId
                color: Material.foreground
                font: Constants.font
                Layout.fillWidth: true
                elide: Text.ElideMiddle
            }

            Button {
                flat: true
                implicitWidth: 32
                implicitHeight: 32
                text: "×"
                onClicked: root.visible = false
            }
        }

        Repeater {
            model: publishStatus.relayStatuses
            delegate: RowLayout {
                spacing: 8

                Image {
                    source: modelData[1] === "" ? "qrc:/icons/check.svg" : "qrc:/icons/error.svg"
                    width: 16
                    height: 16
                }

                Text {
                    text: modelData[0] + (modelData[1] ? ": " + modelData[1] : "")
                    color: Material.foreground
                    font: Constants.font
                }
            }
        }
    }

    layer.enabled: true
    layer.effect: DropShadow {
        transparentBorder: true
        horizontalOffset: 1
        verticalOffset: 1
        radius: Constants.radius_m
        samples: 17
        color: "#80000000"
    }

    Timer {
        id: hideTimer
        interval: root.timeout
        running: true
        onTriggered: root.visible = false
    }

    onPublishStatusChanged: {
        hideTimer.restart()
    }
}
