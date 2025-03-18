import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Rectangle {
    Layout.fillWidth: true
    height: 40
    radius: 20
    color: Material.background
    border.color: Material.accentColor
    border.width: filterInput.activeFocus ? 2 : 1

    property alias filterText: filterInput.text

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 10
        anchors.rightMargin: 10
        spacing: Constants.radius_m

        Image {
            Layout.leftMargin: 10
            source: "qrc:/icons/search.svg"
            sourceSize.width: 20
            sourceSize.height: 20
            Layout.alignment: Qt.AlignVCenter
        }

        TextField {
            id: filterInput
            Layout.fillWidth: true
            Layout.fillHeight: true
            placeholderText: qsTr("Filter follows...")
            background: Item {}
            color: Material.foreground
            font.pixelSize: 14
            verticalAlignment: TextInput.AlignVCenter
            leftPadding: 0
            rightPadding: 0
            topPadding: 0
            bottomPadding: 0
        }

        Image {
            source: "qrc:/icons/close.svg"
            sourceSize.width: 16
            sourceSize.height: 16
            Layout.alignment: Qt.AlignVCenter
            Layout.rightMargin: 10
            visible: filterInput.text.length > 0
            opacity: clearMouseArea.containsMouse ? 0.7 : 1.0

            MouseArea {
                id: clearMouseArea
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: filterInput.text = ""
            }
        }
    }
}
