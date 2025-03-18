import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import Futr 1.0

Rectangle {
    id: root
    color: Material.backgroundColor
    radius: Constants.radius_m
    border.color: Material.dividerColor
    border.width: 1

    property bool hasValidInputs: false

    ColumnLayout {
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 10
        spacing: Constants.spacing_s

        Text {
            Layout.fillWidth: true
            text: qsTr("You are not set up to receive private messsages yet.")
            font: Constants.largeFont
            color: Material.foreground
            wrapMode: Text.WordWrap
        }

        Button {
            text: qsTr("Setup now")
            onClicked: relayMgmtDialog.open()
        }

        Text {
            Layout.fillWidth: true
            text: qsTr("If you believe this message is incorrect, it might be because we can't access your preferred DM relays from your profile, your internet connection is slow, or the necessary settings file is missing from your computer.")
            font: Constants.smallFont
            color: Material.foreground
            wrapMode: Text.WordWrap
        }
    }
}
