import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Pane {
    id: root

    property string placeholderText: qsTr("Type a message...")
    property string buttonText: qsTr("Send")
    property string currentUser: ""

    signal messageSent(string text)

    Layout.fillWidth: true
    Layout.margins: Constants.spacing_xs

    background: Rectangle {
        color: Material.dialogColor
        radius: Constants.radius_m
        border.color: Material.dividerColor
        border.width: 1
    }

    RowLayout {
        width: parent.width
        spacing: Constants.spacing_m

        ProfilePicture {
            imageSource: Util.getProfilePicture(mypicture, currentUser)
        }

        TextArea {
            id: messageInput
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignVCenter
            placeholderText: root.placeholderText
            wrapMode: TextArea.Wrap
            font: Constants.fontMedium
            focus: true

            Keys.onReturnPressed: function(event) {
                if (event.modifiers & Qt.ShiftModifier) {
                    event.accepted = false
                } else {
                    event.accepted = true
                    sendMessage()
                }
            }
        }

        Button {
            text: root.buttonText
            highlighted: true
            enabled: messageInput.text.trim().length > 0
            implicitWidth: 80
            implicitHeight: 36
            Layout.alignment: Qt.AlignVCenter

            onClicked: sendMessage()
        }
    }

    function sendMessage() {
        if (messageInput.text.trim() !== "") {
            root.messageSent(messageInput.text)
            messageInput.text = ""
        }
    }

    function clear() {
        messageInput.text = ""
    }
}
