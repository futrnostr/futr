import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    id: chat
    color: Material.backgroundColor
    radius: 5
    border.color: Material.dividerColor
    border.width: 1

    property string npub: ""
    property var profileData
    property var messages: [
        { content: "Hey there! How's it going?", isOwnMessage: false, timestamp: "10:00 AM" },
        { content: "Hi! I'm doing great, thanks for asking. How about you?", isOwnMessage: true, timestamp: "10:02 AM" },
        { content: "I'm good too! Just working on some new features for our app. Let's put a longer text here for demonstration purposes.", isOwnMessage: false, timestamp: "10:05 AM" },
        { content: "That sounds exciting! Can't wait to see what you come up with.", isOwnMessage: true, timestamp: "10:07 AM" },
        { content: "Thanks! I'll keep you updated on the progress.", isOwnMessage: false, timestamp: "10:10 AM" },
        { content: "By the way, have you seen the latest design mockups?", isOwnMessage: true, timestamp: "10:15 AM" },
        { content: "Not yet, could you send them over?", isOwnMessage: false, timestamp: "10:17 AM" },
        { content: "Sure thing! I'll email them to you right away.", isOwnMessage: true, timestamp: "10:20 AM" },
        { content: "Great, thanks! I'm looking forward to reviewing them.", isOwnMessage: false, timestamp: "10:22 AM" },
        { content: "No problem! Let me know if you need any clarification on the designs.", isOwnMessage: true, timestamp: "10:25 AM" }
    ]

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 1
        spacing: 10

        // Header
        Rectangle {
            Layout.fillWidth: true
            height: 60
            color: Material.primaryColor

            RowLayout {
                anchors.fill: parent
                anchors.margins: 10
                spacing: 10

                Image {
                    source: Util.getProfilePicture(profileData.picture, profileData.npub)
                    Layout.preferredWidth: 40
                    Layout.preferredHeight: 40
                    Layout.alignment: Qt.AlignVCenter
                    smooth: true
                    fillMode: Image.PreserveAspectCrop
                }

                Text {
                    text: profileData.displayName || profileData.name || npub
                    font.pixelSize: 16
                    color: Material.foreground
                    Layout.fillWidth: true
                }
            }
        }

        // Messages List
        ListView {
            id: messageListView
            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true
            spacing: 10
            verticalLayoutDirection: ListView.TopToBottom
            layoutDirection: Qt.LeftToRight

            // Add left margin to create space for non-own messages
            leftMargin: 10
            rightMargin: 10

            model: AutoListModel {
                id: messagesModel
                source: messages
            }

            delegate: Item {
                width: messageListView.width - messageListView.leftMargin - messageListView.rightMargin - (messageListView.ScrollBar.vertical ? messageListView.ScrollBar.vertical.width : 0)
                height: messageBubble.height + timestampText.height + 10

                Rectangle {
                    id: messageBubble
                    anchors {
                        left: modelData.isOwnMessage ? undefined : parent.left
                        right: modelData.isOwnMessage ? parent.right : undefined
                        top: parent.top
                    }
                    width: Math.min(messageContent.implicitWidth + 24, parent.width * 0.8)
                    height: messageContent.height + 20
                    color: modelData.isOwnMessage ? Material.accent : Material.primary
                    radius: 10

                    Text {
                        id: messageContent
                        anchors {
                            left: parent.left
                            right: parent.right
                            verticalCenter: parent.verticalCenter
                            margins: 12
                        }
                        text: modelData.content
                        wrapMode: Text.Wrap
                        color: Material.foreground
                    }
                }

                Text {
                    id: timestampText
                    anchors {
                        top: messageBubble.bottom
                        left: modelData.isOwnMessage ? undefined : messageBubble.left
                        right: modelData.isOwnMessage ? messageBubble.right : undefined
                        topMargin: 2
                    }
                    text: modelData.timestamp
                    font: Constants.smallFont
                    color: Material.hintTextColor
                }
            }

            onCountChanged: {
                positionViewAtEnd()
            }

            Component.onCompleted: {
                positionViewAtEnd()
            }

            ScrollBar.vertical: ScrollBar {
                active: true
                policy: ScrollBar.AsNeeded
            }
        }

        // Message Input
        RowLayout {
            Layout.fillWidth: true
            Layout.bottomMargin: 10
            Layout.leftMargin: 10
            Layout.rightMargin: 10
            spacing: 10

            TextField {
                id: messageInput
                Layout.fillWidth: true
                placeholderText: "Type a message..."
                font.pixelSize: 14
                bottomPadding: 10
                leftPadding: 10
            }

            Button {
                text: "Send"
                highlighted: true
                bottomPadding: 10
                rightPadding: 10
                onClicked: {
                    // Here you would typically call a function to send the message
                    console.log("Sending message:", messageInput.text)
                    messageInput.text = ""
                }
            }
        }
    }
}
