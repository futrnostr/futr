import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Controls.Material.impl 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Futr 1.0
import HsQML.Model 1.0

Dialog {
    id: root
    modal: true
    width: parent.width * 0.95
    height: parent.height * 0.9
    padding: 0
    title: isQuoteMode ? qsTr("Quote Post") : qsTr("Reply")

    property var targetPost: null
    property string inputPlaceholder: qsTr("Post your reply")
    property string buttonText: qsTr("Reply")
    property bool isQuoteMode: false
    required property string currentUser
    required property string currentUserPicture
    signal messageSubmitted(string text)

    background: Rectangle {
        color: Material.dialogColor
        radius: 4
        layer.enabled: true
        layer.effect: ElevationEffect {
            elevation: 24
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: Constants.spacing_m
        spacing: Constants.spacing_m

        ScrollView {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 0
            clip: true
            id: scrollView

            // Use a Loader to conditionally create the post content
            Loader {
                active: root.targetPost !== null
                width: scrollView.width
                height: active ? item.implicitHeight : 0

                sourceComponent: Rectangle {
                    color: Material.backgroundColor
                    radius: Constants.radius_m
                    width: scrollView.width
                    implicitHeight: postContent.implicitHeight + Constants.spacing_xs

                    PostContent {
                        id: postContent
                        anchors.fill: parent
                        anchors.margins: 2
                        post: root.targetPost
                        currentUser: "root.currentUser"
                        showAuthor: true
                    }                
                }
            }
        }

        MessageInput {
            id: replyInput
            Layout.fillWidth: true
            placeholderText: root.inputPlaceholder
            buttonText: root.buttonText
            currentUser: root.currentUser
            currentUserPicture: root.currentUserPicture

            onMessageSent: function(text) {
                root.messageSubmitted(text)
                root.close()
            }
        }
    }

    enter: Transition {
        NumberAnimation { property: "opacity"; from: 0.0; to: 1.0; duration: 150 }
    }
    exit: Transition {
        NumberAnimation { property: "opacity"; from: 1.0; to: 0.0; duration: 150 }
    }
}
