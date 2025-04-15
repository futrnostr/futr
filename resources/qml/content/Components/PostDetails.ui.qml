import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Controls.Material.impl 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Futr 1.0
import HsQML.Model 1.0

Page {
    id: root

    property var post: null
    required property string currentUser
    required property string currentUserPicture

    header: ToolBar {
        RowLayout {
            anchors.fill: parent

            Button {
                icon.source: "qrc:/icons/arrow_back.svg"
                icon.width: 24
                icon.height: 24
                flat: true
                onClicked: stackView.pop()

                ToolTip.visible: hovered
                ToolTip.text: qsTr("Back")
                ToolTip.delay: 500
            }

            Label {
                text: qsTr("Post Details")
                elide: Label.ElideRight
                horizontalAlignment: Qt.AlignHCenter
                verticalAlignment: Qt.AlignVCenter
                Layout.fillWidth: true
            }

            Item { width: 48; height: 48 }
        }
    }

    ScrollView {
        anchors.fill: parent
        anchors.bottomMargin: replyInput.height
        clip: true
        id: scrollView

        ColumnLayout {
            width: scrollView.width
            spacing: Constants.spacing_xs

            PostContent {
                post: root.post
                currentUser: root.currentUser
                Layout.fillWidth: true
                Layout.topMargin: Constants.spacing_xs
                Layout.leftMargin: 0
                Layout.rightMargin: Constants.spacing_m
            }

            Label {
                text: qsTr("Seen on Relays")
                color: Material.foreground
                Layout.leftMargin: 0
                font: Constants.fontMedium
            }

            Repeater {
                model: post ? post.relays : []
                delegate: ColumnLayout {
                    Layout.fillWidth: true
                    Layout.leftMargin: 0
                    Layout.rightMargin: Constants.spacing_m
                    spacing: Constants.spacing_s

                    Text {
                        text: modelData
                        color: Material.foreground
                        wrapMode: Text.Wrap
                        Layout.fillWidth: true
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        height: 1
                        color: Material.dividerColor
                        visible: post && post.relays.length > 1 && index < post.relays.length - 1
                    }
                }
            }

            Label {
                text: qsTr("Comments")
                color: Material.foreground
                Layout.leftMargin: Constants.spacing_m
                font: Constants.fontMedium
                visible: commentsModel.count > 0
            }

            // Comments sections
            ScrollingListView {
                id: commentsView
                Layout.fillWidth: true
                Layout.preferredHeight: Math.min(contentHeight, 1000)
                Layout.leftMargin: Constants.spacing_m
                Layout.rightMargin: Constants.spacing_m

                model: AutoListModel {
                    id: commentsModel
                    source: root.post ? root.post.comments : []
                    mode: AutoListModel.ByKey
                    equalityTest: function (oldItem, newItem) {
                        return oldItem.id === newItem.id
                    }
                }

                delegate: Loader {
                    active: modelData !== undefined && modelData !== null
                    width: commentsView.width - commentsView.leftMargin - commentsView.rightMargin
                    height: active ? item.implicitHeight : 0

                    sourceComponent: PostContent {
                        post: modelData
                        currentUser: root.currentUser
                        Layout.fillWidth: true
                    }
                }
            }

            Item {
                Layout.fillHeight: true
                height: 20
            }
        }
    }

    MessageInput {
        id: replyInput
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: implicitHeight
        placeholderText: qsTr("Reply to post...")
        buttonText: qsTr("Send")
        currentUser: root.currentUser
        currentUserPicture: root.currentUserPicture

        onMessageSent: function(text) {
            if (root.post && root.post.id) {
                sendReply(root.post.id, text)
                text = ""
            }
        }
    }
}
