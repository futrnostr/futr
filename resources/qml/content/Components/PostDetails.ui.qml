import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Controls.Material.impl 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Futr 1.0
import HsQML.Model 1.0

Rectangle {
    id: root
    width: parent.width
    height: parent.height

    property var post: null

    ScrollView {
        Layout.fillWidth: true
        Layout.fillHeight: true
        spacing: 0
        clip: true
        id: scrollView

        ColumnLayout {
            width: scrollView.width - 20
            Layout.fillWidth: true
            Layout.fillHeight: true

            PostContent {
                post: root.post
                Layout.fillWidth: true
            }

            Label {
                text: qsTr("Seen on Relays")
                color: Material.foreground
            }

            Repeater {
                model: post ? post.relays : []
                delegate: ColumnLayout {
                    Layout.fillWidth: true
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

            // Comments sections
            ScrollingListView {
                id: commentsView
                Layout.fillWidth: true
                Layout.fillHeight: true
                width: scrollView.width - 20

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
                        Layout.fillWidth: true
                        Layout.leftMargin: Constants.spacing_m
                        Layout.rightMargin: Constants.spacing_m
                    }
                }
            }

            Item {
                Layout.fillHeight: true
            }

            MessageInput {
                id: replyInput
                width: scrollView.width - 20
                Layout.margins: 0
                spacing: 0
                Layout.fillWidth: true
                placeholderText: "Send..." // root.inputPlaceholder
                buttonText: "Send" // root.buttonText

                onMessageSent: function(text) {
                    root.messageSubmitted(text)
                    root.close()
                }
            }
        }
    }
}
