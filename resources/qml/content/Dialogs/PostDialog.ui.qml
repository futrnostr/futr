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
    property string content: targetPost ? Util.getFormattedContent(targetPost) : ""
    property string inputPlaceholder: qsTr("Post your reply")
    property string buttonText: qsTr("Reply")
    property bool isQuoteMode: false

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

            ColumnLayout {
                width: scrollView.width - 20
                Layout.fillWidth: true
                Layout.fillHeight: true

                // Original post container
                Rectangle {
                    Layout.fillWidth: true
                    color: Material.dialogColor
                    Layout.preferredHeight: contentColumn.height + 32
                    border.width: 1
                    border.color: Material.dividerColor

                    ColumnLayout {
                        id: contentColumn
                        width: parent.width
                        anchors.margins: Constants.spacing_m
                        spacing: Constants.spacing_m

                        // Author info
                        RowLayout {
                            spacing: Constants.spacing_m
                            Layout.topMargin: Constants.spacing_m
                            Layout.leftMargin: Constants.spacing_m
                            Layout.rightMargin: Constants.spacing_m

                            ProfilePicture {
                                imageSource: root.targetPost ?
                                    Util.getProfilePicture(root.targetPost.authorPicture || "", root.targetPost.authorNpub || "") : ""
                                Layout.preferredWidth: 48
                                Layout.preferredHeight: 48
                            }

                            ColumnLayout {
                                spacing: Constants.spacing_s
                                Text {
                                    text: root.targetPost ? (root.targetPost.authorName || "") : ""
                                    font: Constants.fontMedium
                                }
                                Text {
                                    text: root.targetPost ? (root.targetPost.timestamp || "") : ""
                                    color: Material.secondaryTextColor
                                    font: Constants.smallFontMedium
                                }
                            }
                        }

                        // Post content
                        Text {
                            Layout.fillWidth: true
                            Layout.leftMargin: Constants.spacing_m
                            Layout.rightMargin: Constants.spacing_m
                            Layout.topMargin: Constants.spacing_m
                            text: Util.getFormattedContent(root.targetPost)
                            color: Material.foreground
                            wrapMode: Text.Wrap
                            font: Constants.fontMedium
                        }

                        // Engagement stats
                        Rectangle {
                            Layout.fillWidth: true
                            Layout.topMargin: Constants.spacing_m
                            Layout.preferredHeight: 48
                            color: Material.backgroundColor
                            border.width: 1
                            border.color: Material.dividerColor

                            RowLayout {
                                anchors.fill: parent
                                anchors.margins: Constants.spacing_m
                                spacing: Constants.spacing_m

                                Text {
                                    text: (root.targetPost ? (root.targetPost.repostCount || "0") : "0") + qsTr(" Reposts")
                                    color: Material.secondaryTextColor
                                    font: Constants.smallFontMedium
                                }

                                Text {
                                    text: (root.targetPost ? root.targetPost.comments.length : "0") + qsTr(" Comments")
                                    color: Material.secondaryTextColor
                                    font: Constants.smallFontMedium
                                }
                            }
                        }

                        ColumnLayout {
                            spacing: Constants.spacing_m
                            Layout.leftMargin: Constants.spacing_m
                            Layout.rightMargin: Constants.spacing_m

                            Text {
                                text: qsTr("Seen on relays:")
                                color: Material.secondaryTextColor
                                font: Constants.smallFontMedium
                                visible: root.targetPost && root.targetPost.relays.length > 0
                            }

                            Repeater {
                                model: root.targetPost ? root.targetPost.relays : []
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
                                        visible: root.targetPost && root.targetPost.relays.length > 1 && index < root.targetPost.relays.length - 1
                                    }
                                }
                            }
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
                        source: root.targetPost ? root.targetPost.comments : []
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
                    placeholderText: root.inputPlaceholder
                    buttonText: root.buttonText

                    onMessageSent: function(text) {
                        root.messageSubmitted(text)
                        root.close()
                    }
                }
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
