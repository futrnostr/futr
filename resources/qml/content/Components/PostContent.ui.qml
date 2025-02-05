import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import Components 1.0
import Futr 1.0


Pane {
    id: root
    padding: Constants.spacing_s
    width: parent.width
    
    required property var post
    
    signal commentClicked()
    signal repostClicked()
    
    background: Rectangle {
        color: Material.dialogColor
        radius: 10
    }

    ColumnLayout {
        width: parent.width
        spacing: Constants.spacing_s

        // Main post content
        Text {
            Layout.fillWidth: true
            text: (post.content || "").replace(/nostr:(note|nevent|naddr)1[a-zA-Z0-9]+/g, '').trim()
            visible: post.postType === "short_text_note" || post.postType === "quote_repost"
            wrapMode: Text.Wrap
            color: Material.foreground
        }

        Repeater {
            model: post.referencedPosts || []
            delegate: Rectangle {
                visible: true
                Layout.fillWidth: true
                color: Qt.rgba(0, 0, 0, 0.1)
                radius: 8
                border.width: 1
                border.color: Material.dividerColor

                implicitHeight: contentColumn.implicitHeight + 2 * Constants.spacing_m

                ColumnLayout {
                    id: contentColumn
                    anchors {
                        fill: parent
                        margins: Constants.spacing_m
                    }
                    spacing: Constants.spacing_s

                    // Author info row
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: Constants.spacing_m

                        ProfilePicture {
                            Layout.preferredWidth: 36
                            Layout.preferredHeight: 36
                            imageSource: modelData.author ? Util.getProfilePicture(modelData.author.picture, modelData.author.npub) : ""
                        }

                        Text {
                            Layout.fillWidth: true
                            text: modelData.author ? (modelData.author.displayName || modelData.author.name) : ""
                            font: Constants.fontMedium
                            color: Material.foreground
                            elide: Text.ElideRight
                        }

                        Text {
                            text: modelData.timestamp || ""
                            font: Constants.smallFontMedium
                            color: Material.secondaryTextColor
                        }
                    }

                    // Referenced content
                    Text {
                        Layout.fillWidth: true
                        text: (modelData.content || "").replace(/nostr:(note|nevent|naddr)1[a-zA-Z0-9]+/g, '').trim()
                        wrapMode: Text.Wrap
                        color: Material.foreground
                    }
                }
            }
        }

        // Main post actions
        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            spacing: Constants.spacing_l
            Layout.topMargin: Constants.spacing_xs

            Item { Layout.fillWidth: true }

            RowLayout {
                spacing: Constants.spacing_s
                Button {
                    flat: true
                    icon.source: "qrc:/icons/comment.svg"
                    icon.width: 20
                    icon.height: 20
                    implicitWidth: 36
                    implicitHeight: 36
                    padding: 8
                    icon.color: post.comments.length > 0 ? Material.primary : Material.secondaryTextColor
                    onClicked: commentClicked()
                }
                Text {
                    text: post.comments.length || "0"
                    color: post.comments.length > 0 ? Material.primary : Material.secondaryTextColor
                }
            }

            RowLayout {
                spacing: Constants.spacing_s
                Button {
                    flat: true
                    icon.source: "qrc:/icons/repeat.svg"
                    icon.width: 20
                    icon.height: 20
                    implicitWidth: 36
                    implicitHeight: 36
                    padding: 8
                    icon.color: post.repostCount > 0 ? Material.primary : Material.secondaryTextColor
                    onClicked: repostClicked()
                }
                Text {
                    text: post.repostCount || "0"
                    color: post.repostCount > 0 ? Material.primary : Material.secondaryTextColor
                }
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/delete.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 36
                implicitHeight: 36
                padding: 8
                icon.color: Material.secondaryTextColor
                visible: mynpub == npub

                onClicked: deleteDialog.open()
            }

            Item { Layout.fillWidth: true }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: Constants.spacing_s

            Item { Layout.fillWidth: true }

            Text {
                Layout.alignment: Qt.AlignRight
                text: post.timestamp || ""
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
                Layout.topMargin: Constants.spacing_xs
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/menu.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 36
                Layout.alignment: Qt.AlignRight
                onClicked: postMenu.open()

                Menu {
                    id: postMenu
                    y: parent.height

                    MenuItem {
                        text: qsTr("Copy Event ID")
                        onTriggered: {
                            clipboard.copyText(modelData.nevent)
                        }
                    }

                    MenuItem {
                        text: qsTr("Show Event JSON")
                        onTriggered: {
                            eventJsonDialog.targetPost = modelData
                            eventJsonDialog.open()
                        }
                    }

                    MenuItem {
                        text: qsTr("Seen on Relays")
                        onTriggered: {
                            seenOnRelaysDialog.targetPost = modelData
                            seenOnRelaysDialog.open()
                        }
                    }
                }
            }
        }
    }

    Dialog {
        id: deleteDialog
        title: "Delete Post"
        modal: true
        standardButtons: Dialog.Ok | Dialog.Cancel
        anchors.centerIn: parent
        width: 350

        ColumnLayout {
            spacing: Constants.spacing_m
            width: parent.width

            Text {
                text: "Are you sure you want to delete this post?"
                color: Material.foreground
                wrapMode: Text.Wrap
                Layout.fillWidth: true
            }

            TextField {
                id: reasonField
                placeholderText: "Reason for deletion (optional)"
                Layout.fillWidth: true
            }
        }

        onAccepted: {
            deleteEvent(post.id, reasonField.text)
            reasonField.text = ""
        }
    }
}
