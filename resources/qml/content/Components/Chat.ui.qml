import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Dialogs 1.0
import Futr 1.0
import HsQML.Model 1.0
import Profile 1.0

Rectangle {
    id: chat
    color: Material.backgroundColor
    radius: Constants.radius_m
    border.color: Material.dividerColor
    border.width: 1

    property string npub: ""
    required property string currentUser

    Component.onDestruction: {
        console.log("chat destroyed")
    }

    PostDialog {
        id: quoteReplyDialog
        inputPlaceholder: qsTr("Add a quote...")
        buttonText: qsTr("Quote")
        isQuoteMode: true
        currentUser: chat.currentUser

        onMessageSubmitted: function(text) {
            quoteRepost(targetPost.id, text)
        }
    }

    Menu {
        id: repostMenu
        property var targetPost: null

        MenuItem {
            text: qsTr("Repost")
            onTriggered: {
                if (repostMenu.targetPost.postType == "repost") {
                    repost(repostMenu.targetPost.referencedPostId, text)
                } else {
                    repost(repostMenu.targetPost.id, text)
                }
            }
        }

        MenuItem {
            text: qsTr("Quote Post")
            onTriggered: {
                quoteReplyDialog.targetPost = repostMenu.targetPost

                if (repostMenu.targetPost.postType == "repost") {
                    quoteReplyDialog.targetPost = repostMenu.targetPost.referencedPostId
                } else {
                    quoteReplyDialog.targetPost = repostMenu.targetPost
                }

                quoteReplyDialog.open()
            }
        }
    }

    ColumnLayout {
        id: mainContentArea
        anchors.fill: parent
        anchors.margins: Constants.spacing_xs

        TabBar {
            id: chatTypeSelector
            Layout.fillWidth: true
            Layout.preferredHeight: 42
            contentWidth: width

            TabButton {
                id: publicTab
                implicitWidth: chatTypeSelector.width / 2
                text: qsTr("Short Text Notes")

                background: Rectangle {
                    color: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    opacity: parent.down || parent.checked ? 0.12 : 1.0
                }

                contentItem: RowLayout {
                    spacing: Constants.spacing_s
                    width: parent.width
                    height: parent.height

                    Item { Layout.fillWidth: true }

                    Image {
                        source: "qrc:/icons/public.svg"
                        sourceSize.width: 20
                        sourceSize.height: 20
                        Layout.preferredWidth: 20
                        Layout.preferredHeight: 20
                    }

                    Text {
                        text: parent.parent.text
                        color: Material.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }

                    Item { Layout.fillWidth: true }
                }
            }

            TabButton {
                id: privateTab
                implicitWidth: chatTypeSelector.width / 2
                text: npub === currentUser ? qsTr("Messages to myself") : qsTr("Private Chat")

                background: Rectangle {
                    color: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    opacity: parent.down || parent.checked ? 0.12 : 1.0
                }

                contentItem: RowLayout {
                    spacing: Constants.spacing_s
                    width: parent.width
                    height: parent.height

                    Item { Layout.fillWidth: true }

                    Image {
                        source: "qrc:/icons/encrypted.svg"
                        sourceSize.width: 20
                        sourceSize.height: 20
                        Layout.preferredWidth: 20
                        Layout.preferredHeight: 20
                    }

                    Text {
                        text: parent.parent.text
                        color: Material.foreground
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }

                    Item { Layout.fillWidth: true }
                }
            }
        }

        // Stack Layout to switch between public/private views
        StackLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            currentIndex: chatTypeSelector.currentIndex

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true

                // Public notes list
                ScrollingListView {
                    id: postsView
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    leftMargin: 0
                    rightMargin: 2 * Constants.spacing_xs + 10
                    spacing: Constants.spacing_s

                    model: AutoListModel {
                        id: postsModel
                        source: posts
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.id === newItem.id
                        }
                    }

                    delegate: Loader {
                        id: postContentLoader
                        active: modelData !== undefined && modelData !== null
                        width: postsView.width - postsView.rightMargin
                        Layout.preferredHeight: active ? item.implicitHeight : 0

                        Component.onDestruction: {
                            postContentLoader.active = false; // Deactivate loader on destruction
                        }

                        sourceComponent: PostContent {
                            post: modelData
                            width: parent.width
                            currentUser: chat.currentUser

                            onCommentClicked: {
                                if (modelData) {
                                    //commentsDialog.targetPost = modelData
                                    //setCurrentPost(modelData.id)
                                    //commentsDialog.open()
                                }
                            }

                            onRepostClicked: {
                                if (modelData) {
                                    repostMenu.targetPost = modelData
                                    repostMenu.popup()
                                }
                            }

                            onPostClicked: {
                                if (modelData) {
                                    stackView.push(postDetailsComponent, { post: modelData })
                                }
                            }
                        }
                    }
                }

                // Input area for new public notes (at the bottom)
                MessageInput {
                    placeholderText: qsTr("What's on your mind?")
                    visible: currentUser == npub
                    buttonText: qsTr("Post")
                    onMessageSent: function(text) {
                        sendShortTextNote(text)
                    }
                    currentUser: chat.currentUser
                    Layout.fillWidth: true
                    Layout.leftMargin: 0
                    Layout.rightMargin: 0
                    Layout.bottomMargin: 0
                }
            }

            // Private Chat View
            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: Constants.spacing_s

                DMRelays {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    visible: ctxRelayMgmt.dmRelays.length == 0
                }

                // ScrollingListView {
                //     id: privateMessageListView
                //     Layout.fillWidth: true
                //     Layout.fillHeight: true
                //     visible: ctxRelayMgmt.dmRelays.length > 0

                //     model: AutoListModel {
                //         id: messagesModel
                //         source: privateMessages
                //         mode: AutoListModel.ByKey
                //         equalityTest: function (oldItem, newItem) {
                //             return oldItem.id === newItem.id
                //         }
                //     }

                //     delegate: Loader {
                //         active: modelData !== undefined && modelData !== null
                //         width: privateMessageListView.width - privateMessageListView.leftMargin - privateMessageListView.rightMargin - 8
                //         Layout.preferredHeight: active ? item.implicitHeight : 0

                //         sourceComponent: Item {
                //             property var message: modelData
                //             property var author: message ? getProfile(message.authorId) : null

                //             onMessageChanged: {
                //                 //author = message ? getProfile(message.authorId) : null
                //                 author = {
                //                     "npub": message.authorId,
                //                     "displayName": "Test User",
                //                     "name": "Test User",
                //                     "picture": "https://example.com/test-user.png"
                //                 }
                //             }

                //             height: privateRowLayout.height + Constants.spacing_xs

                //             RowLayout {
                //                 id: privateRowLayout
                //                 width: parent.width
                //                 spacing: Constants.spacing_xs
                //                 y: Constants.spacing_s
                //                 layoutDirection: author ? (author.npub == currentUser ? Qt.RightToLeft : Qt.LeftToRight) : Qt.LeftToRight

                //                 ProfilePicture {
                //                     imageSource: author ? Util.getProfilePicture(author.picture, author.npub) : ""
                //                 }

                //                 Pane {
                //                     Layout.fillWidth: true
                //                     Layout.maximumWidth: parent.width * 0.7
                //                     Layout.rightMargin: author ? (author.npub == currentUser ? 0 : Constants.spacing_s) : Constants.spacing_s
                //                     Layout.leftMargin: author ? Constants.spacing_s : 0

                //                     background: Rectangle {
                //                         color: author ? (author.npub == currentUser ? Material.accentColor : Material.dividerColor) : Material.dividerColor
                //                         radius: Constants.radius_m
                //                     }

                //                     ColumnLayout {
                //                         width: parent.width

                //                         Text {
                //                             Layout.fillWidth: true
                //                             text: message ? message.content : ""
                //                             wrapMode: Text.Wrap
                //                             color: Material.foreground
                //                         }

                //                         RowLayout {
                //                             Layout.fillWidth: true

                //                             Item {
                //                                 Layout.fillWidth: true
                //                                 visible: author.npub === currentUser
                //                             }

                //                             Text {
                //                                 Layout.alignment: author ? (author.npub == currentUser ? Qt.AlignRight : Qt.AlignLeft) : Qt.AlignLeft
                //                                 text: message ? message.timestamp : ""
                //                                 font: Constants.smallFontMedium
                //                                 color: Material.secondaryTextColor
                //                                 Layout.topMargin: Constants.spacing_xs
                //                                 visible: author.npub === currentUser
                //                             }

                //                             Button {
                //                                 flat: true
                //                                 icon.source: "qrc:/icons/menu.svg"
                //                                 icon.width: 20
                //                                 icon.height: 20
                //                                 implicitWidth: 28
                //                                 implicitHeight: 28
                //                                 padding: 4
                //                                 Layout.alignment: Qt.AlignRight
                //                                 onClicked: postMenu.open()

                //                                 Menu {
                //                                     id: postMenu
                //                                     y: parent.height

                //                                     MenuItem {
                //                                         text: qsTr("Copy Event ID")
                //                                         onTriggered: {
                //                                             clipboard.copyText(modelData.nevent)
                //                                         }
                //                                     }

                //                                     MenuItem {
                //                                         text: qsTr("Show Event JSON")
                //                                         onTriggered: {
                //                                             eventJsonDialog.targetPost = modelData
                //                                             eventJsonDialog.open()
                //                                         }
                //                                     }

                //                                     MenuItem {
                //                                         text: qsTr("Seen on Relays")
                //                                         onTriggered: {
                //                                             seenOnRelaysDialog.targetPost = modelData
                //                                             seenOnRelaysDialog.open()
                //                                         }
                //                                     }
                //                                 }
                //                             }

                //                             Text {
                //                                 Layout.alignment: author ? (author.npub == currentUser ? Qt.AlignRight : Qt.AlignLeft) : Qt.AlignLeft
                //                                 text: message ? message.timestamp : ""
                //                                 font: Constants.smallFontMedium
                //                                 color: Material.secondaryTextColor
                //                                 Layout.topMargin: Constants.spacing_xs
                //                                 visible: author.npub !== currentUser
                //                             }

                //                             Item {
                //                                 Layout.fillWidth: true
                //                                 visible: author.npub !== currentUser
                //                             }
                //                         }
                //                     }
                //                 }
                //             }
                //         }
                //     }
                // }

                // Private Messages Input
                MessageInput {
                    placeholderText: qsTr("Type a message...")
                    buttonText: qsTr("Send")
                    onMessageSent: function(text) {
                        sendPrivateMessage(text)
                    }
                    currentUser: chat.currentUser
                    Layout.fillWidth: true
                    Layout.leftMargin: 0
                    Layout.rightMargin: 0
                    Layout.bottomMargin: 0
                }
            }
        }
    }

    // Add loading indicator
    BusyIndicator {
        id: loadingIndicator
        anchors.centerIn: parent
        visible: false
        running: visible
        z: 1000
    }

    // Add error banner
    Rectangle {
        id: errorBanner
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        height: errorText.height + 2 * Constants.spacing_s
        color: Material.color(Material.Red)
        visible: false
        z: 1000

        Text {
            id: errorText
            anchors.centerIn: parent
            color: "white"
            font: Constants.fontMedium
        }

        function showError(message) {
            errorText.text = message
            visible = true
            errorTimer.restart()
        }

        Timer {
            id: errorTimer
            interval: 3000
            onTriggered: errorBanner.visible = false
        }
    }

    // Add states for UI elements
    states: [
        State {
            name: "loading"
            PropertyChanges { target: loadingIndicator; visible: true }
            PropertyChanges { target: mainContentArea; opacity: 0.5 }
        },
        State {
            name: "error"
            PropertyChanges { target: errorBanner; visible: true }
        },
        State {
            name: "ready"
            PropertyChanges { target: mainContentArea; opacity: 1.0 }
        }
    ]

    transitions: [
        Transition {
            from: "*"; to: "*"
            NumberAnimation {
                properties: "opacity"
                duration: 150
                easing.type: Easing.InOutQuad
            }
        }
    ]

    EventJSONDialog {
        id: eventJsonDialog
    }

    SeenOnRelaysDialog {
        id: seenOnRelaysDialog
    }
}
