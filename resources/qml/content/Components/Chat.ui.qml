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
    color: "transparent"
    radius: Constants.radius_m
    border.color: Material.dividerColor
    border.width: 1

    required property string npub
    required property string currentUser
    required property string currentUserPicture

    onNpubChanged: {
        chatTypeSelector.currentIndex = 0

        Qt.callLater(function() {
            var publicChat = mainContentArea.children[1].item
            if (publicChat && publicChat.postsView) {
                publicChat.postsView.shouldBeAtBottom = true
            }
        })
    }

    PostDialog {
        id: commentDialog
        isQuoteMode: false
        currentUser: chat.currentUser
        currentUserPicture: chat.currentUserPicture

        onMessageSubmitted: function(text) {
            comment(targetPost.id, text)
        }
    }

    PostDialog {
        id: quoteReplyDialog // used by RepostMenu
        isQuoteMode: true
        currentUser: chat.currentUser
        currentUserPicture: chat.currentUserPicture

        onMessageSubmitted: function(text) {
            quoteRepost(targetPost.id, text)
        }
    }

    RepostMenu {
        id: repostMenu
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

                // I want rounded corner left-upper side, so we paint!
                background: Canvas {
                    width: parent.width
                    height: parent.height

                    property color fillColor: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    property real fillOpacity: parent.down || parent.checked ? 0.2 : 1.0

                    onFillColorChanged: requestPaint()
                    onFillOpacityChanged: requestPaint()

                    onPaint: {
                        var ctx = getContext("2d")
                        ctx.clearRect(0, 0, width, height)

                        if (fillColor !== "transparent") {
                            var colorStr = fillColor.toString()
                            ctx.globalAlpha = fillOpacity
                            ctx.fillStyle = colorStr

                            var radius = Constants.radius_m
                            ctx.beginPath()
                            ctx.moveTo(radius, 0)
                            ctx.lineTo(width, 0)
                            ctx.lineTo(width, height)
                            ctx.lineTo(0, height)
                            ctx.lineTo(0, radius)
                            ctx.arcTo(0, 0, radius, 0, radius)
                            ctx.closePath()
                            ctx.fill()

                            ctx.globalAlpha = 1.0
                        }
                    }
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

                background: Canvas {
                    width: parent.width
                    height: parent.height

                    property color fillColor: parent.down || parent.checked ? Material.primaryColor : "transparent"
                    property real fillOpacity: parent.down || parent.checked ? 0.2 : 1.0

                    onFillColorChanged: requestPaint()
                    onFillOpacityChanged: requestPaint()

                    // again, rounded corners, upper right side, so we paint!
                    onPaint: {
                        var ctx = getContext("2d")
                        ctx.clearRect(0, 0, width, height)

                        if (fillColor !== "transparent") {
                            var colorStr = fillColor.toString()
                            ctx.globalAlpha = fillOpacity
                            ctx.fillStyle = colorStr

                            var radius = Constants.radius_m
                            ctx.beginPath()
                            ctx.moveTo(0, 0)
                            ctx.lineTo(width - radius, 0)
                            ctx.arcTo(width, 0, width, radius, radius)
                            ctx.lineTo(width, height)
                            ctx.lineTo(0, height)
                            ctx.lineTo(0, 0)
                            ctx.closePath()
                            ctx.fill()

                            ctx.globalAlpha = 1.0
                        }
                    }
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

        Loader {
            Layout.fillWidth: true
            Layout.fillHeight: true
            sourceComponent: chatTypeSelector.currentIndex === 0 ? publicNotesComponent : privateChatComponent
        }

        Component {
            id: publicNotesComponent
            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true

                ScrollingListView {
                    id: postsView
                    verticalLayoutDirection: ListView.BottomToTop
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    leftMargin: 0
                    rightMargin: 2 * Constants.spacing_xs + 10
                    spacing: Constants.spacing_s

                    model: AutoListModel {
                        id: postsModel
                        source: posts
                        mode: AutoListModel.ByKey
                    }

                    delegate: PostContent {
                        width: ListView.view.width - postsView.rightMargin
                        post: modelData
                        currentUser: chat.currentUser
                        Layout.minimumHeight: 100

                        onCommentClicked: {
                            if (post) {
                                commentDialog.targetPost = post
                                commentDialog.open()
                            }
                        }

                        onRepostClicked: {
                            if (post) {
                                repostMenu.targetPost = post
                                repostMenu.popup()
                            }
                        }

                        onPostClicked: {
                            if (post) {
                                stackView.push(postDetailsComponent, { post: post })
                            }
                        }
                    }
                }

                MessageInput {
                    placeholderText: qsTr("What's on your mind?")
                    visible: currentUser == npub
                    buttonText: qsTr("Post")
                    onMessageSent: function(text) {
                        sendShortTextNote(text)
                    }
                    currentUser: chat.currentUser
                    currentUserPicture: chat.currentUserPicture
                    Layout.fillWidth: true
                    Layout.leftMargin: 0
                    Layout.rightMargin: 0
                    Layout.bottomMargin: 0
                }
            }
        }

        Component {
            id: privateChatComponent
            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: Constants.spacing_s

                DMRelays {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    visible: ctxRelayMgmt.dmRelays.length == 0
                }

                ScrollingListView {
                    id: privateMessageListView
                    verticalLayoutDirection: ListView.BottomToTop
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    leftMargin: 0
                    rightMargin: 2 * Constants.spacing_xs + 10
                    spacing: Constants.spacing_s
                    visible: ctxRelayMgmt.dmRelays.length > 0

                    model: AutoListModel {
                        id: messagesModel
                        source: privateMessages
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.id === newItem.id
                        }
                    }

                    delegate: RowLayout {
                        width: ListView.view.width - privateMessageListView.rightMargin

                        property var author: modelData ? getProfile(modelData.authorId) : null

                        ProfilePicture {
                            imageSource: author ? Util.getProfilePicture(author.picture, author.npub) : ""
                            Layout.preferredWidth: 34
                            Layout.preferredHeight: 34
                            visible: author && author.npub != chat.currentUser
                        }

                        PostContent {
                            post: modelData
                            currentUser: chat.currentUser
                            privateChatMode: true
                            Layout.fillWidth: true
                            Layout.leftMargin: author && author.npub == chat.currentUser ? 75 : 0
                            Layout.rightMargin: author && author.npub != chat.currentUser ? 75 : 0
                        }

                        ProfilePicture {
                            imageSource: author ? Util.getProfilePicture(author.picture, author.npub) : ""
                            Layout.preferredWidth: 34
                            Layout.preferredHeight: 34
                            visible: author && author.npub == chat.currentUser
                        }
                    }
                }

                MessageInput {
                    placeholderText: qsTr("Type a message...")
                    buttonText: qsTr("Send")
                    onMessageSent: function(text) {
                        sendPrivateMessage(text)
                    }
                    currentUser: chat.currentUser
                    currentUserPicture: chat.currentUserPicture
                    Layout.fillWidth: true
                    Layout.leftMargin: 0
                    Layout.rightMargin: 0
                    Layout.bottomMargin: 0
                }
            }
        }
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

    EventJSONDialog {
        id: eventJsonDialog
    }

    SeenOnRelaysDialog {
        id: seenOnRelaysDialog
    }
}
