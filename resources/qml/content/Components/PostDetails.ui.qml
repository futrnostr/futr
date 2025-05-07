import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Controls.Material.impl 2.15
import QtQuick.Layouts 1.15

import Components 1.0
import Futr 1.0
import Dialogs 1.0
import HsQML.Model 1.0

Page {
    id: root

    property var post: null
    required property string currentUser
    required property string currentUserPicture
    property var comments: post ? post.comments : []

    Component.onCompleted: {
        setCurrentPost(post ? post.id : null)
    }

    Component.onDestruction: {
        setCurrentPost(null)
    }

    onPostChanged: {
        setCurrentPost(post ? post.id : null)
    }

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

    RepostMenu {
        id: repostMenu
    }

    EventJSONDialog {
        id: eventJsonDialog
    }

    SeenOnRelaysDialog {
        id: seenOnRelaysDialog
    }

    PostDialog {
        id: commentDialog
        isQuoteMode: false
        currentUser: root.currentUser
        currentUserPicture: root.currentUserPicture

        onMessageSubmitted: function(text) {
            comment(targetPost.id, text)
        }
    }

    PostDialog {
        id: quoteReplyDialog // used by RepostMenu
        isQuoteMode: true
        currentUser: root.currentUser
        currentUserPicture: root.currentUserPicture

        onMessageSubmitted: function(text) {
            quoteRepost(targetPost.id, text)
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
                showAuthor: true
                disableCommentAction: true

                onRepostClicked: {
                    if (post) {
                        repostMenu.targetPost = post
                        repostMenu.popup()
                    }
                }
            }

            MessageInput {
                id: replyInput
                height: implicitHeight
                placeholderText: qsTr("Reply to post...")
                buttonText: qsTr("Send")
                currentUser: root.currentUser
                currentUserPicture: root.currentUserPicture
                Layout.margins: 0
                Layout.rightMargin: Constants.spacing_m
                width: scrollView.width

                onMessageSent: function(text) {
                    if (root.post && root.post.id) {
                        comment(root.post.id, text)
                        text = ""
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.leftMargin: 0
                Layout.rightMargin: Constants.spacing_m
                width: scrollView.width
                spacing: Constants.spacing_s

                Label {
                    text: qsTr("Seen on relays:")
                    color: Material.foreground
                    Layout.leftMargin: 0
                    font: Constants.fontMedium
                }

                Text {
                    text: post ? post.relays.join(", ") : ""
                    color: Material.foreground
                    wrapMode: Text.Wrap
                    Layout.fillWidth: true
                }
            }

            Label {
                text: qsTr("Comments: ") + comments.length
                color: Material.foreground
                Layout.leftMargin: 0
                font: Constants.fontMedium
            }

            // Comments section
            ScrollingListView {
                id: commentsView
                Layout.fillWidth: true
                Layout.preferredHeight: contentHeight
                Layout.leftMargin: 0
                Layout.rightMargin: Constants.spacing_m
                Layout.maximumHeight: 1000
                leftMargin: 0
                spacing: 2

                model: AutoListModel {
                    id: commentsModel
                    source: comments
                    mode: AutoListModel.ByKey
                }

                delegate: Item {
                    width: commentsView.width - Constants.spacing_m - 1
                    height: postContent.implicitHeight

                    property int indentation: modelData ? modelData.indentationLevel * Constants.spacing_xl : 0

                    PostContent {
                        id: postContent
                        post: modelData ? getPost(modelData.post) : null
                        currentUser: root.currentUser
                        width: parent.width - indentation
                        x: indentation
                        showAuthor: true

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
            }

            Item {
                Layout.fillHeight: true
                height: 20
            }
        }
    }
}
