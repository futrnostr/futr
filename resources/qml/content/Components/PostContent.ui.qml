import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Pane {
    id: root

    property var post
    property string currentUser
    property bool isRefPost: false
    property var author
    property bool privateChatMode: false

    property var componentMap: {
        "text": "PostContent/TextComponent.ui.qml",
        "image": "PostContent/PostImage.ui.qml",
        "video": "PostContent/PostVideo.ui.qml",
        "note": "PostContent/ReferencedPost.ui.qml",
        "nevent": "PostContent/ReferencedPost.ui.qml",
        "naddr": "PostContent/ReferencedPost.ui.qml",
    }

    signal commentClicked()
    signal repostClicked()
    signal postClicked()

    padding: Constants.spacing_s

    Component.onCompleted: {
        if (!post) {
            console.warn("Post is undefined")
            return
        }

        if (post.authorId) {
            author = getProfile(post.authorId)
        }

        if (!post.contentParts) {
            console.warn("Post contentParts is undefined for post:", post.id)
            return
        }

        let parts = post.contentParts

        for (var i = 0; i < parts.length; i++) {
            var type = parts[i][0]
            var value = parts[i][1]

            var component = Qt.createComponent(componentMap[type])

            if (component.status === Component.Ready) {
                let args = {}
                if (componentMap[type] === "PostContent/ReferencedPost.ui.qml") {
                    args = {
                        "value": value,
                        "Layout.fillWidth": true,
                        "currentUser": currentUser,
                        "parent": contentLayout,
                        "Layout.minimumHeight": 100,
                    }
                } else {
                    args = {
                        "value": value,
                        "Layout.fillWidth": true,
                        "parent": contentLayout,
                    }
                }
                var item = component.createObject(contentLayout, args)
                if (item === null) {
                    console.error("Failed to create object for", value)
                }
            } else {
                console.error("Failed to load component:", component.errorString())
            }
        }
    }

    Component.onDestruction: {
        author = null

        for(let i = contentLayout.children.length - 1; i >= 0; i--) {
            contentLayout.children[i].destroy()
        }
    }

    background: Rectangle {
        id: backgroundRect
        color: privateChatMode && author && author.npub == currentUser ? Material.accentColor : Material.dialogColor
        radius: Constants.radius_m

        MouseArea {
            id: postClickArea
            anchors.fill: parent
            enabled: true
            propagateComposedEvents: true
            cursorShape: Qt.PointingHandCursor
            z: -1

            onPressed: {
                if (!mouse.wasHeld) {
                    root.postClicked()
                }
                mouse.accepted = false;
            }
        }
    }

    ColumnLayout {
        id: mainColumn
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.rightMargin: 0
        spacing: Constants.spacing_xs

        RowLayout {
            Layout.fillWidth: true
            Layout.bottomMargin: Constants.spacing_xs
            visible: post != null && post != undefined && (post.postType === "quote_repost" || post.postType === "repost")

            Image {
                source: "qrc:/icons/repeat.svg"
                sourceSize.width: 20
                sourceSize.height: 20
                Layout.alignment: Qt.AlignVCenter
            }

            Text {
                text: post && post.postType === "repost" ? qsTr("Reposted") : qsTr("Quote Reposted")
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }

        RowLayout {
            Layout.fillWidth: true
            visible: isRefPost

            Rectangle {
                Layout.preferredWidth: root.isCollapsed ? 30 : 34
                Layout.preferredHeight: Layout.preferredWidth
                Layout.alignment: Qt.AlignVCenter
                radius: width/2
                color: Material.backgroundColor

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor

                    onClicked: {
                        personalFeed.npub = author.npub
                    }

                    Image {
                        anchors.fill: parent
                        anchors.margins: Constants.spacing_xs
                        source: author ? Util.getProfilePicture(author.picture, author.pubkey) : ""
                        fillMode: Image.PreserveAspectCrop
                        cache: false
                    }
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 2

                Item {
                    Layout.fillHeight: true
                    Layout.fillWidth: true

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor

                        onClicked: {
                            personalFeed.npub = author.npub
                        }
                    }

                    Text {
                        anchors.left: parent.left
                        anchors.bottom: parent.verticalCenter
                        font: Constants.font
                        text: author ? (author.displayName || author.name || "") : ""
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.primaryTextColor
                    }

                    Text {
                        anchors.left: parent.left
                        anchors.top: parent.verticalCenter
                        text: author ? author.npub : ""
                        font.pixelSize: Constants.font.pixelSize * 0.8
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.secondaryTextColor
                    }
                }
            }
        }


        ColumnLayout {
            id: contentLayout
            Layout.fillWidth: true
            Layout.bottomMargin: Constants.spacing_xs
            spacing: 0
        }

        // Main post actions
        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            spacing: Constants.spacing_l
            Layout.topMargin: 0
            Layout.bottomMargin: 0
            Layout.preferredHeight: 36
            visible: !isRefPost && !privateChatMode

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
                    icon.color: post && post.comments && post.comments.length > 0 ? Material.primary : Material.secondaryTextColor
                    onClicked: commentClicked()
                }
                Text {
                    text: post && post.comments ? post.comments.length : "0"
                    color: post && post.comments && post.comments.length > 0 ? Material.primary : Material.secondaryTextColor
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
                    icon.color: post && post.repostCount > 0 ? Material.primary : Material.secondaryTextColor
                    onClicked: repostClicked()
                }
                Text {
                    text: post ? post.repostCount : "0"
                    color: post && post.repostCount > 0 ? Material.primary : Material.secondaryTextColor
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
                visible: post != null && post != undefined && currentUser == post.authorId

                onClicked: deleteDialog.open()
            }

            Item { Layout.fillWidth: true }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.topMargin: 0
            Layout.bottomMargin: 0
            Layout.preferredHeight: 28

            Item { Layout.fillWidth: true }

            Text {
                Layout.alignment: Qt.AlignRight
                text: post ? post.timestamp : ""
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/menu.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 28
                implicitHeight: 28
                padding: 4
                Layout.alignment: Qt.AlignRight
                onClicked: postMenu.open()

                Menu {
                    id: postMenu
                    y: parent.height

                    MenuItem {
                        text: qsTr("Copy Event ID")
                        onTriggered: {
                            clipboard.copyText(post.nevent)
                        }
                    }

                    MenuItem {
                        text: qsTr("Show Event JSON")
                        onTriggered: {
                            eventJsonDialog.targetPost = post
                            eventJsonDialog.open()
                        }
                    }

                    MenuItem {
                        text: qsTr("Seen on Relays")
                        onTriggered: {
                            seenOnRelaysDialog.targetPost = post
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

    function openFullscreenVideo(videoUrl) {
        var component = Qt.createComponent("FullscreenVideoWindow.ui.qml");
        if (component.status === Component.Ready) {
            var window = component.createObject(root, {
                "videoUrl": videoUrl
            });
            window.Component.onDestruction.connect(function() {
                component.destroy();
            });
        } else {
            console.error("Error loading component:", component.errorString());
        }
    }

    function copyToClipboard(text) {
        clipboard.copyText(text)
    }

    function downloadVideo(url) {
        downloadCompleted.connect(videoDownloadCallback)
        downloadAsync(url)
    }

    function videoDownloadCallback(success, filePathOrError) {
        downloadCompleted.disconnect(videoDownloadCallback)
    }
}
