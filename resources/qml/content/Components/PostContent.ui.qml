import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtGraphicalEffects 1.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Pane {
    id: root

    property var post
    property string currentUser
    property bool isRefPost: false
    property var author: visible && post && post.authorId ? getProfile(post.authorId) : null
    property bool privateChatMode: false
    property var contentParts: visible && post ? post.contentParts : []
    property var comments: visible && post ? post.comments : []
    property bool hideActions: false
    property bool showAuthor: false
    property bool disableCommentAction: false

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

    padding: Constants.spacing_xs

    onContentPartsChanged: {
        updateContent()
    }

    onVisibleChanged: {
        if (visible && post && !author && post.authorId) {
            author = getProfile(post.authorId)
        }
        if (visible && post) {
            contentParts = post.contentParts
            comments = post.comments
        }

    }

    Component.onDestruction: {
        author = null

        contentLayout.children = []
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

    Column {
        id: mainColumn
        anchors.fill: parent
        spacing: Constants.spacing_xs

        Row {
            width: parent.width
            visible: post != null && post != undefined && (post.postType === "quote_repost" || post.postType === "repost")

            Image {
                source: "qrc:/icons/repeat.svg"
                sourceSize.width: 20
                sourceSize.height: 20
            }

            Text {
                text: post && post.postType === "repost" ? qsTr("Reposted") : qsTr("Quote Reposted")
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }

        Row {
            width: parent.width
            visible: showAuthor || isRefPost
            spacing: Constants.spacing_s

            Rectangle {
                width: root.isCollapsed ? 30 : 34
                height: root.isCollapsed ? 30 : 34

                radius: width/2
                color: "transparent"
                border.width: 1
                border.color: Material.dividerColor

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor

                    onClicked: {
                        personalFeed.npub = root.author.npub
                    }
                }

                Image {
                    anchors.fill: parent
                    anchors.margins: Constants.spacing_xs
                    source: root.author ? root.author.getProfilePicture(root.author.picture) : ""
                    fillMode: Image.PreserveAspectCrop
                    cache: false
                }
            }

            MouseArea {
                width: parent.width - 50 // Account for profile picture width + spacing
                height: childrenRect.height
                cursorShape: Qt.PointingHandCursor
                anchors.verticalCenter: parent.verticalCenter

                onClicked: {
                    personalFeed.npub = root.author.npub
                }

                Column {
                    spacing: 2
                    width: parent.width

                    Text {
                        font: Constants.font
                        text: root.author ? (root.author.displayName || root.author.name || "") : ""
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.primaryTextColor
                    }

                    Text {
                        text: root.author ? root.author.npub : ""
                        font.pixelSize: Constants.font.pixelSize * 0.8
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.secondaryTextColor
                    }
                }
            }
        }


        Column {
            id: contentLayout
            width: parent.width
            spacing: 0
        }

        // Main post actions
        Row {
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: Constants.spacing_l
            height: 36
            visible: !hideActions && !isRefPost && !privateChatMode

            Row {
                spacing: Constants.spacing_s
                visible: !disableCommentAction

                Button {
                    flat: true
                    icon.source: "qrc:/icons/comment.svg"
                    icon.width: 20
                    icon.height: 20
                    implicitWidth: 36
                    implicitHeight: 36
                    padding: 8
                    icon.color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                    onClicked: commentClicked()
                }

                Text {
                    text: comments.length
                    color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                }
            }

            Row {
                spacing: Constants.spacing_s
                visible: disableCommentAction

                Image {
                    id: commentIcon
                    source: "qrc:/icons/comment.svg"
                    width: 20
                    height: 20
                    anchors.verticalCenter: parent.verticalCenter

                    ColorOverlay {
                        anchors.fill: parent
                        source: parent
                        color: Material.secondaryTextColor
                        cached: true
                    }
                }

                Text {
                    text: comments.length
                    color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                }
            }

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
                anchors.verticalCenter: parent.verticalCenter
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
                anchors.verticalCenter: parent.verticalCenter

                onClicked: deleteDialog.open()
            }
        }

        Row {
            anchors.right: parent.right

            Text {
                text: post ? post.timestamp : ""
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
                anchors.verticalCenter: parent.verticalCenter
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/menu.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 28
                implicitHeight: 28
                padding: 4
                anchors.verticalCenter: parent.verticalCenter

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

        Column {
            spacing: Constants.spacing_m
            width: parent.width

            Text {
                text: "Are you sure you want to delete this post?"
                color: Material.foreground
                wrapMode: Text.Wrap
                width: parent.width
            }

            TextField {
                id: reasonField
                placeholderText: "Reason for deletion (optional)"
                width: parent.width
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

    function updateContent() {
        if (!contentParts || !Array.isArray(contentParts)) {
            return
        }

        contentLayout.children = []
        let parts = contentParts
        for (var i = 0; i < parts.length; i++) {
            var type = parts[i][0]
            var value = parts[i][1]

            if (!componentMap[type]) {
                console.error("Unknown content part type:", type)
                continue
            }

            var component = Qt.createComponent(componentMap[type])

            if (component.status === Component.Ready) {
                let args = {
                    "value": value,
                    "width": Qt.binding(function() { return contentLayout.width }),
                }

                if (type === "image") {
                    args["asynchronous"] = true
                }

                if (componentMap[type] === "PostContent/ReferencedPost.ui.qml") {
                    args["currentUser"] = currentUser
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
}
