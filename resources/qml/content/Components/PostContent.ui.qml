import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0

Pane {
    id: root
    padding: Constants.spacing_s
    width: parent.width - 2 * Constants.spacing_m
    anchors.right: parent.right
    anchors.rightMargin: Constants.spacing_s
    anchors.left: parent.left
    anchors.leftMargin: 0

    required property var post

    signal commentClicked()
    signal repostClicked()

    // HTML escaping utility function
    function escapeHtml(str) {
        if (typeof str !== 'string') return '';
        return str.replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;')
                .replace(/"/g, '&quot;')
                .replace(/'/g, '&#39;')
                .replace(/\r\n|\r/g, '<br>')
                .replace(/\n/g, '<br>')
                .replace(/\t/g, '&nbsp;&nbsp;&nbsp;&nbsp;');
    }

    background: Rectangle {
        color: Material.dialogColor
        radius: 10
    }

    ColumnLayout {
        id: mainColumn
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.rightMargin: 5
        spacing: Constants.spacing_xs

        // Main post content
        ColumnLayout {
            id: contentLayout
            Layout.fillWidth: true
            spacing: Constants.spacing_xs
            visible: post.postType === "short_text_note" || post.postType === "quote_repost"

            TextEdit {
                id: contentTextEdit
                Layout.fillWidth: true
                readOnly: true
                selectByMouse: true
                wrapMode: Text.Wrap
                textFormat: Text.RichText
                color: Material.foreground

                Component.onCompleted: {
                    if (post && post.contentParts) {
                        // Only process text content here
                        text = generateHtmlContent(post.contentParts, false);
                    }
                }

                function generateHtmlContent(parts, isRefPost) {
                    let htmlText = "";

                    for (let i = 0; i < parts.length; i++) {
                        const part = parts[i];
                        const type = part[0];
                        const content = part.length > 1 ? part[1] : "";

                        if (type === "text") {
                            htmlText += escapeHtml(content);
                        } else if (type === "url") {
                            htmlText += `<a href="${content}" style="color: ${Material.accentColor};">${escapeHtml(content)}</a>`;
                        } else if (type === "nprofile" || type === "npub") {
                            const profile = getProfile(content);
                            const displayName = profile && (profile.displayName || profile.name) ?
                                               (profile.displayName || profile.name) :
                                               content.substring(0, 18) + "...";
                            htmlText += `<a href="profile://${content}" style="color: ${Material.accentColor};">@${displayName}</a>`;
                        } else if (type === "image") {
                            // Create image component with debug logging
                            if (!isRefPost) {
                                let imageComponent = Qt.createComponent("PostImage.ui.qml");
                                let imageObject = imageComponent.createObject(contentLayout, {
                                    "source": content,
                                    "width": contentLayout.width,
                                    "clickable": true,
                                    "imageUrl": content
                                });
                                imageObject.Layout.fillWidth = true;
                                imageObject.parent = contentLayout;
                                imageObject.imageClicked.connect(function(url) {
                                    imageViewerDialog.imageSource = url;
                                    imageViewerDialog.open();
                                });
                            }
                        } else if (type === "video") {
                            // Create video component
                            if (!isRefPost) {
                                let videoComponent = Qt.createComponent("PostVideo.ui.qml");
                                if (videoComponent.status === Component.Ready) {
                                    let videoObject = videoComponent.createObject(contentLayout, {
                                        "source": content,
                                        "width": contentLayout.width,
                                        "clickable": true,
                                        "videoUrl": content
                                    });

                                    if (videoObject) {
                                        videoObject.Layout.fillWidth = true;
                                        videoObject.parent = contentLayout;
                                        videoObject.fullScreenRequested.connect(function(url) {
                                            videoObject.pause();
                                            videoViewerDialog.videoSource = url;
                                            videoViewerDialog.open();
                                        });
                                    } else {
                                        console.error("Failed to create video object");
                                    }
                                } else {
                                    console.error("Error loading video component:", videoComponent.errorString());
                                }
                            }
                        } else {
                            // nostr references (note, nevent, etc)
                            let label = type === "note" || type === "nevent" || type === "naddr" ? "📝 Note" : "🔗 Reference";
                            htmlText += `<a href="note://${content}" style="color: ${Material.accentColor};">${label}</a>`;
                        }
                    }

                    return htmlText;
                }

                onLinkActivated: function(link) {
                    if (link.startsWith("profile://")) {
                        // Handle profile navigation
                        let profileId = link.substring(10);
                        mainStack.push("ProfileView.qml", { profileId: profileId });
                    } else if (link.startsWith("note://")) {
                        // Handle note references
                        console.log("Note clicked:", link.substring(7));
                    } else {
                        // External URLs
                        Qt.openUrlExternally(link);
                    }
                }
            }
        }

        // Referenced post component with simplified content
        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: referencedPostLoader.item ? referencedPostLoader.item.height : 0
            visible: post.referencedPost !== null &&
                     (post.postType === "quote_repost" || post.postType === "repost")

            Loader {
                id: referencedPostLoader
                width: parent.width
                anchors.left: parent.left
                anchors.right: parent.right
                active: post.referencedPost !== null
                sourceComponent: Rectangle {
                    width: parent.width
                    color: Qt.rgba(0, 0, 0, 0.1)
                    radius: 8
                    border.width: 1
                    border.color: Material.dividerColor

                    property var refPost: post.referencedPost

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
                                imageSource: refPost && refPost.author ?
                                    Util.getProfilePicture(refPost.author.picture, refPost.author.npub) : ""
                            }

                            Text {
                                Layout.fillWidth: true
                                text: refPost && refPost.author ?
                                    (refPost.author.displayName || refPost.author.name) : ""
                                font: Constants.fontMedium
                                color: Material.foreground
                                elide: Text.ElideRight
                            }

                            Text {
                                text: refPost ? (refPost.timestamp || "") : ""
                                font: Constants.smallFontMedium
                                color: Material.secondaryTextColor
                            }
                        }

                        // Referenced content with proper image handling
                        ColumnLayout {
                            id: refContentLayout
                            Layout.fillWidth: true
                            spacing: Constants.spacing_xs

                            TextEdit {
                                id: refContentTextEdit
                                Layout.fillWidth: true
                                readOnly: true
                                selectByMouse: true
                                wrapMode: Text.Wrap
                                textFormat: Text.RichText
                                color: Material.foreground

                                Component.onCompleted: {
                                    if (refPost && refPost.contentParts) {
                                        text = generateHtmlContent(refPost.contentParts, true);
                                    }
                                }

                                function generateHtmlContent(parts, isRefPost) {
                                    let htmlText = "";

                                    for (let i = 0; i < parts.length; i++) {
                                        const part = parts[i];
                                        const type = part[0];
                                        const content = part.length > 1 ? part[1] : "";

                                        if (type === "text") {
                                            htmlText += escapeHtml(content);
                                        } else if (type === "url") {
                                            htmlText += `<a href="${content}" style="color: ${Material.accentColor};">${escapeHtml(content)}</a>`;
                                        } else if (type === "nprofile" || type === "npub") {
                                            const profile = getProfile(content);
                                            const displayName = profile && (profile.displayName || profile.name) ?
                                                               (profile.displayName || profile.name) :
                                                               content.substring(0, 8) + "...";
                                            htmlText += `<a href="profile://${content}" style="color: ${Material.accentColor};">@${displayName}</a>`;
                                        } else if (type === "image") {
                                            let imageComponent = Qt.createComponent("PostImage.ui.qml");
                                            let imageObject = imageComponent.createObject(refContentLayout, {
                                                "source": content,
                                                "width": refContentLayout.width,
                                                "clickable": true,
                                                "imageUrl": content
                                            });
                                            imageObject.Layout.fillWidth = true;
                                            imageObject.parent = refContentLayout;
                                            imageObject.imageClicked.connect(function(url) {
                                                imageViewerDialog.imageSource = url;
                                                imageViewerDialog.open();
                                            });
                                        } else if (type === "video") {
                                            let videoComponent = Qt.createComponent("PostVideo.ui.qml");
                                            if (videoComponent.status === Component.Ready) {
                                                let videoObject = videoComponent.createObject(refContentLayout, {
                                                    "source": content,
                                                    "width": refContentLayout.width,
                                                    "clickable": true,
                                                    "videoUrl": content
                                                });

                                                if (videoObject) {
                                                    videoObject.Layout.fillWidth = true;
                                                    videoObject.parent = refContentLayout;
                                                    videoObject.videoClicked.connect(function(url) {
                                                        console.log("Video clicked in reference post:", url);
                                                    });
                                                    videoObject.fullScreenRequested.connect(function(url) {
                                                        videoObject.pause();
                                                        videoViewerDialog.videoSource = url;
                                                        videoViewerDialog.open();
                                                    });
                                                } else {
                                                    console.error("Failed to create video object in reference post");
                                                }
                                            } else {
                                                // nostr references (note, nevent, etc)
                                                let label = type === "note" || type === "nevent" || type === "naddr" ? "📝 Note" : "🔗 Reference";
                                                htmlText += `<a href="note://${content}" style="color: ${Material.accentColor};">${label}</a>`;
                                            }
                                        }
                                    }

                                    return htmlText;
                                }

                                onLinkActivated: function(link) {
                                    if (link.startsWith("profile://")) {
                                        // Handle profile navigation
                                        let profileId = link.substring(10);
                                        mainStack.push("ProfileView.qml", { profileId: profileId });
                                    } else if (link.startsWith("note://")) {
                                        // Handle note references
                                        console.log("Note clicked:", link.substring(7));
                                    } else {
                                        // External URLs
                                        Qt.openUrlExternally(link);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Main post actions
        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            spacing: Constants.spacing_l
            Layout.topMargin: 0

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
            Layout.topMargin: 0
            spacing: Constants.spacing_s

            Item { Layout.fillWidth: true }

            Text {
                Layout.alignment: Qt.AlignRight
                text: post.timestamp || ""
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
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

    Dialog {
        id: imageViewerDialog
        title: "View Image"
        modal: true
        standardButtons: Dialog.Close
        anchors.centerIn: Overlay.overlay
        width: Math.min(appWindow.width * 0.9, 1000)
        height: Math.min(appWindow.height * 0.8, 800)

        property string imageSource: ""

        contentItem: Item {
            id: imageDialogContent
            anchors.fill: parent

            Image {
                id: fullSizeImage
                source: imageViewerDialog.imageSource
                fillMode: Image.PreserveAspectFit
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.topMargin: 20
                height: parent.height - buttonRow.height - Constants.spacing_m * 4
            }

            Row {
                id: buttonRow
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottomMargin: Constants.spacing_m * 2
                spacing: Constants.spacing_m
                height: 80

                Button {
                    text: "Save Image"
                    icon.source: "qrc:/icons/download.svg"
                    onClicked: {
                        // Connect to the signal first
                        downloadCompleted.connect(imageDownloadCallback)
                        // Then call the download function
                        downloadAsync(imageViewerDialog.imageSource)
                    }
                }

                Button {
                    text: "Copy URL"
                    icon.source: "qrc:/icons/content_copy.svg"
                    onClicked: {
                        clipboard.copyText(imageViewerDialog.imageSource)
                        saveNotification.setText("URL copied to clipboard")
                    }
                }
            }

            Rectangle {
                id: saveNotification
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.top: fullSizeImage.bottom
                anchors.topMargin: Constants.spacing_m
                width: notificationText.width + 40
                height: 40
                radius: 20
                color: Material.accent
                opacity: 0

                Text {
                    id: notificationText
                    anchors.centerIn: parent
                    text: ""
                    color: "white"
                    font.bold: true
                }

                Behavior on opacity {
                    NumberAnimation { duration: 300 }
                }

                Timer {
                    id: closeTimer
                    interval: 3000
                    running: saveNotification.opacity > 0
                    onTriggered: saveNotification.opacity = 0
                }

                function setText(message) {
                    notificationText.text = message
                    opacity = 1.0
                }
            }
        }
    }

    Dialog {
        id: videoViewerDialog
        title: "Video"
        modal: true
        standardButtons: Dialog.Close
        anchors.centerIn: Overlay.overlay
        width: Math.min(appWindow.width * 0.9, 1000)
        height: Math.min(appWindow.height * 0.8, 800)
        padding: 0  // Remove padding to maximize space

        property string videoSource: ""

        contentItem: Item {
            id: dialogContent
            anchors.fill: parent

            MediaPlayer {
                id: fullscreenVideoPlayer
                source: videoViewerDialog.videoSource
                autoPlay: true
            }

            VideoOutput {
                id: fullscreenVideoOutput
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                // Make sure video doesn't overlap with controls
                height: parent.height - fullscreenProgressBarBackground.height - fullscreenButtonRow.height - Constants.spacing_m * 4
                source: fullscreenVideoPlayer
            }

            // Progress bar for fullscreen video - moved below the video
            Rectangle {
                id: fullscreenProgressBarBackground
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: fullscreenVideoOutput.bottom
                anchors.topMargin: 0
                height: 10
                color: "#80000000"
                z: 10

                Rectangle {
                    id: fullscreenProgressBar
                    anchors.left: parent.left
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    width: fullscreenVideoPlayer.position > 0 && fullscreenVideoPlayer.duration > 0 ?
                           parent.width * (fullscreenVideoPlayer.position / fullscreenVideoPlayer.duration) : 0
                    color: Material.accent
                }

                MouseArea {
                    anchors.fill: parent
                    anchors.topMargin: -10
                    anchors.bottomMargin: -10
                    z: 11

                    onPressed: {
                        if (fullscreenVideoPlayer.seekable) {
                            var newPosition = mouseX / width * fullscreenVideoPlayer.duration
                            fullscreenVideoPlayer.seek(newPosition)
                        }
                    }

                    onPositionChanged: {
                        if (pressed && fullscreenVideoPlayer.seekable) {
                            var newPosition = Math.max(0, Math.min(mouseX, width)) / width * fullscreenVideoPlayer.duration
                            fullscreenVideoPlayer.seek(newPosition)
                        }
                    }
                }
            }

            // Time display for fullscreen - moved next to progress bar
            Text {
                id: fullscreenTimeDisplay
                anchors.left: parent.left
                anchors.leftMargin: Constants.spacing_m
                anchors.top: fullscreenProgressBarBackground.bottom
                anchors.topMargin: Constants.spacing_s
                color: Material.foreground
                font.pixelSize: 14
                z: 10
                text: {
                    function formatTime(ms) {
                        var totalSeconds = Math.floor(ms / 1000);
                        var minutes = Math.floor(totalSeconds / 60);
                        var seconds = totalSeconds % 60;
                        return minutes + ":" + (seconds < 10 ? "0" : "") + seconds;
                    }

                    return formatTime(fullscreenVideoPlayer.position) + " / " +
                           formatTime(fullscreenVideoPlayer.duration);
                }
            }

            // Play/pause button overlay in center of video
            Rectangle {
                anchors.centerIn: fullscreenVideoOutput
                width: 80
                height: 80
                radius: 40
                color: "#80000000"  // Semi-transparent black
                visible: fullscreenVideoPlayer.playbackState !== MediaPlayer.PlayingState ||
                         fullscreenVideoMouseArea.containsMouse
                opacity: fullscreenVideoPlayer.playbackState === MediaPlayer.PlayingState ?
                         (fullscreenVideoMouseArea.containsMouse ? 0.8 : 0) : 0.8
                z: 10

                Behavior on opacity {
                    NumberAnimation { duration: 200 }
                }

                MouseArea {
                    anchors.fill: parent
                    onClicked: {
                        if (fullscreenVideoPlayer.playbackState === MediaPlayer.PlayingState) {
                            fullscreenVideoPlayer.pause()
                        } else {
                            fullscreenVideoPlayer.play()
                        }
                    }
                }

                Image {
                    anchors.centerIn: parent
                    width: 40
                    height: 40
                    source: fullscreenVideoPlayer.playbackState === MediaPlayer.PlayingState
                            ? "qrc:/icons/pause.svg"
                            : "qrc:/icons/play_arrow.svg"
                    fillMode: Image.PreserveAspectFit
                }
            }

            // Mouse area for the entire video to show/hide controls
            MouseArea {
                id: fullscreenVideoMouseArea
                anchors.fill: fullscreenVideoOutput
                hoverEnabled: true
                onClicked: {
                    if (fullscreenVideoPlayer.playbackState === MediaPlayer.PlayingState) {
                        fullscreenVideoPlayer.pause()
                    } else {
                        fullscreenVideoPlayer.play()
                    }
                }
            }

            // Button row below the video
            Row {
                id: fullscreenButtonRow
                anchors.top: fullscreenTimeDisplay.bottom
                anchors.topMargin: Constants.spacing_m
                anchors.horizontalCenter: parent.horizontalCenter
                spacing: Constants.spacing_s
                height: 80

                Button {
                    text: "Save Video"
                    icon.source: "qrc:/icons/download.svg"
                    onClicked: {
                        // Connect to the signal first
                        downloadCompleted.connect(videoDownloadCallback)
                        // Then call the download function
                        downloadAsync(videoViewerDialog.videoSource)
                    }
                }

                Button {
                    text: "Copy URL"
                    icon.source: "qrc:/icons/content_copy.svg"
                    onClicked: {
                        clipboard.copyText(videoViewerDialog.videoSource)
                        videoSaveNotification.setText("URL copied to clipboard")
                    }
                }
            }

            Rectangle {
                id: videoSaveNotification
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.top: fullscreenTimeDisplay.bottom
                anchors.topMargin: Constants.spacing_m
                width: videoNotificationText.width + 40
                height: 40
                radius: 20
                color: Material.accent
                opacity: 0

                Text {
                    id: videoNotificationText
                    anchors.centerIn: parent
                    text: ""
                    color: "white"
                    font.bold: true
                }

                Behavior on opacity {
                    NumberAnimation { duration: 300 }
                }

                Timer {
                    id: videoCloseTimer
                    interval: 3000
                    running: videoSaveNotification.opacity > 0
                    onTriggered: videoSaveNotification.opacity = 0
                }

                function setText(message) {
                    videoNotificationText.text = message
                    opacity = 1.0
                }
            }
        }

        onClosed: {
            fullscreenVideoPlayer.stop()
        }
    }

    function videoDownloadCallback(success, filePathOrError) {
        // Disconnect after receiving the result
        downloadCompleted.disconnect(videoDownloadCallback)

        if (success) {
            // For success, filePath contains the actual file path
            videoSaveNotification.setText("Saved to Downloads folder: " + filePathOrError)
        } else {
            // For failure, filePath contains the error message
            videoSaveNotification.setText("Download failed: " + filePathOrError)
        }
    }

    function imageDownloadCallback(success, filePathOrError) {
        // Disconnect after receiving the result
        downloadCompleted.disconnect(imageDownloadCallback)

        if (success) {
            // For success, filePath contains the actual file path
            saveNotification.setText("Saved to Downloads folder: " + filePathOrError)
        } else {
            // For failure, filePath contains the error message
            saveNotification.setText("Download failed: " + filePathOrError)
        }
    }
}
