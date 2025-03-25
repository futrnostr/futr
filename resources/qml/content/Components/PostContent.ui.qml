import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15
import QtGraphicalEffects 1.15

import Components 1.0
import Futr 1.0

Pane {
    id: root

    required property var post
    property bool clickable: true
    property bool isRefPost: false

    signal commentClicked()
    signal repostClicked()
    signal postClicked()

    padding: Constants.spacing_s

    background: Rectangle {
        id: backgroundRect
        color: Material.dialogColor
        radius: Constants.radius_m

        MouseArea {
            id: postClickArea
            anchors.fill: parent
            enabled: root.clickable
            propagateComposedEvents: true

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
            visible: post.postType === "quote_repost" || post.postType === "repost"

            Image {
                source: "qrc:/icons/repeat.svg"
                sourceSize.width: 20
                sourceSize.height: 20
                Layout.alignment: Qt.AlignVCenter
            }

            Text {
                text: post.postType === "repost" ? qsTr("Reposted") : qsTr("Quote Reposted")
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }

        ColumnLayout {
            id: contentLayout
            Layout.fillWidth: true
            spacing: 0

            Repeater {
                model: post.contentParts

                delegate: Loader {
                    id: contentLoader
                    Layout.fillWidth: true
                    Layout.fillHeight: false

                    property string contentValue: modelData[1]

                    sourceComponent: {
                        let type = modelData[0];

                        if (type === "text") {
                            return textComponent;
                        } else if (type === "image") {
                            return imageComponent;
                        } else if (type === "video") {
                            return videoComponent;
                        } else if (type === "note" || type === "nevent" || type === "naddr") {
                            return referencedPostComponent;
                        } else {
                            console.warn("Unknown content type:", type);
                            return null;
                        }
                    }

                    onLoaded: {
                        if (item) {
                            if (item.hasOwnProperty("value")) {
                                item.value = contentValue;
                            } else {
                                console.warn("Component doesn't have 'value' property");
                                console.warn("data: ", item)
                                console.warn("modelData: ", modelData)
                            }
                        }
                    }
                }
            }
        }

        Component {
            id: textComponent

            TextEdit {
                property string value: ""

                Layout.fillWidth: true
                Layout.topMargin: Constants.spacing_xs
                Layout.bottomMargin: Constants.spacing_xs
                readOnly: true
                selectByMouse: true
                wrapMode: Text.Wrap
                textFormat: Text.RichText
                color: Material.foreground
                text: value

                onLinkActivated: function(link) {
                    if (!link) return;

                    try {
                        if (link.startsWith("profile://")) {
                            let profileId = link.substring(10);

                            if (profileId.startsWith("nprofile")) {
                                profileId = convertNprofileToNpub(profileId);
                                if (!profileId) {
                                    console.error("Failed to convert nprofile to npub");
                                    return;
                                }
                            }

                            stackView.replace(personalFeedComponent, {"npub": profileId})
                        } else if (link.startsWith("note://")) {
                            console.log("Note clicked:", link.substring(7));
                        } else {
                            Qt.openUrlExternally(link);
                        }
                    } catch (e) {
                        console.error("Error handling link activation:", e);
                    }
                }
            }
        }

        Component {
            id: imageComponent

            PostImage {
                property string value: ""

                Layout.fillWidth: true
                clickable: true
                imageUrl: value

                onImageClicked: function(url) {
                    stackView.push("ImageViewer.ui.qml", {"imageSource": url})
                }
            }
        }

        Component {
            id: videoComponent

            PostVideo {
                property string value: ""

                Layout.fillWidth: true
                Layout.preferredHeight: root.isRefPost ? Math.min(width * 0.5625, 200) : Math.min(width * 0.5625, 400)
                clickable: true
                videoUrl: value

                onFullScreenRequested: function(url) {
                    openFullscreenVideo(url)
                }
            }
        }

        Component {
            id: referencedPostComponent

            Item {
                id: referencedPostContainer
                property string value: ""

                Layout.fillWidth: true
                implicitHeight: contentRectangle.implicitHeight + 20

                onValueChanged: {
                    if (value) {
                        let referencedPost = getPost(value);

                        if (referencedPost) {
                            referencedContentLoader.referencedPost = referencedPost;
                            referencedContentLoader.visible = true;
                            loadingIndicator.visible = false;
                        } else {
                            referencedContentLoader.visible = false;
                            loadingIndicator.visible = true;
                        }
                    }
                }

                ColumnLayout {
                    anchors.fill: parent

                    Rectangle {
                        id: contentRectangle
                        Layout.fillWidth: true
                        implicitHeight: loadingIndicator.visible ? loadingContainer.height : 
                                       (referencedContentLoader.item ? referencedContentLoader.item.implicitHeight + 20 : 100)
                        color: Material.backgroundColor
                        radius: Constants.radius_m
                        clip: true

                        BusyIndicator {
                            id: loadingIndicator
                            anchors.centerIn: parent
                            visible: true
                        }

                        Rectangle {
                            id: loadingContainer
                            visible: loadingIndicator.visible
                            height: 100
                            width: parent.width
                            color: Material.dialogColor
                            radius: Constants.radius_m
                            border.color: Material.backgroundColor
                            border.width: Constants.spacing_xs
                            anchors.fill: parent

                            RowLayout {
                                anchors.fill: parent
                                anchors.margins: Constants.spacing_m

                                BusyIndicator {
                                    Layout.alignment: Qt.AlignVCenter
                                    running: true
                                }

                                Text {
                                    Layout.alignment: Qt.AlignLeft
                                    Layout.fillWidth: true
                                    text: qsTr("Event not found. Trying to find it for you...")
                                    font: Constants.smallFontMedium
                                    color: Material.secondaryTextColor
                                }
                            }
                        }

                        Loader {
                            id: referencedContentLoader
                            anchors.fill: parent
                            anchors.margins: Constants.spacing_xs
                            visible: false
                            property var referencedPost: null

                            onReferencedPostChanged: {
                                if (referencedPost) {
                                    setSource("PostContent.ui.qml", {
                                        "post": referencedPost,
                                        "clickable": true,
                                        isRefPost: true
                                    });
                                }
                            }

                            onLoaded: {
                                if (item) {
                                    item.postClicked.connect(function() {
                                        stackView.push("PostDetails.ui.qml", {"post": referencedPost});
                                    });
                                }
                            }

                            onHeightChanged: {
                                if (item && visible) {
                                    contentRectangle.implicitHeight = item.implicitHeight + 20;
                                    referencedPostContainer.implicitHeight = contentRectangle.implicitHeight + 20;
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
            Layout.bottomMargin: 0
            Layout.preferredHeight: 36
            visible: !isRefPost

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
            Layout.bottomMargin: 0
            Layout.preferredHeight: 28

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
