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
    //padding: Constants.spacing_s

    required property var post
    property bool clickable: true

    signal commentClicked()
    signal repostClicked()
    signal postClicked()

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
        id: backgroundRect
        color: Material.dialogColor
        radius: Constants.radius_m

        MouseArea {
            id: postClickArea
            anchors.fill: parent
            enabled: root.clickable
            propagateComposedEvents: true

            onPressed: {
                console.log("Pressed")
                if (!mouse.wasHeld) {
                    console.log("Post clicked")
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
        anchors.rightMargin: Constants.spacing_xs
        spacing: 0

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
                text: post.postType === "repost" ? "Reposted" : "Quote Reposted"
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }

        ColumnLayout {
            id: contentLayout
            Layout.fillWidth: true
            spacing: 0
            visible: post.postType === "short_text_note" || post.postType === "quote_repost" || post.postType === "repost"

            Component.onCompleted: {
                if (post) {
                    generateHtmlContent(post.contentParts);
                }
            }

            function generateHtmlContent(parts) {
                let htmlText = "";
                let hasCurrentText = true;

                function createTextEdit(text) {
                    if (!text.trim()) return; // Skip empty text

                    let textObject = Qt.createQmlObject(`
                        import QtQuick 2.15
                        import QtQuick.Controls 2.15
                        import QtQuick.Controls.Material 2.15
                        import QtQuick.Layouts 1.15

                        import Futr 1.0

                        TextEdit {
                            Layout.fillWidth: true
                            Layout.topMargin: Constants.spacing_xs
                            Layout.bottomMargin: Constants.spacing_xs
                            readOnly: true
                            selectByMouse: true
                            wrapMode: Text.Wrap
                            textFormat: Text.RichText
                            color: Material.foreground
                            text: "${text.replace(/"/g, '\\"')}"
                        }
                    `, contentLayout);

                    textObject.Layout.fillWidth = true;
                    textObject.parent = contentLayout;
                    textObject.linkActivated.connect(function(link) {
                        onLinkActivated(link);
                    });
                }

                function onLinkActivated(link) {
                    if (link.startsWith("profile://")) {
                        let profileId = link.substring(10);
                        // Convert nprofile to npub if necessary
                        if (profileId.startsWith("nprofile")) {
                            profileId = convertNprofileToNpub(profileId);
                            if (!profileId) {
                                console.error("Failed to convert nprofile to npub");
                                return;
                            }
                        }
                        navigationPane.navigateTo(
                            "PersonalFeed.ui.qml",
                            {
                                "npub": profileId
                            }
                        )
                    } else if (link.startsWith("note://")) {
                        console.log("Note clicked:", link.substring(7));
                    } else {
                        Qt.openUrlExternally(link);
                    }
                }

                for (let i = 0; i < parts.length; i++) {
                    const part = parts[i];
                    const type = part[0];
                    const content = part.length > 1 ? part[1] : "";

                    if (type === "text" || type === "url" || type === "nprofile" || type === "npub") {
                        hasCurrentText = true;

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
                        }
                    } else {
                        // If we have accumulated text, create a TextEdit before adding non-text content
                        if (hasCurrentText) {
                            createTextEdit(htmlText);
                            htmlText = "";
                            hasCurrentText = false;
                        }

                        if (type === "image") {
                            let imageComponent = Qt.createComponent("PostImage.ui.qml");
                            let imageObject = imageComponent.createObject(contentLayout, {
                                "source": content,
                                //"width": contentLayout.width,
                                "clickable": true,
                                "imageUrl": content
                            });
                            imageObject.Layout.fillWidth = true;
                            imageObject.parent = contentLayout;
                            imageObject.imageClicked.connect(function(url) {
                                navigationPane.navigateTo(
                                    "ImageViewer.ui.qml",
                                    {
                                        "imageSource": url
                                    }
                                )
                            });
                        } else if (type === "video") {
                            let videoComponent = Qt.createComponent("PostVideo.ui.qml");
                            if (videoComponent.status === Component.Ready) {
                                let videoObject = videoComponent.createObject(contentLayout, {
                                    "source": content,
                                    //"width": contentLayout.width,
                                    "clickable": true,
                                    "videoUrl": content
                                });

                                if (videoObject) {
                                    videoObject.Layout.fillWidth = true;
                                    videoObject.parent = contentLayout;
                                    videoObject.fullScreenRequested.connect(function(url) {
                                        openFullscreenVideo(url);
                                    });
                                } else {
                                    console.error("Failed to create video object");
                                }
                            } else {
                                console.error("Error loading video component:", videoComponent.errorString());
                            }
                        } else if (type === "note" || type === "nevent" || type === "naddr") {
                            let post = getPost(content);

                            if (post) {
                                let referencedPostComponent = Qt.createComponent("ReferencedPost.ui.qml");
                                if (referencedPostComponent.status === Component.Ready) {
                                    let referencedPostObject = referencedPostComponent.createObject(contentLayout, {
                                        "refPost": post
                                    });
                                    if (referencedPostObject) {
                                        referencedPostObject.Layout.fillWidth = true;
                                        referencedPostObject.parent = contentLayout;
                                    } else {
                                        console.error("Failed to create ReferencedPost object.");
                                    }
                                } else {
                                    console.error("Error loading referenced post component:", referencedPostComponent.errorString());
                                }
                            } else {
                                // Display a label indicating the event is not found
                                let notFoundLabel = Qt.createQmlObject('import QtQuick 2.15; Text { text: "Event not found, we\'re trying to find it."; color: "red"; }', contentLayout);
                                notFoundLabel.Layout.fillWidth = true;
                                notFoundLabel.parent = contentLayout;
                            }
                        }
                    }
                }

                // Create the final TextEdit if there's any remaining text
                if (hasCurrentText) {
                    createTextEdit(htmlText);
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

    function openFullscreenVideo(videoUrl) {
        var component = Qt.createComponent("FullscreenVideoWindow.ui.qml");
        if (component.status === Component.Ready) {
            var window = component.createObject(root, {
                "videoUrl": videoUrl
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
