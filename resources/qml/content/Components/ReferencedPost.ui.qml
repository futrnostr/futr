import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0

Item {
    id: referencedPost
    property var refPost

    Layout.fillWidth: true
    Layout.preferredHeight: referencedPostLoader.item ? referencedPostLoader.item.height : 0

    Loader {
        id: referencedPostLoader
        width: parent.width
        anchors.left: parent.left
        anchors.right: parent.right

        sourceComponent: Rectangle {
            width: parent.width
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
                        imageSource: refPost && refPost.author ?
                            Util.getProfilePicture(refPost.author.picture, refPost.author.npub) : ""

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                navigationPane.navigateTo(
                                    "PersonalFeed.ui.qml",
                                    {
                                        "npub": refPost.author.npub
                                    }
                                )
                            }
                        }
                    }

                    Text {
                        Layout.fillWidth: true
                        text: refPost && refPost.author ?
                            (refPost.author.displayName || refPost.author.name || refPost.author.npub) : ""
                        font: Constants.fontMedium
                        color: Material.foreground
                        elide: Text.ElideRight

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                navigationPane.navigateTo(
                                    "PersonalFeed.ui.qml",
                                    {
                                        "npub": refPost.author.npub
                                    }
                                )
                            }
                        }
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
                            if (refPost) {
                                text = generateHtmlContent2(refPost.contentParts);
                            }
                        }

                        function generateHtmlContent2(parts) {
                            let htmlText = "";
                            let hasCurrentText = false;

                            function createTextEdit(text) {
                                if (!text.trim()) return; // Skip empty text

                                let textObject = Qt.createQmlObject(`
                                    import QtQuick 2.15
                                    import QtQuick.Controls 2.15
                                    import QtQuick.Controls.Material 2.15
                                    import QtQuick.Layouts 1.15

                                    TextEdit {
                                        Layout.fillWidth: true
                                        readOnly: true
                                        selectByMouse: true
                                        wrapMode: Text.Wrap
                                        textFormat: Text.RichText
                                        color: Material.foreground
                                        text: "${text.replace(/"/g, '\\"')}"
                                    }
                                `, refContentLayout);

                                textObject.Layout.fillWidth = true;
                                textObject.parent = refContentLayout;
                                textObject.linkActivated.connect(function(link) {
                                    refContentTextEdit.onLinkActivated(link);
                                });
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
                                                         content.substring(0, 8) + "...";
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
                                        let imageObject = imageComponent.createObject(refContentLayout, {
                                            "source": content,
                                            "width": refContentLayout.width,
                                            "clickable": true,
                                            "imageUrl": content
                                        });
                                        imageObject.Layout.fillWidth = true;
                                        imageObject.parent = refContentLayout;
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
                                            let videoObject = videoComponent.createObject(refContentLayout, {
                                                "source": content,
                                                "width": refContentLayout.width,
                                                "clickable": true,
                                                "videoUrl": content
                                            });

                                            if (videoObject) {
                                                videoObject.Layout.fillWidth = true;
                                                videoObject.parent = refContentLayout;
                                                videoObject.fullScreenRequested.connect(function(url) {
                                                    openFullscreenVideo(url);
                                                });
                                            } else {
                                                console.error("Failed to create video object in reference post");
                                            }
                                        } else {
                                            console.error("Error loading video component:", videoComponent.errorString());
                                        }
                                    } else if (type === "note" || type === "nevent" || type === "naddr") {
                                        let referencedPost = getPost(content);

                                        if (referencedPost) {
                                            let referencedComponent = Qt.createComponent("ReferencedPost.ui.qml");
                                            if (referencedComponent.status === Component.Ready) {
                                                let referencedObject = referencedComponent.createObject(refContentLayout, {
                                                    "refPost": referencedPost,
                                                    "width": refContentLayout.width
                                                });
                                                referencedObject.Layout.fillWidth = true;
                                                referencedObject.parent = refContentLayout;
                                            } else {
                                                console.error("Error loading referenced post component:", referencedComponent.errorString());
                                            }
                                        } else {
                                            // Display a label indicating the event is not found
                                            let notFoundLabel = Qt.createQmlObject('import QtQuick 2.15; Text { text: "Event not found, we\'re trying to find it."; color: "red"; }', refContentLayout);
                                            notFoundLabel.Layout.fillWidth = true;
                                            notFoundLabel.parent = refContentLayout;
                                        }
                                    }
                                }
                            }

                            // Create the final TextEdit if there's any remaining text
                            if (hasCurrentText) {
                                createTextEdit(htmlText);
                                return ""; // Return empty string since we've already created the TextEdit objects
                            }

                            return htmlText;
                        }

                        onLinkActivated: function(link) {
                            if (link.startsWith("profile://")) {
                                let profileId = link.substring(10);
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
                    }
                }
            }
        }
    }
}
