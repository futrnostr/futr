import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: root
    width: parent.width
    height: parent.height

    required property string imageSource
    property bool showOriginalSize: false

    ColumnLayout {
        anchors.fill: parent
        spacing: Constants.spacing_xs

        RowLayout {
            Layout.fillWidth: true
            Layout.margins: Constants.spacing_m

            Item { Layout.fillWidth: true }

            Button {
                text: root.showOriginalSize ? "Fit to Screen" : "Original Size"
                icon.source: root.showOriginalSize ? "qrc:/icons/fit_screen.svg" : "qrc:/icons/fullscreen.svg"
                onClicked: {
                    root.showOriginalSize = !root.showOriginalSize
                    flickable.contentX = 0
                    flickable.contentY = 0
                }
            }

            Button {
                text: "Save Image"
                icon.source: "qrc:/icons/download.svg"
                onClicked: {
                    downloadCompleted.connect(imageDownloadCallback)
                    downloadAsync(root.imageSource)
                    saveNotification.show("Image download started")
                }
            }

            Button {
                text: "Copy URL"
                icon.source: "qrc:/icons/content_copy.svg"
                onClicked: {
                    clipboard.copyText(root.imageSource)
                    saveNotification.show("URL copied to clipboard")
                }
            }

            Item { Layout.fillWidth: true }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: "transparent"

            Rectangle {
                anchors.fill: parent
                anchors.margins: Constants.spacing_xs
                color: "transparent"

                Flickable {
                    id: flickable
                    anchors.fill: parent
                    contentWidth: root.showOriginalSize ? image.sourceSize.width : width
                    contentHeight: root.showOriginalSize ? image.sourceSize.height : height
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds

                    ScrollBar.vertical: ScrollBar {
                        policy: ScrollBar.AsNeeded
                        visible: flickable.contentHeight > flickable.height
                    }

                    ScrollBar.horizontal: ScrollBar {
                        policy: ScrollBar.AsNeeded
                        visible: flickable.contentWidth > flickable.width
                    }

                    Image {
                        id: image
                        source: root.imageSource
                        width: root.showOriginalSize ? sourceSize.width : flickable.width
                        height: root.showOriginalSize ? sourceSize.height : flickable.height
                        fillMode: root.showOriginalSize ? Image.Pad : Image.PreserveAspectFit
                        anchors.centerIn: parent
                    }
                }
            }
        }
    }

    NotificationToast {
        id: saveNotification
        parent: Overlay.overlay
        x: (Overlay.overlay.width - width) / 2
        y: Overlay.overlay.height - height - 100
        z: 999999
    }

    function imageDownloadCallback(success, filePathOrError) {
        downloadCompleted.disconnect(imageDownloadCallback)

        if (success) {
            saveNotification.show("Saved to Downloads folder: " + filePathOrError)
        } else {
            saveNotification.show("Download failed: " + filePathOrError)
        }
    }
}
