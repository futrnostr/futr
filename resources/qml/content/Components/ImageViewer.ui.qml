import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Page {
    id: root

    required property string imageSource
    property bool showOriginalSize: false

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
                text: qsTr("Image Viewer")
                elide: Label.ElideRight
                horizontalAlignment: Qt.AlignHCenter
                verticalAlignment: Qt.AlignVCenter
                Layout.fillWidth: true
            }

            Item { width: 48; height: 48 }
        }
    }

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
                    notification.show("Image download started")
                }
            }

            Button {
                text: "Copy URL"
                icon.source: "qrc:/icons/content_copy.svg"
                onClicked: {
                    clipboard.copyText(root.imageSource)
                    notification.show("URL copied to clipboard")
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
                        asynchronous: true
                        width: root.showOriginalSize ? sourceSize.width : flickable.width
                        height: root.showOriginalSize ? sourceSize.height : flickable.height
                        fillMode: root.showOriginalSize ? Image.Pad : Image.PreserveAspectFit
                        anchors.centerIn: parent
                        cache: false
                    }
                }
            }
        }
    }

    function imageDownloadCallback(success, filePathOrError) {
        downloadCompleted.disconnect(imageDownloadCallback)

        if (success) {
            notification.show("Saved to Downloads folder: " + filePathOrError)
        } else {
            notification.show("Download failed: " + filePathOrError)
        }
    }
}
