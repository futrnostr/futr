import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Buttons 1.0
import Futr 1.0

Rectangle {
    id: root
    border.color: "#e0e0e0"
    radius: 5
    width: 400
    implicitHeight: content.implicitHeight

    ColumnLayout {
        id: content
        anchors.fill: parent
        anchors.margins: 1
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignRight
            Layout.rightMargin: 2
            Layout.topMargin: 2

            CloseButton {
                id: closeButton
                target: root
            }
        }

        RowLayout {
            width: parent.width

            ColumnLayout {
                spacing: 10
                width: parent.width

                Rectangle {
                    Layout.fillWidth: true
                    height: 80

                    Image {
                        source: "https://pbs.twimg.com/profile_banners/44196397/1722660499/1500x500"
                        width: parent.width
                        height: 80
                        fillMode: Image.PreserveAspectCrop
                        clip: true
                    }
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 10

                    Rectangle {
                        width: 60
                        height: 60
                        Layout.leftMargin: 10
                        Layout.fillHeight: true

                        Image {
                            source: "https://avatars.githubusercontent.com/u/394428?v=4"
                            width: 60
                            height: 60
                            fillMode: Image.PreserveAspectCrop
                            clip: true

                        }
                    }

                    ColumnLayout {
                        spacing: 10
                        Layout.fillWidth: true

                        Text {
                            text: "prolic"
                            font.bold: true
                        }

                        Text {
                            text: "Sascha-Oliver Prolić"
                        }

                        Text {
                            text: "Nostr Haskell Developer by night. Nightmare by day. I don't know what I am doing, but who does anyway?"
                            Layout.fillWidth: true
                            wrapMode: Text.Wrap
                        }

                        ExternalIdentity {
                            Layout.fillWidth: true
                            icon: ExternalIdentityIcons.github
                            link: "https://github.com/prolic"
                            proof: "some proof"
                            value: "prolic"
                        }

                        ExternalIdentity {
                            Layout.fillWidth: true
                            icon: ExternalIdentityIcons.telegram
                            link: "https://t.me/sasaprolic"
                            proof: "some proof"
                            value: "sasaprolic"
                        }

                        ExternalIdentity {
                            Layout.fillWidth: true
                            icon: ExternalIdentityIcons.x_twitter
                            link: "https://x.com/sasaprolic"
                            proof: "some proof"
                            value: "sasaprolic"
                        }
                    }
                }
            }
        }

        RowLayout {
            width: parent.width
            Layout.alignment: Qt.AlignRight
            Layout.rightMargin: 10
            Layout.bottomMargin: 10

            EditButton {
                id: editButton

                onClicked: {
                    root.visible = false;
                    editMyProfile.visible = true;
                }
            }
        }
    }
}
