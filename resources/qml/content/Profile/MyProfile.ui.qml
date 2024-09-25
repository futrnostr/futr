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
    implicitHeight: contentLoader.implicitHeight
    property var profileData: {}

    Loader {
        id: contentLoader
        anchors.fill: parent
        sourceComponent: profileData && profileData !== {} ? contentComponent : null
    }

    Component {
        id: contentComponent

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
                        visible: profileData.banner !== null && profileData.banner !== ""

                        Image {
                            source: profileData.banner ?? ""
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
                                width: 60
                                height: 60
                                fillMode: Image.PreserveAspectCrop
                                clip: true

                                Component.onCompleted: {
                                    source = Util.getProfilePicture(profileData.picture, mynpub)
                                }
                            }
                        }

                        ColumnLayout {
                            spacing: 10
                            Layout.fillWidth: true

                            Text {
                                text: profileData.display_name ?? ""
                                font.bold: true
                            }

                            Text {
                                text: profileData.name ?? ""
                            }

                            RowLayout {
                                Text {
                                    text: mynpub
                                    elide: Text.ElideRight
                                    Layout.fillWidth: true
                                    Layout.maximumWidth: parent.width - copyButton.width - parent.spacing
                                }

                                Button {
                                    id: copyButton
                                    icon.source: "qrc:/icons/content_copy.svg"
                                    flat: true
                                    Layout.preferredWidth: 50
                                    Layout.preferredHeight: 50
                                    Layout.rightMargin: 10

                                    ToolTip.visible: hovered
                                    ToolTip.text: qsTr("Copy to clipboard")

                                    onClicked: {
                                        clipboard.copyText(mynpub)
                                    }
                                }
                            }

                            Text {
                                text:  profileData.about ?? ""
                                Layout.fillWidth: true
                                wrapMode: Text.Wrap
                            }

                            ExternalIdentity {
                                Layout.fillWidth: true
                                icon: ExternalIdentityIcons.github
                                link: profileData.githubLink ?? ""
                                proof: profileData.githubProof ?? ""
                                value: profileData.githubUsername ?? ""
                            }

                            ExternalIdentity {
                                Layout.fillWidth: true
                                icon: ExternalIdentityIcons.telegram
                                link: profileData.telegramLink ?? ""
                                proof: profileData.telegramProof ?? ""
                                value: profileData.telegramUsername ?? ""
                            }

                            ExternalIdentity {
                                Layout.fillWidth: true
                                icon: ExternalIdentityIcons.x_twitter
                                link: profileData.twitterLink ?? ""
                                proof: profileData.twitterProof ?? ""
                                value: profileData.twitterUsername ?? ""
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
}
