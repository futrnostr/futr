import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Buttons 1.0
import Futr 1.0

Rectangle {
    color: Material.backgroundColor
    radius: Constants.radius_m
    width: 400
    border.color: Material.dividerColor
    border.width: 1

    property var profileData
    property var npub
    required property var currentUser

    Component.onCompleted: {
        profileData = getProfile(npub)
    }

    ColumnLayout {
        id: content
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 1
        spacing: 0

        Rectangle {
            Layout.fillWidth: true
            height: 80
            visible: profileData !== null && profileData.banner !== null && profileData.banner !== ""

            Image {
                source: profileData.banner ?? ""
                width: parent.width
                height: 80
                fillMode: Image.PreserveAspectCrop
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.rightMargin: 10

            Rectangle {
                width: 60
                height: 60
                color: Material.backgroundColor

                Image {
                    id: profileImage
                    source: Util.getProfilePicture(profileData.picture, npub)
                    anchors.fill: parent
                    fillMode: Image.PreserveAspectCrop
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 5

                Text {
                    Layout.fillWidth: true
                    text: profileData.displayName || profileData.name || npub
                    font: Constants.largeFont
                    color: Material.primaryTextColor
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    text: profileData.name || ""
                    font: Constants.font
                    color: Material.primaryTextColor
                    visible: profileData.displayName
                    elide: Text.ElideRight
                }
            }

            Item {
                Layout.fillWidth: true
            }

            EditButton {
                id: editButton
                visible: npub === currentUser

                onClicked: {
                    personalFeed.editMode = true
                }
            }

            Button {
                visible: npub !== currentUser
                text: profileData.isFollow ? qsTr("Unfollow") : qsTr("Follow")
                font: Constants.font
                highlighted: true
                Material.background: Material.primary

                onClicked: {
                    if (profileData.isFollow) {
                        unfollow(npub)
                    } else {
                        follow(npub)
                    }
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.leftMargin: 10
            Layout.rightMargin: 10
            Layout.topMargin: 10
            spacing: 10

            RowLayout {
                Layout.fillWidth: true

                Text {
                    text: npub
                    elide: Text.ElideRight
                    Layout.fillWidth: true
                    font: Constants.font
                    color: Material.primaryTextColor
                }

                Button {
                    id: copyButton
                    icon.source: "qrc:/icons/content_copy.svg"
                    flat: true
                    Layout.preferredWidth: 40
                    Layout.preferredHeight: 40

                    ToolTip.visible: hovered
                    ToolTip.text: qsTr("Copy to clipboard")

                    onClicked: {
                        clipboard.copyText(npub)
                    }
                }
            }

            Text {
                text: profileData.about ?? ""
                Layout.fillWidth: true
                wrapMode: Text.Wrap
                font: Constants.font
                color: Material.primaryTextColor
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
