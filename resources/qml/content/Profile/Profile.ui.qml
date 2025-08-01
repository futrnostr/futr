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

    property var profileData: null
    property string npub
    required property string currentUser

    Component.onCompleted: {
        profileData = getProfile(npub)
    }

    onNpubChanged: {
        profileData = getProfile(npub)
    }

    Component.onDestruction: {
        profileData = null
        currentUser = null
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
                source: profileData !== null ? (profileData.banner ?? "") : ""
                width: parent.width
                height: 80
                fillMode: Image.PreserveAspectCrop
                cache: false
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.rightMargin: 10

            ProfilePicture {
                imageSource: profileData !== null ? profileData.getProfilePicture(profileData.picture) : ""
                Layout.preferredWidth: 60
                Layout.preferredHeight: 60
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 5

                Text {
                    Layout.fillWidth: true
                    text: profileData !== null ? profileData.displayName || profileData.name || npub : ""
                    font: Constants.largeFont
                    color: Material.primaryTextColor
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    text: profileData !== null ? profileData.name || "" : ""
                    font: Constants.font
                    color: Material.primaryTextColor
                    visible: profileData !== null && profileData.displayName
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
                text: profileData !== null ? (profileData.isFollow ? qsTr("Unfollow") : qsTr("Follow")) : ""
                font: Constants.font
                highlighted: true
                Material.background: Material.primary

                onClicked: {
                    if (profileData === null) {
                        return
                    }

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
                text: profileData !== null ? profileData.about : ""
                Layout.fillWidth: true
                wrapMode: Text.Wrap
                font: Constants.font
                color: Material.primaryTextColor
            }
/*
            ExternalIdentity {
                Layout.fillWidth: true
                icon: ExternalIdentityIcons.github
                link: profileData !== null ? profileData.githubLink : ""
                proof: profileData !== null ? profileData.githubProof : ""
                value: profileData !== null ? profileData.githubUsername : ""
            }

            ExternalIdentity {
                Layout.fillWidth: true
                icon: ExternalIdentityIcons.telegram
                link: profileData !== null ? profileData.telegramLink : ""
                proof: profileData !== null ? profileData.telegramProof : ""
                value: profileData !== null ? profileData.telegramUsername : ""
            }

            ExternalIdentity {
                Layout.fillWidth: true
                icon: ExternalIdentityIcons.x_twitter
                link: profileData !== null ? profileData.twitterLink : ""
                proof: profileData !== null ? profileData.twitterProof : ""
                value: profileData !== null ? profileData.twitterUsername : ""
            }
*/
        }
    }
}
