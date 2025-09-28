import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Buttons 1.0
import Components 1.0
import Futr 1.0

Rectangle {
    id: root
    color: Material.backgroundColor
    radius: Constants.radius_m
    width: 400
    border.color: Material.dividerColor
    border.width: 1

    property var profileData: null
    property string npub

    property var profile_id: profileData ? profileData.id : ""
    //property var profile_npub: profileData ? profileData.npub : ""
    property var profile_name: profileData ? profileData.name : ""
    property var profile_displayName: profileData ? profileData.displayName : ""
    property var profile_about: profileData ? profileData.about : ""
    property var profile_picture: profileData ? profileData.picture : ""
    property var profile_nip05: profileData ? profileData.nip05 : ""
    property var profile_banner: profileData ? profileData.banner : ""
    property var profile_isFollow: profileData ? profileData.isFollow : ""

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

        ProfileBanner {
            url: profile_banner
            Layout.fillWidth: true
            Layout.preferredHeight: 60
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.rightMargin: 10

            ProfilePicture {
                url: profile_picture
                Layout.preferredWidth: 60
                Layout.preferredHeight: 60
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 5

                Text {
                    Layout.fillWidth: true
                    text: profileData ? (profile_displayName || profile_name || root.npub) : ""
                    font: Constants.largeFont
                    color: Material.primaryTextColor
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    text: profileData ? (profile_name || "") : ""
                    font: Constants.font
                    color: Material.primaryTextColor
                    visible: profileData && profile_displayName
                    elide: Text.ElideRight
                }
            }

            Item {
                Layout.fillWidth: true
            }

            EditButton {
                id: editButton
                visible: root.npub === root.currentUser

                onClicked: {
                    personalFeed.editMode = true
                }
            }

            Button {
                visible: root.npub !== root.currentUser
                text: profileData !== null ? (profile_isFollow === "1" ? qsTr("Unfollow") : qsTr("Follow")) : ""
                font: Constants.font
                highlighted: true
                Material.background: Material.primary

                onClicked: {
                    if (root.profileData === null) {
                        return
                    }

                    if (root.profileData.isFollow) {
                        unfollow(root.npub)
                    } else {
                        follow(root.npub)
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
                    text: root.npub
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
                        clipboard.copyText(root.npub)
                    }
                }
            }

            Text {
                text: profileData ? profile_about : ""
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
