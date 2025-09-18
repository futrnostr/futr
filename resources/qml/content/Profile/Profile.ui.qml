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

    // 0: id(hex), 1: npub, 2: name, 3: displayName, 4: about
    // 5: picture, 6: nip05, 7: banner, 8: isFollow
    property var profile_id: profileData ? profileData[0] : ""
    //property var profile_npub: profileData ? profileData[1] : ""
    property var profile_name: profileData ? profileData[2] : ""
    property var profile_displayName: profileData ? profileData[3] : ""
    property var profile_about: profileData ? profileData[4] : ""
    property var profile_picture: profileData ? profileData[5] : ""
    property var profile_nip05: profileData ? profileData[6] : ""
    property var profile_banner: profileData ? profileData[7] : ""
    property var profile_isFollow: profileData ? profileData[8] : ""

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

        NostrProfileBanner {
            Layout.fillWidth: true
            url: profile_banner
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.rightMargin: 10

            NostrProfileAvatar {
                url: profile_picture
                npub: root.npub
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
