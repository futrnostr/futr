import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0
import Profile 1.0

Rectangle {
    id: homeScreen
    width: parent.width
    height: parent.height

    Column {
        width: parent.width

        Row {
            height: 80
            anchors.right: parent.right

            RoundButton {
                width: 75
                height: 75
                flat: true

                icon.width: 65
                icon.height: 65
                icon.color: "transparent"

                Component.onCompleted: {
                    icon.source = Util.getProfilePicture(mypicture, mynpub)
                }

                Material.elevation: 10

                ToolTip.visible: hovered
                ToolTip.delay: 500
                ToolTip.timeout: 5000
                ToolTip.text: qsTr("My Profile")

                onClicked: {
                    myProfile.visible = true
                    var profile = JSON.parse(getProfile(mynpub))
                    myProfile.profileData = profile
                    editMyProfile.profileData = profile
                }
            }
        }

        MyProfile {
            id: myProfile
            visible: false

            anchors.right: parent.right
            anchors.rightMargin: 10
            anchors.topMargin: -100
        }

        EditMyProfile {
            id: editMyProfile
            visible: false

            anchors.right: parent.right
            anchors.rightMargin: 10
            anchors.topMargin: 10
        }
    }
}
