import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Rectangle {
    id: referencedPostContainer

    property string value
    property var currentUser
    property var cachedPost

    Layout.fillWidth: true
    height: 100
    Layout.preferredHeight: height
    color: Material.backgroundColor
    radius: Constants.radius_m

    Component.onCompleted: {
        if (value) {
            cachedPost = getPost(value)

            if (cachedPost) {
                referencedPostContent.post = cachedPost
                referencedPostContent.visible = true
                loadingIndicator.visible = false
            }
        }
    }

    Rectangle {
        id: loadingContainer
        height: 100
        width: parent.width
        color: Material.dialogColor
        radius: Constants.radius_m

        border.color: Material.backgroundColor
        border.width: Constants.spacing_xs

        anchors.left: parent.left
        anchors.right: parent.right

        RowLayout {
            anchors.fill: parent
            anchors.margins: Constants.spacing_xs

            BusyIndicator {
                Layout.alignment: Qt.AlignVCenter
                running: true
            }

            Text {
                Layout.alignment: Qt.AlignLeft
                Layout.fillWidth: true
                text: qsTr("Event not found. Trying to find it for you...")
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }
    }

    PostContent {
        id: referencedPostContent
        post: referencedPostContainer.post
        isRefPost: true
        currentUser: referencedPostContainer.currentUser
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 2
        visible: false

        onPostClicked: {
            stackView.push("../PostDetails.ui.qml", {
                "post": referencedPostContainer.post,
                "isRefPost": true,
                "currentUser": referencedPostContainer.currentUser
            })
        }

        onHeightChanged: {
            heightUpdateTimer.restart()
        }
    }

    Timer {
        id: heightUpdateTimer
        interval: 16
        running: false
        repeat: false
        onTriggered: {
            let targetHeight = referencedPostContent.visible 
                            ? referencedPostContent.implicitHeight + 4 // 4 for borders
                            : loadingContainer.height

            if (Math.abs(referencedPostContainer.height - targetHeight) > 1) {
                referencedPostContainer.height = targetHeight
                referencedPostContainer.Layout.preferredHeight = targetHeight
            }
        }
    }

    BusyIndicator {
        id: loadingIndicator
        anchors.centerIn: parent
        visible: false
        running: visible
    }
}
