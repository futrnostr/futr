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
    property bool isLoading: true

    implicitHeight: (isLoading ? 40 : (nestedPostContent.visible ? nestedPostContent.implicitHeight + Constants.spacing_xs : 40))

    color: isLoading ? Material.dialogColor : Material.backgroundColor
    radius: Constants.radius_m
    border.color: isLoading ? Material.backgroundColor : "transparent"
    border.width: isLoading ? 1 : 0

    Component.onCompleted: {
        console.log("[ReferencedPost] completed value=", value)
        if (value) {
            var result = getPost(value)

            if (result && Array.isArray(result) && result.length > 0) {
                cachedPost = result
                isLoading = false
            }
        }
    }

    RowLayout {
        visible: isLoading
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: Constants.spacing_xs

        BusyIndicator {
            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: 36
            Layout.preferredHeight: 36
            running: referencedPostContainer.isLoading
        }

        Text {
            Layout.alignment: Qt.AlignLeft
            Layout.fillWidth: true
            text: qsTr("Event not found. Trying to find it for you...")
            font: Constants.font
            color: Material.secondaryTextColor
        }
    }

    PostContent {
        id: nestedPostContent
        anchors.fill: parent
        anchors.margins: 1
        visible: !isLoading && cachedPost && cachedPost != null

        post: cachedPost
        currentUser: referencedPostContainer.currentUser
        isRefPost: true

        onPostClicked: {
            stackView.push("../PostDetails.ui.qml", {
                "post": referencedPostContainer.cachedPost,
                "isRefPost": true,
                "currentUser": referencedPostContainer.currentUser
            })
        }
    }
}
