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

    height: !isLoading
                ? (nestedLoader.item ? nestedLoader.item.implicitHeight + Constants.spacing_xs : 40)
                : 40

    color: isLoading ? Material.dialogColor : Material.backgroundColor
    radius: Constants.radius_m
    border.color: isLoading ? Material.backgroundColor : "transparent"
    border.width: isLoading ? 1 : 0

    Component.onCompleted: {
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
        anchors.centerIn: parent
        width: parent.width - (Constants.spacing_xs * 2)

        BusyIndicator {
            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: 36
            Layout.preferredHeight: 36
            running: true
        }

        Text {
            Layout.alignment: Qt.AlignLeft
            Layout.fillWidth: true
            text: qsTr("Event not found. Trying to find it for you...")
            font: Constants.font
            color: Material.secondaryTextColor
        }
    }

    Loader {
        id: nestedLoader
        active: !isLoading && cachedPost && cachedPost != null
        sourceComponent: postContentComponent
        width: parent.width

        onLoaded: {
            if (item && cachedPost) {
                item.post = cachedPost
                item.currentUser = referencedPostContainer.currentUser
                item.isRefPost = true
            }
        }
    }

    Component {
        id: postContentComponent

        PostContent {
            width: parent.width
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.margins: 2
            visible: !isLoading

            onPostClicked: {
                stackView.push("../PostDetails.ui.qml", {
                    "post": referencedPostContainer.post,
                    "isRefPost": true,
                    "currentUser": referencedPostContainer.currentUser
                })
            }
        }
    }

    function updateNestedPost() {
        if (nestedLoader.item && cachedPost && cachedPost != null) {
            nestedLoader.item.post = cachedPost
            nestedLoader.item.currentUser = referencedPostContainer.currentUser
            nestedLoader.item.isRefPost = true
        }
    }

    onCachedPostChanged: {
        if (cachedPost) {
            isLoading = false
        }
        
        if (nestedLoader.item && cachedPost && cachedPost != null) {
            nestedLoader.item.post = cachedPost
        }
    }
    
    onCurrentUserChanged: {
        if (cachedPost && cachedPost != null) {
            updateNestedPost()
        }
    }
}
