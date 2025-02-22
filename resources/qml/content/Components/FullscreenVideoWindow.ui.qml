import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Window 2.15
import QtMultimedia 5.15

import Components 1.0

Window {
    id: fullscreenVideoWindow
    visible: true
    width: Screen.width
    height: Screen.height
    visibility: Window.FullScreen
    color: "black"
    flags: Qt.Window | Qt.FramelessWindowHint

    property string videoUrl: ""

    NotificationToast {
        id: notification
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 100
    }

    VideoPlayer {
        id: fullscreenVideoPlayer
        anchors.fill: parent
        videoSource: fullscreenVideoWindow.videoUrl
        isFullscreen: true

        onShowNotification: function(message) {
            notification.show(message)
        }

        Component.onCompleted: {
            fullscreenVideoPlayer.play()
        }

        onFullScreenRequested: {
            fullscreenVideoPlayer.stop()
            fullscreenVideoWindow.close()
        }
    }

    Item {
        anchors.fill: parent
        focus: true
        Keys.onEscapePressed: {
            fullscreenVideoPlayer.stop()
            fullscreenVideoWindow.close()
        }
    }

    Component.onDestruction: {
        if (fullscreenVideoPlayer) {
            fullscreenVideoPlayer.stop()
        }
    }
}
