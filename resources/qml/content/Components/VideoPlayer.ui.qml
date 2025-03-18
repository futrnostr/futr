import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtMultimedia 5.15

Item {
    id: videoPlayerContainer
    property bool isFullscreen: false
    property bool controlsVisible: true
    property string videoSource: ""
    property alias videoUrl: videoPlayerContainer.videoSource

    signal fullScreenRequested()
    signal saveVideoRequested()
    signal copyUrlRequested()
    signal showNotification(string message)

    MediaPlayer {
        id: mediaPlayer
        source: videoSource
        autoPlay: false
    }

    VideoOutput {
        id: videoOutput
        anchors.fill: parent
        source: mediaPlayer
        fillMode: isFullscreen ? VideoOutput.PreserveAspectFit : VideoOutput.PreserveAspectCrop
    }

    MouseArea {
        id: videoMouseArea
        anchors.fill: parent
        hoverEnabled: true

        function isMouseInControls() {
            return containsMouse ||
                   progressMouseArea.containsMouse ||
                   controlsContainer.containsMouse
        }

        onClicked: {
            var controlsPos = controlsContainer.mapToItem(videoMouseArea, 0, 0)
            if (mouseY < controlsPos.y) {
                if (mediaPlayer.playbackState === MediaPlayer.PlayingState) {
                    mediaPlayer.pause()
                } else {
                    mediaPlayer.play()
                }
            }
        }

        onPositionChanged: {
            if (isFullscreen) {
                controlsVisible = true
                cursorShape = Qt.ArrowCursor
                hideControlsTimer.restart()
            }
        }
    }

    Rectangle {
        id: playPauseButton
        anchors.centerIn: parent
        width: 80
        height: 80
        radius: 40
        color: "#80000000"
        visible: (!isFullscreen && mediaPlayer.playbackState !== MediaPlayer.PlayingState)
                 || (isFullscreen && controlsVisible)
        opacity: 0.8

        Behavior on opacity {
            NumberAnimation { duration: 200 }
        }

        Image {
            anchors.centerIn: parent
            width: 40
            height: 40
            source: mediaPlayer.playbackState === MediaPlayer.PlayingState
                    ? "qrc:/icons/pause.svg"
                    : "qrc:/icons/play_arrow.svg"
            fillMode: Image.PreserveAspectFit
        }
    }

    Timer {
        id: hideControlsTimer
        interval: 2000
        running: false
        repeat: false
        onTriggered: {
            if (!videoMouseArea.isMouseInControls()) {
                controlsVisible = false
                videoMouseArea.cursorShape = Qt.BlankCursor
            }
        }
    }

    property alias mediaObject: mediaPlayer
    property alias status: mediaPlayer.status
    property alias duration: mediaPlayer.duration
    property alias position: mediaPlayer.position
    property alias playbackState: mediaPlayer.playbackState

    function play() {
        mediaPlayer.play()
    }

    function pause() {
        mediaPlayer.pause()
    }

    function stop() {
        mediaPlayer.stop()
    }

    function seek(position) {
        mediaPlayer.seek(position)
    }

    Text {
        id: timeDisplay
        anchors.left: parent.left
        anchors.bottom: progressBarBackground.top
        anchors.bottomMargin: 5
        anchors.leftMargin: 10
        color: "white"
        font.pixelSize: 12
        text: {
            function formatTime(ms) {
                var totalSeconds = Math.floor(ms / 1000);
                var minutes = Math.floor(totalSeconds / 60);
                var seconds = totalSeconds % 60;
                return minutes + ":" + (seconds < 10 ? "0" : "") + seconds;
            }
            return formatTime(mediaPlayer.position) + " / " +
                   formatTime(mediaPlayer.duration);
        }
        visible: !isFullscreen || controlsVisible
        style: Text.Outline
        styleColor: "black"
        z: 10
    }

    Rectangle {
        id: progressBarBackground
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 8
        color: "#80000000"
        visible: !isFullscreen || controlsVisible
        z: 10

        Rectangle {
            id: progressBar
            anchors.left: parent.left
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            width: mediaPlayer.position > 0 && mediaPlayer.duration > 0 ?
                   parent.width * (mediaPlayer.position / mediaPlayer.duration) : 0
            color: Material.accent
        }

        MouseArea {
            id: progressMouseArea
            anchors.fill: parent
            anchors.topMargin: -15
            anchors.bottomMargin: -5
            hoverEnabled: true

            onPressed: {
                if (mediaPlayer.seekable) {
                    var newPosition = mouseX / width * mediaPlayer.duration
                    mediaPlayer.seek(newPosition)
                }
            }

            onPositionChanged: {
                if (pressed && mediaPlayer.seekable) {
                    var newPosition = Math.max(0, Math.min(mouseX, width)) / width * mediaPlayer.duration
                    mediaPlayer.seek(newPosition)
                }
            }
        }
    }

    Rectangle {
        id: controlsContainer
        anchors.bottom: progressBarBackground.top
        anchors.right: parent.right
        anchors.rightMargin: 20
        height: 50
        width: 150
        color: "transparent"
        visible: !isFullscreen || controlsVisible

        opacity: visible ? 1.0 : 0.0

        Behavior on opacity {
            NumberAnimation { duration: 200 }
        }

        property bool containsMouse: false

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            onContainsMouseChanged: {
                controlsContainer.containsMouse = containsMouse
                if (containsMouse && isFullscreen) {
                    controlsVisible = true
                    hideControlsTimer.restart()
                }
            }
            propagateComposedEvents: true
        }

        Row {
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
            spacing: 8

            Button {
                icon.source: "qrc:/icons/download.svg"
                ToolTip.visible: hovered
                ToolTip.text: "Save Video"
                onClicked: {
                    downloadCompleted.connect(videoDownloadCallback)
                    downloadAsync(videoPlayerContainer.videoSource)
                    notification.show("Video download started")
                }
            }

            Button {
                icon.source: "qrc:/icons/content_copy.svg"
                ToolTip.visible: hovered
                ToolTip.text: "Copy URL"
                onClicked: {
                    clipboard.copyText(videoPlayerContainer.videoSource)
                    notification.show("URL copied to clipboard")
                }
            }

            Button {
                icon.source: isFullscreen ? "qrc:/icons/fullscreen_exit.svg" : "qrc:/icons/fullscreen.svg"
                ToolTip.visible: hovered
                ToolTip.text: isFullscreen ? "Exit fullscreen" : "Fullscreen"
                onClicked: videoPlayerContainer.fullScreenRequested()
            }
        }

        onVisibleChanged: {
            if (visible && isFullscreen) {
                Qt.callLater(function() {
                    var temp = width
                    width = temp
                })
            }
        }
    }

    function isMouseInControls() {
        return videoMouseArea.isMouseInControls()
    }

    function videoDownloadCallback(success, filePathOrError) {
        downloadCompleted.disconnect(videoDownloadCallback)

        if (success) {
            notification.show("Saved to Downloads folder: " + filePathOrError)
        } else {
            notification.show("Download failed: " + filePathOrError)
        }
    }
}
