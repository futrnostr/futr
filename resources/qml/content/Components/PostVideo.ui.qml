import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtMultimedia 5.15

import Futr 1.0

Item {
    id: root

    property string source: ""
    property bool clickable: true
    property string videoUrl: ""
    property int maxHeight: 400  // Maximum height for the video container

    signal videoClicked(string url)
    signal fullScreenRequested(string url)

    // Add these functions to expose the video player methods
    function play() {
        videoPlayer.play()
    }

    function pause() {
        videoPlayer.pause()
    }

    implicitHeight: videoContainer.height

    Rectangle {
        id: videoContainer
        width: parent.width
        height: Math.min(width * 9/16, root.maxHeight)  // 16:9 aspect ratio by default, with max height
        color: "black"
        radius: 8

        Video {
            id: videoPlayer
            anchors.fill: parent
            source: root.source
            fillMode: VideoOutput.PreserveAspectCrop
            autoPlay: false

            onStatusChanged: {
                if (status === MediaPlayer.Loaded) {
                    // Adjust container height based on video's actual aspect ratio
                    if (videoPlayer.metaData.resolution &&
                        videoPlayer.metaData.resolution.height > 0 &&
                        videoPlayer.metaData.resolution.width > 0) {
                        var aspectRatio = videoPlayer.metaData.resolution.height / videoPlayer.metaData.resolution.width

                        // Set a maximum aspect ratio to prevent extremely tall videos
                        var maxAspectRatio = 2.0  // Maximum height:width ratio (2:1)
                        var minAspectRatio = 0.5  // Minimum height:width ratio (1:2)

                        // Clamp the aspect ratio within reasonable bounds
                        aspectRatio = Math.max(minAspectRatio, Math.min(aspectRatio, maxAspectRatio))

                        // Apply the calculated aspect ratio with max height constraint
                        videoContainer.height = Math.min(videoContainer.width * aspectRatio, root.maxHeight)
                    }
                }
            }
        }

        Rectangle {
            anchors.centerIn: parent
            width: 60
            height: 60
            radius: 30
            color: "#80000000"  // Semi-transparent black
            visible: videoPlayer.playbackState !== MediaPlayer.PlayingState

            Image {
                anchors.centerIn: parent
                width: 30
                height: 30
                source: videoPlayer.playbackState === MediaPlayer.PlayingState
                        ? "qrc:/icons/pause.svg"
                        : "qrc:/icons/play_arrow.svg"
                fillMode: Image.PreserveAspectFit
            }
        }

        // Main video MouseArea (for play/pause)
        MouseArea {
            id: videoMouseArea
            anchors.fill: parent
            onClicked: {
                if (root.clickable) {
                    if (videoPlayer.playbackState === MediaPlayer.PlayingState) {
                        videoPlayer.pause()
                    } else {
                        videoPlayer.play()
                    }
                }
            }
        }

        // Full screen button
        Rectangle {
            id: fullScreenButton
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            anchors.margins: 10
            width: 40
            height: 40
            radius: 20
            color: "#80000000"  // Semi-transparent black
            z: 2  // Ensure this is above other elements

            Image {
                anchors.centerIn: parent
                width: 24
                height: 24
                source: "qrc:/icons/fullscreen.svg"
                fillMode: Image.PreserveAspectFit
            }

            MouseArea {
                anchors.fill: parent
                onClicked: {
                    var videoSource = root.videoUrl && root.videoUrl !== "" ? root.videoUrl : root.source
                    root.fullScreenRequested(videoSource)
                }
            }
        }

        Text {
            id: timeDisplay
            anchors.left: videoContainer.left
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

                return formatTime(videoPlayer.position) + " / " +
                       formatTime(videoPlayer.duration);
            }

            style: Text.Outline
            styleColor: "black"
        }

        Rectangle {
            id: progressBarBackground
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            height: 8
            color: "#80000000"  // Semi-transparent black

            Rectangle {
                id: progressBar
                anchors.left: parent.left
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                width: videoPlayer.position > 0 && videoPlayer.duration > 0 ?
                       parent.width * (videoPlayer.position / videoPlayer.duration) : 0
                color: Material.accent
            }

            MouseArea {
                anchors.fill: parent
                anchors.topMargin: -15  // Increased touch area upward
                anchors.bottomMargin: -5  // Extend the touch area downward

                onPressed: {
                    if (videoPlayer.seekable) {
                        var newPosition = mouseX / width * videoPlayer.duration
                        videoPlayer.seek(newPosition)
                    }
                }

                onPositionChanged: {
                    if (pressed && videoPlayer.seekable) {
                        var newPosition = Math.max(0, Math.min(mouseX, width)) / width * videoPlayer.duration
                        videoPlayer.seek(newPosition)
                    }
                }
            }
        }
    }

    BusyIndicator {
        anchors.centerIn: videoContainer
        running: videoPlayer.status === MediaPlayer.Loading ||
                 videoPlayer.status === MediaPlayer.Buffering
    }
}
