import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtMultimedia 5.15

import Components 1.0
import Futr 1.0

Item {
    id: root

    property string source: ""
    property bool clickable: true
    property string videoUrl: ""
    property int maxHeight: 400

    signal videoClicked(string url)
    signal fullScreenRequested(string url)
    signal showNotification(string message)

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
        height: Math.min(width * 9/16, root.maxHeight)
        color: "black"
        radius: Constants.radius_m

        VideoPlayer {
            id: videoPlayer
            anchors.fill: parent
            videoSource: root.videoUrl
            isFullscreen: false

            onFullScreenRequested: {
                videoPlayer.stop()
                root.fullScreenRequested(root.videoUrl)
            }

            onSaveVideoRequested: {
                root.showNotification("Video download started")
            }

            onCopyUrlRequested: {
                root.showNotification("URL copied to clipboard")
            }

            onStatusChanged: {
                if (status === MediaPlayer.Loaded) {
                    // Adjust container height based on video's actual aspect ratio
                    if (mediaObject &&
                        mediaObject.metaData.resolution &&
                        mediaObject.metaData.resolution.height > 0 &&
                        mediaObject.metaData.resolution.width > 0) {
                        var aspectRatio = mediaObject.metaData.resolution.height / mediaObject.metaData.resolution.width
                        var maxAspectRatio = 2.0
                        var minAspectRatio = 0.5
                        aspectRatio = Math.max(minAspectRatio, Math.min(aspectRatio, maxAspectRatio))
                        videoContainer.height = Math.min(videoContainer.width * aspectRatio, root.maxHeight)
                    }
                }
            }

            onShowNotification: function(message) {
                console.log("PostVideo: Forwarding notification:", message)
                root.showNotification(message)
            }
        }
    }

    BusyIndicator {
        anchors.centerIn: videoContainer
        running: videoPlayer.status === MediaPlayer.Loading ||
                 videoPlayer.status === MediaPlayer.Buffering
    }
}
