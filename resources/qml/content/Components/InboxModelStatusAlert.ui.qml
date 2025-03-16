import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls.Material 2.15
import QtGraphicalEffects 1.15

import Futr 1.0

Rectangle {
    id: inboxStatusAlert
    property int liveProcessingTimeout: 1500

    visible: inboxModelState !== undefined && opacity > 0
    opacity: inboxModelState !== undefined ? 1 : 0
    width: Math.min(parent.width * 0.7, 350)
    height: inboxStatusLayout.implicitHeight + 16
    radius: 4

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.top: parent.top
    anchors.topMargin: 16

    color: {
        if (inboxModelState === "Stopped") {
            return Material.color(Material.Red, Material.LightShade);
        } else if (inboxModelState === "LiveProcessing") {
            return Material.color(Material.Green, Material.LightShade);
        } else {
            return Material.color(Material.Blue, Material.LightShade);
        }
    }

    ColumnLayout {
        id: inboxStatusLayout
        anchors.fill: parent
        anchors.margins: 8
        spacing: 8

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            Image {
                source: {
                    if (inboxModelState === "LiveProcessing") {
                        return "qrc:/icons/check.svg";
                    } else if (inboxModelState === "Stopped") {
                        return "qrc:/icons/error.svg";
                    } else {
                        return "qrc:/icons/sync.svg";
                    }
                }
                width: 16
                height: 16

                ColorOverlay {
                    anchors.fill: parent
                    source: parent
                    color: {
                        if (inboxModelState === "Stopped") {
                            return Material.color(Material.Red);
                        } else if (inboxModelState === "LiveProcessing") {
                            return Material.color(Material.Green);
                        } else {
                            return Material.color(Material.Blue);
                        }
                    }
                }
            }

            Text {
                text: {
                    if (inboxModelState === "Stopped") {
                        return "Inbox Model Stopped";
                    } else if (inboxModelState === "InitialBootstrap") {
                        return "Bootrapping...";
                    } else if (inboxModelState === "SyncingHistoricData") {
                        return "Downloading events from relays...";
                    } else if (inboxModelState === "LiveProcessing") {
                        return "Live Processing";
                    } else {
                        return "Unknown relay status";
                    }
                }
                color: {
                    if (inboxModelState === "Stopped") {
                        return Material.color(Material.Red);
                    } else if (inboxModelState === "LiveProcessing") {
                        return Material.color(Material.Green);
                    } else {
                        return Material.color(Material.Blue);
                    }
                }
                font {
                    family: Constants.font.family
                    pixelSize: Constants.font.pixelSize
                    weight: Font.Medium
                }
            }

            Item {
                Layout.fillWidth: true
                Layout.fillHeight: true
            }

            Button {
                flat: true
                implicitWidth: 40
                implicitHeight: 40
                onClicked: inboxStatusAlert.opacity = 0

                icon.source: "qrc:/icons/close.svg"
                icon.width: 36
                icon.height: 36

                icon.color: {
                    if (inboxModelState === "Stopped") {
                        return Material.color(Material.Red, Material.Shade800);
                    } else if (inboxModelState === "LiveProcessing") {
                        return Material.color(Material.Green, Material.Shade800);
                    } else {
                        return Material.color(Material.Blue, Material.Shade800);
                    }
                }

                background: Rectangle {
                    implicitWidth: 40
                    implicitHeight: 40
                    color: "transparent"
                    radius: width / 2

                    Rectangle {
                        anchors.fill: parent
                        radius: width / 2
                        color: parent.parent.hovered ? "#20000000" : "transparent"
                    }
                }
            }
        }
    }

    layer.enabled: true
    layer.effect: DropShadow {
        transparentBorder: true
        horizontalOffset: 1
        verticalOffset: 1
        radius: 8.0
        samples: 17
        color: "#80000000"
    }

    Timer {
        id: hideInboxStatusTimer
        interval: liveProcessingTimeout
        running: false
        repeat: false
        onTriggered: {
            inboxStatusAlert.opacity = 0;
        }
    }

    Behavior on opacity {
        NumberAnimation {
            duration: 500
            easing.type: Easing.InOutQuad
        }
    }

    onVisibleChanged: {
        if (visible && inboxModelState === "LiveProcessing") {
            hideInboxStatusTimer.restart()
        }
    }

    StateGroup {
        states: [
            State {
                name: "liveProcessingState"
                when: inboxModelState === "LiveProcessing"
                StateChangeScript {
                    script: {
                        hideInboxStatusTimer.restart()
                    }
                }
            }
        ]
    }

    Component.onCompleted: {
        if (inboxModelState === "LiveProcessing") {
            hideInboxStatusTimer.start()
        }
    }
}
