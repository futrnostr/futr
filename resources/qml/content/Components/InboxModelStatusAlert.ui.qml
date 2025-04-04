import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls.Material 2.15
import QtGraphicalEffects 1.15

import Futr 1.0

Rectangle {
    id: inboxStatusAlert

    property int liveProcessingTimeout: 1500
    required property string currentState

    opacity: currentState !== undefined ? 1 : 0
    width: Math.min(parent.width * 0.7, 350)
    height: inboxStatusLayout.implicitHeight + 16
    radius: Constants.radius_m

    anchors.horizontalCenter: parent.horizontalCenter
    anchors.top: parent.top
    anchors.topMargin: 16

    color: {
        if (currentState === "Stopped") {
            return Material.color(Material.Red, Material.LightShade);
        } else if (currentState === "LiveProcessing") {
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
                    if (currentState === "LiveProcessing") {
                        return "qrc:/icons/check.svg";
                    } else if (currentState === "Stopped") {
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
                        if (currentState === "Stopped") {
                            return Material.color(Material.Red);
                        } else if (currentState === "LiveProcessing") {
                            return Material.color(Material.Green);
                        } else {
                            return Material.color(Material.Blue);
                        }
                    }
                }
            }

            Text {
                text: {
                    if (currentState === "Stopped") {
                        return "Inbox Model Stopped";
                    } else if (currentState === "InitialBootstrap") {
                        return "Bootrapping...";
                    } else if (currentState === "SyncingHistoricData") {
                        return "Downloading events from relays...";
                    } else if (currentState === "LiveProcessing") {
                        return "Live Processing";
                    } else {
                        return "Unknown relay status";
                    }
                }
                color: {
                    if (currentState === "Stopped") {
                        return Material.color(Material.Red);
                    } else if (currentState === "LiveProcessing") {
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
                    if (currentState === "Stopped") {
                        return Material.color(Material.Red, Material.Shade800);
                    } else if (currentState === "LiveProcessing") {
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
        radius: Constants.radius_m
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
        if (visible && currentState === "LiveProcessing") {
            hideInboxStatusTimer.restart()
        }
    }

    StateGroup {
        states: [
            State {
                name: "liveProcessingState"
                when: currentState === "LiveProcessing"
                StateChangeScript {
                    script: {
                        hideInboxStatusTimer.restart()
                    }
                }
            }
        ]
    }

    Component.onCompleted: {
        if (currentState === "LiveProcessing") {
            hideInboxStatusTimer.start()
        }
    }
}
