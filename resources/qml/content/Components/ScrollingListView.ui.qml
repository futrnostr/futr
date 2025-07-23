import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15

import Futr 1.0

ListView {
    id: root
    clip: true
    leftMargin: Constants.spacing_m
    rightMargin: Constants.spacing_m
    spacing: Constants.spacing_m
    bottomMargin: 0
    focus: true
    keyNavigationEnabled: true
    keyNavigationWraps: false

    cacheBuffer: Math.max(200, height * 2)
    displayMarginBeginning: 100
    displayMarginEnd: 100

    property bool shouldBeAtBottom: false

    Component.onCompleted: {
        if (verticalLayoutDirection === ListView.BottomToTop) {
            positionViewAtBeginning()
        } else {
            positionViewAtEnd()
        }
        shouldBeAtBottom = true
    }

    onCountChanged: {
        if (shouldBeAtBottom) {
            if (verticalLayoutDirection === ListView.BottomToTop) {
                positionViewAtBeginning()
            } else {
                positionViewAtEnd()
            }
        }
    }

    onContentHeightChanged: {
        if (shouldBeAtBottom) {
            if (verticalLayoutDirection === ListView.BottomToTop) {
                positionViewAtBeginning()
            } else {
                positionViewAtEnd()
            }
        }
    }

    onMovementStarted: {
        shouldBeAtBottom = false
    }

    onMovementEnded: {
        if (verticalLayoutDirection === ListView.BottomToTop) {
            if (atYBeginning) {
                shouldBeAtBottom = true
            }
        } else {
            if (atYEnd) {
                shouldBeAtBottom = true
            }
        }
    }

    onFlickStarted: {
        shouldBeAtBottom = false
    }

    onFlickEnded: {
        if (verticalLayoutDirection === ListView.BottomToTop) {
            if (atYBeginning) {
                shouldBeAtBottom = true
            }
        } else {
            if (atYEnd) {
                shouldBeAtBottom = true
            }
        }
    }

    ScrollBar.vertical: ScrollBar {
        id: verticalScrollBar
        active: true
        interactive: true
        policy: ScrollBar.AlwaysOn
        topPadding: topArrow.height
        bottomPadding: bottomArrow.height
        minimumSize: 0.1

        onActiveChanged: {
            if (active) {
                root.shouldBeAtBottom = false
            } else {
                if (root.verticalLayoutDirection === ListView.BottomToTop) {
                    if (root.atYBeginning) {
                        root.shouldBeAtBottom = true
                    }
                } else {
                    if (root.atYEnd) {
                        root.shouldBeAtBottom = true
                    }
                }
            }
        }

        onPressedChanged: {
            if (pressed && verticalLayoutDirection !== ListView.BottomToTop) {
                root.shouldBeAtBottom = root.atYEnd
            }
        }

        Rectangle {
            id: topArrow
            width: 16
            height: 16
            anchors.top: parent.top
            anchors.horizontalCenter: parent.horizontalCenter
            color: topMouseArea.pressed ? Qt.darker(Material.scrollBarColor, 1.2) : Material.scrollBarColor
            border.color: Qt.darker(Material.scrollBarColor, 1.5)
            border.width: 1
            z: 1

            Canvas {
                anchors.fill: parent
                anchors.margins: 2
                onPaint: {
                    var ctx = getContext("2d");
                    ctx.fillStyle = Material.foreground;
                    ctx.beginPath();
                    ctx.moveTo(width/2, 2);
                    ctx.lineTo(width-2, height-2);
                    ctx.lineTo(2, height-2);
                    ctx.closePath();
                    ctx.fill();
                }
            }

            MouseArea {
                id: topMouseArea
                anchors.fill: parent
                onPressed: {
                    root.flick(0, 800);
                    scrollTimer.direction = -1;
                    scrollTimer.start();
                }
                onReleased: {
                    scrollTimer.stop();
                }
            }
        }

        Rectangle {
            id: bottomArrow
            width: 16
            height: 16
            anchors.bottom: parent.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            color: bottomMouseArea.pressed ? Qt.darker(Material.scrollBarColor, 1.2) : Material.scrollBarColor
            border.color: Qt.darker(Material.scrollBarColor, 1.5)
            border.width: 1
            z: 1

            Canvas {
                anchors.fill: parent
                anchors.margins: 2
                onPaint: {
                    var ctx = getContext("2d");
                    ctx.fillStyle = Material.foreground;
                    ctx.beginPath();
                    ctx.moveTo(2, 2);
                    ctx.lineTo(width-2, 2);
                    ctx.lineTo(width/2, height-2);
                    ctx.closePath();
                    ctx.fill();
                }
            }

            MouseArea {
                id: bottomMouseArea
                anchors.fill: parent
                onPressed: {
                    root.flick(0, -800);
                    scrollTimer.direction = 1;
                    scrollTimer.start();
                }
                onReleased: {
                    scrollTimer.stop();
                }
            }
        }

        Timer {
            id: scrollTimer
            interval: 50
            repeat: true
            property int direction: 0
            onTriggered: {
                if (direction < 0) {
                    root.flick(0, 800);
                } else {
                    root.flick(0, -800);
                }
            }
        }

        background: Rectangle {
            implicitWidth: 16
            color: Qt.rgba(Material.scrollBarColor.r,
                           Material.scrollBarColor.g,
                           Material.scrollBarColor.b, 0.3)
        }

        contentItem: Rectangle {
            implicitWidth: 6
            radius: width / 2
            color: parent.pressed ? Material.scrollBarPressedColor :
                   parent.hovered ? Material.scrollBarHoveredColor :
                                    Material.scrollBarColor
            opacity: 1
        }
    }
}
