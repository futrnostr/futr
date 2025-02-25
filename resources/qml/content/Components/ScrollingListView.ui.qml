import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15

import Futr 1.0

ListView {
    id: root
    clip: true
    verticalLayoutDirection: ListView.TopToBottom
    leftMargin: Constants.spacing_m
    rightMargin: Constants.spacing_m
    spacing: Constants.spacing_m
    bottomMargin: 0
    focus: true
    keyNavigationEnabled: true
    keyNavigationWraps: false

    Component.onCompleted: positionViewAtEnd();

    onCountChanged: {
        if (atEnd) positionViewAtEnd();
    }

    property bool atEnd: true

    onContentYChanged: {
        atEnd = contentY <= contentHeight - height;
    }

    ScrollBar.vertical: ScrollBar {
        id: verticalScrollBar
        active: true
        interactive: true
        policy: ScrollBar.AlwaysOn
        topPadding: topArrow.height
        bottomPadding: bottomArrow.height

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
                    root.flick(0, 800); // Increased flick speed from 300 to 800
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
                    root.flick(0, -800); // Increased flick speed from -300 to -800
                    scrollTimer.direction = 1;
                    scrollTimer.start();
                }
                onReleased: {
                    scrollTimer.stop();
                }
            }
        }

        // Timer for continuous scrolling while button is held
        Timer {
            id: scrollTimer
            interval: 50 // Reduced from 100ms to 50ms for more responsive scrolling
            repeat: true
            property int direction: 0 // -1 for up, 1 for down
            onTriggered: {
                if (direction < 0) {
                    root.flick(0, 800); // Increased flick speed from 300 to 800
                } else {
                    root.flick(0, -800); // Increased flick speed from -300 to -800
                }
            }
        }

        // Background track for the scrollbar
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
