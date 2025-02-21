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

    property bool autoScroll: true
    property real scrollThreshold: 50  // pixels from bottom
    property int lastContentY: 0
    property int lastContentHeight: 0

    focus: true
    keyNavigationEnabled: true
    keyNavigationWraps: false

    onContentHeightChanged: {
        // If content height increased and we were at bottom, maintain bottom position
        if (contentHeight > lastContentHeight && autoScroll) {
            scrollToBottom()
        }
        lastContentHeight = contentHeight
    }

    onContentYChanged: {
        // Update autoScroll based on user's scroll position
        if (contentHeight > height) {
            autoScroll = contentHeight - (contentY + height) < scrollThreshold
        }
        lastContentY = contentY
    }

    function scrollToBottom() {
        Qt.callLater(() => {
            positionViewAtEnd()
            autoScroll = true  // Ensure autoScroll is enabled after manual scroll
        })
    }

    Component.onCompleted: {
        // Initial scroll to bottom when view is created
        Qt.callLater(() => {
            scrollToBottom()
        })
    }

    onModelChanged: {
        if (model) {
            scrollToBottom()
        }
    }

    ScrollBar.vertical: ScrollBar {
        active: true
        interactive: true
        policy: ScrollBar.AlwaysOn

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
