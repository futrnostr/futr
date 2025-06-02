import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import Components 1.0
import Dialogs 1.0
import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Item {
    id: homeScreen
    width: parent.width
    height: parent.height

    property string currentUserNpub: mynpub
    property string currentUserPicture: mypicture

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        TopBar {
            Layout.fillWidth: true
            height: 80
            currentUser: homeScreen.currentUserNpub
            currentUserPicture: homeScreen.currentUserPicture
        }

        Item {
            id: mainContainer
            Layout.fillWidth: true
            Layout.fillHeight: true

            property bool sidebarExpanded: true

            Row {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 10

                Item {
                    id: sidebarContainer
                    width: mainContainer.sidebarExpanded ? parent.width * 0.3 : 80
                    height: parent.height

                    layer.enabled: true
                    layer.effect: OpacityMask {
                        maskSource: Rectangle {
                            width: sidebarContainer.width
                            height: sidebarContainer.height
                            radius: Constants.radius_m
                        }
                    }

                    Behavior on width {
                        NumberAnimation {
                            duration: 200
                            easing.type: Easing.InOutQuad
                        }
                    }

                    Rectangle {
                        anchors.fill: parent
                        color: Material.backgroundColor
                        border.color: Material.dividerColor
                        border.width: 1
                        radius: Constants.radius_m
                    }

                    FollowList {
                        id: followListElement
                        anchors.fill: parent
                        anchors.margins: 10

                        isCollapsed: !mainContainer.sidebarExpanded
                        stackView: stackView
                        currentUser: homeScreen.currentUserNpub
                        currentUserPicture: homeScreen.currentUserPicture
                        personalFeed: personalFeed

                        Behavior on opacity {
                            NumberAnimation {
                                duration: 150
                                easing.type: Easing.InOutQuad
                            }
                        }
                    }

                    Rectangle {
                        id: toggleButton
                        anchors.right: parent.right
                        anchors.verticalCenter: parent.verticalCenter
                        width: 20
                        height: 40
                        color: "transparent"

                        Rectangle {
                            anchors.fill: parent
                            anchors.leftMargin: -8
                            color: toggleMouseArea.containsMouse ?
                                Qt.rgba(Material.accentColor.r,
                                       Material.accentColor.g,
                                       Material.accentColor.b, 0.2) :
                                Qt.rgba(Material.accentColor.r,
                                       Material.accentColor.g,
                                       Material.accentColor.b, 0.1)
                            radius: Constants.radius_m

                            Behavior on color {
                                ColorAnimation { duration: 150 }
                            }

                            Text {
                                anchors.centerIn: parent
                                text: mainContainer.sidebarExpanded ? "◀" : "▶"
                                color: Material.foreground
                                font.pixelSize: 12
                                opacity: 0.7
                            }
                        }

                        MouseArea {
                            id: toggleMouseArea
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            hoverEnabled: true
                            onClicked: mainContainer.sidebarExpanded = !mainContainer.sidebarExpanded

                            ToolTip {
                                visible: parent.containsMouse
                                text: mainContainer.sidebarExpanded ?
                                    "Collapse sidebar" :
                                    "Expand sidebar"
                                delay: 500
                            }
                        }
                    }

                    Column {
                        visible: !mainContainer.sidebarExpanded
                        opacity: mainContainer.sidebarExpanded ? 0 : 1
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.top: parent.top
                        anchors.margins: 8
                        spacing: 8

                        Behavior on opacity {
                            NumberAnimation { duration: 150 }
                        }
                    }
                }

                StackView {
                    id: stackView

                    width: mainContainer.sidebarExpanded ?
                        parent.width * 0.7 - parent.spacing :
                        parent.width - sidebarContainer.width - parent.spacing
                    height: parent.height

                    focus: true
                    Keys.onReleased: if (event.key === Qt.Key_Back && stackView.depth > 1) {
                        stackView.pop();
                        event.accepted = true;
                    }

                    initialItem: PersonalFeed {
                        id: personalFeed
                        npub: homeScreen.currentUserNpub
                        currentUser: homeScreen.currentUserNpub
                        currentUserPicture: homeScreen.currentUserPicture
                    }
                }

                Component {
                    id: postDetailsComponent

                    PostDetails {
                        post: null
                        currentUser: homeScreen.currentUserNpub
                        currentUserPicture: homeScreen.currentUserPicture
                    }
                }

                Component {
                    id: imageViewerComponent

                    ImageViewer {
                        imageSource: ""
                    }
                }
            }
        }

        Rectangle {
            Layout.fillWidth: true
            height: 30
            color: Material.backgroundColor

            Rectangle {
                width: parent.width
                height: 1
                color: Material.dividerColor
                anchors.top: parent.top
            }

            Text {
                anchors.right: parent.right
                anchors.bottom: parent.bottom
                anchors.margins: 10
                text: version
                font: Constants.smallFont
                color: Material.secondaryTextColor
            }
        }
    }

    RelayMgmtDialog {
        id: relayMgmtDialog
    }

    ShowKeysDialog {
        id: showKeysDialog
    }

    AlertManager {
        id: alertManager
    }

    InboxModelStatusAlert {
        id: inboxModelStatusAlert
        currentState: inboxModelState
    }

    NotificationToast {
        id: notification
        z: 999

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.top: parent.top
        anchors.topMargin: 80
    }
}
