import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Rectangle {
    id: root
    color: "transparent"

    property bool isCollapsed: false
    property var stackView: null
    property string currentUser: ""
    property string currentUserPicture: ""
    required property var personalFeed

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: root.isCollapsed ? 4 : 10
        spacing: root.isCollapsed ? 4 : 10

        FollowListFilter {
            id: followListFilter
            Layout.fillWidth: true
            visible: !root.isCollapsed
        }

        // Follows list
        ListView {
            id: followsView
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: Constants.spacing_xs

            cacheBuffer: height * 15

            property string selectedPubkey: ""

            model: followList

            ScrollBar.vertical: ScrollBar {
                active: true
                policy: ScrollBar.AlwaysOn
            }

            delegate: Component {
                Rectangle {
                    id: followItem
                    property var follow: modelData
                    property string follow_pubkey: follow ? follow[0] : ""
                    property string follow_petname: follow ? follow[1] : ""
                    property string follow_displayName: follow ? follow[2] : ""
                    property string follow_name: follow ? follow[3] : ""
                    property string follow_picture: follow ? follow[4] : ""
                    property bool mouseHover: false
                    height: visible ? (root.isCollapsed ? 34 : 54) : 0
                    width: followsView.width
                    visible: {
                        if (!follow) return false;
                        if (followListFilter.filterText === "") return true;
                        var searchText = followListFilter.filterText.toLowerCase();
                        var displayName = follow_displayName || "";
                        var petname = follow_petname || "";
                        var name = follow_name || "";
                        var pubkey = follow_pubkey || "";
                        return pubkey.toLowerCase().includes(searchText) ||
                            displayName.toLowerCase().includes(searchText) ||
                            petname.toLowerCase().includes(searchText) ||
                            name.toLowerCase().includes(searchText);
                    }

                    color: {
                        if (mouseHover) return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.2);
                        if (follow && follow_pubkey === followsView.selectedPubkey)
                            return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.15);
                        if (follow && follow_pubkey === currentUser)
                            return Qt.rgba(Material.primaryColor.r, Material.primaryColor.g, Material.primaryColor.b, 0.1);
                        return "transparent";
                    }

                    radius: Constants.radius_m

                    RowLayout {
                        anchors.fill: parent
                        anchors.margins: root.isCollapsed ? 2 : 7
                        spacing: root.isCollapsed ? 0 : 8

                        ProfilePicture {
                            imageSource: getProfilePicture(follow_pubkey, follow_picture)
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 3
                            visible: !root.isCollapsed

                            Text {
                                text: follow_petname || follow_displayName || follow_name || follow_pubkey
                                font: Constants.font
                                color: Material.primaryTextColor
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                visible: follow && follow_pubkey !== currentUser
                            }

                            Text {
                                text: follow_name || follow_pubkey
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: follow && follow_pubkey !== currentUser && ((follow_displayName !== "") || (follow_name !== ""))
                            }

                            Text {
                                text: "Myself"
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: follow && follow_pubkey === currentUser
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor

                        onEntered: followItem.mouseHover = true
                        onExited: followItem.mouseHover = false

                        ToolTip {
                            visible: parent.containsMouse && root.isCollapsed
                            text: follow_petname || follow_displayName || follow_name || follow_pubkey
                            delay: 500
                        }

                        onClicked: {
                            if (!follow) return;
                            followsView.selectedPubkey = follow_pubkey
                            personalFeed.npub = follow_pubkey
                        }
                    }
                }
            }
        }
    }
}
