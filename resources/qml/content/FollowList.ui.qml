import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Components 1.0
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

    property string currentFilter: "all" // "all" or "follow"

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: root.isCollapsed ? 4 : 10
        spacing: root.isCollapsed ? 4 : 10

        TabBar {
            id: filterTabs
            Layout.fillWidth: true
            visible: !root.isCollapsed

            TabButton {
                text: "All"
                Layout.preferredHeight: 32
                width: implicitWidth
                checked: root.currentFilter === "all"
                onClicked: root.currentFilter = "all"
            }

            TabButton {
                text: "Follows"
                Layout.preferredHeight: 32
                width: implicitWidth
                checked: root.currentFilter === "follow"
                onClicked: root.currentFilter = "follow"
            }
        }

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
            clip: true

            //cacheBuffer: height * 15

            property string selectedPubkey: ""

            model: AutoListModel {
                source: followList
                mode: AutoListModel.ByKey
            }

            ScrollBar.vertical: ScrollBar {
                active: true
                policy: ScrollBar.AlwaysOn
            }

            delegate: Component {
                Rectangle {
                    id: followItem

                    property string follow_pubkey: modelData ? modelData.pubkey : ""
                    property string follow_petname: modelData ? modelData.petname : ""
                    property string follow_displayName: modelData ? modelData.displayName : ""
                    property string follow_name: modelData ? modelData.name : ""
                    property string follow_picture: modelData ? modelData.picture : ""
                    property string follow_type: modelData ? modelData.follow_type : ""
                    property bool mouseHover: false
                    height: modelData && isFiltered ? (root.isCollapsed ? 34 : 54) : 0
                    width: followsView.width
                    visible: modelData && isFiltered

                    property bool isFiltered: {
                        if (!modelData) return false;
                        if (root.currentFilter !== "all" && root.currentFilter !== follow_type) return false;
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
                        if (modelData && follow_pubkey === followsView.selectedPubkey)
                            return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.15);
                        if (modelData && follow_pubkey === currentUser)
                            return Qt.rgba(Material.primaryColor.r, Material.primaryColor.g, Material.primaryColor.b, 0.1);
                        return "transparent";
                    }

                    radius: Constants.radius_m

                    RowLayout {
                        anchors.fill: parent
                        anchors.margins: root.isCollapsed ? 2 : 7
                        spacing: root.isCollapsed ? 0 : 8

                        ProfilePicture {
                            url: follow_picture
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
                                visible: modelData && follow_pubkey !== currentUser
                            }

                            Text {
                                text: follow_name || follow_pubkey
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: modelData && follow_pubkey !== currentUser && ((follow_displayName !== "") || (follow_name !== ""))
                            }

                            Text {
                                text: qsTr("Myself")
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: modelData && follow_pubkey === currentUser
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
                            visible: parent && parent.containsMouse && root.isCollapsed
                            text: (modelData && follow_pubkey === currentUser)
                                ? qsTr("Myself")
                                : (follow_petname || follow_displayName || follow_name || follow_pubkey)
                            delay: 500
                        }

                        onClicked: {
                            if (!modelData) return;
                            if (stackView.depth > 1) {
                                stackView.pop();
                            }
                            followsView.selectedPubkey = follow_pubkey
                            personalFeed.npub = follow_pubkey
                        }
                    }
                }
            }
        }
    }
}
