import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import HsQML.Model 1.0
import Futr 1.0
import Profile 1.0

Rectangle {
    id: root
    color: Material.backgroundColor
    radius: Constants.radius_m

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

            property string selectedPubkey: ""

            model: AutoListModel {
                id: followsModel
                source: followList
                mode: AutoListModel.ByKey
                equalityTest: function (oldItem, newItem) {
                    return oldItem.pubkey === newItem.pubkey
                        && oldItem.petname === newItem.petname
                        && oldItem.displayName === newItem.displayName
                        && oldItem.name === newItem.name
                        && oldItem.picture === newItem.picture
                }
            }

            ScrollBar.vertical: ScrollBar {
                active: true
                policy: ScrollBar.AlwaysOn
            }

            delegate: Component {
                Rectangle {
                    id: followItem
                    property bool mouseHover: false
                    height: visible ? (root.isCollapsed ? 34 : 54) : 0
                    width: followsView.width
                    visible: {
                        if (!modelData) return false;
                        if (followListFilter.filterText === "") return true;
                        var searchText = followListFilter.filterText.toLowerCase();
                        var displayName = modelData.displayName || "";
                        var petname = modelData.petname || "";
                        var name = modelData.name || "";
                        var pubkey = modelData.pubkey || "";
                        return pubkey.toLowerCase().includes(searchText) ||
                            displayName.toLowerCase().includes(searchText) ||
                            petname.toLowerCase().includes(searchText) ||
                            name.toLowerCase().includes(searchText);
                    }

                    color: {
                        if (mouseHover) return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.2);
                        if (modelData && modelData.pubkey === followsView.selectedPubkey)
                            return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.15);
                        if (modelData && modelData.pubkey === currentUser)
                            return Qt.rgba(Material.primaryColor.r, Material.primaryColor.g, Material.primaryColor.b, 0.1);
                        return "transparent";
                    }

                    radius: Constants.radius_m

                    RowLayout {
                        anchors.fill: parent
                        anchors.margins: root.isCollapsed ? 2 : 7
                        spacing: root.isCollapsed ? 0 : 8

                        ProfilePicture {
                            imageSource: Util.getProfilePicture(modelData.picture, modelData.pubkey)
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 3
                            visible: !root.isCollapsed

                            Text {
                                text: modelData.petname || modelData.displayName || modelData.name || modelData.pubkey
                                font: Constants.font
                                color: Material.primaryTextColor
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                visible: modelData !== undefined && modelData !== null && modelData.pubkey !== currentUser
                            }

                            Text {
                                text: modelData.name || modelData.pubkey
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: modelData !== undefined && modelData !== null
                                        && modelData.pubkey !== currentUser
                                        && (modelData.displayName !== "" || modelData.name !== "")
                            }

                            Text {
                                text: "Myself"
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                font: Constants.smallFont
                                color: Material.secondaryTextColor
                                visible: modelData !== undefined && modelData !== null && modelData.pubkey === currentUser
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
                            text: modelData.petname || modelData.displayName || modelData.name || modelData.pubkey
                            delay: 500
                        }

                        onClicked: {
                            if (modelData === undefined || modelData === null) return;
                            followsView.selectedPubkey = modelData.pubkey
                            personalFeed.npub = modelData.pubkey
                        }
                    }
                }
            }
        }
    }
}
