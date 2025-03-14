import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    id: root
    color: Material.backgroundColor
    radius: 8

    property bool isCollapsed: false

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
        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            color: "transparent"

            ListView {
                id: followsView
                anchors.fill: parent
                clip: true
                spacing: root.isCollapsed ? 4 : 5
                property string selectedPubkey: ""

                model: AutoListModel {
                    id: followsModel
                    source: followList
                    mode: AutoListModel.ByKeyNoReorder
                    keyFunction: function(item) {
                        return item ? item.pubkey : ""
                    }
                }

                cacheBuffer: 200
                reuseItems: true

                ScrollBar.vertical: ScrollBar {
                    active: true
                    policy: ScrollBar.AlwaysOn
                }

                delegate: Loader {
                    id: delegateLoader
                    width: followsView.width - followsView.ScrollBar.vertical.width
                    height: active && item && visible ? (root.isCollapsed ? 34 : 54) : 0
                    active: modelData !== undefined && modelData !== null
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

                    sourceComponent: Rectangle {
                        id: followItem
                        property bool mouseHover: false
                        height: delegateLoader.visible ? (root.isCollapsed ? 34 : 54) : 0
                        width: parent.width
                        visible: delegateLoader.visible
                        color: {
                            if (mouseHover) return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.2);
                            if (modelData && modelData.pubkey === followsView.selectedPubkey)
                                return Qt.rgba(Material.accentColor.r, Material.accentColor.g, Material.accentColor.b, 0.15);
                            if (modelData && modelData.pubkey === mynpub)
                                return Qt.rgba(Material.primaryColor.r, Material.primaryColor.g, Material.primaryColor.b, 0.1);
                            return "transparent";
                        }
                        radius: root.isCollapsed ? height/2 : 6

                        Behavior on color {
                            ColorAnimation { duration: 150 }
                        }

                        RowLayout {
                            anchors.fill: parent
                            anchors.margins: root.isCollapsed ? 2 : 7
                            spacing: root.isCollapsed ? 0 : 8

                            Rectangle {
                                Layout.preferredWidth: root.isCollapsed ? 30 : 34
                                Layout.preferredHeight: Layout.preferredWidth
                                Layout.alignment: Qt.AlignVCenter
                                radius: width/2
                                color: Material.backgroundColor

                                Image {
                                    anchors.fill: parent
                                    anchors.margins: 2
                                    source: Util.getProfilePicture(modelData.picture, modelData.pubkey)
                                    smooth: true
                                    fillMode: Image.PreserveAspectCrop
                                    layer.enabled: true
                                    layer.effect: OpacityMask {
                                        maskSource: Rectangle {
                                            width: 30
                                            height: 30
                                            radius: width/2
                                        }
                                    }
                                }
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
                                    visible: modelData !== undefined
                                        && modelData !== null
                                        && modelData.pubkey !== mynpub
                                }

                                Text {
                                    text: modelData.name || modelData.pubkey
                                    elide: Text.ElideRight
                                    Layout.fillWidth: true
                                    font: Constants.smallFont
                                    color: Material.secondaryTextColor
                                    visible: modelData !== undefined
                                        && modelData !== null
                                        && modelData.pubkey !== mynpub
                                        && (modelData.displayName !== "" || modelData.name !== "")
                                }

                                Text {
                                    text: "Myself"
                                    elide: Text.ElideRight
                                    Layout.fillWidth: true
                                    font: Constants.smallFont
                                    color: Material.secondaryTextColor
                                    visible: modelData !== undefined
                                        && modelData !== null
                                        && modelData.pubkey === mynpub
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

                                navigationPane.navigateTo(
                                    "PersonalFeed.ui.qml",
                                    {
                                        "npub": modelData.pubkey
                                    }
                                );
                            }
                        }
                    }
                }
            }
        }
    }
}
