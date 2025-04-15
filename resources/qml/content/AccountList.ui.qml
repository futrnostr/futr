import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Dialogs 1.0
import Futr 1.0
import HsQML.Model 1.0

ScrollView {
    Layout.fillWidth: true
    Layout.fillHeight: true
    Layout.alignment: Qt.AlignHCenter

    ListView {
        id: accountsView
        focus: true
        clip: true
        Layout.fillWidth: true
        Layout.alignment: Qt.AlignHCenter
        spacing: 5

        model: AutoListModel {
            source: ctxKeyMgmt.accounts
            mode: AutoListModel.ByKey
            equalityTest: function (oldItem, newItem) {
                return oldItem.displayName === newItem.displayName
                    && oldItem.nsec === newItem.nsec
                    && oldItem.npub === newItem.npub
                    && oldItem.picture === newItem.picture
            }
        }

        delegate: Rectangle {
            property bool mouseHover: false

            height: 60
            color: mouseHover ? Material.accentColor : Material.backgroundColor
            border.color: Material.dividerColor
            radius: Constants.radius_m
            width: ListView.view.width

            RowLayout {
                anchors.fill: parent
                anchors.margins: 10
                spacing: 10

                Image {
                    source: Util.getProfilePicture(modelData.picture || "", modelData.npub  || "")
                    Layout.preferredWidth: 40
                    Layout.preferredHeight: 40
                    Layout.alignment: Qt.AlignVCenter
                    cache: false
                    fillMode: Image.PreserveAspectCrop

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        onEntered: parent.parent.parent.mouseHover = true
                        onExited: parent.parent.parent.mouseHover = false

                        onClicked: {
                            connectingModal.open()
                            delayLogin.npub = modelData.npub
                            delayLogin.start()
                        }
                    }
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    spacing: 2

                    Item {
                        Layout.fillHeight: true
                        Layout.fillWidth: true

                        Text {
                            anchors.left: parent.left
                            anchors.bottom: parent.verticalCenter
                            font: Constants.font
                            text: modelData.displayName || ""
                            elide: Text.ElideRight
                            width: parent.width
                            color: Material.primaryTextColor
                        }

                        Text {
                            anchors.left: parent.left
                            anchors.top: parent.verticalCenter
                            text: modelData.npub || ""
                            font.pixelSize: Constants.font.pixelSize * 0.8
                            elide: Text.ElideRight
                            width: parent.width
                            color: Material.secondaryTextColor
                        }
                    }

                    MouseArea {
                        x: 0
                        y: 0
                        width: parent.width
                        height: parent.height
                        hoverEnabled: true
                        onEntered: parent.parent.parent.mouseHover = true
                        onExited: parent.parent.parent.mouseHover = false

                        onClicked: {
                            connectingModal.open()
                            delayLogin.npub = modelData.npub
                            delayLogin.start()
                        }
                    }
                }

                RoundButton {
                    Layout.preferredWidth: 40
                    Layout.preferredHeight: 40
                    Layout.alignment: Qt.AlignVCenter
                    Layout.rightMargin: 20

                    icon.source: "qrc:/icons/delete.svg"
                    icon.width: 34
                    icon.height: 34

                    onClicked: {
                        confirmRemoveAccount.accountToRemove = modelData.npub
                    }
                }
            }
        }
    }
}
