import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    id: accountScreen

    ColumnLayout {
        anchors.fill: parent
        Layout.margins: 20
        spacing: 20

        Rectangle {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 10
            height: 80
            color: "#f0f0f0"
            border.color: "#e0e0e0"
            border.width: 2
            radius: 10

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 10
                spacing: 5

                Text {
                    text: qsTr("Welcome to Futr")
                    font: Constants.largeFont
                    Layout.alignment: Qt.AlignCenter
                }

                Text {
                    text: qsTr("Your gateway to the future - global, decentralized, censorship-resistant")
                    font: Constants.font
                    Layout.alignment: Qt.AlignCenter
                }
            }
        }

        Text {
            text: qsTr("Select an account from the list below:")
            font: Constants.font
            Layout.alignment: Qt.AlignLeft
            Layout.leftMargin: 10
        }

        // Accounts list
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
                    source: ctxAccounts.accounts
                    mode: AutoListModel.ByKey
                }

                delegate: Rectangle {
                    property bool mouseHover: false

                    height: 80
                    color: mouseHover ? "lightsteelblue" : "#f0f0f0"
                    border.color: "gray"
                    radius: 10
                    Layout.alignment: Qt.AlignHCenter
                    width: parent.width

                    RowLayout {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        spacing: 5

                        Image {
                            source: modelData.picture
                            width: 60
                            height: 60
                            Layout.preferredWidth: 60
                            Layout.preferredHeight: 60
                            smooth: true
                            fillMode: Image.PreserveAspectCrop
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            spacing: 20

                            Text {
                                font: Constants.largeFont
                                text: modelData.displayName
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                Layout.fillWidth: true
                                clip: true
                                topPadding: modelData.displayName ? 10 : 0
                            }

                            Text {
                                text: modelData.npub
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                Layout.fillWidth: true
                                clip: true
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        onEntered: parent.mouseHover = true
                        onExited: parent.mouseHover = false
                    }
                }

            }
        }

        Rectangle {
            Layout.fillHeight: true
            color: "transparent"
        }

        // Instruction text after the accounts list
        Text {
            text: qsTr("Or click here to import another account, or click here to create another account.")
            font: Constants.font
            Layout.alignment: Qt.AlignHCenter
        }
    }
}
