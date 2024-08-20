import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    id: accountScreen

    ColumnLayout {
        spacing: 20

        Rectangle {
            Layout.preferredWidth: parent.width - 40
            Layout.preferredHeight: 80
            Layout.alignment: Qt.AlignCenter
            color: "#f0f0f0"
            border.color: "#e0e0e0"
            border.width: 2
            radius: 10

            ColumnLayout {
                Layout.margins: 10
                spacing: 5

                Text {
                    text: qsTr("Welcome to Futr")
                    font: Constants.largeFont
                    Layout.alignment: Qt.AlignHCenter
                }

                Text {
                    text: qsTr("Your gateway to the future - global, decentralized, censorship-resistant")
                    font: Constants.font
                    Layout.alignment: Qt.AlignHCenter
                }
            }
        }

        Text {
            text: qsTr("Select an account from the list below:")
            font: Constants.font
            Layout.alignment: Qt.AlignLeft
        }

        // Accounts list
        ScrollView {
            Layout.fillWidth: true  // Take up the full available width
            Layout.preferredHeight: 400  // Set a preferred height for the scroll area
            Layout.alignment: Qt.AlignHCenter  // Center the scroll area horizontally

            ListView {
                id: accountsView
                width: parent.width  // ListView width is constrained by ScrollView
                focus: true
                clip: true
                model: AutoListModel {
                    source: ctxAccounts.accounts
                    mode: AutoListModel.ByKey
                }

                delegate: Rectangle {
                    Layout.fillWidth: true
                    height: 80
                    property bool mouseHover: false
                    color: mouseHover ? "lightsteelblue" : "lightgray"
                    border.color: "gray"
                    radius: 10

                    RowLayout {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        spacing: 10
                        anchors.margins: 10  // Internal spacing for padding

                        // Account's picture
                        Image {
                            source: modelData.picture
                            width: 60
                            height: 60
                            Layout.alignment: Qt.AlignVCenter
                            fillMode: Image.PreserveAspectCrop
                        }

                        // Display npub and displayName
                        ColumnLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            spacing: 5

                            Text {
                                text: modelData.displayName ? modelData.displayName + " (" + modelData.npub + ")" : modelData.npub
                                elide: Text.ElideRight  // Elide text to prevent overflow
                                wrapMode: Text.NoWrap
                            }
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        onEntered: parent.mouseHover = true
                        onExited: parent.mouseHover = false
                        // onClicked: ctxAccounts.login(modelData.npub)
                    }
                }
            }
        }

        // Instruction text after the accounts list
        Text {
            text: qsTr("Or click here to import another account, or click here to create another account.")
            font: Constants.font
            Layout.alignment: Qt.AlignHCenter
        }
    }
}
