import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0

Rectangle {
    id: accountScreen

    ColumnLayout {
        anchors.fill: parent
        spacing: 20

        Rectangle {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 10
            height: 80
            color: "#f0f0f0"
            border.color: "#e0e0e0"
            border.width: 2
            radius: 5

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
                    radius: 5
                    width: parent ? parent.width : 200

                    RowLayout {
                        anchors.fill: parent
                        spacing: 5

                        Image {
                            source: modelData.picture
                            width: 60
                            height: 60
                            Layout.preferredWidth: 60
                            Layout.preferredHeight: 60
                            smooth: true
                            fillMode: Image.PreserveAspectCrop
                            Layout.leftMargin: 10

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: parent.parent.parent.mouseHover = true
                                onExited: parent.parent.parent.mouseHover = false

                                onClicked: {
                                    ctxAccounts.selectAccount(modelData.npub)
                                }
                            }
                        }

                        ColumnLayout {
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            Layout.leftMargin: 10

                            Text {
                                font: Constants.largeFont
                                text: modelData.displayName
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                clip: true
                                Layout.alignment: Qt.AlignVCenter
                                Layout.topMargin: 10
                                Layout.fillWidth: true
                            }

                            Text {
                                text: modelData.npub
                                elide: Text.ElideRight
                                wrapMode: Text.NoWrap
                                clip: true
                                Layout.alignment: Qt.AlignBottom
                                Layout.fillWidth: true
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
                                    ctxAccounts.selectAccount(modelData.npub)
                                }
                            }
                        }

                        RoundButton {
                            width: 10
                            height: 10

                            icon.source: "qrc:/icons/delete.svg"
                            icon.width: 12
                            icon.height: 12

                            Layout.rightMargin: 10

                            onClicked: {
                                confirmRemoveAccount.accountToRemove = modelData.npub
                            }
                        }
                    }
                }
            }
        }

        Rectangle {
            Layout.fillHeight: true
            color: "transparent"
        }

        Text {
            text: qsTr("Or click here to import another account, or click here to create another account.")
            font: Constants.font
            Layout.alignment: Qt.AlignHCenter
            Layout.bottomMargin: 10
        }
    }

    Dialog {
        property string accountToRemove: ""

        id: confirmRemoveAccount
        title: "Are you sure you want to remove this account?"
        standardButtons: Dialog.Ok | Dialog.Cancel
        anchors.centerIn: parent

        visible: accountToRemove !== ""

        onAccepted: {
            ctxAccounts.removeAccount(accountToRemove)
        }

        onRejected: {
            accountToRemove = ""
        }
    }
}
