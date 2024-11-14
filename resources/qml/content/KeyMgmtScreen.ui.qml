import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Dialogs 1.0
import Futr 1.0

Rectangle {
    id: accountScreen
    color: Material.backgroundColor

    Timer {
        id: delayLogin
        interval: 250
        repeat: false
        property string npub: ""
        onTriggered: {
            loginStatusChanged.connect(loginCallback)
            login(npub)
        }
    }

    function loginCallback(success, message) {
        if (connectingModal) {
            connectingModal.close()
        }

        if (! success) {
            loginErrorDialog.errorMessage = message
            loginErrorDialog.open()
        }
    }

    Rectangle {
        width: 900
        height: parent.height
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.margins: 10

        Rectangle {
            width: 1
            height: parent.height
            color: Material.dividerColor
            anchors.left: parent.left
        }

        Rectangle {
            width: 1
            height: parent.height
            color: Material.dividerColor
            anchors.right: parent.right
        }

        Rectangle {
            anchors.fill: parent
            anchors.leftMargin: 1
            anchors.rightMargin: 1
            color: Material.backgroundColor

            ColumnLayout {
                anchors.fill: parent
                anchors.margins: 10
                spacing: 20

                Rectangle {
                    Layout.fillWidth: true
                    Layout.preferredHeight: welcomeColumn.implicitHeight + 20
                    Layout.alignment: Qt.AlignHCenter
                    Layout.topMargin: 10
                    Layout.leftMargin: 10
                    Layout.rightMargin: 10
                    color: Material.backgroundColor
                    border.color: Material.dividerColor
                    border.width: 1
                    radius: 5

                    ColumnLayout {
                        id: welcomeColumn
                        anchors.fill: parent
                        anchors.margins: 10
                        spacing: 5

                        Label {
                            Layout.fillWidth: true
                            text: qsTr("Welcome to Futr")
                            font: Constants.largeFont
                            wrapMode: Text.WordWrap
                            horizontalAlignment: Text.AlignHCenter
                        }

                        Label {
                            Layout.fillWidth: true
                            text: qsTr("Your gateway to the future - global, decentralized, censorship-resistant")
                            font: Constants.font
                            wrapMode: Text.WordWrap
                            horizontalAlignment: Text.AlignHCenter
                        }
                    }
                }

                Text {
                    text: qsTr("Select an account from the list below:")
                    font: Constants.font
                    Layout.alignment: Qt.AlignLeft
                    color: Material.primaryTextColor
                }

                AccountList {}

                Text {
                    text: qsTr("Or:")
                    font: Constants.font
                    Layout.alignment: Qt.AlignLeft
                    color: Material.primaryTextColor
                }

                Row {
                    height: 320
                    spacing: 30
                    Layout.fillHeight: true
                    Layout.alignment: Qt.AlignHCenter

                    Button {
                        text: qsTr("Import Account")
                        font: Constants.font
                        highlighted: true
                        Layout.alignment: Qt.AlignLeft
                        width: implicitWidth + 80

                        onClicked: {
                            importAccountDialog.visible = true
                            ctxKeyMgmt.errorMsg = ""
                        }
                    }

                    Button {
                        id: generatebutton
                        text: qsTr("Create new account")
                        font: Constants.font
                        highlighted: true
                        Layout.alignment: Qt.AlignRight
                        width: implicitWidth + 80

                        onClicked: function () {
                            if (ctxKeyMgmt.createAccount()) {
                                showKeysDialog.visible = true
                            }
                        }
                    }
                }
            }
        }
    }

    ImportAccountDialog {
        id: importAccountDialog
    }

    ShowKeysDialog {
        id: showKeysDialog
    }

    ConnectingModal {
        id: connectingModal
    }

    LoginErrorDialog {
        id: loginErrorDialog
    }

    RemoveAccountDialog {
        id: confirmRemoveAccount
    }

    ImportSuccessDialog {
        id: importSuccessDialog
    }
}
