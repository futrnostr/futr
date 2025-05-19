import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Dialog {
    title: qsTr("Import Account")
    modal: true
    standardButtons: Dialog.Cancel
    anchors.centerIn: parent
    visible: false
    height: 610
    width: 740

    Rectangle {
        width: 700
        height: 470
        color: Material.dialogColor
        border.color: Material.dividerColor
        border.width: 2
        radius: Constants.radius_m

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 10
            spacing: 10
            width: parent.width

            Row {
                Layout.fillWidth: true
                Layout.alignment: Qt.AlignHCenter
                spacing: 12

                Image {
                    source: "qrc:/icons/account_box.svg"
                    height: 48
                    width: 48
                    Layout.alignment: Qt.AlignHCenter
                }

                Text {
                    text: qsTr("Import existing account")
                    font: Constants.largeFont
                    Layout.alignment: Qt.AlignHCenter
                    horizontalAlignment: Text.AlignHCenter
                }
            }

            Flow {
                Layout.fillWidth: true

                ButtonGroup {
                    id: importTypeGroup
                    onCheckedButtonChanged: {
                        ctxKeyMgmt.errorMsg = ""
                        secretkey.visible = checkedButton === radionsec
                        seedphrase.visible = checkedButton === radioseedphrase
                        password.visible = checkedButton === radioseedphrase
                    }
                }

                RadioButton {
                    text: qsTr("Import secret key (nsec or hex format)")
                    id: radionsec
                    checked: true
                    Layout.alignment: Qt.AlignLeft
                    ButtonGroup.group: importTypeGroup
                }

                RadioButton {
                    text: qsTr("Import from seedphrase")
                    id: radioseedphrase
                    checked: false
                    Layout.alignment: Qt.AlignRight
                    ButtonGroup.group: importTypeGroup
                }
            }

            TextField {
                id: secretkey
                placeholderText: qsTr("Enter nsec or hex key")
                Layout.alignment: Qt.AlignHCenter
                Layout.preferredWidth: parent.width - 20
                visible: importTypeGroup.checkedButton === radionsec
            }

            TextArea {
                id: seedphrase
                placeholderText: qsTr("Enter seedphrase (no commas)")
                Layout.alignment: Qt.AlignHCenter
                Layout.preferredWidth: parent.width - 20
                visible: importTypeGroup.checkedButton === radioseedphrase
            }

            TextField {
                id: password
                placeholderText: qsTr("Enter password for seedphrase (optional)")
                Layout.alignment: Qt.AlignHCenter
                Layout.preferredWidth: parent.width - 20
                visible: importTypeGroup.checkedButton === radioseedphrase
            }

            Text {
                id: errorMessage
                text: ctxKeyMgmt.errorMsg
                color: "red"
                visible: errorMessage != ""
                Layout.alignment: Qt.AlignLeft
                Layout.leftMargin: 10
            }

            Item {
                Layout.fillHeight: true
                Layout.alignment: Qt.AlignTop
            }

            Button {
                id: importButton
                text: qsTr("Import")
                font: Constants.font
                highlighted: true
                Layout.alignment: Qt.AlignRight
                Layout.rightMargin: 10
                Layout.preferredWidth: implicitWidth + 80

                onClicked: {
                    if (radionsec.checked) {
                        var res = ctxKeyMgmt.importSecretKey(secretkey.text);
                        if (res === true) {
                            importSuccessDialog.visible = true
                            importAccountDialog.visible = false
                        }
                    } else if (radioseedphrase.checked) {
                        var res = ctxKeyMgmt.importSeedphrase(seedphrase.text, password.text)
                        if (res === true) {
                            importSuccessDialog.visible = true
                            importAccountDialog.visible = false
                        }
                    }
                }
            }
        }
    }
    onRejected: {
        importAccountDialog.visible = false
    }
}
