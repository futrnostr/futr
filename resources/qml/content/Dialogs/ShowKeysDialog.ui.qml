import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Dialog {
    visible: false
    modal: true
    standardButtons: Dialog.Ok
    anchors.centerIn: parent
    width: 940
    height: 675

    Rectangle {
        width: 900
        height: 600
        color: Material.dialogColor
        border.color: Material.dividerColor
        border.width: 2
        radius: 10

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 10
            spacing: 5
            width: parent.width

            Row {
                Layout.fillWidth: true
                Layout.margins: 10
                spacing: 20

                Image {
                    source: "qrc:/icons/warning.svg"
                    width: 25
                    height: 25
                }

                Text {
                    text: qsTr("Important: Store your keys securely!")
                    font.pixelSize: 24
                    font.bold: true
                    color: Material.foreground
                    wrapMode: Text.WordWrap
                    Layout.alignment: Qt.AlignLeft
                }
            }

            Text {
                id: seedPhraseLabel
                text: qsTr("Seed Phrase:")
                font: Constants.font
                color: Material.foreground
                horizontalAlignment: Text.AlignLeft
                visible: ctxKeyMgmt.seedphrase !== ""
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10
                visible: ctxKeyMgmt.seedphrase !== ""

                TextArea {
                    id: seedPhraseText
                    text: ctxKeyMgmt.seedphrase
                    Layout.fillWidth: true
                    font: Constants.font
                    readOnly: true
                    wrapMode: Text.Wrap
                    selectByKeyboard: true
                    selectByMouse: true
                }

                Button {
                    icon.source: "qrc:/icons/content_copy.svg"
                    Layout.preferredWidth: 34
                    Layout.preferredHeight: 34

                    ToolTip.visible: hovered
                    ToolTip.text: qsTr("Copy to clipboard")

                    onClicked: {
                        clipboard.copyText(seedPhraseText.text)
                    }
                }

            }

            Text {
                id: privateKeyLabel
                text: qsTr("Private Key (nsec format):")
                color: Material.foreground
                font: Constants.font
                horizontalAlignment: Text.AlignLeft
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                TextArea {
                    id: privateKeyText
                    text: ctxKeyMgmt.nsec
                    Layout.fillWidth: true
                    readOnly: true
                    wrapMode: Text.Wrap
                    selectByKeyboard: true
                    selectByMouse: true
                }

                Button {
                    icon.source: "qrc:/icons/content_copy.svg"
                    Layout.preferredWidth: 34
                    Layout.preferredHeight: 34

                    ToolTip.visible: hovered
                    ToolTip.text: qsTr("Copy to clipboard")

                    onClicked: {
                        clipboard.copyText(privateKeyText.text)
                    }
                }
            }

            Text {
                id: publicKeyLabel
                text: qsTr("Public Key (npub format):")
                color: Material.foreground
                font: Constants.font
                horizontalAlignment: Text.AlignLeft
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                TextArea {
                    id: publicKeyText
                    text: ctxKeyMgmt.npub
                    Layout.fillWidth: true
                    readOnly: true
                    wrapMode: Text.Wrap
                    selectByKeyboard: true
                    selectByMouse: true
                }

                Button {
                    icon.source: "qrc:/icons/content_copy.svg"
                    Layout.preferredWidth: 34
                    Layout.preferredHeight: 34

                    ToolTip.visible: hovered
                    ToolTip.delay: 500
                    ToolTip.timeout: 5000
                    ToolTip.text: qsTr("Copy to clipboard")

                    onClicked: {
                        clipboard.copyText(publicKeyText.text)
                    }
                }
            }

            Text {
                id: infoText
                text: qsTr("Please write down either the seed phrase or the private key and store them in a secure place. The seed phrase is easier to remember, but the private key is more secure. Choose based on your preference.")
                font: Constants.font
                color: Material.secondaryTextColor
                horizontalAlignment: Text.AlignCenter
                wrapMode: Text.WordWrap
                Layout.fillWidth: true
                Layout.preferredWidth: parent.width * 0.8
            }

            Item {
                Layout.fillHeight: true
            }
        }
    }

    onAccepted: {
        showKeysDialog.visible = false
        currentScreen = "Home"
    }
}
