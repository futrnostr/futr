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
                    source: ctxKeyMgmt.accounts
                    mode: AutoListModel.ByKey
                }

                delegate: Rectangle {
                    property bool mouseHover: false

                    height: 60
                    color: mouseHover ? "lightsteelblue" : "#f0f0f0"
                    border.color: "gray"
                    radius: 5
                    width: parent ? parent.width : 200

                    RowLayout {
                        anchors.fill: parent
                        spacing: 5

                        Image {
                            source: modelData.picture
                            cache: true
                            width: 50
                            height: 50
                            Layout.preferredWidth: 50
                            Layout.preferredHeight: 50
                            smooth: true
                            fillMode: Image.PreserveAspectCrop
                            Layout.leftMargin: 10

                            MouseArea {
                                anchors.fill: parent
                                hoverEnabled: true
                                onEntered: parent.parent.parent.mouseHover = true
                                onExited: parent.parent.parent.mouseHover = false

                                onClicked: {
                                    login(modelData.npub)
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
                                    login(modelData.npub)
                                }
                            }
                        }

                        RoundButton {
                            width: 8
                            height: 8

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

        Text {
            text: qsTr("Or:")
            font: Constants.font
            Layout.alignment: Qt.AlignLeft
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

                onClicked: {
                    importAccountDialog.visible = true
                }
            }

            Button {
                id: generatebutton
                text: qsTr("Generate new keys")
                font: Constants.font
                highlighted: true
                Layout.alignment: Qt.AlignRight
                Layout.rightMargin: 30

                onClicked: function () {
                    ctxKeyMgmt.generateSeedphrase()
                    keysGeneratedDialog.visible = true
                }
            }
        }
    }

    Dialog {
        id: importAccountDialog
        title: "Import Account"
        modal: true
        standardButtons: Dialog.Cancel
        anchors.centerIn: parent
        visible: false
        height: 440
        width: 540

        Rectangle {
            width: 500
            height: 300
            color: "#f0f0f0"
            border.color: "#e0e0e0"
            border.width: 2
            radius: 10

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
                        height: 24
                        width: 24
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

                    RadioButton {
                        text: qsTr("Import secret key (nsec or hex format)")
                        id: radionsec
                        checked: true
                        Layout.alignment: Qt.AlignLeft
                        onClicked: ctxKeyMgmt.errorMsg = ""
                    }

                    RadioButton {
                        text: qsTr("Import from seedphrase")
                        id: radioseedphrase
                        checked: false
                        Layout.alignment: Qt.AlignRight
                        onClicked: ctxKeyMgmt.errorMsg = ""
                    }
                }

                TextField {
                    id: secretkey
                    placeholderText: qsTr("Enter nsec or hex key")
                    Layout.alignment: Qt.AlignHCenter
                    Layout.preferredWidth: parent.width - 20
                    visible: radionsec.checked
                }

                TextArea {
                    id: seedphrase
                    placeholderText: qsTr("Enter seedphrase")
                    Layout.alignment: Qt.AlignHCenter
                    Layout.preferredWidth: parent.width - 20
                    visible: radioseedphrase.checked
                }

                TextField {
                    id: password
                    placeholderText: qsTr("Enter password for seedphrase (optional)")
                    Layout.alignment: Qt.AlignHCenter
                    Layout.preferredWidth: parent.width - 20
                    visible: radioseedphrase.checked
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
                    id: importbutton
                    text: qsTr("Import")
                    font: Constants.font
                    highlighted: true
                    Layout.alignment: Qt.AlignRight
                    Layout.rightMargin: 10

                    Connections {
                        target: importbutton
                        onClicked: function () {
                            if (radionsec.checked) {
                                var res = ctxKeyMgmt.importSecretKey(secretkey.text);
                                if (res !== null) {
                                    login(res)
                                    importAccountDialog.visible = false
                                }
                            } else if (radioseedphrase.checked) {
                                var res = ctxKeyMgmt.importSeedphrase(seedphrase.text, password.text)
                                if (res !== null) {
                                    login(res)
                                    importAccountDialog.visible = false
                                }
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

    Dialog {
        id: keysGeneratedDialog
        visible: false
        modal: true
        standardButtons: Dialog.Ok
        anchors.centerIn: parent
        width: 740
        height: 425

        Rectangle {
            width: 700
            height: 350
            color: "#f0f0f0"
            border.color: "#e0e0e0"
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
                        text: "Important: Store your keys securely!"
                        font.pixelSize: 24
                        color: "#000000"
                        font.bold: true
                        wrapMode: Text.WordWrap
                        Layout.alignment: Qt.AlignLeft
                    }
                }

                Text {
                    id: seedPhraseLabel
                    text: "Seed Phrase:"
                    font: Constants.font
                    color: "#000000"
                    horizontalAlignment: Text.AlignLeft
                }

                RowLayout {
                    Layout.fillWidth: true
                    spacing: 10

                    TextArea {
                        id: seedPhraseText
                        text: ctxKeyMgmt.seedphrase
                        Layout.fillWidth: true
                        font: Constants.font
                        readOnly: true
                        wrapMode: Text.Wrap
                        color: "#000000"
                        background: Rectangle {
                            color: "#E8EAF6"
                            radius: 5
                        }
                    }

                    Button {
                        icon.source: "qrc:/icons/content_copy.svg"
                        Layout.preferredWidth: 50
                        Layout.preferredHeight: 50

                        ToolTip.visible: hovered
                        ToolTip.text: qsTr("Copy to clipboard")
                        
                        onClicked: {
                            clipboard.copyText(seedPhraseText.text)
                        }
                    }

                }

                Text {
                    id: privateKeyLabel
                    text: "Private Key (nsec format):"
                    color: "#000000"
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
                        color: "#000000"
                        font: Constants.font
                        background: Rectangle {
                            color: "#E8EAF6"
                            radius: 5
                        }
                    }

                    Button {
                        icon.source: "qrc:/icons/content_copy.svg"
                        Layout.preferredWidth: 50
                        Layout.preferredHeight: 50

                        ToolTip.visible: hovered
                        ToolTip.text: qsTr("Copy to clipboard")
                        
                        onClicked: {
                            clipboard.copyText(privateKeyText.text)
                        }
                    }
                }

                Text {
                    id: publicKeyLabel
                    text: "Public Key (npub format):"
                    color: "#000000"
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
                        color: "#000000"
                        font: Constants.font
                        background: Rectangle {
                            color: "#E8EAF6"
                            radius: 5
                        }
                    }

                    Button {
                        icon.source: "qrc:/icons/content_copy.svg"
                        Layout.preferredWidth: 50
                        Layout.preferredHeight: 50

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
                    text: "Please write down either the seed phrase or the private key and store them in a secure place. The seed phrase is easier to remember, but the private key is more secure. Choose based on your preference."
                    font: Constants.font
                    color: "#616161"
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
            keysGeneratedDialog.visible = false
            currentScreen = "Home"
        }
    }

    Dialog {
        property string accountToRemove: ""

        id: confirmRemoveAccount
        title: "Are you sure you want to remove this account?"
        standardButtons: Dialog.Ok | Dialog.Cancel
        modal: true
        anchors.centerIn: parent

        visible: accountToRemove !== ""

        onAccepted: {
            ctxKeyMgmt.removeAccount(accountToRemove)
        }

        onRejected: {
            accountToRemove = ""
        }
    }
}
