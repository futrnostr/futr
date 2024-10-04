import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0
import QtQuick.Controls.Material 2.15

Rectangle {
    id: accountScreen
    color: Material.backgroundColor

    ColumnLayout {
        anchors.fill: parent
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
                    color: mouseHover ? Material.accentColor : Material.backgroundColor
                    border.color: Material.dividerColor
                    radius: 5
                    width: ListView.view.width

                    RowLayout {
                        anchors.fill: parent
                        anchors.margins: 10
                        spacing: 10

                        Image {
                            source: Util.getProfilePicture(modelData.picture, modelData.npub)
                            Layout.preferredWidth: 40
                            Layout.preferredHeight: 40
                            Layout.alignment: Qt.AlignVCenter
                            smooth: true
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
                                    text: modelData.displayName
                                    elide: Text.ElideRight
                                    width: parent.width
                                    color: Material.primaryTextColor
                                }

                                Text {
                                    anchors.left: parent.left
                                    anchors.top: parent.verticalCenter
                                    text: modelData.npub
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
                                    connectingModal.visible = true
                                    delayLogin.npub = modelData.npub
                                    delayLogin.start()
                                    connectingModal.visible = false
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
                text: qsTr("Generate new keys")
                font: Constants.font
                highlighted: true
                Layout.alignment: Qt.AlignRight
                width: implicitWidth + 80

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
        height: 610
        width: 740

        Rectangle {
            width: 700
            height: 470
            color: Material.dialogColor
            border.color: Material.dividerColor
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

                    Connections {
                        target: importButton
                        onClicked: function () {
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
                        text: "Important: Store your keys securely!"
                        font.pixelSize: 24
                        font.bold: true
                        wrapMode: Text.WordWrap
                        Layout.alignment: Qt.AlignLeft
                    }
                }

                Text {
                    id: seedPhraseLabel
                    text: "Seed Phrase:"
                    font: Constants.font
                    color: Material.foreground
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
                    text: "Private Key (nsec format):"
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
                    text: "Public Key (npub format):"
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
                    text: "Please write down either the seed phrase or the private key and store them in a secure place. The seed phrase is easier to remember, but the private key is more secure. Choose based on your preference."
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
            keysGeneratedDialog.visible = false
        }
    }

    Dialog {
        id: connectingModal
        standardButtons: Dialog.NoButton
        modal: true
        anchors.centerIn: parent
        width: 300
        height: 200

        ColumnLayout {
            anchors.fill: parent
            spacing: 20

            BusyIndicator {
                running: true
                Layout.alignment: Qt.AlignHCenter
            }

            Text {
                text: qsTr("Connecting...")
                color: Material.foreground
                font.pixelSize: 16
                font.bold: true
                Layout.alignment: Qt.AlignHCenter
            }
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

    Dialog {
        id: importSuccessDialog
        title: "Nostr account succesfully imported"
        standardButtons: Dialog.Ok
        modal: true
        anchors.centerIn: parent
    }

    Timer {
        id: delayLogin
        interval: 5
        repeat: false
        property string npub: ""
        onTriggered: {
            login(npub)
            connectingModal.close()
        }
    }
}
