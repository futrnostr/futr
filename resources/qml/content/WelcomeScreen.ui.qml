import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import Futr 1.0
import QtQuick.Studio.Components 1.0

Rectangle {
    id: welcomeScreen
    
    Column {
        width: parent.width

        Row {
            height: 100
            spacing: 10
            anchors.horizontalCenter: parent.horizontalCenter
            topPadding: 10

            Rectangle {
                width: parent.parent.width - 20
                height: 80
                color: "#f0f0f0" // Light grey background
                border.color: "#e0e0e0" // Slightly darker grey for border
                border.width: 2
                anchors.margins: 10

                radius: 10 // Rounded corners

                Column {
                    width: parent.width
                    height: parent.height
                    anchors.fill: parent
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.verticalCenter: parent.verticalCenter
                    topPadding: 10
                    spacing: 10

                    Text {
                        text: qsTr("Welcome to Futr")
                        font: Constants.largeFont
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width
                        horizontalAlignment: Text.AlignHCenter
                    }

                    Text {
                        text: qsTr("Your gateway to the future - global, decentralized, censorship-resistant")
                        font: Constants.font
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width
                        horizontalAlignment: Text.AlignHCenter
                    }
                }
            }
        }

        Row {
            height: 420
            spacing: 10
            anchors.horizontalCenter: parent.horizontalCenter

            Rectangle {
                width: parent.parent.width * 0.6 - 15
                height: 400
                color: "#f0f0f0" // Light grey background
                border.color: "#e0e0e0" // Slightly darker grey for border
                border.width: 2
                radius: 10 // Rounded corners

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10
                    width: parent.width

                    Item {
                        height: 70
                    }

                    Image {
                        source: "svg/account_box.svg"
                        height: 100
                        width: 100
                        Layout.alignment: Qt.AlignHCenter
                    }

                    Text {
                        text: qsTr("Import existing account")
                        font: Constants.largeFont
                        Layout.alignment: Qt.AlignHCenter
                        horizontalAlignment: Text.AlignHCenter
                    }

                    Flow {
                        Layout.fillWidth: true

                        RadioButton {
                            text: qsTr("Import secret key (nsec or hex format)")
                            id: radionsec
                            checked: true
                            Layout.alignment: Qt.AlignLeft
                        }

                        RadioButton {
                            text: qsTr("Import from seedphrase")
                            id: radioseedphrase
                            checked: false
                            Layout.alignment: Qt.AlignRight
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
                        Layout.margins: 10
                        
                        Connections {
                            target: importbutton
                            onClicked: function () {
                                if (radionsec.checked) {
                                    importSecretKey(secretkey.text)
                                } else if (radioseedphrase.checked) {
                                    importSeedPhrase(seedphrase.text,
                                                     password.text)
                                }
                            }
                        }
                    }
                }
            }

            Rectangle {
                width: parent.parent.width * 0.4 - 15
                height: 400
                color: "#f0f0f0" // Light grey background
                border.color: "#e0e0e0" // Slightly darker grey for border
                border.width: 2
                radius: 10 // Rounded corners

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10
                    width: parent.width

                    Item {
                        height: 70
                    }

                    Image {
                        source: "svg/add.svg"
                        height: 100
                        width: 100
                        Layout.alignment: Qt.AlignHCenter
                    }

                    Text {
                        text: qsTr("Create new account")
                        font: Constants.largeFont
                        Layout.alignment: Qt.AlignHCenter
                        horizontalAlignment: Text.AlignHCenter
                    }

                    Item {
                        Layout.fillHeight: true
                        Layout.alignment: Qt.AlignTop
                    }

                    Button {
                        id: generatebutton
                        text: qsTr("Generate new keys")
                        font: Constants.font
                        highlighted: true
                        Layout.alignment: Qt.AlignRight
                        Layout.margins: 10
                        onClicked: function () {
                            generateSeedphrase()
                        }
                    }
                }
            }
        }
    }
}

/*##^##
Designer {
    D{i:0;formeditorZoom:0.5}D{i:5}D{i:6}D{i:4}D{i:3}D{i:2}D{i:10}D{i:11}D{i:12}D{i:14}
D{i:15}D{i:13}D{i:16}D{i:17}D{i:18}D{i:19}D{i:20}D{i:9}D{i:8}D{i:23}D{i:24}D{i:25}
D{i:26}D{i:27}D{i:22}D{i:21}D{i:7}D{i:1}
}
##^##*/

