import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

Rectangle {
    id: keysGeneratedScreen
    width: parent.width * 0.8
    height: parent.height * 0.8
    anchors.centerIn: parent
    color: "#FFFFFF"
    border.color: "#6200EA" // Material Design primary color
    border.width: 4
    radius: 12
    Material.elevation: 8 // Adds shadow to create depth

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 20
        spacing: 20  
            
        Text {
            id: importantText
            text: "Important: Store your keys securely!"
            font.pixelSize: 24
            color: "#000000"
            font.bold: true
            wrapMode: Text.WordWrap
            Layout.alignment: Qt.AlignLeft
        }

        Image {
            source: "svg/warning.svg"
            width: 25
            height: 25
        }

        Text {
            id: seedPhraseLabel
            text: "Seed Phrase:"
            font.pixelSize: 18
            color: "#000000"
            horizontalAlignment: Text.AlignLeft
        }

        TextArea {
            id: seedPhraseText
            text: ctxWelcomeScreen.seedphrase
            readOnly: true
            wrapMode: Text.Wrap
            font.pixelSize: 16
            color: "#000000"
            padding: 10 // Add padding for margin effect
            background: Rectangle {
                color: "#E8EAF6" // Light background color
                radius: 8
            }
        }

        Text {
            id: privateKeyLabel
            text: "Private Key (nsec format):"
            font.pixelSize: 18
            color: "#000000"
            horizontalAlignment: Text.AlignLeft
        }

        TextArea {
            id: privateKeyText
            text: ctxWelcomeScreen.nsec
            readOnly: true
            wrapMode: Text.Wrap
            font.pixelSize: 16
            color: "#000000"
            padding: 10 // Add padding for margin effect
            background: Rectangle {
                color: "#E8EAF6" // Light background color
                radius: 8
            }
        }

        Text {
            id: publicKeyLabel
            text: "Public Key (npub format):"
            font.pixelSize: 18
            color: "#000000"
            horizontalAlignment: Text.AlignLeft
        }

        TextArea {
            id: publicKeyText
            text: ctxWelcomeScreen.npub
            readOnly: true
            wrapMode: Text.Wrap
            font.pixelSize: 16
            color: "#000000"
            padding: 10 // Add padding for margin effect
            background: Rectangle {
                color: "#E8EAF6" // Light background color
                radius: 8
            }
        }

        Text {
            id: infoText
            text: "Please write down either the seed phrase or the private key and store them in a secure place. The seed phrase is easier to remember, but the private key is more secure. Choose based on your preference."
            font.pixelSize: 14
            color: "#616161" // Material Design text color
            horizontalAlignment: Text.AlignCenter
            wrapMode: Text.WordWrap
            Layout.fillWidth: true // Ensures it takes the available width
            Layout.preferredWidth: parent.width * 0.8 // Ensures wrapping
        }

        Button {
            text: "I've stored my keys securely, continue"
            Layout.alignment: Qt.AlignRight
            anchors.margins: 10
            highlighted: true
            onClicked: {
                ctxWelcomeScreen.seedphrase = ""
                currentScreen = "HomeScreen"
            }
        }
    }
}
