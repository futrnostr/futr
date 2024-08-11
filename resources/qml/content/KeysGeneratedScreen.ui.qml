import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import Futr 1.0
import QtQuick.Studio.Components 1.0

Rectangle {
    id: keysGeneratedScreen
    width: parent.width * 0.8
    height: parent.height * 0.6
    color: "#FFFFFF"
    radius: 16
    Material.elevation: 4
    //elevation: 4  // Material Design elevation for shadow effect
    anchors.centerIn: parent
    visible: true  // Set to false initially and toggle visibility when needed

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 24
        spacing: 16

        Text {
            id: titleText
            text: "Important: Backup Your Keys"
            font.pixelSize: 24
            color: Material.primaryColor
            font.bold: true
            Layout.alignment: Qt.AlignHCenter
        }

        Text {
            id: infoText
            text: "Please write down either your seed phrase or your private key in 'nsec' format. " +
                  "We recommend using the seed phrase for easier recovery. " +
                  "Ensure you store this information securely."
            wrapMode: Text.WordWrap
            font.pixelSize: 16
            color: "#555555"
            Layout.alignment: Qt.AlignHCenter
        }

        Rectangle {
            id: seedPhraseContainer
            width: parent.width
            height: 100
            color: "#F5F5F5"
            radius: 8
            border.color: Material.primaryColor
            Layout.alignment: Qt.AlignHCenter

            Text {
                id: seedPhraseText
                text: "your seed phrase here"
                anchors.centerIn: parent
                wrapMode: Text.WordWrap
                font.pixelSize: 16
                color: "#333333"
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
            }
        }

        Rectangle {
            id: privateKeyContainer
            width: parent.width
            height: 60
            color: "#F5F5F5"
            radius: 8
            border.color: Material.primaryColor
            Layout.alignment: Qt.AlignHCenter

            Text {
                id: privateKeyText
                text: "nsec_private_key_here"
                anchors.centerIn: parent
                wrapMode: Text.WordWrap
                font.pixelSize: 16
                color: "#333333"
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
            }
        }

        Button {
            id: continueButton
            text: "I've securely stored my keys"
            Layout.alignment: Qt.AlignHCenter
            Material.primary: Material.DeepOrange

            onClicked: {
                keysGeneratedScreen.visible = false
                // Add logic to proceed in the app
            }
        }
    }
}
