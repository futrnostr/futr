import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import HsQML.Clipboard 1.0
import Futr 1.0

ApplicationWindow {
    id: root
    width: Constants.width
    height: Constants.height
    minimumWidth: 720
    minimumHeight: 525
    visible: true
    title: "Futr"
    font: Constants.font

    Material.theme: Material.Light
    Material.accent: Material.Teal
    Material.primary: Material.BlueGrey

    ClipboardHelper {
        id: clipboard
    }

    Rectangle {
        width: 900
        height: parent.height
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.margins: 10

        Rectangle {
            width: 1
            height: parent.height
            color: "#000000"
            anchors.left: parent.left
        }

        Rectangle {
            width: 1
            height: parent.height
            color: "#000000"
            anchors.right: parent.right
        }

        Rectangle {
            anchors.fill: parent
            anchors.leftMargin: 1
            anchors.rightMargin: 1

            KeyMgmtScreen {
                anchors.margins: 10
                anchors.fill: parent

                visible: currentScreen == "KeyMgmt"
            }

            WelcomeScreen {
                anchors.margins: 10
                anchors.fill: parent
                visible: currentScreen == "Welcome"
            }

            HomeScreen {
                anchors.margins: 10
                anchors.fill: parent
                visible: currentScreen == "Home"
            }
        }
    }   
}
