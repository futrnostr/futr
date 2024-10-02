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
    minimumWidth: 920
    minimumHeight: 525
    visible: true
    title: "Futr"
    font: Constants.font

    property bool isDarkTheme: false
    property color accentColor: "#9C27B0"

    Material.theme: isDarkTheme ? Material.Dark : Material.Light
    Material.accent: accentColor
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

            KeyMgmtScreen {
                anchors.margins: 10
                anchors.fill: parent
                visible: currentScreen == "KeyMgmt"
            }

            Loader {
                id: myHomeScreenLoader
                active: currentScreen == "Home"
                anchors.fill: parent
                sourceComponent: HomeScreen {
                    anchors.margins: 10
                    anchors.fill: parent
                }
            }
        }
    }

    Button {
            id: themeToggle
            anchors.top: parent.top
            anchors.right: parent.right
            anchors.margins: 10
            icon.source: isDarkTheme ? "qrc:/icons/light_mode.png" : "qrc:/icons/dark_mode.png"
            icon.color: Material.foreground
            onClicked: isDarkTheme = !isDarkTheme
            flat: true

            ToolTip.visible: hovered
            ToolTip.text: "Switch to " + (isDarkTheme ? "Light" : "Dark") + " Mode"
        }
}
