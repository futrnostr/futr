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

    Loader {
        id: screenLoader
        anchors.fill: parent
        source: {
            if (currentScreen === "Home") {
                return "HomeScreen.ui.qml";
            } else if (currentScreen === "KeyMgmt") {
                return "KeyMgmtScreen.ui.qml";
            } else {
                return "";
            }
        }
        active: currentScreen === "Home" || currentScreen === "KeyMgmt"
    }

    Button {
        id: themeToggle
        anchors.top: parent.top
        anchors.right: parent.right
        anchors.margins: 10
        icon.source: isDarkTheme ? "qrc:/icons/light_mode.svg" : "qrc:/icons/dark_mode.svg"
        icon.color: Material.foreground
        onClicked: isDarkTheme = !isDarkTheme
        flat: true

        ToolTip.visible: hovered
        ToolTip.text: qsTr("Switch to " + (isDarkTheme ? "Light" : "Dark") + " Mode")
    }
}
