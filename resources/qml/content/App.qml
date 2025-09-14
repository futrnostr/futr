import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import HsQML.Clipboard 1.0
import Futr 1.0

ApplicationWindow {
    id: appWindow
    width: Constants.width
    height: Constants.height
    minimumWidth: 920
    minimumHeight: 525
    visible: true
    title: qsTr("Futr")
    font: Constants.font
    color: Material.backgroundColor

    property bool isDarkTheme: true
    property color accentColor: "#9C27B0"
    property string activeScreen: currentScreen

    Material.theme: isDarkTheme ? Material.Dark : Material.Light
    Material.accent: accentColor
    Material.primary: Material.BlueGrey

    // // Minimal UI - just a label
    // Text {
    //     anchors.centerIn: parent
    //     text: "Memory Test - QML Disabled"
    //     color: "white"
    //     font.pixelSize: 24
    // }

    // Component.onCompleted: {
    //     console.log("Memory Test - QML Disabled")
    //     login("npub18wxf0t5jsmcpy57ylzx595twskx8eyj382lj7wp9rtlhzdg5hnnqvt4xra")
    // }

    ClipboardHelper {
        id: clipboard
    }

    Loader {
        id: screenLoader
        anchors.fill: parent
        source: {
            switch (activeScreen) {
                case "Home": return "HomeScreen.ui.qml";
                case "KeyMgmt": return "KeyMgmtScreen.ui.qml";
                default: return "";
            }
        }
        active: activeScreen === "Home" || activeScreen === "KeyMgmt"
    }
}
