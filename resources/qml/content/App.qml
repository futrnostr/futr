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

    property bool isDarkTheme: true
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
}
