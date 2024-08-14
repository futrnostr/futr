import QtQuick 2.15
import QtQuick.Window 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
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

    // Apply the Material theme
    Material.theme: Material.Light
    Material.accent: Material.Teal
    Material.primary: Material.BlueGrey

    WelcomeScreen {
        anchors.fill: parent
        visible: currentScreen == "WelcomeScreen"

        KeysGeneratedScreen {
            visible: ctxWelcomeScreen.seedphrase != ""
        }
    }

    HomeScreen {
        anchors.fill: parent
        visible: currentScreen == "HomeScreen"
    }
}
