pragma Singleton
import QtQuick 2.15
import QtQuick.Controls.Material 2.15

QtObject {
    readonly property int width: 1280
    readonly property int height: 720

    readonly property font font: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize,
        color: Material.primaryTextColor,
        wrapMode: Text.WordWrap
    })
    readonly property font largeFont: Qt.font({
        family: "Arial",
        pixelSize: Qt.application.font.pixelSize * 2,
        color: Material.primaryTextColor,
        wrapMode: Text.WordWrap
    })

    readonly property color backgroundColor: Material.backgroundColor
}
