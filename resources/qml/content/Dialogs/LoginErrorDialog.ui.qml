import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

Dialog {
    title: qsTr("Login Error")
    standardButtons: Dialog.Ok
    modal: true
    anchors.centerIn: parent
    width: Math.min(parent.width - 40, 300)
    height: 160

    property string errorMessage: ""

    ColumnLayout {
        id: contentColumn
        anchors.fill: parent
        anchors.margins: 0

        Text {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignLeft
            Layout.preferredWidth: contentColumn.width
            horizontalAlignment: Text.AlignLeft
            text: errorMessage
            color: Material.foreground
            font.pixelSize: 14
            wrapMode: Text.WordWrap
        }
    }
}
