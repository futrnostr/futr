import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

Dialog {
    title: qsTr("Login Error")
    standardButtons: Dialog.Ok
    modal: true
    anchors.centerIn: parent

    property string errorMessage: ""

    width: Math.min(parent.width - 200, 400)
    height: Math.min(contentColumn.implicitHeight + 80, parent.height - 40)

    ColumnLayout {
        id: contentColumn
        width: parent.width
        spacing: 10

        Text {
            Layout.fillWidth: true
            text: errorMessage
            color: Material.foreground
            font.pixelSize: 14
            wrapMode: Text.WordWrap
        }
    }
}
