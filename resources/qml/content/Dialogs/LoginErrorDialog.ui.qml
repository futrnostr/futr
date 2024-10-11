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

    Text {
        text: loginErrorDialog.errorMessage
        color: Material.foreground
        font.pixelSize: 14
        wrapMode: Text.WordWrap
    }
}
