import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

Dialog {
    standardButtons: Dialog.NoButton
    modal: true
    anchors.centerIn: parent
    width: 300
    height: 200
    closePolicy: Popup.NoAutoClose

    ColumnLayout {
        anchors.fill: parent
        spacing: 20

        BusyIndicator {
            running: true
            Layout.alignment: Qt.AlignHCenter
        }

        Text {
            text: qsTr("Connecting...")
            color: Material.foreground
            font.pixelSize: 16
            font.bold: true
            Layout.alignment: Qt.AlignHCenter
        }
    }
}