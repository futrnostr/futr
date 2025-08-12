import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0

Item {
    id: relaysScreen
    anchors.fill: parent

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 10
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            spacing: 10
            Button {
                text: qsTr("Back")
                icon.source: "qrc:/icons/arrow_back.svg"
                flat: true
                onClicked: stackView.pop()
            }
            Text { text: qsTr("Relay Management"); font: Constants.largeFont; color: Material.primaryTextColor }
        }

        // Reuse the dialog content as a full screen view
        Loader {
            Layout.fillWidth: true
            Layout.fillHeight: true
            source: "qrc:/qml/content/Dialogs/RelayMgmtDialog.ui.qml"
        }
    }
}


