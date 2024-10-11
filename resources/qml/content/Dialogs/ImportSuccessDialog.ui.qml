import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Dialog {
    title: qsTr("Nostr account succesfully imported")
    standardButtons: Dialog.Ok
    modal: true
    anchors.centerIn: parent
}
