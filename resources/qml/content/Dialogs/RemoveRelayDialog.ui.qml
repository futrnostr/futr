import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Dialog {
    property string relayToRemove: ""
    property string relayType

    title: qsTr("Are you sure you want to remove this relay?")
    standardButtons: Dialog.Ok | Dialog.Cancel
    modal: true
    anchors.centerIn: parent
    visible: relayToRemove !== ""

    onAccepted: {
        switch (relayType) {
            case "dm":
                ctxRelayMgmt.removeDMRelay(relayToRemove)
            case "general":
                ctxRelayMgmt.removeGeneralRelay(relayToRemove)
        }
    }

    onRejected: {
        relayToRemove = ""
    }
}
