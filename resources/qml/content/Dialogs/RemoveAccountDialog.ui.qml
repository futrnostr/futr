import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Dialog {
    property string accountToRemove: ""

    title: qsTr("Are you sure you want to remove this account?")
    standardButtons: Dialog.Ok | Dialog.Cancel
    modal: true
    anchors.centerIn: parent
    visible: accountToRemove !== ""

    onAccepted: {
        ctxKeyMgmt.removeAccount(accountToRemove)
    }

    onRejected: {
        accountToRemove = ""
    }
}