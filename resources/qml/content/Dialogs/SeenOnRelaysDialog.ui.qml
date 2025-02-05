import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import Components 1.0
import Futr 1.0

Dialog {
    id: seenOnRelaysDialog
    title: "Seen on Relays"
    modal: true
    standardButtons: Dialog.Close
    anchors.centerIn: parent
    width: 350
    height: 310

    property var targetPost: null

    ColumnLayout {
        width: seenOnRelaysDialog.availableWidth - 20
        spacing: Constants.spacing_m

        Repeater {
            model: seenOnRelaysDialog.targetPost ? seenOnRelaysDialog.targetPost.relays : []
            delegate: ColumnLayout {
                Layout.fillWidth: true
                spacing: Constants.spacing_s

                Text {
                    text: modelData
                    color: Material.foreground
                    wrapMode: Text.Wrap
                    Layout.fillWidth: true
                }

                Rectangle {
                    Layout.fillWidth: true
                    height: 1
                    color: Material.dividerColor
                    visible: seenOnRelaysDialog.targetPost && seenOnRelaysDialog.targetPost.relays.length > 1 && index < seenOnRelaysDialog.targetPost.relays.length - 1
                }
            }
        }
    }
}
