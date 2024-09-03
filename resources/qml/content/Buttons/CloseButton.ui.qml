import QtQuick 2.15
import QtQuick.Controls 2.15

Button {
    id: root
    property Item target

    icon.source: "qrc:/icons/close.svg"

    width: 10
    height: 10

    flat: true

    ToolTip.visible: hovered
    ToolTip.delay: 500
    ToolTip.timeout: 5000
    ToolTip.text: qsTr("Close")

    onClicked: {
        if (closeButton.target) {
            closeButton.target.visible = false;
        }
    }
}
