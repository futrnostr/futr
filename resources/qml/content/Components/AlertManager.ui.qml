import QtQuick 2.15
import QtQuick.Layouts 1.15

Column {
    id: root
    spacing: 8

    anchors.top: parent.top
    anchors.horizontalCenter: parent.horizontalCenter
    anchors.topMargin: 16

    width: Math.min(600, parent.width - 32)  // max 600, or window width minus margins
    z: 1000

    function showPublishStatus(status) {
        var component = Qt.createComponent("PublishAlert.ui.qml")
        if (component.status === Component.Ready) {
            var alert = component.createObject(root, {
                "publishStatus": status,
                "width": root.width
            })
            // Remove the alert when it's hidden
            alert.onVisibleChanged.connect(function() {
                if (!alert.visible) {
                    alert.destroy()
                }
            })
        }
    }

    Repeater {
        model: publishStatuses
        delegate: PublishAlert {
            width: root.width
            publishStatus: modelData
        }
    }
}
