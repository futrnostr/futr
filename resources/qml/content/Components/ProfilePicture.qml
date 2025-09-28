import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: root

    property string url

    Layout.preferredWidth: 36
    Layout.preferredHeight: 36
    Layout.alignment: Qt.AlignVCenter

    width: 36
    height: 36

    Image {
        id: image
        anchors.fill: parent
        fillMode: Image.PreserveAspectFit
        cache: false
        asynchronous: true
        source: root.url || "qrc:/icons/person.svg"

        onStatusChanged: {
            if (image.status === Image.Error) {
                image.source = "qrc:/icons/person.svg"
            }
        }
    }
}