import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Futr 1.0

Item {
    id: root

    property string url

    Layout.alignment: Qt.AlignVCenter

    Image {
        id: image
        anchors.fill: parent
        fillMode: Image.PreserveAspectFit
        cache: false
        asynchronous: true
        source: root.url || ""
    }
}