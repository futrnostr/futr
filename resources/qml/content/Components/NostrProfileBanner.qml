import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Components 1.0

NostrImage {
    Layout.fillWidth: true
    image.fillMode: Image.PreserveAspectCrop
    visible: status === "ready"
}
