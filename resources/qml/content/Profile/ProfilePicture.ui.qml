import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

Rectangle {
    id: root
    Layout.preferredWidth: 36
    Layout.preferredHeight: 36
    Layout.alignment: Qt.AlignVCenter
    Layout.topMargin: 0
    radius: width / 2
    color: Material.dividerColor

    layer.enabled: true
    layer.effect: OpacityMask {
        maskSource: Rectangle {
            width: root.width
            height: root.height
            radius: root.radius
        }
    }

    property string imageSource

    Image {
        id: profileImage
        anchors.fill: parent
        source: parent.imageSource
        smooth: true
        fillMode: Image.PreserveAspectCrop
        cache: false

        layer.enabled: true
        layer.effect: OpacityMask {
            maskSource: Item {
                width: profileImage.width
                height: profileImage.height

                Rectangle {
                    anchors.fill: parent
                    radius: width / 2
                }
            }
        }
    }
}
