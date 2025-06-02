import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtGraphicalEffects 1.15

import Components 1.0
import Futr 1.0

Dialog {
    id: eventJsonDialog
    title: "Event JSON"
    modal: true
    standardButtons: Dialog.Close
    anchors.centerIn: parent
    width: 700
    height: 530

    property var targetPost: null

    ScrollView {
        anchors.fill: parent

        layer.enabled: true
        layer.effect: OpacityMask {
            maskSource: Rectangle {
                width: accountsView.width
                height: accountsView.height
                radius: Constants.radius_m
            }
        }

        ScrollBar.vertical.policy: ScrollBar.AlwaysOn
        ScrollBar.horizontal.policy: ScrollBar.AlwaysOn

        ColumnLayout {
            width: eventJsonDialog.availableWidth - 20
            spacing: Constants.spacing_m

            Repeater {
                model: eventJsonDialog.targetPost ? eventJsonDialog.targetPost.raw : []
                delegate: ColumnLayout {
                    Layout.fillWidth: true
                    spacing: Constants.spacing_s

                    Text {
                        text: eventJsonDialog.targetPost && eventJsonDialog.targetPost.raw.length > 1 ?
                                index === 0 ? "Giftwrap Event" :
                                index === 1 ? "Sealed Event" :
                                "Rumor Event" : ""
                        visible: eventJsonDialog.targetPost && eventJsonDialog.targetPost.raw.length > 1
                        color: Material.primaryTextColor
                        font: Constants.largeFontMedium
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: Constants.spacing_m

                        Button {
                            icon.source: "qrc:/icons/content_copy.svg"
                            flat: true
                            width: 10
                            height: 10
                            onClicked: {
                                clipboard.copyText(modelData)
                            }
                        }

                        Text {
                            text: modelData
                            color: Material.foreground
                            wrapMode: Text.Wrap
                            Layout.fillWidth: true
                        }
                    }

                    Rectangle {
                        Layout.fillWidth: true
                        height: 1
                        color: Material.dividerColor
                        visible: eventJsonDialog.targetPost && eventJsonDialog.targetPost.raw.length > 1 && index < eventJsonDialog.targetPost.raw.length - 1
                    }
                }
            }
        }
    }
}
