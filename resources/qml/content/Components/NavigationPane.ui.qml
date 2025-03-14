import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15

import Components 1.0

Rectangle {
    id: navigationPane
    color: Material.backgroundColor

    property ListModel historyStack: ListModel {}
    property ListModel forwardStack: ListModel {}

    property int currentIndex: -1
    property var currentView: null
    property var currentProps: null

    // Direct manipulation function that won't be affected by property bindings
    function setCurrentState(source, props) {
        primaryLoader.setSource(source, props || {});
        navigationPane.currentView = source;
        navigationPane.currentProps = props || {};
    }

    function navigateTo(source, properties) {
        if (currentView) {
            historyStack.append({
                "source": String(currentView),
                "properties": JSON.stringify(currentProps || {})
            })
            forwardStack.clear()
        }
        setCurrentState(source, properties);
    }

    function navigateBack() {
        if (historyStack.count > 0) {
            forwardStack.append({
                "source": String(currentView || ""),
                "properties": JSON.stringify(currentProps || {})
            })
            var previous = historyStack.get(historyStack.count - 1);
            var prevSource = String(previous.source);
            var prevProps = JSON.parse(previous.properties || "{}");
            historyStack.remove(historyStack.count - 1);
            setCurrentState(prevSource, prevProps);
        }
    }

    function navigateForward() {
        if (forwardStack.count > 0) {
            historyStack.append({
                "source": String(currentView || ""),
                "properties": JSON.stringify(currentProps || {})
            })
            var nextIndex = forwardStack.count - 1;
            var nextItem = forwardStack.get(nextIndex);
            var sourceValue = nextItem.source;
            var propsValue = nextItem.properties;
            forwardStack.remove(nextIndex);
            var nextProps = propsValue ? JSON.parse(propsValue) : {};
            setCurrentState(sourceValue, nextProps);
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Rectangle {
            Layout.fillWidth: true
            height: 40
            color: Material.dialogColor

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 10
                anchors.rightMargin: 10
                spacing: 8

                Button {
                    id: backButton
                    icon.source: "qrc:/icons/arrow_back.svg"
                    icon.width: 20
                    icon.height: 20
                    flat: true
                    enabled: historyStack.count > 0
                    onClicked: navigationPane.navigateBack()
                    implicitWidth: 40
                    implicitHeight: 40
                    padding: 0
                    Layout.alignment: Qt.AlignVCenter

                    ToolTip {
                        visible: parent.hovered
                        text: "Back"
                        delay: 500
                        parent: backButton
                        y: parent.height
                        margins: 0
                    }
                }

                Button {
                    id: forwardButton
                    icon.source: "qrc:/icons/arrow_forward.svg"
                    icon.width: 20
                    icon.height: 20
                    flat: true
                    enabled: forwardStack.count > 0
                    onClicked: navigationPane.navigateForward()
                    implicitWidth: 40
                    implicitHeight: 40
                    padding: 0
                    Layout.alignment: Qt.AlignVCenter

                    ToolTip {
                        visible: parent.hovered
                        text: "Forward"
                        delay: 500
                        parent: forwardButton
                        y: parent.height
                        margins: 0
                    }
                }

                Item { Layout.fillWidth: true }
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true

            Loader {
                id: primaryLoader
                anchors.fill: parent
                anchors.margins: 10
            }
        }
    }
}
