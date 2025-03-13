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
    property var currentSecondaryView: null
    property var currentSecondaryProps: null

    // Direct manipulation function that won't be affected by property bindings
    function setCurrentState(source, props, secondarySource, secondaryProps) {
        primaryLoader.setSource(source, props || {});
        secondaryLoader.setSource(secondarySource || "", secondaryProps || {});
        navigationPane.currentView = source;
        navigationPane.currentProps = props || {};
        navigationPane.currentSecondaryView = secondarySource || "";
        navigationPane.currentSecondaryProps = secondaryProps || {};
    }

    function navigateTo(source, properties, secondarySource, secondaryProperties) {
        if (currentView) {
            historyStack.append({
                "source": String(currentView),
                "properties": JSON.stringify(currentProps || {}),
                "secondarySource": String(currentSecondaryView || ""),
                "secondaryProperties": JSON.stringify(currentSecondaryProps || {})
            })
            forwardStack.clear()
        }
        setCurrentState(source, properties, secondarySource, secondaryProperties);
    }

    function navigateBack() {
        if (historyStack.count > 0) {
            forwardStack.append({
                "source": String(currentView || ""),
                "properties": JSON.stringify(currentProps || {}),
                "secondarySource": String(currentSecondaryView || ""),
                "secondaryProperties": JSON.stringify(currentSecondaryProps || {})
            })
            var previous = historyStack.get(historyStack.count - 1);
            var prevSource = String(previous.source);
            var prevProps = JSON.parse(previous.properties || "{}");
            var prevSecondarySource = String(previous.secondarySource || "");
            var prevSecondaryProps = JSON.parse(previous.secondaryProperties || "{}");
            historyStack.remove(historyStack.count - 1);
            setCurrentState(prevSource, prevProps, prevSecondarySource, prevSecondaryProps);
        }
    }

    function navigateForward() {
        if (forwardStack.count > 0) {
            historyStack.append({
                "source": String(currentView || ""),
                "properties": JSON.stringify(currentProps || {}),
                "secondarySource": String(currentSecondaryView || ""),
                "secondaryProperties": JSON.stringify(currentSecondaryProps || {})
            })
            var nextIndex = forwardStack.count - 1;
            var nextItem = forwardStack.get(nextIndex);
            var sourceValue = nextItem.source;
            var propsValue = nextItem.properties;
            var secondarySourceValue = nextItem.secondarySource;
            var secondaryPropsValue = nextItem.secondaryProperties;
            forwardStack.remove(nextIndex);
            var nextProps = propsValue ? JSON.parse(propsValue) : {};
            var nextSecondaryProps = secondaryPropsValue ? JSON.parse(secondaryPropsValue) : {};
            if (nextProps && nextSecondaryProps) {
                if (!nextProps.profileData && nextSecondaryProps.profileData) {
                    nextProps.profileData = nextSecondaryProps.profileData;
                }
                if (!nextSecondaryProps.profileData && nextProps.profileData) {
                    nextSecondaryProps.profileData = nextProps.profileData;
                }
            }
            setCurrentState(sourceValue, nextProps, secondarySourceValue, nextSecondaryProps);
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
                    implicitWidth: 36
                    implicitHeight: 36
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
                    implicitWidth: 36
                    implicitHeight: 36
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

            Row {
                anchors.fill: parent
                spacing: 10

                Item {
                    width: parent.width * 0.6
                    height: parent.height

                    Loader {
                        id: primaryLoader
                        anchors.fill: parent
                        anchors.margins: 10
                    }
                }

                Item {
                    width: parent.width * 0.4
                    height: parent.height

                    Loader {
                        id: secondaryLoader
                        anchors.fill: parent
                        anchors.margins: 10
                    }
                }
            }
        }
    }
}
