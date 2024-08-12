import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import Futr 1.0
import QtQuick.Studio.Components 1.0

Rectangle {
    id: errorScreen
    height: 50
    color: "red"
    border.color: "darkred"
    radius: 10
    width: parent.width - 20
    anchors.left: parent.left
    anchors.top: parent.top
    anchors.margins: 10

    Text {
        id: errorMessage
        text: errorMsg
        color: "black"
        anchors.centerIn: parent
        font.bold: true
        padding: 10
    }


    Rectangle {
        width: 20  // Set the width
        height: width  // Make the height equal to width for a circle
        color: "lightgray"  // Background color for the button
        border.color: "darkgrey"
        radius: width / 2  // Make the rectangle a circle

        anchors.verticalCenter: parent.verticalCenter
        anchors.right: parent.right
        anchors.margins: 10

        MouseArea {
            anchors.fill: parent
            onClicked: errorMsg = ""
            
            Text {
                anchors.centerIn: parent
                text: "X"
                color: "black"
                font.bold: true
            }
        }
    }
}

