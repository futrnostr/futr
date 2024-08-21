import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import Futr 1.0

Rectangle {
    id: homeScreen
    width: parent.width
    height: parent.height
    
    Column {
        width: parent.width

        Row {
            height: 100
            spacing: 10
            anchors.horizontalCenter: parent.horizontalCenter
            topPadding: 10

            Rectangle {
                width: parent.parent.width - 20
                height: 80
                color: "#f0f0f0" // Light grey background
                border.color: "#e0e0e0" // Slightly darker grey for border
                border.width: 2
                anchors.margins: 10

                radius: 5

                Column {
                    width: parent.width
                    height: parent.height
                    anchors.fill: parent
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.verticalCenter: parent.verticalCenter
                    topPadding: 10
                    spacing: 10

                    Text {
                        text: qsTr("Welcome to Futr")
                        font: Constants.largeFont
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width
                        horizontalAlignment: Text.AlignHCenter
                    }

                    Text {
                        text: qsTr("Your gateway to the future - global, decentralized, censorship-resistant")
                        font: Constants.font
                        anchors.horizontalCenter: parent.horizontalCenter
                        width: parent.width
                        horizontalAlignment: Text.AlignHCenter
                    }
                }
            }
        }

        
    }
}
