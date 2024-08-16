import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15
import HsQML.Model 1.0
import Futr 1.0

ScrollView {
    ListView {
        id: accountsView;
        focus: true;
        model: AutoListModel {
            source: ctxAccounts.accounts; mode: AutoListModel.ByKey;
        }
        delegate: Rectangle {
            width: parent.width
            height: 80

            property bool mouseHover: false

            color: mouseHover ? "lightsteelblue" : "lightgray"
            border.color: "gray"
            radius: 10

            // Display the account's picture
            Image {
                id: accountImage
                source: modelData.picture
                width: 60
                height: 60
                anchors.left: parent.left
                anchors.leftMargin: 10
                anchors.verticalCenter: parent.verticalCenter
                fillMode: Image.PreserveAspectCrop
            }

            // Display the npub and displayName
            Column {
                anchors.left: accountImage.right
                anchors.leftMargin: 10
                anchors.fill: parent
                anchors.verticalCenter: parent.verticalCenter

                Text {
                    text: modelData.displayName ? modelData.displayName + " (" + modelData.npub + ")" : modelData.npub
                }
            }

            MouseArea {
                id: mouseArea
                anchors.fill: parent
                hoverEnabled: true
                //onClicked: ctxAccounts.login(modelData.npub)

                onEntered: parent.mouseHover = true
                onExited: parent.mouseHover = false
            }
        }
        highlight: Rectangle {
            color: 'lightsteelblue';
        }
    }
}
