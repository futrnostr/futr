import QtQuick.Controls 2.15
import Dialogs 1.0

import Futr 1.0

Menu {
    id: repostMenu

    property var targetPost: null

    MenuItem {
        text: qsTr("Repost")
        onTriggered: {
            if (repostMenu.targetPost.postType == "repost") {
                repost(repostMenu.targetPost.referencedPostId, text)
            } else {
                repost(repostMenu.targetPost.id, text)
            }
        }
    }

    MenuItem {
        text: qsTr("Quote Post")
        onTriggered: {
            if (repostMenu.targetPost.postType == "repost") {
                quoteReplyDialog.targetPost = getPost(repostMenu.targetPost.referencedPostId)
            } else {
                quoteReplyDialog.targetPost = getPost(repostMenu.targetPost.id)
            }

            quoteReplyDialog.open()
        }
    }
}
