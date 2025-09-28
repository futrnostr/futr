import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtQuick.Window 2.15
import QtGraphicalEffects 1.15

import Components 1.0
import Futr 1.0
import Profile 1.0

Pane {
    id: root

    property var post

    property var post_id: post ? post.id : null
    property var post_nevent: post ? post.nevent : null
    property var post_relays: post ? post.relays : null
    property var post_postType: post ? post.postType : null
    property var post_content: post ? post.content : null
    property var post_timestamp: post ? post.timestamp : null
    property var post_authorId: post ? post.authorId : null
    property var post_referencedPostId: post ? post.referencedPostId : null

    property string currentUser
    property bool isRefPost: false
    property var author: null

    property var author_id: author ? author.id : ""
    property var author_npub: author ? author.npub : ""
    property var author_name: author ? author.name : ""
    property var author_displayName: author ? author.displayName : ""
    property var author_about: author ? author.about : ""
    property var author_picture: author ? author.picture : ""
    property var author_nip05: author ? author.nip05 : ""
    property var author_banner: author ? author.banner : ""
    property bool privateChatMode: false

    property var contentParts: []
    property var comments: [] // @todo
    property bool hideActions: false
    property bool showAuthor: false
    property bool disableCommentAction: false

    property var processedUrls: {}
    property string lastProcessedContent: ""
    property string lastProcessedPostId: ""
    property bool isInitialLoad: true
    property bool contentParsed: false

    property var componentMap: {
        "text": "PostContent/TextComponent.ui.qml",
        "image": "PostContent/PostImage.ui.qml", 
        "video": "PostContent/PostVideo.ui.qml",
        "url": "PostContent/TextComponent.ui.qml",
        "profile": "PostContent/TextComponent.ui.qml"
    }

    signal commentClicked()
    signal repostClicked()
    signal postClicked()

    padding: Constants.spacing_xs
    implicitHeight: mainColumn.implicitHeight + (padding * 2)

    onPost_contentChanged: {
        if (post_content !== lastProcessedContent) {
            lastProcessedContent = post_content || ""
            renderInitialText()
            parseContent()
        }
    }

    onPostChanged: {
        var currentPostId = post ? post[0] : null

        if (currentPostId && currentPostId !== lastProcessedPostId) {
            lastProcessedPostId = currentPostId
            isInitialLoad = true
            contentParsed = false

            if (post && post_content && post_content !== lastProcessedContent) {
                lastProcessedContent = post_content || ""
                renderInitialText()
                parseContent()
            }
        } else if (currentPostId === null && lastProcessedPostId !== null) {
            lastProcessedPostId = null
            isInitialLoad = true
            contentParsed = false
        }
    }

    function renderInitialText() {
        if (!post_content) {
            contentLayout.children = []
            return
        }

        contentLayout.children = []

        var component = Qt.createComponent("PostContent/TextComponent.ui.qml")

        if (component.status === Component.Ready) {
            var item = component.createObject(contentLayout, {
                "width": Qt.binding(function() { return contentLayout.width }),
                "value": replaceNewlines(post_content)
            })

            if (!item) {
                console.error("Failed to create initial text component for post:", post_id)
            }
        } else {
            console.error("Failed to load text component:", component.errorString())
        }

        isInitialLoad = false
    }

    function parseContent() {
        if (!post_content || contentParsed) {
            return
        }

        if (!author && post_authorId) {
            author = getProfile(post_authorId)
        }

        var parts = parseContentParts(post_content)
        contentParts = parts
        contentParsed = true

        updateContent()
    }

    function mediaPeekCallback(url, mimeType) {
        if (!post_id) {
            return
        }

        if (!processedUrls) {
            processedUrls = ({})
        }

        processedUrls[url] = {type: "checked", mimeType: mimeType}

        if (mimeType.indexOf("image/") === 0 || mimeType.indexOf("video/") === 0) {
            processedUrls[url] = {type: "caching", mimeType: mimeType}

            mediaCacheCompleted.connect(mediaCacheCallback)
            cacheMedia(url)
        } else if (post_id) {
            updateContent()
        }

        mediaPeekCompleted.disconnect(mediaPeekCallback)
    }

    function mediaCacheCallback(url, success, pathOrError) {
        if (!post_id) {
            return
        }

        if (!processedUrls) {
            processedUrls = ({})
        }

        if (success) {
            if (processedUrls.hasOwnProperty(url) && processedUrls[url]) {
                processedUrls[url].type = "cached"
                processedUrls[url].cachePath = pathOrError

                var mimeType = processedUrls[url].mimeType || ""
                var mediaType = ""

                if (mimeType.indexOf("image/") === 0) {
                    mediaType = "image"
                } else if (mimeType.indexOf("video/") === 0) {
                    mediaType = "video"
                }

                if (mediaType && post_id) {
                    splitContentAroundResolvedMedia(url, mediaType, pathOrError, url)
                } else if (post_id) {
                    updateContent()
                }
            }
        } else if (processedUrls.hasOwnProperty(url) && processedUrls[url]) {
            processedUrls[url].type = "failed"
        }

        mediaCacheCompleted.disconnect(mediaCacheCallback)
    }

    Component.onDestruction: {
        post = null
        author = null
        lastProcessedPostId = null
        contentParsed = false

        if (processedUrls) {
            processedUrls = ({})
        }

        if (contentLayout && contentLayout.children) {
            for (var i = 0; i < contentLayout.children.length; i++) {
                contentLayout.children[i].destroy()
            }
            contentLayout.children = []
        }

        mediaPeekCompleted.disconnect(mediaPeekCallback)        
        mediaCacheCompleted.disconnect(mediaCacheCallback)
    }

    background: Rectangle {
        id: backgroundRect
        color: privateChatMode && author && author_npub == currentUser ? Material.accentColor : Material.dialogColor
        radius: Constants.radius_m

        MouseArea {
            id: postClickArea
            anchors.fill: parent
            enabled: true
            propagateComposedEvents: true
            cursorShape: Qt.PointingHandCursor
            z: -1

            onPressed: {
                if (!mouse.wasHeld) {
                    root.postClicked()
                }
                mouse.accepted = false;
            }
        }
    }

    Column {
        id: mainColumn
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        spacing: Constants.spacing_xs

        Row {
            width: parent.width
            visible: post != null && post != undefined && (post_postType === "quote_repost" || post_postType === "repost")

            Image {
                source: "qrc:/icons/repeat.svg"
                sourceSize.width: 20
                sourceSize.height: 20
            }

            Text {
                text: post && post_postType === "repost" ? qsTr("Reposted") : qsTr("Quote Reposted")
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
            }
        }

        Row {
            width: parent.width
            visible: showAuthor || isRefPost
            spacing: Constants.spacing_s

            Rectangle {
                width: root.isCollapsed ? 30 : 34
                height: root.isCollapsed ? 30 : 34

                radius: width/2
                color: "transparent"
                border.width: 1
                border.color: Material.dividerColor

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor

                    onClicked: {
                        personalFeed.npub = author_npub
                    }
                }

                ProfilePicture {
                    anchors.fill: parent
                    anchors.margins: Constants.spacing_xs
                    url: author_picture
                }
            }

            MouseArea {
                width: parent.width - 50
                height: childrenRect.height
                cursorShape: Qt.PointingHandCursor
                anchors.verticalCenter: parent.verticalCenter

                onClicked: {
                    personalFeed.npub = author_npub
                }

                Column {
                    spacing: 2
                    width: parent.width

                    Text {
                        font: Constants.font
                        text: author ? (author_displayName || author_name || "") : ""
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.primaryTextColor
                    }

                    Text {
                        text: author_npub
                        font.pixelSize: Constants.font.pixelSize * 0.8
                        elide: Text.ElideRight
                        width: parent.width
                        color: Material.secondaryTextColor
                    }
                }
            }
        }

        Column {
            id: contentLayout
            width: parent.width
            spacing: 0
        }

        // Main post actions
        Row {
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: Constants.spacing_l
            height: 36
            visible: !hideActions && !isRefPost && !privateChatMode

            Row {
                spacing: Constants.spacing_s
                visible: !disableCommentAction

                Button {
                    flat: true
                    icon.source: "qrc:/icons/comment.svg"
                    icon.width: 20
                    icon.height: 20
                    implicitWidth: 36
                    implicitHeight: 36
                    padding: 8
                    icon.color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                    onClicked: commentClicked()
                }

                Text {
                    text: comments.length
                    color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                }
            }

            Row {
                spacing: Constants.spacing_s
                visible: disableCommentAction

                Image {
                    id: commentIcon
                    source: "qrc:/icons/comment.svg"
                    width: 20
                    height: 20
                    anchors.verticalCenter: parent.verticalCenter

                    ColorOverlay {
                        anchors.fill: parent
                        source: parent
                        color: Material.secondaryTextColor
                        cached: true
                    }
                }

                Text {
                    text: comments.length
                    color: Material.secondaryTextColor
                    anchors.verticalCenter: parent.verticalCenter
                }
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/repeat.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 36
                implicitHeight: 36
                padding: 8
                icon.color: Material.secondaryTextColor
                onClicked: repostClicked()
                anchors.verticalCenter: parent.verticalCenter
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/delete.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 36
                implicitHeight: 36
                padding: 8
                icon.color: Material.secondaryTextColor
                visible: post && currentUser == author_npub
                anchors.verticalCenter: parent.verticalCenter

                onClicked: deleteDialog.open()
            }
        }

        Row {
            anchors.right: parent.right

            Text {
                text: post_timestamp ? post_timestamp : ""
                font: Constants.smallFontMedium
                color: Material.secondaryTextColor
                anchors.verticalCenter: parent.verticalCenter
            }

            Item {
                width: Constants.spacing_m
            }

            Button {
                flat: true
                icon.source: "qrc:/icons/menu.svg"
                icon.width: 20
                icon.height: 20
                implicitWidth: 28
                implicitHeight: 28
                padding: 4
                anchors.verticalCenter: parent.verticalCenter

                onClicked: postMenu.open()

                Menu {
                    id: postMenu
                    y: parent.height

                    MenuItem {
                        text: qsTr("Copy Event ID")
                        onTriggered: {
                            clipboard.copyText(post_nevent)
                        }
                    }

                    MenuItem {
                        text: qsTr("Show Event JSON")
                        onTriggered: {
                            if (post_id) {
                                eventJsonDialog.targetPost = {
                                    raw: post.raw
                                }
                                eventJsonDialog.open()
                            }
                        }
                    }

                    MenuItem {
                        text: qsTr("Seen on Relays")
                        onTriggered: {
                            seenOnRelaysDialog.targetPost = post
                            seenOnRelaysDialog.open()
                        }
                    }
                }
            }
        }
    }

    Dialog {
        id: deleteDialog
        title: "Delete Post"
        modal: true
        standardButtons: Dialog.Ok | Dialog.Cancel
        anchors.centerIn: parent
        width: 350

        Column {
            spacing: Constants.spacing_m
            width: parent.width

            Text {
                text: "Are you sure you want to delete this post?"
                color: Material.foreground
                wrapMode: Text.Wrap
                width: parent.width
            }

            TextField {
                id: reasonField
                placeholderText: "Reason for deletion (optional)"
                width: parent.width
            }
        }

        onAccepted: {
            deleteEvent(post_id, reasonField.text)
            reasonField.text = ""
        }
    }

    function openFullscreenVideo(videoUrl) {
        var component = Qt.createComponent("FullscreenVideoWindow.ui.qml");
        if (component.status === Component.Ready) {
            var window = component.createObject(root, {
                "videoUrl": videoUrl
            });
            window.Component.onDestruction.connect(function() {
                component.destroy();
            });
        } else {
            console.error("Error loading component:", component.errorString());
        }
    }

    function copyToClipboard(text) {
        clipboard.copyText(text)
    }

    function downloadVideo(url) {
        downloadCompleted.connect(videoDownloadCallback)
        downloadAsync(url)
    }

    function videoDownloadCallback(success, filePathOrError) {
        downloadCompleted.disconnect(videoDownloadCallback)
    }

    function parseContentParts(contentText) {
        if (!contentText || contentText.length === 0) {
            return []
        }

        var parts = []
        var currentPos = 0

        var urlRegex = /(https?:\/\/[^\s<>"'()[\]{},;]+)/g
        var nostrRegex = /(nostr:[a-zA-Z0-9]+)/g
        var allMatches = []

        var match
        while ((match = urlRegex.exec(contentText)) !== null) {
            allMatches.push({
                start: match.index,
                end: match.index + match[0].length,
                text: match[0],
                type: "url"
            })
        }

        while ((match = nostrRegex.exec(contentText)) !== null) {
            var nostrText = match[0]
            var bech32Part = nostrText.substring(6) // Remove "nostr:"
             var type = "note"
             if (bech32Part.indexOf("npub1") === 0) type = "npub"
             else if (bech32Part.indexOf("nprof") === 0) type = "nprofile" 
             else if (bech32Part.indexOf("neven") === 0) type = "nevent"
             else if (bech32Part.indexOf("naddr") === 0) type = "naddr"

            allMatches.push({
                start: match.index,
                end: match.index + match[0].length,
                text: nostrText,
                bech32: bech32Part,
                type: type
            })
        }

        allMatches.sort(function(a, b) { return a.start - b.start })

        for (var i = 0; i < allMatches.length; i++) {
            var match = allMatches[i]

            if (match.start > currentPos) {
                var textBefore = contentText.substring(currentPos, match.start)
                if (textBefore.length > 0) {
                    parts.push(["text", replaceNewlines(textBefore)])
                }
            }

            if (match.type === "url") {
                parts.push(["url", match.text, match.text]) // [type, url, originalUrl]
            } else if (match.type === "npub" || match.type === "nprofile") {
                parts.push(["profile", match.bech32])
            } else {
                parts.push([match.type, match.bech32])
            }

            currentPos = match.end
        }

        if (currentPos < contentText.length) {
            var remaining = contentText.substring(currentPos)
            if (remaining.length > 0) {
                parts.push(["text", replaceNewlines(remaining)])
            }
        }

        return parts
    }

    function replaceNewlines(text) {
        return text.replace(/\n/g, "<br>")
    }

    function updateContent() {
        if (!post_id || !contentParsed || !contentParts || contentParts.length === 0) {
            return
        }

        contentLayout.children = []
        renderContentParts(contentLayout, contentParts)
    }

    function renderContentParts(container, parts) {
        for (var i = 0; i < parts.length; i++) {
            var part = parts[i]
            var type = part[0]
            var value = part[1]
            var originalUrl = part.length > 2 ? part[2] : value

            if ("note" === type || "nevent" === type || "naddr" === type) {
                // Create framed container
                var qml = "import QtQuick 2.15; import QtQuick.Controls 2.15; import QtQuick.Controls.Material 2.15; import QtQuick.Layouts 1.15; import Futr 1.0;\n"
                        + "Rectangle { id: refFrame; color: \"transparent\"; radius: Constants.radius_m; border.width: 1; border.color: Material.backgroundColor; clip: true;\n"
                        + "  property int frameWidth: 0; width: frameWidth;\n"
                        + "  property bool isLoading: true;\n"
                        + "  property alias contentRootItem: contentRoot;\n"
                        + "  RowLayout { id: placeholder; visible: refFrame.isLoading; anchors.left: parent.left; anchors.right: parent.right; anchors.top: parent.top; anchors.margins: Constants.spacing_xs;\n"
                        + "    BusyIndicator { Layout.alignment: Qt.AlignVCenter; Layout.preferredWidth: 36; Layout.preferredHeight: 36; running: false }\n"
                        + "    Text { Layout.alignment: Qt.AlignLeft; Layout.fillWidth: true; text: qsTr(\"Event not found. Trying to find it for you...\"); font: Constants.font; color: Material.secondaryTextColor }\n"
                        + "  }\n"
                        + "  Column { id: contentRoot; anchors.left: parent.left; anchors.right: parent.right; anchors.top: parent.top; anchors.margins: Constants.spacing_xs; spacing: 0; visible: !refFrame.isLoading;\n"
                        + "           layer.enabled: true; layer.smooth: true; layer.mipmap: true }\n"
                        + "  implicitHeight: (refFrame.isLoading ? placeholder.implicitHeight : contentRoot.implicitHeight) + Constants.spacing_xs\n"
                        + "}"

                var frame = Qt.createQmlObject(qml, container)
                if (frame) {
                    frame.frameWidth = Qt.binding(function() { return container.width })
                    var cachedPost = getPost(value)

                    if (cachedPost) {
                        frame.isLoading = false

                        var authorProfile = cachedPost.authorId ? getProfile(cachedPost.authorId) : null
                        var authorNpubVal = authorProfile ? authorProfile.npub : ""
                        var authorDisplay = authorProfile ? (authorProfile.displayName || authorProfile.name || "") : ""
                        var authorPic = authorProfile ? authorProfile.picture : ""

                        var headerQml = "import QtQuick 2.15; import QtQuick.Controls 2.15; import QtQuick.Controls.Material 2.15; import QtQuick.Layouts 1.15; import Futr 1.0; import Components 1.0;\n"
                                     + "Row { id: refHeader; width: parent.width; spacing: Constants.spacing_s;\n"
                                     + "  property string authorNpub: \"\"; property string authorDisplayName: \"\"; property string authorPictureUrl: \"\";\n"
                                     + "  Rectangle { width: 34; height: 34; radius: width/2; color: \"transparent\"; border.width: 1; border.color: Material.dividerColor;\n"
                                     + "    MouseArea { anchors.fill: parent; cursorShape: Qt.PointingHandCursor; onClicked: { personalFeed.npub = refHeader.authorNpub } }\n"
                                     + "    ProfilePicture { anchors.fill: parent; anchors.margins: Constants.spacing_xs; url: refHeader.authorPictureUrl }\n"
                                     + "  }\n"
                                     + "  MouseArea { width: parent.width - 50; height: childrenRect.height; cursorShape: Qt.PointingHandCursor; anchors.verticalCenter: parent.verticalCenter; onClicked: { personalFeed.npub = refHeader.authorNpub }\n"
                                     + "    Column { spacing: 2; width: parent.width;\n"
                                     + "      Text { font: Constants.font; text: refHeader.authorDisplayName; elide: Text.ElideRight; width: parent.width; color: Material.primaryTextColor }\n"
                                     + "      Text { text: refHeader.authorNpub; font.pixelSize: Constants.font.pixelSize * 0.8; elide: Text.ElideRight; width: parent.width; color: Material.secondaryTextColor }\n"
                                     + "    }\n"
                                     + "  }\n"
                                     + "}"

                        var headerObj = Qt.createQmlObject(headerQml, frame.contentRootItem, "RefHeader")
                        if (headerObj) {
                            headerObj.authorNpub = authorNpubVal
                            headerObj.authorDisplayName = authorDisplay
                            headerObj.authorPictureUrl = authorPic
                        }

                        var nestedContent = cachedPost.content
                        if (nestedContent) {
                            var nestedParts = parseContentParts(nestedContent)
                            renderContentParts(frame.contentRootItem, nestedParts)
                        }
                    } else {
                        frame.isLoading = true
                    }
                } else {
                    console.error("Failed to create referenced frame for:", value)
                }
                break
            }

            if (!componentMap[type]) {
                console.error("Failed to create component for:", type)
                break
            }

            var finalType = type
            var args = {
                "width": Qt.binding(function() { return container.width }),
            }

            if (type === "text") {
                args["value"] = value
            } else if (type === "profile") {
                var profile = getProfile(value)
                var displayName = ""
                if (profile) {
                    displayName = profile.displayName || profile.name || ""
                }
                if (!displayName) {
                    displayName = value.substring(0, 8) + "..."
                }
                args["value"] = "<a href=\"profile://" + value + 
                               "\" style=\"color: #9C27B0\">@" + displayName + "</a>"
            } else if (type === "url") {
                var downloadStatus = hasDownload(value) || []

                if (downloadStatus && downloadStatus.length === 2) {
                    var cacheFile = downloadStatus[0]  
                    var mimeType = downloadStatus[1]   

                    if (mimeType.indexOf("image/") === 0) {
                        finalType = "image"
                        args["value"] = cacheFile
                        args["original"] = originalUrl
                    } else if (mimeType.indexOf("video/") === 0) {
                        finalType = "video" 
                        args["value"] = cacheFile
                        args["original"] = originalUrl
                    } else {
                        args["value"] = "<a href=\"" + value + "\" style=\"color: #9C27B0\">" + value + "</a>"
                    }
                } else {
                    if (!processedUrls) {
                        processedUrls = ({})
                    }

                    var urlInfo = processedUrls.hasOwnProperty(value) ? processedUrls[value] : null

                    if (urlInfo && urlInfo.type === "cached") {
                        if (urlInfo.mimeType.indexOf("image/") === 0) {
                            finalType = "image"
                            args["value"] = urlInfo.cachePath
                            args["original"] = originalUrl
                        } else if (urlInfo.mimeType.indexOf("video/") === 0) {
                            finalType = "video" 
                            args["value"] = urlInfo.cachePath
                            args["original"] = originalUrl
                        } else {
                            args["value"] = "<a href=\"" + value + "\" style=\"color: #9C27B0\">" + value + "</a>"
                        }
                    } else if (!urlInfo || urlInfo.type === "failed") {
                        args["value"] = "<a href=\"" + value + "\" style=\"color: #9C27B0\">" + value + "</a>"
                        if (!urlInfo) {
                            processedUrls[value] = {type: "checking"}
                            mediaPeekCompleted.connect(mediaPeekCallback)
                            peekMimeType(value)
                        }
                    } else {
                        args["value"] = "<a href=\"" + value + "\" style=\"color: #9C27B0\">" + value + "</a>"
                    }
                }
            } else if (type === "image") {
                finalType = "image"
                args["value"] = value
                args["original"] = originalUrl
            } else if (type === "video") {
                finalType = "video"
                args["value"] = value
                args["original"] = originalUrl
            } else {
                args["value"] = value
            }

            var component = Qt.createComponent(componentMap[finalType])

            if (component.status === Component.Ready) {
                if (finalType === "image") {
                    args["asynchronous"] = true
                }

                if (componentMap[finalType] === "PostContent/ReferencedPost.ui.qml") {
                    args["currentUser"] = currentUser
                }

                var item = component.createObject(container, args)

                if (item === null) {
                    console.error("Failed to create object for:", value)
                }
            } else {
                console.error("Failed to load component:", component.errorString())
            }
        }
    }

    function splitContentAroundResolvedMedia(url, mediaType, cachePath, originalUrl) {
        if (!post_id || !contentParts || contentParts.length === 0) {
            return
        }

        var newParts = []
        var foundUrl = false

        for (var i = 0; i < contentParts.length; i++) {
            var part = contentParts[i]
            var partType = part[0]
            var partValue = part[1]

            if (partType === "text" && partValue.indexOf(url) !== -1) {
                foundUrl = true
                var urlIndex = partValue.indexOf(url)

                if (urlIndex > 0) {
                    var textBefore = partValue.substring(0, urlIndex)
                    newParts.push(["text", textBefore])
                }

                newParts.push([mediaType, cachePath, originalUrl])

                var urlEndIndex = urlIndex + url.length
                if (urlEndIndex < partValue.length) {
                    var textAfter = partValue.substring(urlEndIndex)
                    newParts.push(["text", textAfter])
                }
            } else if (partType === "url" && partValue === url) {
                foundUrl = true
                newParts.push([mediaType, cachePath, originalUrl])
            } else {
                newParts.push(part)
            }
        }

        if (foundUrl) {
            contentParts = newParts
            contentLayout.children = []
            if (post_id && contentParts) {
                renderContentParts(contentLayout, contentParts)
            }
        }
    }
}
