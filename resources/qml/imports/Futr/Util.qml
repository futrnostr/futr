pragma Singleton
import QtQuick 2.15

QtObject {
    function getProfilePicture(picture, npub) {
        return picture ? picture : "https://robohash.org/" + npub + ".png?size=50x50";
    }
}
