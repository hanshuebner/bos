$ = jQuery;

$(document).ready(init);

var poi_id;

Date.prototype.renderDate = function() {
    return this.getDate() + '.' + this.getMonth() + '.' + (1900 + this.getYear());
}

var makeMediumMenuEntry = {
    image: function (medium) {
        return LI(null,
                  IMG({ src: '/image/' + medium.id, width: 40, height: 40 }),
                  (new Date(medium.timestamp)).renderDate(),
                  BR(),
                  A({ href: '#' }, medium.title || medium.name));
    },
    panorama: function (medium) {
        return LI(null,
                  IMG({ src: '/static/panorama-icon.gif', width: 40, height: 40 }),
                  (new Date(medium.timestamp)).renderDate(),
                  BR(),
                  A({ href: '#' }, medium.title || medium.name));
    },
    movie: function (medium) {
        return LI(null,
                  IMG({ src: '/static/movie-icon.gif', width: 40, height: 40 }),
                  (new Date(medium.timestamp)).renderDate(),
                  BR(),
                  A({ href: '#' }, medium.title || medium.name));
    }

};

function loadPoi(poi) {
    document.title = poi.title;
    $('#hd h1').html(poi.title);
    $('#hd h2').html(poi.subtitle);
    $('#content').empty().html(poi.description);
    $('#media-list').empty();
    map(function (medium) {
        if (makeMediumMenuEntry[medium.mediumType]) {
            $('#media-list').append(makeMediumMenuEntry[medium.mediumType](medium));
        }
    }, poi.media);
}

function loadData(data) {
    var pois = data.pois;

    for (var i in pois) {
        if (pois[i].id == poi_id) {
            loadPoi(pois[i]);
            return;
        }
    }

    alert('invalid poi id (not found)');
}

function init() {
    poi_id = document.location.hash.replace(/#/, "");

    if (poi_id.match(/^[0-9]+$/)) {
        loadJSONDoc('/poi-json').addCallback(loadData);
    } else {
        alert('invalid poi id');
    }
}