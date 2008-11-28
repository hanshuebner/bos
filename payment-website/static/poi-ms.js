$ = jQuery;

$(document).ready(init);

var poi_id;
var poi;

Date.prototype.renderDate = function() {
    return this.getDate() + '.' + this.getMonth() + '.' + (1900 + this.getYear());
}

var B = createDOMFunc('b', null);
var OBJECT = createDOMFunc('object');
var PARAM = createDOMFunc('param');
var EMBED = createDOMFunc('embed');
var APPLET = createDOMFunc('applet');

var mediaHandlers = {
    image: {
        icon: function (medium) { return IMG({ src: '/image/' + medium.id, width: 40, height: 40 }) },
        action: function (medium) {
            $('#content')
            .empty()
            .append(H2(null, medium.title),
                    IMG({ src: '/image/' + medium.id }), BR(),
                    H3(null, medium.subtitle),
                    P(null, medium.description));
        }
    },
    panorama: {
        icon: function (medium) { return IMG({ src: '/static/panorama-icon.gif', width: 40, height: 40 }) },
        action: function (medium) {
            $('#content')
            .empty()
            .append(H2(null, medium.title),
                    APPLET({ archive: '/static/ptviewer.jar',
                             code: 'ptviewer.class',
                             width: 400,
                             height: 300},
                           PARAM({ name: 'file', value: '/image/' + medium.id}),
                           PARAM({ name: 'cursor', value: 'MOVE' })),
                    H3(null, medium.subtitle),
                    P(null, medium.description));
        }
    },
    movie: {
        icon: function (medium) { return IMG({ src: '/static/movie-icon.gif', width: 40, height: 40 }) },
        action: function (medium) {
            $('#content')
            .empty()
            .append(H2(null, medium.title),
                    OBJECT({ width: 360, height: 360 },
                           PARAM({ name: "movie", value: medium.url }),
                           EMBED({ src: medium.url, type: 'application/x-shockwave-flash',
                                   width: 400, height: 300 })), BR(),
                    H3(null, medium.subtitle),
                    P(null, medium.description));
        }
    }
};

function selectMedium(fn, e) {
    $('#media-list *').removeClass('active');
    $(e.target).addClass('active');
    fn();
}

function loadMainInfo() {
    var map = [];
    for (var y = -1; y < 3; y++) {
        var tiles = [];
        for (var x = -1; x < 3; x++) {
            tiles.push(IMG({ 'class': 'map-tile',
                             src: '/overview/'
                             + (Math.floor(poi.x / 90) + x) * 90
                             + '/'
                             + (Math.floor(poi.y / 90) + y) * 90,
                             width: 90, height: 90 }));
        }
        map.push(DIV(null, tiles));
    }
    map.push(IMG({ 'class': 'icon',
                   src: '/images/' + poi.icon + '.gif',
                   style: 'left: ' + (poi.x - ((Math.floor(poi.x / 90) - 1) * 90) - 8) + 'px; '
                                   + 'top: ' + (poi.y - ((Math.floor(poi.y / 90) - 1) * 90) - 8) + 'px'}));

    $('#content').empty().append(H2(null, poi.subtitle),
                                 DIV({ 'class': 'map' }, map),
                                 P(null, poi.description));
}

var B = createDOMFunc('b', null);

function loadPoi() {
    document.title = poi.title;
    $('.yui-b h1').html(poi.title);
    loadMainInfo();
    $('#media-list').empty();
    map(function (medium) {
        if (mediaHandlers[medium.mediumType]) {
            $('#media-list')
            .append($(A({ href: '#' },
                        LI(null,
                           mediaHandlers[medium.mediumType].icon(medium),
                           (new Date(medium.timestamp)).renderDate(),
                           BR(),
                           B(null, medium.title || medium.name))))
                    .bind('click', null, partial(selectMedium, partial(mediaHandlers[medium.mediumType].action, medium))));
        }
    }, poi.media);
}

function loadData(data) {
    try {
        var pois = data.pois;

        for (var i in pois) {
            if (pois[i].id == poi_id) {
                poi = pois[i];
                loadPoi();
                return;
            }
        }

        alert('invalid poi id (not found)');
    }
    catch (e) {
        alert(e);
    }
}

function init() {
    poi_id = document.location.hash.replace(/#/, "");

    if (poi_id.match(/^[0-9]+$/)) {
        loadJSONDoc('/poi-json').addCallback(loadData);
    } else {
        alert('invalid poi id');
    }
}