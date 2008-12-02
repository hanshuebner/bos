$ = jQuery;

$(document).ready(init);

var googleMapKey = "ABQIAAAA5meUSZ1F7u46UjJHEXSlJhQkkdysj0TmG3bX_n9aMEXHvIwNeRQLmdjbjYpAetJRis7naMxi-fqMRQ";
var pois = {};
var sponsors = [];

Date.prototype.renderDate = function() {
    return this.getDate() + '.' + this.getMonth() + '.' + (this.getYear() > 2000 ? this.getYear() : (1900 + this.getYear()));
}

function NLS(key) {
    return key;                 // for now
}

function log2(x) {
    return Math.log(x) / Math.LN2;
}

var B = createDOMFunc('b', null);
var OBJECT = createDOMFunc('object');
var PARAM = createDOMFunc('param');
var EMBED = createDOMFunc('embed');
var APPLET = createDOMFunc('applet');

var mediaHandlers = {
    image: {
        icon: function (medium) {
            return IMG({ src: '/image/' + medium.id + '/thumbnail,,40,40', width: 40, height: 40 })
        },
        makeViewer: function (medium) {
            return IMG({ src: '/image/' + medium.id,
                         width: medium.width,
                         height: medium.height });
        }
    },
    panorama: {
        icon: function (medium) {
            return IMG({ src: '/static/panorama-icon.gif', width: 40, height: 40 })
        },
        makeViewer: function (medium) {
            return APPLET({ id: 'applet',
                            archive: '/static/ptviewer.jar',
                            code: 'ptviewer.class',
                            width: 400,
                            height: 300},
                          PARAM({ name: 'file', value: '/image/' + medium.id}),
                          PARAM({ name: 'cursor', value: 'MOVE' }));
        }
    },
    movie: {
        icon: function (medium) {
            return IMG({ src: '/static/movie-icon.gif', width: 40, height: 40 })
        },
        makeViewer: function (medium) {
            /* can't use DOM objects like below because IE does not grok it
             * return OBJECT({ id: 'applet',
             *                 width: 360, height: 360,
             *                 type: "application/x-shockwave-flash",
             *                 data: "c.swf?path=" + medium.url },
             *               PARAM({ name: "movie",
             *                       value: "c.swf?path=" + medium.url }));
             */
            var div = DIV();
            div.innerHTML =
                "<object id='applet' width='360' height='360' type='application/x-shockwave-flash' "
                + "data='c.swf?path=" + medium.url + "'>"
                + "<param name='movie' value='" + medium.url + "'/>"
                + "</object>";
            return div;

        }
    }
};

function showMedium(e) {
    var medium = e.data;

    /* Work around jQuery bug when trying to remove applet from DOM with IE. */
    var applet = $("#applet")[0];
    if (applet) {
        applet.parentNode.removeChild(applet);
    }

    $('#media-list *').removeClass('active');
    $(e.target).addClass('active');

    $('#content')
    .empty()
    .append(H2(null, medium.title),
            mediaHandlers[medium.mediumType].makeViewer(medium),
            H3(null, medium.subtitle),
            P(null, medium.description));
}

var SAT_MAP_SIZE = 28800;

function makePath(size, x, y) {
    var depth = log2(SAT_MAP_SIZE / size);
    var path = '';
    var xPos = 0;
    var yPos = 0;
    var currentSize = SAT_MAP_SIZE;
    for (var i = 0; i < Math.min(depth, 6); i++) {
        currentSize /= 2;
        var index
            = ((x > (xPos + currentSize)) ? 1 : 0)
            + ((y > (yPos + currentSize)) ? 2 : 0);
        if (index & 1) {
            xPos += currentSize;
        }
        if (index & 2) {
            yPos += currentSize;
        }
        path += index;
    }
    return path;
}

function makeMap(centerX, centerY) {
    var rows = [];
    
    for (var y = -1; y < 3; y++) {
        var tiles = [];
        for (var x = -1; x < 3; x++) {
            tiles.push(IMG({ 'class': 'map-tile',
                             src: '/overview/'
                             + (Math.floor(centerX / 90) + x) * 90
                             + '/'
                             + (Math.floor(centerY / 90) + y) * 90,
                             width: 90, height: 90 }));
        }
        rows.push(DIV(null, tiles));
    }

    return DIV(null, rows);
}

function positionMapIcon(img, x, y) {
    img.style.left = (x - (Math.floor(x / 90) - 1) * 90) + 'px';
    img.style.top = (y - (Math.floor(y / 90) - 1) * 90) + 'px';
    return img;
}

function loadMainInfo(poi) {

    $('#content')
    .empty()
    .append(H2(null, poi.subtitle),
            DIV({ 'class': 'map' },
                makeMap(poi.x, poi.y),
                positionMapIcon(IMG({ 'class': 'icon',
                                      src: '/images/' + poi.icon + '.gif',
                                      width: 16, height: 16}),
                                poi.x - 8, poi.y - 8)),
            P(null, poi.description));
}

function showPOI(e) {
    var poi = pois[(e.target && e.target.value) || e.data];


    $('#left-bar')
    .empty()
    .append(UL({ id: 'media-list' }));
    $('#poi-selector').val(poi.id);

    document.title = poi.title;
    $('.yui-b h1').html(poi.title);
    loadMainInfo(poi);
    map(function (medium) {
        if (mediaHandlers[medium.mediumType]) {
            $('#media-list')
            .append($(A({ href: '#' },
                        LI(null,
                           mediaHandlers[medium.mediumType].icon(medium),
                           (new Date(medium.timestamp)).renderDate(),
                           BR(),
                           B(null, medium.title || medium.name))))
                    .bind('click', medium, showMedium));
        }
    }, poi.media);
}

function pointToPath(point, level) {
    var x = point.x;
    var y = point.y;
    var path = '';
    for (var i = 0; i < level; i++) {
        path = ((x & 1) + ((y & 1) << 1)) + path;
        x >>= 1;
        y >>= 1;
    }
    return path;
}

function showGoogleMap() {
    var mapDiv = DIV({ id: 'google-map' });
    $('#content')

    .empty()
    .append(H2(null, NLS('Google Map')),
            mapDiv);

    $('#left-bar')
    .empty();

    var map = new GMap2(mapDiv);

    var copyright
        = new GCopyright(1,
                         new GLatLngBounds(new GLatLng(-90, -180), new GLatLng(90, 180)),
                         3,
                         "Copyright BOS Deutschland e.V.");
    var copyrightCollection = new GCopyrightCollection('Map');
    copyrightCollection.addCopyright(copyright);
    var tileLayers = [new GTileLayer(copyrightCollection, 0, 7)];
    var projection = new GMercatorProjection(7);
    tileLayers[0].getTileUrl = function(point, level) {
        if (level < 7) {
            var path = pointToPath(point, level);
            log('getTileUrl: x:' + point.x + ' y:' + point.y + ' level:' + level + ' path: ' + path);
            return '/simple-map/sl_utm50s-0?path=' + path;
        } else {
            return null;
        }
    }
    var customMap = new GMapType(tileLayers, projection, 'Map', { errorMessage: NLS("Keine Daten in dieser Zoomstufe") });
    map.addMapType(customMap);
    map.addControl(new GLargeMapControl());

    map.setCenter(new GLatLng(0, 0), 1, customMap);
}

var pages = {
    overview: showOverview,
    map: showGoogleMap
}

function selectPage(e) {
    var value = e.target.value;

    if (value.match(/^\d+/)) {
        showPOI(e);
    } else if (pages[value]) {
        pages[value](e);
    }
}

function showSponsor(e) {
    var sponsor = e.data;
    var contract = sponsor.contracts[0];
    $('#content')
    .empty()
    .append(H2(null, sponsor.name),
            DIV({ 'class': 'map' },
                makeMap(contract.left, contract.top),
                positionMapIcon(IMG({ 'class': 'contract',
                                      src: '/contract-image/' + contract.id,
                                      width: contract.width, height: contract.height}),
                                contract.left, contract.top))
           );
}

function showOverview() {

    $('#poi-selector').val('overview');
    
    var elements = [];
    elements.push(IMG({ src: '/infosystem/bilder/karte_uebersicht.jpg', width: 360, height: 360 }));
    for (var i in pois) {
        var poi = pois[i];
        var link = A({ href: '#' },
                     IMG({ 'class': 'icon',
                            src: '/images/' + poi.icon + '.gif',
                            width: 16, height: 16,
                            title: poi.title,
                            style: 'left: ' + (Math.round(poi.x / 30) - 8) + 'px; '
                                   + 'top: ' + (Math.round(poi.y / 30) - 8) + 'px' }));
        $(link).bind('click', poi.id, showPOI);
        elements.push(link);
    }

    $('#content')
    .empty()
    .append(H2(null, NLS('Übersicht')),
            DIV({ 'class': 'map' }, elements));

    $('#left-bar')
    .empty();
}

function showSponsors() {

    $('#left-bar')
    .empty()
    .append(H3(NLS("Letzte Sponsoren")),
            UL({ id: 'sponsor-list' }));

    map(function (sponsor) {
        $('#sponsor-list')
        .append($(A({ href: '#' },
                    LI(null,
                       IMG({ src: '/images/flags/' + sponsor.country.toLowerCase() + '.gif'}),
                       (new Date(sponsor.contracts[0].timestamp)).renderDate(),
                       BR(),
                       B(null, sponsor.anonymous ? NLS('anonym') : sponsor.name),
                       " ", sponsor.contracts[0].count, " m²")))
                .bind('click', sponsor, showSponsor));
    }, sponsors.slice(0, 10));
}

function loadSponsors(data) {
    try {
        for (var i in data.sponsors) {
            var sponsor = data.sponsors[i];
            sponsors.push(sponsor);
        }

        var poi_id = document.location.hash.replace(/#/, "");
        if (poi_id) {
            showPOI({ data: poi_id });
        } else {
            showOverview();
        }
    }
    catch (e) {
        alert(e);
    }
}

function loadPOIs(data) {
    try {
        for (var i in data.pois) {
            var poi = data.pois[i];
            pois[poi.id] = poi;
            $('#poi-selector').append(OPTION({ value: poi.id }, poi.title));
        }
        $('#poi-selector').bind('change', null, selectPage);

        loadJSONDoc('/sponsors-json').addCallback(loadSponsors);
    }
    catch (e) {
        alert(e);
    }
}

function init() {
    $('#small-map a').bind('click', showPOI);

    loadJSONDoc('/poi-json').addCallback(loadPOIs);
}