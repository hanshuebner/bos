$ = jQuery;

$(document).ready(init);

var googleMapKey = "ABQIAAAA5meUSZ1F7u46UjJHEXSlJhQkkdysj0TmG3bX_n9aMEXHvIwNeRQLmdjbjYpAetJRis7naMxi-fqMRQ";
var pois = {};
var sponsors = [];
var mainMap;

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

    $('#content-body')
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

function loadMainInfo(poi) {

    $('#content-body')
    .empty()
    .append(H2(null, poi.subtitle),
            P(null, poi.description));
}

function showPOI(poi) {

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

    mainMap.hide();
//    mainMap.zoomTo(poi.x, poi.y);
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

function showOverview() {
    $('#content-body')
    .empty()
    .append(H2(null, NLS('Google Map')));

    $('#left-bar')
    .empty();

    mainMap.overview();
}

function Map() {
    this.map = new GMap2($('#map')[0]);

    var copyright
        = new GCopyright(1,
                         new GLatLngBounds(new GLatLng(-90, -180), new GLatLng(90, 180)),
                         3,
                         "Copyright BOS Deutschland e.V.");
    var copyrightCollection = new GCopyrightCollection('Map');
    copyrightCollection.addCopyright(copyright);
    var tileLayers = [new GTileLayer(copyrightCollection, 0, 12)];
    var projection = new GMercatorProjection(12);
    tileLayers[0].getTileUrl = function(point, level) {
        if (level < 15) {
            var path = pointToPath(point, level);
            log('getTileUrl: x:' + point.x + ' y:' + point.y + ' level:' + level + ' path: ' + path);
            return '/simple-map/contracts?path=' + path;
        } else {
            return null;
        }
    }
    var customMap = new GMapType(tileLayers, projection, 'Map', { errorMessage: NLS("Keine Daten in dieser Zoomstufe") });

    this.map.addMapType(customMap);

    this.controls = [ new GLargeMapControl() ];

    this.addControls = function() {
        for (var i in this.controls) {
            this.map.addControl(this.controls[i]);
        }
    }
    this.removeControls = function() {
        for (var i in this.controls) {
            this.map.removeControl(this.controls[i]);
        }
    }

    this.map.enableContinuousZoom();
    this.map.enableScrollWheelZoom();

    this.overview = function () {
        this.show();
        $('#map').removeClass('small');
        $('#map').addClass('large');
        this.addControls();
        this.map.setCenter(projection.fromPixelToLatLng(new GPoint(7000, 6350), 6), 3, customMap);
        this.map.checkResize();
    }

    this.zoomTo = function (x, y) {
        $('#map').removeClass('large');
        $('#map').addClass('small');
        this.removeControls();
        this.map.setCenter(projection.fromPixelToLatLng(new GPoint(x, y), 6), 6);
        this.map.checkResize();
    }

    this.hide = function () {
        $('#map').css('display', 'none');
    }

    this.show = function () {
        $('#map').css('display', 'block');
    }

    this.overview();

    function pointToLatLng(x, y) {
        return projection.fromPixelToLatLng(new GPoint(x, y), 6);
    }

    for (var i in pois) {
        var marker = new GMarker(pointToLatLng(pois[i].x, pois[i].y));
        GEvent.addListener(marker, "click", partial(showPOI, pois[i]));
        this.map.addOverlay(marker);
    }
}

var pages = {
    overview: showOverview,
}

function selectPage(e) {
    var value = e.target.value;

    if (value.match(/^\d+/)) {
        showPOI(pois[value]);
    } else if (pages[value]) {
        pages[value](e);
    }
}

function showSponsor(e) {
    var sponsor = e.data;
    var contract = sponsor.contracts[0];
    $('#content-body')
    .empty()
    .append(H2(null, sponsor.name));
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
            showPOI(pois[poi_id]);
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

        mainMap = new Map();

        loadJSONDoc('/sponsors-json').addCallback(loadSponsors);
    }
    catch (e) {
        alert(e);
    }
}

function init() {
    $('#small-map a').bind('click', showOverview);

    loadJSONDoc('/poi-json').addCallback(loadPOIs);
}