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
            return IMG({ src: 'panorama-icon.jpg', width: 40, height: 40 })
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
            return IMG({ src: 'film-icon.jpg', width: 40, height: 40 })
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
    var poi = e.data[0];
    var medium = e.data[1];

    mainMap.hide();

    /* Work around jQuery bug when trying to remove applet from DOM with IE. */
    var applet = $("#applet")[0];
    if (applet) {
        applet.parentNode.removeChild(applet);
    }

    $('#media-list *').removeClass('active');
    $(e.target).addClass('active');

    $('#title').text(poi.title);
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

function compareMedia (a, b) {
    if (a.mediumType == b.mediumType) {
        return (b.timestamp < a.timestamp) ? -1 : 1;
    } else {
        return (a.mediumType < b.mediumType) ? -1 : 1;
    }
}

function showPOI(poi) {
    if (poi.data) {
        poi = poi.data;
    }

    mainMap.poiDetail(poi.x, poi.y);

    $('#back').css('visibility', 'inherit');
    $('#left-bar')
    .empty()
    .append(UL({ id: 'media-list' }));
    $('#poi-selector').val(poi.id);

    document.title = poi.title;

    $('#title').text(poi.title);
            
    $('#content-body')
    .empty()
    .append(H2(null, poi.subtitle),
            P(null, poi.description));
    map(function (medium) {
        if (mediaHandlers[medium.mediumType]) {
            $('#media-list')
            .append($(LI(null,
                        A({ href: '#' },
                           mediaHandlers[medium.mediumType].icon(medium),
                           B(null, medium.title || medium.name))))
                    .bind('click', [ poi, medium ], showMedium));
        }
    }, poi.media.sort(compareMedia));

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
    $('#back').css('visibility', 'hidden');

    $('#title').text(NLS('Übersicht'));

    $('#content-body')
    .empty();

    $('#left-bar')
    .empty()
    .append(UL({ id: 'poi-list' }));

    for (var i in pois) {
        var poi = pois[i];
        $('#poi-list')
        .append($(LI(null,
                     A({ href: '#' },
                       IMG({ src: poi.mapIcon }),
                       B(poi.title))))
                .bind('click', poi, showPOI));
    }
    
    mainMap.overview();
}

function FakeMap() {

    this.hide = function () {
        $('#map').css('display', 'none');
    }

    this.overview = function () {
        $('#map').css('display', 'block');
    }

    var index = 0;
    for (var i in pois) {
        var poi = pois[i];
        var letter = String.fromCharCode("A".charCodeAt(0) + index++);

        poi.mapIcon = "http://www.google.com/mapfiles/marker" + letter + ".png";
    }
}

function Map() {
    this.map = new GMap2($('#map')[0]);

    var copyrightCollection = new GCopyrightCollection('Map');
    copyrightCollection
    .addCopyright(new GCopyright(1,
                                 new GLatLngBounds(new GLatLng(-90, -180), new GLatLng(90, 180)),
                                 3,
                                 "Copyright BOS Deutschland e.V."));

    this.layers = {};

    this.makeLayer = function (name) {
        var tileLayer = new GTileLayer(copyrightCollection, 0, 14);
        tileLayer.getTileUrl = function(point, level) {
            if (level < 15) {
                var path = pointToPath(point, level);
//                log('getTileUrl: x:' + point.x + ' y:' + point.y + ' level:' + level + ' path: ' + path);
                return '/simple-map/' + name + '?path=' + path;
            } else {
                return null;
            }
        }
        this.layers[name] = tileLayer;
        return tileLayer;
    }

    var projection = new GMercatorProjection(14);
    var customMap = new GMapType(
        [ this.makeLayer('sat-2002'),
          this.makeLayer('contracts') ],
        projection, 'Map', { errorMessage: NLS("Keine Daten in dieser Zoomstufe") });

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

//    this.map.enableContinuousZoom();
//    this.map.enableScrollWheelZoom();

    this.mapClicked = function (overlay, latlng, overlaylatlng) {
        log('map clicked, overlay: ' + overlay + ' latlng: ' + latlng + ' overlaylatlng: ' + overlaylatlng);
    }

    function latLngToPoint(latLng) {
        return projection.fromLatLngToPixel(latLng, 6);
    }

    this.moveEnd = function () {
        var bounds = this.map.getBounds();
        var sw = latLngToPoint(bounds.getSouthWest());
        var ne = latLngToPoint(bounds.getNorthEast());
        log('map has moved: ' + sw.x + ',' + ne.y + ',' + ne.x + ',' + sw.y);

        this.sponsorQuery = sw.x + ',' + ne.y + ',' + ne.x + ',' + sw.y;
    }

    GEvent.addListener(this.map, "click", bind(this.mapClicked, this));
    GEvent.addListener(this.map, "moveend", bind(this.moveEnd, this));

    this.overview = function () {
        this.show();
        $('#map').removeClass('small');
        $('#map').addClass('large');
        this.addControls();
        this.map.checkResize();
        this.map.setCenter(projection.fromPixelToLatLng(new GPoint(6500, 6350), 6), 2, customMap);
    }

    this.poiDetail = function (x, y) {
        $('#map').removeClass('large');
        $('#map').addClass('small');
        this.removeControls();
        this.map.checkResize();
        this.map.setCenter(projection.fromPixelToLatLng(new GPoint(x, y), 6), 6);
    }

    this.zoomTo = function (x, y, level) {
        this.map.setCenter(projection.fromPixelToLatLng(new GPoint(x, y), 6), level);
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

    var poiBaseIcon = new GIcon(G_DEFAULT_ICON);

    poiBaseIcon.shadow = "http://www.google.com/mapfiles/shadow50.png";
    poiBaseIcon.iconSize = new GSize(20, 34);
    poiBaseIcon.shadowSize = new GSize(37, 34);
    poiBaseIcon.iconAnchor = new GPoint(9, 34);
    poiBaseIcon.infoWindowAnchor = new GPoint(9, 2);

    var index = 0;
    for (var i in pois) {
        var poi = pois[i];
        var letter = String.fromCharCode("A".charCodeAt(0) + index++);
        var letteredIcon = new GIcon(poiBaseIcon);

        poi.mapIcon = 
        letteredIcon.image = "http://www.google.com/mapfiles/marker" + letter + ".png";

        var marker = new GMarker(pointToLatLng(poi.x, poi.y), { icon: letteredIcon });
        GEvent.addListener(marker, "click", partial(showPOI, poi));
        this.map.addOverlay(marker);
    }

    this.sponsorMarkers = [];

    this.setSponsorMarker = function (sponsor) {
        var position = pointToLatLng(sponsor.contracts[0].centerX, sponsor.contracts[0].centerY);
        var sponsorMarker = new GMarker(position);
        this.map.addOverlay(sponsorMarker);
        this.sponsorMarkers.push(sponsorMarker);
    }

    this.removeSponsorMarkers = function () {
        try {
            map(bind(this.map.removeOverlay, this.map), this.sponsorMarkers);
            this.sponsorMarkers = [];
        }
        catch (e) {
            log('error removing sponsor markers: ' + e);
        }
    }

    this.startMapMovedChecker = function () {
    }

    this.putSponsorPlacemarks = function(data) {
        log('got ' + data.sponsors.length + ' sponsors to display');
        this.removeSponsorMarkers();
        try {
            map(bind(this.setSponsorMarker, this), data.sponsors);
            this.checkMapMoved();
        }
        catch (e) {
            log('error removing sponsor markers: ' + e);
        }
    }

    this.checkMapMoved = function() {
        if (this.sponsorQuery) {
            loadJSONDoc('/sponsors-json?at=' + this.sponsorQuery)
            .addCallback(bind(this.putSponsorPlacemarks, this));
            this.sponsorQuery = null;
        } else {
            callLater(0.5, bind(this.checkMapMoved, this));
        }
    }

    this.checkMapMoved();
}

var pages = {
    overview: showOverview,
    sponsors: showSponsors
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

    mainMap.zoomTo(contract.left, contract.top, 8);
    mainMap.setSponsorMarker(sponsor);
    
    // Math.max(contract.width, contract.height)
}

function showSponsors() {

    $('#left-bar')
    .empty()
    .append(H3(NLS("Letzte Sponsoren")),
            UL({ id: 'sponsor-list' }));

    map(function (sponsor) {
        $('#sponsor-list')
        .append($(LI(null,
                     A({ href: '#' },
                       IMG({ src: '/images/flags/' + sponsor.country.toLowerCase() + '.gif'}),
                       (new Date(sponsor.contracts[0].timestamp)).renderDate(),
                       BR(),
                       B(null, sponsor.anonymous ? NLS('anonym') : sponsor.name),
                       " ", sponsor.contracts[0].count, " m²")))
                .bind('click', sponsor, showSponsor));
    }, sponsors.slice(0, 10));

    mainMap.overview();
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
        }

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