$ = jQuery;

$(document).ready(init);

var pois = {};

Date.prototype.renderDate = function() {
    return this.getDate() + '.' + this.getMonth() + '.' + (this.getYear() > 2000 ? this.getYear() : (1900 + this.getYear()));
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
        makeViewer: function (medium, container) {
            replaceChildNodes(container,
                              IMG({ src: '/image/' + medium.id,
                                    width: medium.width,
                                    height: medium.height }));
        }
    },
    panorama: {
        icon: function (medium) {
            return IMG({ src: '/static/panorama-icon.gif', width: 40, height: 40 })
        },
        makeViewer: function (medium, container) {
            replaceChildNodes(container,
                              APPLET({ id: 'applet',
                                       archive: '/static/ptviewer.jar',
                                       code: 'ptviewer.class',
                                       width: 400,
                                       height: 300},
                                     PARAM({ name: 'file', value: '/image/' + medium.id}),
                                     PARAM({ name: 'cursor', value: 'MOVE' })));
        }
    },
    movie: {
        icon: function (medium) {
            return IMG({ src: '/static/movie-icon.gif', width: 40, height: 40 })
        },
        makeViewer: function (medium, container) {
            /* can't use DOM objects like below because IE does not grok it
             * return OBJECT({ id: 'applet',
             *                 width: 360, height: 360,
             *                 type: "application/x-shockwave-flash",
             *                 data: "c.swf?path=" + medium.url },
             *               PARAM({ name: "movie",
             *                       value: "c.swf?path=" + medium.url }));
             */
            container.innerHTML =
                "<object id='applet' width='360' height='360' type='application/x-shockwave-flash' "
                + "data='c.swf?path=" + medium.url + "'>"
                + "<param name='movie' value='" + medium.url + "'/>"
                + "</object>";

        }
    }
};

function showMedium(e) {
    var medium = e.data;

    /* work around jQuery bug when trying to remove applet from dom with IE */
    var applet = $("#applet")[0];
    if (applet) {
        applet.parentNode.removeChild(applet);
    }

    $('#media-list *').removeClass('active');
    $(e.target).addClass('active');

    var container = DIV();
    mediaHandlers[medium.mediumType].makeViewer(medium, container);

    $('#content')
    .empty()
    .append(H2(null, medium.title),
            container,
            H3(null, medium.subtitle),
            P(null, medium.description));
}

function loadMainInfo(poi) {
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
                   width: 16, height: 16,
                   style: 'left: ' + (poi.x - ((Math.floor(poi.x / 90) - 1) * 90) - 8) + 'px; '
                                   + 'top: ' + (poi.y - ((Math.floor(poi.y / 90) - 1) * 90) - 8) + 'px'}));

    $('#content').empty().append(H2(null, poi.subtitle),
                                 DIV({ 'class': 'map' }, map),
                                 P(null, poi.description));
}

function showPOI(e) {
    var poi = pois[(e.target && e.target.value) || e.data];

    $('#media-list').empty();
    if (!poi) {
        showOverview();
    } else {
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
}

function showOverview() {

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
    .append(H2(null, 'XXuebersichtXX'),
            DIV({ 'class': 'map' }, elements));
}

function loadData(data) {
    try {
        for (var i in data.pois) {
            var poi = data.pois[i];
            pois[poi.id] = poi;
            $('#poi-selector').append(OPTION({ value: poi.id }, poi.title));
        }
        $('#poi-selector').bind('change', null, showPOI);

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

function init() {
    loadJSONDoc('/poi-json').addCallback(loadData);
}