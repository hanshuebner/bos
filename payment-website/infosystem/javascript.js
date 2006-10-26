// JavaScript Document -*- Java -*-

// Originally written by Matthias, Systemtakt neue Medien GbR
// This program needs a lot of refactoring.

// XXX bei klick auf Ã¼bersichtskarte bleiben die links der poi-thumbnails aktiv
// XXX beim schliessen des opener-fensters funktioniert "m2 kaufen" nicht mehr

// Debugger anzeigen?

var show_debugger = false;

// var http_pfad = "http:// createrainforest.org:8080";
var http_pfad = "";

// Initialisieren aller globalen Vaiablen
var aktuelles_objekt = "uebersicht"; // das aktuelle aktivierte Objekt

// Hilfe-Fenster
var helpwin;

var isNav; // ob der Browser = Navigator
var isIE; // ob Browser = IExplorer
var mouse_button = false; // ob ein Mousebutton gedrueckt ist

var start_ueberschrift = ""; // Überschrift der Startseite
var erzeugte_positionen = 0; // Anzahl der erzeugten Positionen in der Vergroeßerung, wird zum Loeschen der entsprechenden Layer benoetiget
var erzeugte_zeilen = 0; // Anzahl der erzeugten Quadratmeter in der Vergroeßerung, wird zum Loeschen der entsprechenden Layer benoetiget
var n_zeilen = 0; // Anzahl der erzeugten Nachbar-Quadratmeter in der Vergroeßerung, wird zum Loeschen der entsprechenden Layer benoetiget

// Arrays, die Daten werden jeweils erst spaeter und nach bedarf gelden
var poi = new Array; // Array in dem die Daten fuer die POI gespeichert werden
var qm = new Array; // Array in dem die Daten fuer die QM gespeichert werden
var uebersicht_icons = new Array;  // Array in dem die Daten fuer die Icons der Übersichtskarte gespeichert werden
var profil = new Array; // Array in dem die Daten fuer das Profil gespeichert werden
var n_profil = new Array; // Array in dem die Daten fuer das Nachbar-Profil gespeichert werden

var loginstatus = false; // Status ob Anwender eingeloggt sind wird ueber login_pruefen() gefuellt
var variablen=document.location.search; // string der angehangenen Variablen in der URL
var sponsorid = ""; // Variable des Inputfeldes wird durch init() gefuellt
var passwort = "";
var logout = "0"; // Variable der URL wird durch init() gefuellt
var login = 1; // Anzeiger ob der Anwender sich ein loggen wollte
var timer = 0; // Laufvariable fuer Timeout bei Ladefuntkionen

var anzahlSponsoren = 0;
var anzahlVerkauft = 0;

var poicomplete = false; // Endwert der poi_laden() - Funktion wenn der Wert auf true gestellt ist ist der Datensatz komplett geladen
var m2complete = false; // Endwert der qm_laden() - Funktion wenn der Wert auf true gestellt ist ist der Datensatz komplett geladen

var x_ausschnitt = 0; // X-Anfangswert der Lupe
var y_ausschnitt = 0;

var x_anf = 0; // X-Anfangswert der Ausschnittskarte, beschreibt die erste Kachel
var y_anf = 0; // >-Anfangswert der Ausschnittskarte, beschreibt die erste Kachel

var debugger_text = ""; // Text der verschiedene Stati ausgibt
var angezeigt = "uebersicht";

var all_layers = [ 'background', 'areas', 'contracts' ];
var active_layers = [];

// Browserweiche
if (parseInt(navigator.appVersion) >= 4) {
    if (navigator.appName == "Netscape") {
	isNav = true;
    } else {
	isIE = true;
    }
}

// Internationalisierung
function msg(key)
{
    if (document.messages && document.messages[key]) {
	return document.messages[key];
    } else {
	return key;
    }
}

function dbg(text) {
    // Schriebt einen Text in die Debugger-Ebene
    debugger_text = debugger_text + text;
    document.getElementById("debugger").innerHTML = debugger_text;
    return true;
}

function faehnchen_einblenden(x, y, text) {
    var align = "left";

    document.getElementById("Faehnchen").style.left = x + "px";
    document.getElementById("Faehnchen").style.top = y + "px";
    document.getElementById("Faehnchen").style.visibility = "visible";
    document.getElementById("Faehnchen").innerHTML = '<span align="' + align + '" class="Faehnchen">' + text + '</span>';
}

function faehnchen_ausblenden() {
    document.getElementById("Faehnchen").style.visibility = "hidden";
}

function UebersichtNavi() {

    var navitext = "";
    var i = 1;

    while (poi[i]) {
	if(poi[i]['thumbnail'] != 0) {		
	    if (aktuelles_objekt == i) {
		navitext += "- <span class='PoiNavigationAktiv'>" + poi[i]['name'] + "</span><br/>";
	    } else {
		navitext += "<a href='#' onClick='PoiDetail_anzeigen(" + i + ");' class='PoiNavigation'>"
		 + poi[i]['name'] + "</a><br/>";
	    }
	}
	i++;
    }
    document.getElementById("UebersichtPoiNavi").innerHTML = navitext;
}

function init() {
    poicomplete = false;
    loginstatus = false;

    document.language = document.location.href;
    document.language = document.language.replace(/.*\/(..)\/[^\/]+$/, "$1");

    for (var i = 0; i < all_layers.length; i++) {
	active_layers[all_layers[i]] = true;
    }

    if (!document.cookie) {
	alert(msg("Diese Anwendung benÃ¶tigt Cookies, um zu funktionieren.  Bitte schalten Sie Cookies in Ihrem Browser ein."));
	close();
    }
    
    dbg("<br/> init() <br/>");
    // initialisierung startet die Ladefuntkionen
    // parst den URL-String und trennt logout, sponsorid und passwort
    // Debugger anzeigen oder ausblenden
    if (show_debugger) {
	document.getElementById("debugger").style.visibility = "visible";
    }

    start_ueberschrift = document.getElementById("Ueberschrift").innerHTML;

    login_pruefen();
    // Funktion zum POI laden starten
    return true;
}

function poi_laden() {
    // poi laden
    // URL wird in ein script geladen, damit stehen die daten aus der URL zur Verfuegung

    timer = 0;

    dbg("<br/> -> lade POI");
    poicomplete = false;
    window.frames['data'].window.location.replace(http_pfad + "/poi-javascript");
    poi_warten(); // starten der Wartenfunktion

    return true;
}

function bestellung () {
    window.opener.document.location = "/" + document.language + "/bestellung";
    window.opener.focus();
}

function poi_fertig(_poi, _anzahlSponsoren, _anzahlVerkauft) {
    poi = _poi;
    poi.unshift(0);		// adjust for base 1 arrays
    anzahlSponsoren = _anzahlSponsoren;
    anzahlVerkauft = _anzahlVerkauft;
    poicomplete = true;
}

function poi_warten() {
    // Wartefunktion, da das Laden etwas traege ist wartet dieses Script bis derf Datensatz komplatt geladen ist
    // poicomplet ist dei letzte Variable im Script daher wenn sie gesetzt ist ist das Ende erreicht
    if (poicomplete) {
	// wenn der Datensatz komplett geladen ist wird der timer auf Null gesetzt und je nachdem ob sich eingeloggt wurde oder nicht die loginueberpruefung oder die Punkterzeugung gestartet
	dbg("<br/> -> <b>POI geladen! login: " + login + "</b>");
	document.getElementById("Info3Text").innerHTML = '<b>' + msg('Anzahl Sponsoren') + '</b><br />'
	 + anzahlSponsoren
	 + '<br /><br /><b>' + msg('Anzahl verkaufte mÂ²') + '</b><br />'
	 + anzahlVerkauft;
	if (profil['name']) {
	    document.getElementById("SponsorInfoText").innerHTML = "<b>" + profil['name'] + "</b>";
	}
	UebersichtNavi();
	qm_zusammenfassen();
	icon_versatz(); 
	poi_faehnchen_erzeugen();
    } else {
	// wenn der Datensatz noch nicht komplett geladen ist wird der timer eroeht und die Funktion nochmal gestartet
	timer++; 
	if (timer < 100) {
	    dbg("."); 
	    setTimeout("poi_warten()", 100);
	} else {
	    dbg("<br/> -> <b>POI konnten nicht geladen werden</b>");
	    alert(msg('Fehler beim Laden der POI-Informationen, bitte probieren Sie es spÃ¤ter noch einmal'));
	}
    }
    return true;
}

function set_loginstatus(_loginstatus) {
    loginstatus = _loginstatus;
}

function login_pruefen() {

    // login pruefung

    timer = 0;
    var user = document.form0.__sponsorid.value;
    var password = document.form0.__password.value;

    var current_url = '' + document.location;

    if (user == '' && current_url.match(/__sponsorid/)) {
	user = current_url.replace(/.*__sponsorid=([^?]*).*/, "$1");
	password = current_url.replace(/.*__password=([^?]*).*/, "$1");
    }

    var url = http_pfad + "/sponsor-login";
    if (user != "") {
	url += "?__sponsorid=" + user + "&__password=" + password;
    }
    loginstatus = undefined;
    window.frames['data'].window.location.replace(url);

    dbg("<br/> -> lade Login-Status - url ist " + url + '<br/>');

    login_warten(); // Wartefunktion starten

    return false;
}

function do_login() {
    Uebersicht_anzeigen();
    login_pruefen();
}

function login_warten() {
    // Wartefunktion, da das Laden etwas traege ist wartet dieses Script bis derf Datensatz komplatt geladen ist
    // wenn loginstatus gesetzt ist ist das Ende erreicht

    if (loginstatus) { 
	dbg("<br/> -> <b>Login-Status geladen: " + loginstatus + "</b>");
	// wenn loginstatus gesetzt ist wir timer auf Null gesetzt
	// wenn lohinstatus = "login-failed" ist wird eine Fehlermeldung eingeblendet
	if (loginstatus == "not-logged-in") {
	    dbg("<br/> -> <b>nicht eingeloggt!</b>");
	}

	if (loginstatus == "login-failed") {
	    document.getElementById("Loginfehler").style.visibility = 'visible';
	    dbg("<br/> -> <b>Login fehlgeschlagen!</b>");
	}
	// wenn lohinstatus = "logged-in" ist wird das Anmeldefeld ausgelendet, das Logoutfeld eingeblendet und die Sponsorid angezeigt
	// danach werden die Punkte erzuegt und die Quadratmeter geladen
	if (loginstatus == "logged-in") {
	    document.getElementById("Anmelden").style.visibility = "hidden";
	    document.getElementById("SponsorInfo").style.visibility = "visible";
	    dbg("<br/> -> <b>Login erfolgreich!</b>");
	} else {
	    document.getElementById("Anmelden").style.visibility = "visible";
	    document.getElementById("SponsorInfo").style.visibility = "hidden";
	}
	qm_laden();
	
    } else {
	// wenn der Datensatz noch nicht komplett geladen ist wird der timer eroeht und die Funktion nochmal gestartet	
	timer++; 
	if (timer < 100) {
	    dbg("."); 
	    setTimeout("login_warten()", 100);
	} else {
	    dbg("<br/> -> <b>Loginstatus konnten nicht geladen werden</b>");
	}
    }
    return true;
}

function ausloggen() {
    // Seesion loeschen -> ausloggen
    window.frames['data'].window.location.replace(http_pfad + "/logout");
    dbg("<br/> -> ausloggen");
    qm_laden();
    return true;
}

function qm_laden() {
    // Quadratmeter laden
    // URL wird in ein script geladen, damit stehen die daten aus der URL zur Verfuegung		
    timer = 0;

    profil_variable = 'profil';

    m2complete = false;
    window.frames['data'].window.location.replace(http_pfad + "/m2-javascript/");

    dbg("<br/> -> lade Quadratmeter ");
    qm_warten(); // Wartefunktion starten
    return true;
}

function qm_warten() {
    // Wartefunktion, da das Laden etwas traege ist wartet dieses Script bis derf Datensatz komplatt geladen ist
    // m2complete ist die letzte Variable im Script daher wenn sie gesetzt ist ist das Ende erreicht
    if (m2complete) {
	dbg("<br/> -> <b>Quadratmeter geladen!</b>");		
	poi_laden();
    } else {
	// wenn der Datensatz noch nicht komplett geladen ist wird der timer eroeht und die Funktion nochmal gestartet
	timer++; 
	if (timer < 100) {
	    dbg("."); 
	    setTimeout("qm_warten()", 100);
	} else {
	    dbg("<br/> -> <b>qm konnten nicht geladen werden</b>");
	}
    }
    return true;
}

var profil_variable;

function qm_fertig(_profil) {
    if (_profil) {
	eval(profil_variable + " = _profil;");
    }
    m2complete = true;
}

function n_qm_laden() {
    // laden der Nachbar-Quadratmeter
    // URL wird in ein script geladen, damit stehen die daten aus der URL zur Verfuegung		

    // der Datensatz wird vorher auf Nullwerte gesetzt damit fals keine Daten in der URL enthalten sind der Quadratmeter als unverkauft angezeigt wird
    m2complete = false;
    timer=0;
    n_profil = {
	name: msg("noch nicht verkauft")
    };

    profil_variable = 'n_profil';

    m2complete = false;
    window.frames['data'].window.location.replace(http_pfad + "/m2-javascript/" + fremd_x + "/" + fremd_y);
    n_qm_warten(); // Wartefunktion starten
    dbg("<br/> -> lade Nachbar-Quadratmeter (" + fremd_x + "/" + fremd_y + ")");		
    return true;
}

function n_qm_warten() {
    // Wartefunktion, da das Laden etwas traege ist wartet dieses Script bis derf Datensatz komplatt geladen ist
    // m2complete ist die letzte Variable im Script daher wenn sie gesetzt ist ist das Ende erreicht
    if (m2complete) {
	// timer wird auf Nullgesetzt und display_selected_contract wird gestartet
	timer = 0;
	dbg("<br/> -> <b>Nachbar-Quadratmeter geladen!</b>");		
	// text fuer das Nachbarprofil wird zusammengesetzt
	if (n_profil['name'] == msg("noch nicht verkauft")) {
	    var text = '<table width="155" border="0" cellspacing="0" cellpadding="0"><tr><td colspan="2" class="PoiNavigation">'
		+ msg('Dieser mÂ² wurde bisher noch nicht verkauft') + '</td></tr></table>';
	} else {	
	    var text = '<table width="155" border="0" cellspacing="0" cellpadding="0"><tr><td width="60" class="PoiNavigation">'
		+ msg('Sponsor-ID') + ':</td><td class="PoiNavigation">'
		+ n_profil['id']
		+ '</td></tr><tr><td width="60" class="PoiNavigation">'
		+ msg('Name') + ':</td><td class="PoiNavigation">'
		+ n_profil['name']
		+ '</td></tr><tr> <td width="60" class="PoiNavigation">'
		+ msg('Land') + ':</td><td class="PoiNavigation">'
		+ n_profil['country']
		+ '</td></tr><tr> <td colspan="2" class="PoiNavigation"><img src="/infosystem/bilder/spacer.gif" width="1" height="10"/></td></tr>'
		+ '<tr> <td width="60" class="PoiNavigation">' + msg('gesponsort') + ':</td><td class="PoiNavigation">'
		+ n_profil['anzahl']
		+ ' mÂ²</td></tr><tr> <td width="60" class="PoiNavigation">'
		+ msg('seit') + ':</td><td class="PoiNavigation">' + n_profil.contracts[0].date
		+ '</td></tr><tr> <td colspan="2" class="PoiNavigation"><img src="/infosystem/bilder/spacer.gif" width="1" height="20"/></td></tr>'
		+ '<tr> <td colspan="2" class="PoiNavigation">'
		+ n_profil['nachricht']
		+ '</td></tr></table>';
	}
	// Inhalt der Ueberschrift und des Infotextes werden gesetzt
	document.getElementById("qmLaden").style.visibility = "hidden";
	if (n_profil.contracts) {
	    document.getElementById("Ueberschrift").innerHTML = msg("Verkaufte mÂ²");
	} else {
	    document.getElementById("Ueberschrift").innerHTML = msg("zu verkaufen!");
	}
	document.getElementById("qmInfoText").innerHTML = text;
	display_selected_contract();
    } else {
	// wenn der Datensatz noch nicht komplett geladen ist wird der timer eroeht und die Funktion nochmal gestartet	
	timer++; 
	if (timer < 100) {
	    setTimeout("n_qm_warten()", 100);
	} else {
	    document.getElementById("qmLaden").style.visibility = "hidden"; dbg("<br/> -> <b>Nachbar-Quadratmeter konnten nicht geladen werden</b>");
	}
    }
    return true;
}

function load_contract_image(contract, image, factor, color) 
{
    var container = image.parentNode;

    if (!color) {
	color = 'ffff00';
    }

    container.style.visibility = 'hidden';
    image.onload = function () {
	this.parentNode.style.visibility = 'inherit';
    }
    image.src = '/contract-image/' + contract.id + '/' + color;
    image.width = contract.width * factor;
    image.height = contract.height * factor;

    // Falls der Vertrag aus dem angezeigten Bereich herausragt, wird das bild entsprechend geclipped.
    container.style.clip
	= 'rect('
	+ Math.max(0,  y_anf - contract.top) * factor + 'px '
	+ Math.min(contract.width, contract.width - (contract.left + contract.width - x_anf - 360)) * factor + 'px '
	+ Math.min(contract.height, contract.height - (contract.top + contract.height - y_anf - 360)) * factor + 'px '
	+ Math.max(0, x_anf - contract.left) * factor + 'px'
	+ ')';

    container.style.left = (contract.left - x_anf) * factor + 'px';
    container.style.top = (contract.top - y_anf) * factor + 'px';

}

function display_selected_contract()
{
    // Anzeigen der ausgewÃ¤hlten Nachbarquadratmeter

    if (n_profil.contracts) {
	var contract = n_profil.contracts[0];

	load_contract_image(contract,
			    document.getElementById('selected_contract_img'),
			    1);
	load_contract_image(contract,
			    document.getElementById('lupe_selected_contract_img'),
			    5);
    } else {
	document.getElementById('selected_contract_img').src = '../bilder/spacer.gif';
	document.getElementById('lupe_selected_contract_img').src = '../bilder/spacer.gif';
    }
}

function display_own_sqm()
{
    var contract = profil.contracts[0];
    var img = document.getElementById('own_contract_img');
    var enlarged_image = document.getElementById('lupe_own_contract_img');

    load_contract_image(contract, img, 1, "ff0000");
    load_contract_image(contract, enlarged_image, 5, "ff0000");
}

function qm_zusammenfassen() {
    // zusammenfassen mehererer Quadratmeterfähnchen zu einem Fähnchen.
    // es wird geprüft, ob sich auf der Detailkarte des qm noch mehr qm anzeigen lassen dadurch wird die Darstellung der Fähnchen vereinfacht

    return;
    
    var i=1;
    while (qm[i]) {
	var qmV = qm[i];
	if (!qmV['status']) {
	    qmV['status'] = "mitte"
		// Bestimmen des Mittelpunktes der von diesem qm dargestllten Kacheln
		var Kachel_x = parseInt((Math.floor(qmV['x'] / 90) * 90));
	    var Kachel_y = parseInt((Math.floor(qmV['y'] / 90) * 90));
	    if (Kachel_x < 180) {Kachel_x = 180};
	    if (Kachel_y < 180) {Kachel_y = 1800};
	    if (Kachel_x > 10620) {Kachel_x = 10620};
	    if (Kachel_y > 10620) {Kachel_y = 10620};
			
	    // Ermitteln des maximalen und minimalen Wertes der Karte
	    var Kachel_x_min = (Kachel_x - 90);
	    var Kachel_x_max = (Kachel_x + 270);
	    var Kachel_y_min = (Kachel_y - 90);
	    var Kachel_y_max = (Kachel_y + 270);
	    var j = 1;
	    while (qm[j]) {
		var qm_vergleich_V = qm[j];
		// Vergleichen mit den anderen qm;
		if ((j != i) && (!qm_vergleich_V['status'])) {					
		    // wenn es sich nicht um den selben qm handelt ...
		    qm_vergleich_V_x = qm_vergleich_V['x'];
		    qm_vergleich_V_y = qm_vergleich_V['y'];
		    if ((qm_vergleich_V_x > Kachel_x_min)
			&& (qm_vergleich_V_x < Kachel_x_max)
			&& (qm_vergleich_V_y > Kachel_y_min)
			&& (qm_vergleich_V_y < Kachel_y_max)) {
			qm_vergleich_V['status']= i;
		    }
		}
		j++;
	    }
	}
	i++;
    }
    return true;
}

function kollisonsabfrage(x1, y1, x2, y2) {
    // Funktion zur abfrage ob sich Icons auf der Übersichtskarte berühren

    var versatz  = new Array;
    // versatz[0] = 0 - keine Versatz nötig
    // versatz[0] = 1 - Versatz nach rechts kleinste Verschiebung
    // versatz[0] = 2 - Versatz nach unten kleinste Verschiebung
    // versatz[0] = 3 - Versatz nach links kleinste Verschiebung
    // versatz[0] = 4 - Versatz nach oben kleinste Verschiebung

    // versatz[1] - Wert für Rechtsverschiebung
    // versatz[2] - Wert für Verschiebung nach unten
    // versatz[3] - Wert für Linksverschiebung
    // versatz[4] - Wert für Verschiebung nach oben

    test_x = x1 - x2;							
    test_y = y1 - y2;
							
    if ((Math.abs(test_x) < 510) && (Math.abs(test_y) < 510)) {
	versatz[1] = (510 - test_x);
	versatz[2] = (510 - test_y);
	versatz[3] = (-510 - test_x);
	versatz[4] = (-510 - test_y);
	if (Math.abs(test_x) < Math.abs(test_y)) {
	    if (test_x > 0) {
		versatz[0] = 1;					
	    } else {
		versatz[0] = 3;
	    }
	} else {
	    if (test_y > 0) {
		versatz[0] = 2;
	    } else {
		versatz[0] = 4;
	    }						
	}
    } else {
	versatz[0] = 0;
    }
    return versatz;
} 

function icon_versatz() {
    var index = 1;
    var i = 1;
    while (poi[i]) {
	var poiV = poi[i];
	var uebV = uebersicht_icons[index];
	uebersicht_icons[index] = new Array;
	uebersicht_icons[index]['x'] = poiV['x'];
	uebersicht_icons[index]['y'] = poiV['y'];
	uebersicht_icons[index]['icon'] = poiV['icon'];
	uebersicht_icons[index]['name'] = poiV['name'];
	uebersicht_icons[index]['id'] = i;
	index++;
	i++;
    }
    if (profil.contracts) {
	var contract = profil.contracts[0];
	uebersicht_icons[index++] = {
	    x: contract.left,
	    y: contract.top,
	    icon: 'qm',
	    name: msg("meine mÂ²")
	};
    }

    var i=1;
    while (uebersicht_icons[i]) {
	var uebV_x = uebersicht_icons[i]['x'];			
	var uebV_y = uebersicht_icons[i]['y'];			
	for (j = (i-1); j > 0; j--) {
	    var vergleichV_x = uebersicht_icons[j]['x'] + 240;			
	    var vergleichV_y = uebersicht_icons[j]['y'] + 240;
	    versatz = kollisonsabfrage(uebV_x + 240, uebV_y + 240, vergleichV_x, vergleichV_y);
	    // if (versatz[0]) {dbg("<br/> -> POI[" + i + "] Richtungsaenderungvorschlag: " + versatz[0]);}
	    var test = new Array;
	    test[0] = versatz[0];
	    var versatz_index = versatz[0] + 1;
	    var k = 0;
	    var richtungsfehler = false;
	    if (versatz[0] != 0) {richtungsfehler=true;}
	    while ((richtungsfehler) && (k < 4)) {
		versatz_index--;
		if (versatz_index < 1) {versatz_index = 4;}
		k++;
		// dbg("<br/> -> Richtungsaenderungstest bei " + versatz_index + " Fehler: " + richtungsfehler);

		if (versatz_index == 1) {
		    richtungsfehler = false;
		    for (l = (i-1); l > 0; l--) {				
			if (l != j) {
			    var testV_x = uebersicht_icons[l]['x'] + 240;			
			    var testV_y = uebersicht_icons[l]['y'] + 240;
			    test = kollisonsabfrage(((uebV_x + 240) + versatz[versatz_index]), (uebV_y + 240) , testV_x, testV_y);
			    if (test[0] != 0) {
				richtungsfehler = true; 
				// dbg("<br/> -> Kollision mit " + l);
			    }
			}
		    }
		}

		if (versatz_index == 2) {
		    richtungsfehler = false;
		    for (l = (i-1); l > 0; l--) {				
			if (l != j) {
			    var testV_x = uebersicht_icons[l]['x'] + 240;			
			    var testV_y = uebersicht_icons[l]['y'] + 240;
			    test = kollisonsabfrage((uebV_x + 240), ((uebV_y + 240) + versatz[versatz_index]), testV_x, testV_y);
			    if (test[0] != 0) {
				richtungsfehler = true;
				// dbg("<br/> -> Kollision mit " + l);
			    }
			}
		    }
		}

		if (versatz_index == 3) {
		    richtungsfehler = false;
		    for (l = (i-1); l > 0; l--) {				
			if (l != j) {
			    var testV_x = uebersicht_icons[l]['x'] + 240;			
			    var testV_y = uebersicht_icons[l]['y'] + 240;
			    test = kollisonsabfrage(((uebV_x + 240) + versatz[versatz_index]), (uebV_y + 240) , testV_x, testV_y);
			    if (test[0] != 0) {
				richtungsfehler = true;
				// dbg("<br/> -> Kollision mit " + l);
			    }
			}
		    }
		}

		if (versatz_index == 4) {
		    richtungsfehler = false;
		    for (l = (i-1); l > 0; l--) {				
			if (l != j) {
			    var testV_x = uebersicht_icons[l]['x'] + 240;			
			    var testV_y = uebersicht_icons[l]['y'] + 240;
			    test = kollisonsabfrage((uebV_x + 240), ((uebV_y + 240) + versatz[versatz_index]), testV_x, testV_y);
			    if (test[0] != 0) {
				richtungsfehler = true;
				// dbg("<br/> -> Kollision mit " + l);
			    }
			}
		    }
		}

	    }
		
	    if ((!richtungsfehler) && (versatz[0] != 0)) {
		if (versatz_index == 1) {uebersicht_icons[i]['x'] = uebersicht_icons[i]['x'] + versatz[versatz_index];}
		if (versatz_index == 2) {uebersicht_icons[i]['y'] = uebersicht_icons[i]['y'] + versatz[versatz_index];}
		if (versatz_index == 3) {uebersicht_icons[i]['x'] = uebersicht_icons[i]['x'] + versatz[versatz_index];}				
		if (versatz_index == 4) {uebersicht_icons[i]['y'] = uebersicht_icons[i]['y'] + versatz[versatz_index];}
		// dbg("<br/> -> versetze POI[" + i + "] durch POI[" + j + "] nach " + versatz_index + " um " + versatz[versatz_index] + "<br/>");
		uebV_x = uebersicht_icons[i]['x'];			
		uebV_y = uebersicht_icons[i]['y'];
	    }

	}
	i++;
    }
}

function poi_faehnchen_erzeugen() {
    // Erzeugen der Faehnchen fuer die POI, der array wird durchlaufen und die entsprechenden Informationen in Ebenen dargestellt
    var i = 1;
    while (uebersicht_icons[i]) {
	// neue Ebene erstellen, Ebene ist abhaengig von <Uebersicht>
	var neueebene=document.createElement("DIV");
	document.getElementById("Uebersicht").appendChild(neueebene);
			
	// Testen ob Icon links oder rechts steht --> Ebene muß um 150 px versetzt werden oder nicht
	var x = parseInt(Math.round(uebersicht_icons[i]['x'] / 30) + 170 - 8);
	var y = parseInt(Math.round(uebersicht_icons[i]['y'] / 30) + 101 - 8);

	// "aha!"
	if (y > 360 + 99 - 13) {
	    y = 360 + 99 - 13;
	}

	// definieren der Styles
	neueebene.style.position="absolute";
	neueebene.style.left = x + "px";
	neueebene.style.top = y + "px";
	neueebene.style.width = "15px";
	neueebene.style.height = "15px";
	neueebene.style.zIndex = 6;
	neueebene.id = uebersicht_icons[i]['symbol'];
			
	neueebene.align = "left";
	var faehnchentext = msg(uebersicht_icons[i]['name']);
	if (uebersicht_icons[i]['icon'] == "sale") {
	    var index = uebersicht_icons[i]['id'];
	    neueebene.innerHTML = '<a href="#" onClick="qmDetail_anzeigen(' + poi[index]['x'] + ', ' + poi[index]['y'] + ', 0);" class="FaehnchenLink" onMouseOver="faehnchen_einblenden(' + (x + 17) + ', ' + y + ', &quot;' + faehnchentext + '&quot;)" onMouseOut="faehnchen_ausblenden();"><img src="/images/' + uebersicht_icons[i]['icon'] + '.gif" border="0"/></a>';
	} else if (uebersicht_icons[i]['icon'] == "qm") {
	    neueebene.innerHTML = '<a href="#" onClick="qmDetail_anzeigen(' + profil.contracts[0].left + ', ' + profil.contracts[0].top + ', 0);" onMouseOver="faehnchen_einblenden(' + (x + 17) + ', ' + y + ', &quot;' + faehnchentext + '&quot;)" onMouseOut="faehnchen_ausblenden();"><img src="/images/qm.gif" border="0"/></a>';
	} else {
	    neueebene.innerHTML = '<a href="#" onClick="PoiDetail_anzeigen(' + uebersicht_icons[i]['id'] + ');" class="FaehnchenLink" onMouseOver="faehnchen_einblenden(' + (x + 17) + ', ' + y + ', &quot;' + faehnchentext + '&quot;)" onMouseOut="faehnchen_ausblenden();"><img src="/images/' + uebersicht_icons[i]['icon'] + '.gif" border="0" /></a>';
	}				
	i++;
    }
    dbg("<br/> -> <b>" + (i-1) + " Faehnchen erzeugt</b>");		
    return true;

}
function poi_pos_setzen(objekt, i) {
    // qm setzen	

    dbg("<br> -> Position gestezt");
    var x_obj = parseInt(Math.floor(objekt['x'] - x_anf));
    var y_obj = parseInt(Math.floor(objekt['y'] - y_anf));

    var x_qm = x_obj - 8;
    var y_qm = y_obj - 8;
    if (x_qm < 0) { x_qm = 0}
    if (x_qm > 344) { x_qm = 344}
    if (y_qm < 0) { y_qm = 0}
    if (y_qm > 344) { y_qm = 344}

    document.getElementById("PoiPos").style.left = x_qm + "px";
    document.getElementById("PoiPos").style.top = y_qm + "px";
	
    return true;
}

function qm_pos_setzen(objekt, i) {
    // qm setzen	
    dbg("<br> -> Position gestezt");
    var x_obj = parseInt(Math.floor(objekt['x'] - x_anf));
    var y_obj = parseInt(Math.floor(objekt['y'] - y_anf));

    var x_qm = x_obj - 8;
    var y_qm = y_obj - 8;
    if (x_qm < 0) { x_qm = 0}
    if (x_qm > 344) { x_qm = 344}
    if (y_qm < 0) { y_qm = 0}
    if (y_qm > 344) { y_qm = 344}

    // neue Ebene erstellen, Ebene ist abhaengig von <Uebersicht>
    var neueebene = document.createElement("DIV");
    document.getElementById("qmDetailKarte").appendChild(neueebene);
	
    // definieren der Styles
    neueebene.style.position="absolute";
    neueebene.style.left = x_qm + "px";
    neueebene.style.top = y_qm + "px";
    neueebene.style.width = "16px";
    neueebene.style.height = "16px";
    neueebene.style.zIndex = 10;
    neueebene.style.visibility = "inherit";
    neueebene.id = "pos" + i;		
    neueebene.align = "left";
    neueebene.innerHTML = '<img src="/images/qm.gif" width="16" height="16"/>';

    return true;
}

function qm_einzeichnen(objekt, zeilen) {
    // qm einzeichnen
    var i = 1;
    for (i=1; i < objekt['qm_x'].length; i++) {

	// neue Ebene erstellen, Ebene ist abhaengig von <Uebersicht>
	var neueebene=document.createElement("DIV");
	document.getElementById("qmAusschnitt").appendChild(neueebene);
		
	// Testen ob Icon links oder rechts steht --> Ebene muß um 150 px versetzt werden oder nicht
	var x = parseInt(Math.round(objekt['qm_x'][i] - x_anf) * 5);
	var y = parseInt(Math.round(objekt['qm_y'][i] - y_anf) * 5);
	var width=5;
	while (objekt['qm_y'][i] == objekt['qm_y'][(i + 1)]) {
	    width += 5;
	    i++	;
	}
	// definieren der Styles
	neueebene.style.position="absolute";
	neueebene.style.left = x + "px";
	neueebene.style.top = y + "px";
	neueebene.style.height = "5px";
	neueebene.style.width = width + "px";
	neueebene.style.zIndex =9;
	neueebene.style.visibility = "inherit";
	neueebene.id = "qm" + zeilen;		
	neueebene.align = "left";
	neueebene.innerHTML = '<img src="/infosystem/bilder/rot.gif" height="5" width="' + width + '"/>';
	zeilen++;
    }
    return zeilen;
}

function Uebersicht_anzeigen() {
    // Funktion zur Anzeige der Ãœbersicht
    // Ebenen entsprechen ein- oder ausblenden
    hide_poi_panorama();
    hide_poi_luftbild();

    n_profil = {};
    display_selected_contract();

    show_page("uebersicht");
    document.getElementById("qmDetail").style.visibility = "hidden";
    document.getElementById("PoiDetail").style.visibility = "hidden";
    document.getElementById("Uebersicht").style.visibility = "visible";
    document.getElementById("UebersichtPosition").style.visibility = "hidden";
    document.getElementById("Ueberschrift").innerHTML = start_ueberschrift;
    document.getElementById("Untertitel").innerHTML = "";	
    dbg("<br/> -> <b>Uebersicht anzeigen</b>");		
    return true;
}

function show_poi_luftbild() {
    help_page = 'luftbild';
    hide_poi_panorama();
    if (poi[aktuelles_objekt]['luftbild']) {
	document.getElementById("Luftbild").style.visibility = "visible";
    }
}

function hide_poi_luftbild() {
    document.getElementById("Luftbild").style.visibility = "hidden";
}

function show_poi_panorama() {
    var the_poi = poi[aktuelles_objekt];
    help_page = 'panorama';
    hide_poi_luftbild();
    if (the_poi.panoramas) {
	var panorama_id = the_poi.panoramas[0];
	document.getElementById("Panorama").style.visibility = "visible";
	document.getElementById("PoiInfoText").innerHTML = msg('Das Laden des Panoramas dauert einen Moment und benÃ¶tigt Java in Ihrem Browser.<br /><br />Klicken und Ziehen Sie mit der Maus, um sich im Panorama umzusehen!');
	document.getElementById("PanoramaApplet").innerHTML
	    = '<applet archive="/static/ptviewer.jar" code="ptviewer.class" width="360" height="340"> '
	    + ' <param name="file" value="/image/' + panorama_id + '" />'
	    + ' <param name="cursor" value="MOVE" />'
	    + ' <param name="auto" value=".2" />'
	    + '</applet>';
    }
}

function hide_poi_panorama() {
    document.getElementById("Panorama").style.visibility = "hidden";
}

function show_poi_satbild() {
    hide_poi_luftbild(); // for now
    hide_poi_panorama();
}

var poi_menu_items;
var poi_active_menu_item;

function poi_menu_select(index) {
    poi_active_menu_item = poi_menu_items[index][0];
    make_poi_menu();
    document.getElementById("PoiInfoText").innerHTML = poi[aktuelles_objekt]['text'];
    poi_menu_items[index][1].call();
}

function make_poi_menu() {
    var poi_menu = '';
    
    for (var i = 0; i < poi_menu_items.length; i++) {
	var item_name = poi_menu_items[i][0];
	if (item_name == poi_active_menu_item) {
	    poi_menu += '<span class="PoiMenuItemActive">' + item_name + '</span>';
	} else {
	    poi_menu += '<a class="PoiMenuItem" href="#" onclick="poi_menu_select(' + i + ');">' + item_name + '</a>';
	}
    }

    document.getElementById("PoiMenu").innerHTML = poi_menu;
}

function PoiDetail_anzeigen(index) {
    var the_poi = poi[index];

    poi_menu_items = [ [ msg("Sat-Karte"), show_poi_satbild ] ];
    if (the_poi['luftbild']) {
	poi_menu_items.push([ msg('Luftbild'), show_poi_luftbild ]);
    }
    if (the_poi.panoramas) {
	poi_menu_items.push([ msg('Panorama'), show_poi_panorama ]);
    }

    poi_active_menu_item = msg('Sat-Karte');

    make_poi_menu();
    
    // Funktion zur Anzeige der POIs im Detail
    aktuelles_objekt = index;
    show_page("poidetail");

    hide_poi_luftbild();
    hide_poi_panorama();

    if (the_poi['luftbild']) {
	document.poiluftbild.src = "/image/" + the_poi['luftbild'];
    }

    document.poiicon.src="/images/" + the_poi['icon'] + ".gif";
    // alte Bilder (Kacheln & Thumbs) loeschen
    // Kachlen
    for (var x = 0; x < 4; x++) {
	for (var y = 0; y < 4; y++) {
	    var img = eval("document.img" + (x + 1) + "" + (y + 1));
	    img.src = "/infosystem/bilder/spacer.gif";
	}
    }
    // Thumbs
    for (var i = 1; i <= 6; i++) {
	var thumb = eval("document.thumb" + i);
	thumb.src = "/infosystem/bilder/spacer.gif";
    }
	
    // Koordinaten auf einen geraden Wert innerhalb des Rasters rechen (es sind nur Vielfache von 90 gueltig),
    // Startwert der Kacheln ermitteln
    x_anf = Math.max(0, Math.round((the_poi['x'] - 180) / 90) * 90);
    y_anf = Math.min(10440, Math.round((the_poi['y'] - 180) / 90) * 90);

    dbg("<br/> -> Kacheln laden");
    // Kacheln von Server holen und dem entsprechenden Bild zuordnen	
    for (var x = 0; x < 4; x++) {
	for (var y = 0; y < 4; y++) {
	    var img = eval("document.img" + (x + 1) + "" + (y + 1));
	    img.src = http_pfad + "/overview/" + (x_anf + (x * 90 )) + "/" + (y_anf + (y * 90 ));
	}
    }

    poi_pos_setzen(the_poi, 1);	
    // Thumbnails von serverholen und dem entsprechenden Bild zu ordenen, 
    // wenn weniger als 6 Thumbnails vorhanden sind werden die Restlichen mit spacern gefuellt,
    // einblenden bzw. ausblenden der Ebenen auf den der Link zur Anzeige des großen Bildes liegt
    var i = 1;
    while (i <= the_poi['thumbnail']) {
	var thumb = eval("document.thumb" + i);
	thumb.src = http_pfad + "/poi-image/" + the_poi['symbol'] + "/" + i + "/thumbnail,,55,55";

	var thumblink = eval("document.getElementById('ThumbLink" + i + "')");
	thumblink.style.visibility = "visible";

	i++;
    }
	
    while (i <= 6) {
	var thumb = eval("document.thumb" + i);
	thumb.src = "/infosystem/bilder/spacer.gif";
	var thumblink = eval("document.getElementById('ThumbLink" + i + "')");
	thumblink.style.visibility = "hidden";
	i++;
    }

    // setzen des Positionskaestchens auf der kleinen Übersichtskarte	
    uebersicht_x = parseInt(((the_poi['x'] / 10800) * 118) + 8);
    uebersicht_y = parseInt(((the_poi['y'] / 10800) * 118) + 8) - 37;

    document.getElementById("UebersichtPosition").style.left = uebersicht_x + "px";
    document.getElementById("UebersichtPosition").style.top = uebersicht_y + "px";	

    // erstellen des Navigationstextes
    var navitext = "";
    var i = 1;
    while (poi[i]) {
	if(poi[i]['thumbnail'] != 0) {
	    if (aktuelles_objekt == i) {
		navitext += "- <span class='PoiNavigationAktiv'>" + poi[i]['name'] + "</span><br/>";
	    } else {
		navitext += "<a href='#' onClick='PoiDetail_anzeigen(" + i + ");' class='PoiNavigation'>" + poi[i]['name'] + "</a><br/>";
	    }
	}
	i++;
    }

    // setzen der Texte
    document.getElementById("Ueberschrift").innerHTML = the_poi['name'];
    if (isIE) {
	document.getElementById("PoiInfoText").style.width = "178px";
    }
    document.getElementById("PoiInfoText").innerHTML = the_poi['text'];
    document.getElementById("Untertitel").innerHTML = the_poi['untertitel'];
    document.getElementById("PoiNavi").innerHTML = navitext;

    // ein bzw. ausblenden der Ausgangs-Ebenen
    document.getElementById("PoiDetailThumb").style.visibility = "hidden";		
    document.getElementById("PoiDetailKarte").style.visibility = "inherit";	
    document.getElementById("Uebersicht").style.visibility = "hidden";
    document.getElementById("PoiDetail").style.visibility = "visible";	
    document.getElementById("UebersichtPosition").style.visibility = "visible";
    dbg("<br/> -> <b>POI-Detailansicht anzeigen</b>");
    return true;
}

function ThumbVergoessern(bild) {
    // Funktion zum anzeigen des großen Detailbildes eines Thumbnails
    // setzen des Bildes;
    // ausblenden der Karte und einblenden des Bildes
    show_page("poifoto");

    hide_poi_luftbild();
    hide_poi_panorama();

    document.DetailThumb.src = "/infosystem/bilder/spacer.gif";
    document.DetailThumb.src = http_pfad + "/poi-image/" + poi[aktuelles_objekt]['symbol'] + "/" + bild;
    document.getElementById("PoiDetailKarte").style.visibility = "hidden";	
    document.getElementById("PoiDetailThumb").style.visibility = "inherit";

    // setzen der Texte
    document.getElementById("PoiInfoText").innerHTML =  poi[aktuelles_objekt]['imagetext'][bild - 1];		
    document.getElementById("Ueberschrift").innerHTML = poi[aktuelles_objekt]['imageueberschrift'][bild - 1];
    document.getElementById("Untertitel").innerHTML = poi[aktuelles_objekt]['imageuntertitel'][bild - 1];
    dbg("<br/> -> <b>POI-Foto-Detailansicht anzeigen</b>");
    return true;
}

function toggle_layer(layer_name) {
    if (active_layers[layer_name]) {
	active_layers[layer_name] = false;
	document.getElementById("LayerMenu_" + layer_name).className = "LayersMenuItem";
    } else {
	active_layers[layer_name] = true;
	document.getElementById("LayerMenu_" + layer_name).className = "LayersMenuItemActive";
    }
    load_sqm_tiles();
}

function active_layer_names() {
    var retval = [];
    for (var i = 0; i < all_layers.length; i++) {
	var layer_name = all_layers[i];
	if (active_layers[layer_name]) {
	    retval.push(layer_name);
	}
    }
    return retval;
}

function load_sqm_tiles() {

    for (var x = 0; x < 4; x++) {
	for (var y = 0; y < 4; y++) {
	    
	    var img = document["qmimg" + (x + 1) + "" + (y + 1)];
	    img.src = http_pfad + "/overview/" + (x_anf + (x * 90)) + "/" + (y_anf + (y * 90)) + "/" + active_layer_names().join("/");

	    var img = document["qmlupe" + (x + 1) + "" + (y + 1)];
	    img.src = http_pfad + "/overview/" + (x_anf + (x * 90)) + "/" + (y_anf + (y * 90)) + "/" + active_layer_names().join("/");

	}
    }
}
	
function qmDetail_anzeigen(x_koord, y_koord, objekt)
{
    // Funktion zum Anzeigen der "meine qm" Karte
    // Funktion zur Anzeige der POIs im Detail
    aktuelles_objekt = objekt;
    show_page("qmdetail");

    // alte Kacheln loeschen
    for (var x = 0; x < 4; x++) {
	for (var y = 0; y < 4; y++) {
	    var img = document["img" + (x + 1) + "" + (y + 1)];
	    img.src = "/infosystem/bilder/spacer.gif";

	    var img = document["qmlupe" + (x + 1) + "" + (y + 1)];
	    img.src = "/infosystem/bilder/spacer.gif";
	}
    }
	
    // Koordinaten auf einen geraden Wert innerhalb des Rasters
    // umrechnen (es sind nur Vielfache von 90 gueltig), Startwert der
    // Kacheln ermitteln
    x_anf = Math.max(0, Math.round((x_koord - 180) / 90) * 90);
    y_anf = Math.min(10440, Math.round((y_koord - 180) / 90) * 90);
	
    dbg("<br/> -> Kacheln laden (" + x_anf + " / " + y_anf + ")");		
    // Kacheln von Server holen und dem entsprechenden Bild zuordnen

    load_sqm_tiles(x_anf, y_anf);

    // qm loeschen
    for (var i  = 1; i < erzeugte_zeilen; i++) {
	var loeschen = eval("document.getElementById('qm" + i + "')");
	document.getElementById("qmAusschnitt").removeChild(loeschen);
    }
    dbg("<br/> -> " + erzeugte_zeilen + " zeilen geloescht");

    // qm loeschen
    for (var i  = 1; i <= erzeugte_positionen; i++) {
	var loeschen = eval("document.getElementById('pos" + i + "')");
	document.getElementById("qmDetailKarte").removeChild(loeschen);
    }
    dbg("<br/> -> " + erzeugte_positionen + " Positionen geloescht");

    x_obj = parseInt(Math.floor(x_koord - x_anf));
    y_obj = parseInt(Math.floor(y_koord - y_anf));
	

    fenster_setzen(x_obj, y_obj);
    if (objekt != 0) {
	erzeugte_zeilen = qm_einzeichnen(qm[objekt], 1);
	qm_pos_setzen(qm[objekt], 1);
	var i=1;
	erzeugte_positionen = 1;
	while (qm[i]) {
	    var qm_vergleich_V = qm[i];
	    if ((i != objekt) && (qm_vergleich_V['status'] == objekt)) {
		erzeugte_positionen++;
		qm_pos_setzen(qm[i], erzeugte_positionen);
		erzeugte_zeilen = qm_einzeichnen(qm[i], erzeugte_zeilen);	
	    }
			
	    i++;
	}	
	dbg("<br/> -> " + erzeugte_positionen + " Quadratmeterpositionen");
	dbg("<br/> -> " + erzeugte_zeilen + " zeilen fuer Quadratmeter eingezeichnet");
	
    }
    // setzen des Positionskaestchens auf der kleinen Übersichtskarte	
    var uebersicht_x = parseInt(((x_koord / 10800) * 118) + 14);
    var uebersicht_y = parseInt(((y_koord / 10800) * 118 ) + 10) - 37;
    if (uebersicht_y < 13) {
	uebersicht_y = 13;
    }
    if (uebersicht_x > 125) {
	uebersicht_x = 125;
    }

    document.getElementById("UebersichtPosition").style.left = uebersicht_x + "px";	
    document.getElementById("UebersichtPosition").style.top = uebersicht_y + "px";	

    if (objekt != 0) {
	var text = '<table width="155" border="0" cellspacing="0" cellpadding="0">'
	    + '<tr><td width="60" class="PoiNavigation">' + msg('Sponsor-ID') + ':</td><td class="PoiNavigation">' + profil['id'] + '</td></tr>'
	    + '<tr><td width="60" class="PoiNavigation">' + msg('Name') + ':</td><td class="PoiNavigation">' + profil['name'] + '</td></tr>'
	    + '<tr> <td width="60" class="PoiNavigation">' + msg('Land') + ':</td><td class="PoiNavigation">' + profil['country'] + '</td></tr>'
	    + '<tr> <td colspan="2" class="PoiNavigation"><img src="/infosystem/bilder/spacer.gif" width="1" height="10"/></td></tr>'
	    + '<tr> <td width="60" class="PoiNavigation">' + msg('gesponsort') + ':</td><td class="PoiNavigation">' + profil['anzahl'] + ' mÂ²</td></tr>'
	    + '<tr> <td width="60" class="PoiNavigation">' + msg('seit') + ':</td><td class="PoiNavigation">' + qm[aktuelles_objekt]['date'] + '</td></tr>'
	    + '<tr> <td colspan="2" class="PoiNavigation"><img src="/infosystem/bilder/spacer.gif" width="1" height="20"/></td></tr>'
	    + '<tr> <td colspan="2" class="PoiNavigation">' + profil['nachricht'] + '</td></tr>'
	    + '</table>';
    }
	
    if (aktuelles_objekt == 0) {
	document.getElementById("Ueberschrift").innerHTML = msg("Zu Verkaufen");
    } else {
	document.getElementById("Ueberschrift").innerHTML = msg("Verkaufte mÂ²");
    }

    if (isIE) {
	document.getElementById("qmInfoText").style.width = "178px";
    }
    if (aktuelles_objekt == 0) {
	document.getElementById("Ueberschrift").innerHTML = msg("mÂ²-Verkaufsgebiet");
    } else {
	document.getElementById("qmInfoText").innerHTML = text;
    }
    document.getElementById("Untertitel").innerHTML = "";

    document.getElementById("qmDetail").style.visibility = "visible";	
    document.getElementById("Uebersicht").style.visibility = "hidden";	
    document.getElementById("UebersichtPosition").style.visibility = "visible";

    if (isNav) {
	document.captureEvents(Event.MOUSEDOWN | Event.MOUSEMOVE | Event.MOUSEUP);
    }
    var element = document.getElementById("MapCaptureImage");

    element.onmousedown = maus_gedrueckt;
    element.onmousemove = maus_bewegen;
    element.onmouseup = maus_losgelassen;

    element = document.getElementById("qmAusschnitt");

    element.onmousedown = maus_gedrueckt;
    element.onmouseup = maus_losgelassen;

    if (profil.contracts) {
	display_own_sqm();
    }

    dbg("<br/> -> <b>qm-Detailansicht anzeigen</b>");
    return true;
}

function fenster_setzen(x, y) {

    var x_lupe = x - 18;
    var y_lupe = y - 12;

    if (x_lupe < 0) { x_lupe = 0}
    if (x_lupe > 324) { x_lupe = 324}
    if (y_lupe < 0) { y_lupe = 0}
    if (y_lupe > 336) { y_lupe = 336}

    // qmAusschnitt clippen
    x_ausschnitt = (x_lupe * 5);
    y_ausschnitt = (y_lupe * 5);

    var left_ausschnitt = (551 - x_ausschnitt);
    var top_ausschnitt = (113 - y_ausschnitt);		

    var cliprect = "rect(" + y_ausschnitt + "px " + (x_ausschnitt + 178) + "px " + (y_ausschnitt + 118) + "px " + (x_ausschnitt) + "px)";

    document.getElementById("qmLupe").style.left = x_lupe + "px";	
    document.getElementById("qmLupe").style.top = y_lupe + "px";

    document.getElementById('qmAusschnitt').style.left = left_ausschnitt + "px";
    document.getElementById('qmAusschnitt').style.top = top_ausschnitt + "px";
    document.getElementById('qmAusschnitt').style.clip = cliprect;

    return true;
}

function maus_gedrueckt(e) {
    if (angezeigt == "qmdetail") {
	var mouse_x = 0;
	var mouse_y = 0;
	
	if (isNav) {
	    mouse_x = e.pageX - 170;
	    mouse_y = e.pageY - 100;
	} else {
	    mouse_x = event.clientX - 170;
	    mouse_y = event.clientY - 100;
	}
		
	if (isNav) {
	    ausschnitt_mouse_x = e.pageX - 551;
	    ausschnitt_mouse_y = e.pageY - 113;
	} else {
	    ausschnitt_mouse_x = event.clientX - 551;
	    ausschnitt_mouse_y = event.clientY - 113;
	}
	
	if ((mouse_x > 0) && (mouse_x < 360) && (mouse_y > 0)  && (mouse_y < 360)) {
	    mouse_button = true;
	    fenster_setzen(mouse_x, mouse_y);
	}
	
	if ((ausschnitt_mouse_x > 0) && (ausschnitt_mouse_x < 178) && (ausschnitt_mouse_y > 0)  && (ausschnitt_mouse_y < 118)) {
	    fremd_x = parseInt((Math.floor(ausschnitt_mouse_x / 5)) + (Math.floor(x_ausschnitt / 5)) + x_anf); 
	    fremd_y = parseInt((Math.floor(ausschnitt_mouse_y / 5)) + (Math.floor(y_ausschnitt / 5)) + y_anf); 		
	    n_qm_laden();
	}
    }			
    return false;
}

function maus_bewegen(e) {
    if (mouse_button) {
	var mouse_x = 0;
	var mouse_y = 0;
	mouse_button = true;
	
	if (isNav) {
	    mouse_x = e.pageX - 170;
	    mouse_y = e.pageY - 100;
	} else {
	    mouse_x = event.clientX - 170;
	    mouse_y = event.clientY - 100;
	}
	fenster_setzen(mouse_x, mouse_y); 
    }
    return false;
}

function maus_losgelassen() {
    mouse_button = false;		
    return true; 
}

function Thumbausblenden() {
    document.getElementById("PoiDetailKarte").style.visibility = "inherit";	
    document.getElementById("PoiDetailThumb").style.visibility = "hidden";

    // setzen der Texte
    document.getElementById("Ueberschrift").innerHTML = poi[aktuelles_objekt]['name'];
    document.getElementById("PoiInfoText").innerHTML = poi[aktuelles_objekt]['text'];
    document.getElementById("Untertitel").innerHTML = poi[aktuelles_objekt]['untertitel'];

    poi_active_menu_item = msg('Sat-Karte');

    make_poi_menu();

    return true;
}
function sponsorfeld_leeren() {
    var inhalt = document.form0.__sponsorid.value;
    if (inhalt == "Sponsoren-ID") {
	document.form0.__sponsorid.value = "";
	document.form0.__password.value = "";
    }
    return true;
}

function enlarge_info() {
    document.getElementById("qmInfo").style.top = "99px";
    document.getElementById("qmInfo").style.height = "361px";
    document.getElementById("qmInfoText").style.height = "338px";
}

function collapse_info() {
    document.getElementById("qmInfo").style.top = "242px";
    document.getElementById("qmInfo").style.height = "218px";
    document.getElementById("qmInfoText").style.height = "178px";
}

function show_page(page_name)
{
    angezeigt = page_name;
    if (helpwin && !helpwin.closed) {
	helpwin.location = "/" + document.language + "/infosys-help-" + angezeigt;
    }
}

// ***  extrafenster fuer hilfe *** // 
function window_infosystem_hilfe() { 
    helpwin = open("/" + document.language + "/infosys-help-" + angezeigt,
	       "detailwin",
	       "width=462,height=650,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=no,left=600,top=50");
    helpwin.focus();
}
// -->
