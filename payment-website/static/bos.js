// -*- Java -*- Script

// ***  extrafenster fuer impressum, kontakt etc. *** //
function window_extra(target) { 
    mywin=open(target,"detailwin","width=482,height=600,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes,left=100,top=100");
    mywin.focus();
};

		
// ***  extrafenster für das Ringschema *** //
function window_ringdetail() { 
    mywin=open("ring-detail","ringdetail","width=492,height=450,status=no,toolbar=no,menubar=no,resizable=no,scrollbars=no,left=100,top=100");
    mywin.focus();
};

		
// ***  extrafenster fuer news + archive *** //
function window_news(target) { 
    mywin=open(target,"newswin","width=480,height=400,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes,left=100,top=100");
    mywin.focus();
};

// ***  extrafenster fuer satellitenkarte *** //
function window_infosys() {
    var url = "/infosystem";

    var sponsorid_input = document.getElementById('sponsorid-input');
    var password_input = document.getElementById('password-input');

    if (sponsorid_input && password_input) {
	url += "?__sponsorid=" + sponsorid_input.value + "&__password=" + password_input.value;
    }
	
    mywin=open(url,
	       "infowin",
	       "width=740,height=500,status=no,toolbar=no,menubar=no,resizable=no,scrollbars=no,left=250,top=50");
    mywin.focus();
};

// Sprachumschaltung

function jumpMenu(targ, selObj, restore) {

    eval(targ + ".location='" + selObj.options[selObj.selectedIndex].value + "'");
    if (restore)
	selObj.selectedIndex=0;
}

// Formularcheck für Profilsetup

function check_profil_setup() {

    if (document.form.password.value == "") {
	alert('Das Kennwort darf nicht leer sein');
	document.form.password.focus();
	return false;
    }

    if (document.form.password.value != document.form.password1.value) {
	alert('Bitte geben Sie zwei mal das gleiche Kennwort ein');
	document.form.password.focus();
	return false;
    }

    window_infosys();

    return true;
}

// Formularchecks für Bestellung

function show_disclaimer() {
    document.bestellformular.disclaimer_read.checked = true;
    window_extra('disclaimer');
}


function check_ueberweisung() {

    //	alert("numsqm: " +  + " numsqm1: " + );

    if (!document.bestellformular.disclaimer_read.checked) {
	alert("Bitte lesen Sie die Verzichtsklausel und bestätigen Sie sie Ihr Einverständnis durch Setzen des Häkchens");
	return false;
    }

    if (document.bestellformular.numsqm[0].checked
	|| (document.bestellformular.numsqm[4].checked
	    && (document.bestellformular.numsqm1.value < 5))) {

	alert("Aufgrund des hohen manuellen Bearbeitungsaufands sind Überweisungen erst ab einer Summe von 15 Euro (5 Quadratmeter) möglich");
	return false;
    }

    if (document.bestellformular.gift.checked) {
	alert("Den Geschenkservice können wir nur bei Online-Überweisungen anbieten");
	return false;
    }
}

function check_online() {

    if (!document.bestellformular.disclaimer_read.checked) {
	alert("Bitte lesen Sie die Verzichtsklausel und bestätigen Sie sie Ihr Einverständnis durch Ankreuzen der Checkbox");
	return false;
    }

    if (document.bestellformular.numsqm[4].checked && !document.bestellformular.numsqm1.value.match(/^\d+/)) {
	alert('Bitte geben Sie die Anzahl der Quadratmeter ein, die Sie "kaufen" möchten!');
	document.bestellformular.numsqm1.focus();
	return false;
    }

    if (document.bestellformular.gift.checked
	&& (document.bestellformular.numsqm[0].checked
	    || document.bestellformular.numsqm[1].checked
	    || (document.bestellformular.numsqm[4].checked
		&& (document.bestellformular.numsqm1.value < 10)))) {

	alert("Das Verschenken von Quadratmetern ist erst ab einer Summe von 30 Euro möglich");
	return false;
    }

    return true;
}

// Formularcheck für Versandinformationen

function check_versand_info() {

    if ((document.formular.name.value == '')
	|| (document.formular.address.value == '')) {
	alert("Bitte geben Sie einen Namen für die Urkunde sowie die Versandadresse an");
	return false;
    }

    return true;
}

// Funktion zum Verschicken von Info-Mail-Requests

function send_info_request() {
    var address = document.form.email.value;

    if (!is_valid_email(address)) {
	alert('Die von Ihnen eingegebene Email-Adresse "' + address + '" konnte von unserem Server nicht erkannt werden.  Bitte senden '
	      + 'Sie uns Ihre Anfrage per Email an service@createrainforest.org');
    } else {
	if (confirm('Wünschen Sie, daß wir Ihnen an die Email-Adresse "' + address + '" Informationen zu BOS und Samboja Lestari schicken?')) {
	    document.form.email.value = '';
	    open("info-request?email=" + escape(address),
		 "mailwin", "width=480,height=235,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes,left=100,top=100");
	}
    }

    return false;
}

function is_valid_email(address) {
    var filter  = /^([a-zA-Z0-9_\.\-])+\@(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,4})+$/;

    return filter.test(address);
}

// Allgemeiner Formular-Check

function MM_findObj(n, d) {
    var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
	d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
    if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
    for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);
    if(!x && d.getElementById) x=d.getElementById(n); return x;
}
 
function YY_checkform() { //v4.71
    //copyright (c)1998,2002 Yaromat.com
    var a=YY_checkform.arguments,oo=true,v='',s='',err=false,r,o,at,o1,t,i,j,ma,rx,cd,cm,cy,dte,at;
    for (i=1; i<a.length;i=i+4){
	if (a[i+1].charAt(0)=='#'){r=true; a[i+1]=a[i+1].substring(1);}else{r=false}
	o=MM_findObj(a[i].replace(/\[\d+\]/ig,""));
	o1=MM_findObj(a[i+1].replace(/\[\d+\]/ig,""));
	v=o.value;t=a[i+2];
	if (o.type=='text'||o.type=='password'||o.type=='hidden'){
	    if (r&&v.length==0){err=true}
	    if (v.length>0)
		if (t==1){ //fromto
		    ma=a[i+1].split('_');if(isNaN(v)||v<ma[0]/1||v > ma[1]/1){err=true}
		} else if (t==2){
		    rx=new RegExp("^[\\w\.=-]+@[\\w\\.-]+\\.[a-zA-Z]{2,4}$");if(!rx.test(v))err=true;
		} else if (t==3){ // date
		    ma=a[i+1].split("#");at=v.match(ma[0]);
		    if(at){
			cd=(at[ma[1]])?at[ma[1]]:1;cm=at[ma[2]]-1;cy=at[ma[3]];
			dte=new Date(cy,cm,cd);
			if(dte.getFullYear()!=cy||dte.getDate()!=cd||dte.getMonth()!=cm){err=true};
		    }else{err=true}
		} else if (t==4){ // time
		    ma=a[i+1].split("#");at=v.match(ma[0]);if(!at){err=true}
		} else if (t==5){ // check this 2
		    if(o1.length)o1=o1[a[i+1].replace(/(.*\[)|(\].*)/ig,"")];
		    if(!o1.checked){err=true}
		} else if (t==6){ // the same
		    if(v!=MM_findObj(a[i+1]).value){err=true}
		}
	} else
	    if (!o.type&&o.length>0&&o[0].type=='radio'){
		at = a[i].match(/(.*)\[(\d+)\].*/i);
		o2=(o.length>1)?o[at[2]]:o;
		if (t==1&&o2&&o2.checked&&o1&&o1.value.length/1==0){err=true}
		if (t==2){
		    oo=false;
		    for(j=0;j<o.length;j++){oo=oo||o[j].checked}
		    if(!oo){s+='* '+a[i+3]+'\n'}
		}
	    } else if (o.type=='checkbox'){
		if((t==1&&o.checked==false)||(t==2&&o.checked&&o1&&o1.value.length/1==0)){err=true}
	    } else if (o.type=='select-one'||o.type=='select-multiple'){
		if(t==1&&o.selectedIndex/1==0){err=true}
	    }else if (o.type=='textarea'){
		if(v.length<a[i+1]){err=true}
	    }
	if (err){s+='* '+a[i+3]+'\n'; err=false}
    }
    if (s!=''){alert('Die benötigten Informationen sind unvollständig oder fehlerhaft:\t\t\t\t\t\n\n'+s)}
    document.MM_returnValue = (s=='');
}
