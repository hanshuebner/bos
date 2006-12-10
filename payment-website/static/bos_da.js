// -*- Java -*- Script

// Formularcheck für Profilsetup

function check_profil_setup() {

    if (document.form.password.value == "") {
	alert('Indtast venligst dit personlige password.');
	document.form.password.focus();
	return false;
    }

    if (document.form.password.value != document.form.password1.value) {
	alert('Tast venligst dit password ind to gange.');
	document.form.password.focus();
	return false;
    }

    window_infosys();

    return true;
}

function check_ueberweisung() {

    //	alert("numsqm: " +  + " numsqm1: " + ) "Please read the waiver clause and confirm your agreement with a click to the check box.";

    if (!document.bestellformular.disclaimer_read.checked) {
	alert("Læs venligst waiver klausulen og bekræft at du er enig med et klik i afkrydsnings-feltet.");
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
	alert("Læs venligst waiver klausulen og bekræft at du er enig med et klik i afkrydsnings-feltet.");
	return false;
    }

    if (document.bestellformular.numsqm[4].checked && !document.bestellformular.numsqm1.value.match(/^\d+/)) {
	alert('Indtast venligst det antal kvadratmeter du ønsker at "købe"!');
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

// Formularcheck für Versandinformationen -- Please enter a name and address for your rainforest certificate.

function check_versand_info() {

    if ((document.formular.name.value == '')
	|| (document.formular.address.value == '')) {
	alert("Indtast venligst et navn og adresse til deres Regnskovs Diplom.");
	return false;
    }

    return true;
}

// Funktion zum Verschicken von Info-Mail-Requests

function send_info_request() {
    var address = document.form.email.value;

    if (!is_valid_email(address)) {
	alert('Den indtastede e-mail adresse  "' + address + '" blev ikke genkendt af vores server. Send venligst deres forespørgsel til '
	      + 'service@createrainforest.org');
    } else {
	if (confirm('Vil du gerne modtage yderligere information om BOS og Samboja Lestari til  "' + address + '"?')) {
	    document.form.email.value = '';
	    open("info-request?email=" + escape(address),
		 "mailwin", "width=480,height=235,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes,left=100,top=100");
	}
    }

    return false;
}
