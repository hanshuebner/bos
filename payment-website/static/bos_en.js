// -*- Java -*- Script

// Formularcheck für Profilsetup

function check_profil_setup() {

    if (document.form.password.value == "") {
	alert('Please enter your personal password.');
	document.form.password.focus();
	return false;
    }

    if (document.form.password.value != document.form.password1.value) {
	alert('Please enter your personal password again.');
	document.form.password.focus();
	return false;
    }

    window_infosys();

    return true;
}

function check_ueberweisung() {

    //	alert("numsqm: " +  + " numsqm1: " + );

    if (!document.bestellformular.disclaimer_read.checked) {
	alert("Please read the waiver clause and confirm your agreement with a click to the check box.");
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
	alert("Please read the waiver clause and confirm your agreement with a click to the check box.");
	return false;
    }

    if (!document.bestellformular.numsqm1.value.match(/^\d+/)) {
	alert('Please enter the number of square meters that you want to "buy"!');
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
	alert("Please enter a name and address for your rainforest certificate.");
	return false;
    }

    return true;
}

// Funktion zum Verschicken von Info-Mail-Requests

function send_info_request() {
    var address = document.form.email.value;

    if (!is_valid_email(address)) {
	alert('The email address you entered  "' + address + '" was not recogniced by our server. Please send your request to '
	      + 'service@createrainforest.org');
    } else {
	if (confirm('Would you like to receive informations about BOS and Samboja Lestari to  "' + address + '"?')) {
	    document.form.email.value = '';
	    open("info-request?email=" + escape(address),
		 "mailwin", "width=480,height=235,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes,left=100,top=100");
	}
    }

    return false;
}
