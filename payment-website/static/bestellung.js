function check_online() {

    document.bestellformular.numsqm1.value = document.bestellformular.numsqm1.value.replace(/[^0-9]/g, "");

    var email = document.getElementById('email');
    if (email == "") {
        alert('Bitte geben Sie Ihre Email-Adresse ein');
        email.focus();
        return false;
    }

    if (!email.value.match(/^([a-zA-Z0-9_\.\-])+\@(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,4})+$/)) {
        alert('Ungültige Email-Addresse, bitte geben Sie Ihre Email-Adresse ein');
        email.focus();
        return false;
    }

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
		&& (document.bestellformular.numsqm1.value < 30)))) {

	alert("Das Verschenken von Quadratmetern ist erst ab einer Summe von 90 Euro möglich");
	return false;
    }

    return true;
}

$(document).ready(function () {
    if (window.location.protocol != "https:") {
        window.location = window.location.href.replace(/^http:/, "https:");
    }

    document.bestellformular.onsubmit = check_online;
});