// JavaScript Document

function anonymizecheck ()
{
	if (confirm('Persönliche Daten löschen?')) {
		document.form.name.value = '';
		document.form.infotext.value = '';
		return true;
	}
	return false;
}

function buycheck ()
{
	if (!document.form.numsqm.value.match(/^\d+$/)
		|| (document.form.numsqm.value.match(/^0+/))) {
		alert("Ungültige Anzahl Quadratmeter");
		document.form.numsqm.select();
		document.form.numsqm.focus();
		return false;
	}
	return true;
}

function formcheck ()
{
	if (document.form.password.value != document.form.password1.value) {
		alert('Die beiden eingegeben Kennwörter stimmen nicht überein');
		document.form.password.select();
		document.form.password.focus();
		return false;
	}

	alert('Ihre Änderungen werden gespeichert');

	return true;
}

function window_urkunde (sponsorid) {
	var url = "/certificate/";
	var certwin = open(url, "certwin", "width=480,height=620,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=no");
	certwin.focus();

	return false
};

