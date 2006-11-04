// This may look like -*- Java -*-, but it really is ECMAScript

// Formularcheck für Sponsoren-Erzeugung

function check_complete_sale() {

    if (document.form.name && (document.form.name.value == "")) {
	alert('Missing name for certificate');
	return false;
    }

    if (document.form.numsqm.value == ""
	|| !document.form.numsqm.value.match(/^\d+$/)) {
	alert('Invalid number of square meters');
	document.form.numsqm.focus();
	document.form.numsqm.select();
	return false;
    }

    if (!document.form.date.value.match(/^[0-9][0-9]\.[0-9][0-9]\.[0-9][0-9][0-9][0-9]$/)) {
	alert('Invalid date format, use DD.MM.YYYY');
	return false;
    }

    var mail_message;

    if (document.form.email.value == "") {
	mail_message = 'No welcome email will be sent\n';
    } else {
	mail_message = 'Welcome email will be sent to ' + document.form.email.value + '\n';
    }

    var send_cert_message;

    if (document.form.postaladdress && document.form.postaladdress.value.match(/^\s*$/)) {
	send_cert_message = 'No printed certificate will be mailed\n';
    } else {
	send_cert_message = 'Printed certificate will be mailed to:\n' + document.form.postaladdress.value;
    }

    var numsqm = document.form.numsqm.value;
    var price = numsqm * 3;

    return confirm(numsqm + ' square meters have been bought for ' + price + ' euros\n'
		   + mail_message
		   + send_cert_message);
}

function statistic_selected ()
{
    var select = document.getElementById('selector');
    var stats_name = select[select.options.selectedIndex].value;

    document.getElementById('stats').innerHTML
 	= '<embed src="/images/statistics/' + stats_name + '.svg" width="800" height="600" type="image/svg+xml"></embed>';

    return true;
}
