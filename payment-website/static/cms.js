// This may look like -*- Java -*-, but it really is ECMAScript

// Allgemeine Initialisierungsfunktion fuer alle CMS-Seiten

function init() {
}

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

function $(id) { return document.getElementById(id); }

function parse_youtube_link (input)
{
    var text = input.value;

    text = text.replace(/.*src=" *(http:\/\/www.youtube.com[^"]+).*/, "$1")
    input.value = text;
    if ((input.value != "") && !input.value.match(/^http:\/\/www.youtube.com\/v\/\S+/)) {
        alert("Invalid YouTube URL: " + input.value);
        input.value = "";
        return false;
    } else {
        return true;
    }
}

function confirm_delete(field_name, value, confirm_string)
{
    $(field_name).value = value;
    return confirm(confirm_string);
}

function upload_new_medium_input_toggle(medium_type)
{
    var upload_new_medium_form = $("upload_new_medium_form");
    var upload_new_medium_input = $("upload_new_medium_input");
    var upload_new_medium_input_label = $("upload_new_medium_input_label");
    if (medium_type == "poi-movie") {
        upload_new_medium_input.setAttribute("type", "text");
        upload_new_medium_input.setAttribute("name", "url");
        upload_new_medium_form.setAttribute("onsubmit", "return parse_youtube_link($(\"upload_new_medium_input\"));");
        upload_new_medium_input_label.innerHTML = "URL";
    } else {
        upload_new_medium_input.setAttribute("type", "file");
        upload_new_medium_input.setAttribute("name", "image-file");
        upload_new_medium_input.setAttribute("onchange", "return true;");
        upload_new_medium_form.setAttribute("onsubmit", "return true;");
        upload_new_medium_input_label.innerHTML = "File";
    }
}

