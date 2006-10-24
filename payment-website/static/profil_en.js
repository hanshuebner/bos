// JavaScript Document

function anonymizecheck ()
{
	if (confirm('Delete your personal data?')) {
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
		alert("Invalid number of square metres.");
		document.form.numsqm.select();
		document.form.numsqm.focus();
		return false;
	}
	return true;
}

function formcheck ()
{
	if (document.form.password.value != document.form.password1.value) {
		alert('The passwords you have entered do not correspond');
		document.form.password.select();
		document.form.password.focus();
		return false;
	}

	alert('Your changes will be saved.');

	return true;
}

function window_urkunde (sponsorid) {
	var url = "/certificate/";
	var certwin = open(url, "certwin", "width=480,height=620,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=no");
	certwin.focus();

	return false
};

