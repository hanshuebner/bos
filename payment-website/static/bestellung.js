function check_form() {

    var email = $('#email');
    if (email.val() == "") {
        alert('Bitte geben Sie Ihre Email-Adresse ein');
        email.focus();
        return false;
    }

    if (!email.val().match(/^([a-zA-Z0-9_\.\-])+\@(([a-zA-Z0-9\-])+\.)+([a-zA-Z0-9]{2,4})+$/)) {
        alert('Ungültige Email-Addresse, bitte geben Sie Ihre Email-Adresse ein');
        email.focus();
        return false;
    }

    if (!$('#disclaimer_read:checked').val()) {
	alert("Bitte lesen Sie die Verzichtsklausel und bestätigen Sie sie Ihr Einverständnis durch Ankreuzen der Checkbox");
        $('#disclaimer_read').focus();
	return false;
    }

    return true;
}

function setWantedSqm(count)
{
    $('#numsqm').val(count);
    $('#amount').html(count * 3);

    if (count < 30) {
        $('#choose_printed_cert').hide();
        $('#announce_printed_cert').show();
    } else {
        $('#choose_printed_cert').show();
        $('#announce_printed_cert').hide();
    }
}

function recalculateFromSlider(count)
{
    if (count < 10) {
        count = count + 1;
    } else if (count < 28) {
        count = 10 + (count - 9) * 5;
    } else if (count < 68) {
        count = 100 + (count - 27) * 10;
    } else {
        count = 500 + (count - 67) * 100;
    }
    setWantedSqm(count);
}

function changeSqmCount(event, ui)
{
    recalculateFromSlider(ui.value);
}

$(document).ready(function () {
    if (window.location.protocol != "https:") {
        window.location = window.location.href.replace(/^http:/, "https:");
    }

    document.bestellformular.onsubmit = check_form;

    $(":text").labelify({ labelledClass: "labelHighlight" });
    $('#slider').slider({ slide: changeSqmCount, value: 9 });
    recalculateFromSlider($('#slider').slider('value'));
    $('#numsqm').change(function () {
        var value = $(this).val().replace(/[^0-9]/g, "");
        
        setWantedSqm(value);
    });
    $('#printed_cert').change(function () {
        if (this.checked) {
            $('#anschrift').show();
        } else {
            $('#anschrift').hide();
        }
    });
});