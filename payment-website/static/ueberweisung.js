$(document).ready(function () {
    // Make sure that all our input and form elements ave an id
    // attribute, required by form validator.
    $(':input, form').each(function () {
        if (!$(this).attr('id')) {
            $(this).attr('id', $(this).attr('name'));
        }
    });

    if ($('#cert-name').val() == " ") {
        // No name entered on "bestellung" page (defaults to "$(firstname) $(lastname)", hence the space
        $('#cert-name').val("");
    }

    function cleanupInputs () {
        $('form').validationEngine('hideAll');
        $(":text").labelify({ labelledClass: "labelHighlight" });
    }

    $(':input')
        .focus(cleanupInputs);
    $('input[type=checkbox]')
        .click(cleanupInputs);
    $('#title').val($('#title-default').val());
    $('#academic-title').val($('#academic-title-default').val());
    $('form').validationEngine();

    // need to use direct dom method, not jquery, so that interaction with labelify works
    document['ueberweisung-form'].onsubmit = function () {
        return $('form').validationEngine('validate');
    }

    $(":text").labelify({ labelledClass: "labelHighlight" });
    $('#spendenbescheinigung').change(function () {
        if (this.checked) {
            $('#anschrift').show();
        } else {
            $('#anschrift').hide();
        }
    });
});