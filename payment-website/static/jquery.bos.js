$(function () {
    $('.hasInfo').each(function () {
        // tracker
        var shown = false;
        
        var trigger = $(this)
        var popup = $('.infoPopup', this);
        var displayContainer = $('#textbox_left_form');

        // set the mouseover and mouseout on both element
        $([trigger.get(0), popup.get(0)]).mouseover(function () {
            if (!shown) {
                shown = true;

                displayContainer
                    .empty()
                    .html('<div></div>')
                    .css('display', 'block');
                var newInfo =
                    popup
                    .clone()
                    .removeClass('infoPopup')
                    .appendTo(displayContainer);
                var topMargin = trigger.offset().top - displayContainer.offset().top;
                var excess = topMargin + newInfo.height() - displayContainer.height();
                if (excess > 0) {
                    topMargin -= excess;
                }
                $(displayContainer.children().get(0))
                    .css('height', topMargin);
            }
        }).mouseout(function () {
            shown = false;
            displayContainer.empty();
        });
    });
});
