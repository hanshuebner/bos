$(function () {
    $('.infoPopup').parent().each(function () {
        // tracker
        var shown = false;
        
        var trigger = $(this)
        var popup = $('.infoPopup', this);
        var displayContainer = $('#info_area');

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
                var topMargin = $(this).offset().top - displayContainer.offset().top;
                var excess = topMargin + newInfo.height() - $(displayContainer.get(0).parentNode).height() + 20;
                if (excess > 0) {
                    topMargin -= excess;
                }
                $(displayContainer.children().get(0))
                    .css('min-height', topMargin);
            }
        }).mouseout(function () {
            shown = false;
            displayContainer.empty();
        });
    });
});
