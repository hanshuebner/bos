// JavaScript Document
//
//  In my case I want to load them onload, this is how you do it!
//

Event.observe(window, 'load', loadAccordions, false);
//
//      Set up all accordions
//
function loadAccordions() {


    var mainAccordion = new accordion('content_main',{
            resizeSpeed : 15,

            defaultSize : {
                height:75
            }
        });

    //     var VerticalAccordion = new accordion('vertical_container', {

    //             classNames : {
    //                 toggle : 'vertical_accordion_toggle',
    //                 toggleActive : 'vertical_accordion_toggle_active',
    //                 content : 'vertical_accordion_content'
    //             }
    //         });

    // Open first one
    mainAccordion.activate($$('#content_main .accordion_toggle')[0]);

    //VerticalAccordion.activate($$('#vertical_container .vertical_accordion_toggle')[0]);
    // Special thanks go out to Will Shaver @ http://primedigit.com/

    //     var verticalAccordions = $$('.accordion_toggle');
    //     verticalAccordions.each(function(accordion) {
    //             $(accordion.next(0)).setStyle(
    //                                           {
    //                                               height: '0px'}
    //                                           );
    //         });

}

// remove the registerOverlay call to disable the controlbar
hs.registerOverlay(
                   {
                       thumbnailId: null,
                           overlayId: 'controlbar',
                           position: 'top right',
                           hideOnMouseOut: true
                           }
                   );

hs.graphicsDir = '/static/poi/graphics/';
hs.outlineType = 'rounded-white';
// Tell Highslide to use the thumbnail's title for captions
//hs.captionEval = 'this.thumb.title';
