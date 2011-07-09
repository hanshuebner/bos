
function get_iframe_url() {
    return $('#textbox_spendino iframe').attr('src');
}

var old_iframe_url = get_iframe_url();

function do_poll()
{
    var iframe_url = get_iframe_url();
    console.log('poll', iframe_url);

    if (iframe_url != old_iframe_url) {
        console.log('url', iframe_url);
        old_iframe_url = iframe_url;
    }
}


$(document).ready(function() {
    console.log('start polling');
    setInterval(do_poll, 1000);
});