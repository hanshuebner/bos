var _initialized = false;
var _imageloaded = false;
var _pos = 0;            // Current scroll position
var _increment = 1;      // How many pixels to shift per frame
var _paneWidth = 200;    // Width of the scrollpane
var _img;                // The image
var _pane;               // The scrollpane
var _htmlBackup;         // Backup of the pane content

/*
 * scroll(image, width)
 * 
 *  Setup a pane for scrolling.
 *  The pane (normally a <div>) must have the id "scrollpane".
 *
 * Arguments:
 *
 *  image: URI to the image that's to be scrolled
 *
 *  width: optional, the width of the scroll pane
 */
function scroll(image, width, div_id) {
        if (!_initialized) {
			//alert(div_id);
            _pane = document.getElementById(div_id);
            if (width) {
                _paneWidth = width;
            }
             _pane.style.width = 200 + "px";
            _pane.style.height = 50 + "px";
            _htmlBackup = _pane.innerHTML;
           _pane.innerHTML =
                "<div style=\"text-align:center; \
                font-family: monospace; \
                font-weight: normal;\">Image loading...</div>";
            _pane.style.border = "1px solid black";
            _pane.style.backgroundImage = "url('" + image + "')";
			//document.getElementById('scrollpane').style.backgroundImage= "url('" + image + "')";

			
            _img = new Image();
            _img.src = image;
            _img.onload = imageHasLoaded;
            _initialized = true;
        }
        
        if (_imageloaded) {
            _pane.style.backgroundPosition = _pos + 'px 0px';
            
            _pos = (_pos - _increment) % _img.width;
        }
        
        self.setTimeout("scroll()", 50);
    
}

function imageHasLoaded() {
    _pane.innerHTML = _htmlBackup;
    //_pane.style.height = _img.height + "px";
	//_pane.style.height = (_img.height /2) + "px";
	_pane.style.height =  "360px";
    _pane.style.width = _paneWidth + "px";
    _imageloaded = true;
}

function makeFaster() {
    _increment++;
}

function makeSlower() {
    _increment--;
}

function makeBigger() {
   // if (_initialized) {
        _paneWidth += 20;
        _pane.style.width = _paneWidth + "px";
    //}
}

function makeSmaller() {
    //if (_initialized) {
        _paneWidth -= 20;
        _pane.style.width = _paneWidth + "px";
    //}
}