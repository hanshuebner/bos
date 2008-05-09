// JavaScript Document
function Pano()
{

	var _initialized = false;
	var _imageloaded = false;
	var _pos = 0;            // Current scroll position
	var _increment = 1;      // How many pixels to shift per frame
	var _paneWidth = 200;    // Width of the scrollpane
	var _img;                // The image
	var _pane;               // The scrollpane
	var _htmlBackup;         // Backup of the pane content
    var _height = "360px";
	
	this.scrollPano = scrollPano;
	this.imageHasLoaded = imageHasLoaded;
	
/*	function setDiv(div){
		_div = 	div;
	}*/
	
	function scrollPano(image, width, height, div) {
        if (!_initialized) {
			//alert(div_id);
            _pane = document.getElementById(div);
            if (width) {
                _paneWidth = width;
            }
			 if (height) {
                _height = height + "px";
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
        
        //self.setTimeout("scrollPano()", 50);
        setTimeout(function() { scrollPano(); }, 30);
		
    
     }
	 
	 function imageHasLoaded() {
    _pane.innerHTML = _htmlBackup;
    //_pane.style.height = _img.height + "px";
	//_pane.style.height = (_img.height /2) + "px";
	_pane.style.height =  _height;
    _pane.style.width = _paneWidth + "px";
    _imageloaded = true;
     }

   
}
