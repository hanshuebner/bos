
/**
 * Module dependencies.
 */

var express = require('express');
var fs = require('fs');

var app = module.exports = express.createServer({
    key: fs.readFileSync('privatekey.pem'),
    cert: fs.readFileSync('certificate.pem')
});

// Configuration

app.configure(function(){
  app.set('views', __dirname + '/views');
  app.set('view engine', 'jade');
  app.use(express.bodyParser());
  app.use(express.methodOverride());
  app.use(app.router);
  app.use(express.static(__dirname + '/public'));
});

app.configure('development', function(){
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true })); 
});

app.configure('production', function(){
  app.use(express.errorHandler()); 
});

// Routes

app.post('/status', function(req, res) {
    res.render('index', {
        title: 'Status updated'
    });
    console.log('xtxid:', req.body.xtxid, 'status:', req.body.status);
});

app.get('/status', function(req, res) {
    res.render('index', {
        title: 'All well'
    });
    console.log("GET /status request");
});

app.listen(8077);
console.log("Express server listening on port %d in %s mode", app.address().port, app.settings.env);
