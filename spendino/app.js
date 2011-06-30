
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

// Allowed IP addresses for status update callbacks
var allowedPeers = {
    '89.238.64.138': true,      // spendino
    '89.238.76.182': true,      // spendino
    '178.63.163.33': true       // netzhansa.com
}

// Routes

app.post('/status', function(req, res) {
    var from = req.connection.socket.remoteAddress;
    if (allowedPeers[from]) {
        res.render('index', {
            title: 'Status updated'
        });
        console.log('from:', from, 'xtxid:', req.body.xtxid, 'status:', req.body.status);
    } else {
        console.log('from:', from, 'rejected');
        res.statusCode = 400;
        res.end('invocation of handler prohibited');
    }
});

app.get('/status', function(req, res) {
    res.render('index', {
        title: 'All well'
    });
    console.log("GET /status request");
});

app.get('/buy-success', function(req, res) {
    res.render('index', {
        title: 'donation successful'
    });
});

app.get('/buy-failure', function(req, res) {
    res.render('index', {
        title: 'donation not successful'
    });
});

app.listen(8077);
console.log("Express server listening on port %d in %s mode", app.address().port, app.settings.env);
