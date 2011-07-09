
/**
 * Module dependencies.
 */

var express = require('express');
var nodemailer = require('nodemailer');
var mustache = require('mustache');
var fs = require('fs');

nodemailer.SMTP = { host: 'localhost' };

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
};

// Database for unfulfilled payments.  When the payment process with
// spendino is started, the Lisp backend invokes the /start-payment
// URL to register the payment and the URL that it was invoked by with
// this process.  When the payment is completed or aborted, the
// callback is invoked and the user is redirected to the registered
// URL so that the iframe containing the payment process can access
// the containing page (which the callback handler can't do, as it is
// in a different security domain).

var expire = 600;               // expiry period for unfulfilled payments
var payments = [];              // outstanding payments

function findPayment(contractId)
{
    var retval;
    var newPayments = [];
    var now = (new Date()).getTime();
    // Scan for contract ID, garbage collect expired entries
    for (var i = 0; i < payments.length; i++) {
        var payment = payments[i];
        if (payment.contractId == contractId) {
            retval = payment;
        } else if ((now - payment.time) < expire) {
            newPayments.push(payment);
        } else {
            console.log('expired', payment);
        }
    }
    payments = newPayments;
    return retval;
}

// test callback: https://neu.schafft-lebenswald.de:8077/buy-success?xtxid=

function processCallback(req, res, success)
{
    var contractId = req.param('xtxid');
    var payment = findPayment(contractId);
    console.log('callback - success', success, 'contractId', contractId, 'payment', payment);
    if (payment) {
        if (success) {
            fs.readFile('mail-template.txt', 'utf-8', function (err, template) {
                if (err) {
                    console.log('error reading mail-template.txt:', err);
                    return;
                }
                nodemailer.send_mail({
                    sender: 'BOS Deutschland e.V. <mxm@bos-deutschland.de>',
                    to: payment.email,
                    subject: "Ihre Zugangsdaten",
                    body: mustache.to_html(template, { sponsorId: payment.sponsorId,
                                                       sponsorMasterCode: payment.sponsorMasterCode })
                },
                                     function(error) {
                                         if (error) {
                                             console.log('failed to send email to', payment.email,
                                                         'sponsor id', payment.sponsorId, 'error', error);
                                         }
                                     });
            });
        }
        res.redirect(payment.url
                     + '?success=' + success
                     + '&contract-id=' + contractId
                     + '&cartId=' + contractId
                     + '&email=' + payment.email
                     + '&name=' + ''
                     + '&sponsor-id=' + payment.sponsorId
                     + '&master-code=' + payment.sponsorMasterCode);
    } else {
        res.send('invalid transaction id', 404);
    }
}

// Routes

app.get('/start-payment', function(req, res) {
    var contractId = req.param('contract-id');
    var sponsorId = req.param('sponsor-id');
    var sponsorMasterCode = req.param('sponsor-master-code');
    var url = req.param('url');
    var email = req.param('email');
    console.log('/start-payment contract-id', contractId, 'url', url, 'email', email);
    payments.push({ contractId: contractId,
                    sponsorId: sponsorId,
                    sponsorMasterCode: sponsorMasterCode,
                    url: url,
                    email: email,
                    time: (new Date()).getTime() });
    res.send('payment registered');
});

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
    // garbage collect unfulfilled payments
    findPayment();
});

app.get('/buy-success', function(req, res) {
    processCallback(req, res, true);
});
app.get('/buy-failure', function(req, res) {
    processCallback(req, res, false);
});

app.listen(8077);
console.log("Express server listening on port %d in %s mode", app.address().port, app.settings.env);
