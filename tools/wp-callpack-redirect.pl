#!/usr/bin/perl -w

use strict;

use HTTP::Daemon;
use HTTP::Status;
use LWP::UserAgent;

my $port = "3456";

my $ua = LWP::UserAgent->new;
my $daemon = HTTP::Daemon->new(LocalPort => 3456, ReuseAddr => 1);

while (my $client = $daemon->accept) {
    my $request = $client->get_request;
    if ($request) {
	my $content = $request->content;

	my $is_test = ($content =~ /testMode=100/);
	my $host = $is_test ? "test.createrainforest.org" : "localhost";

	$ua->default_header('Content-Type', $request->header('Content-Type'));
	my $response = $ua->get("http://" . $host . ":8080/handle-sale?" . $content);
	$client->send_response($response);
	print "Redirected request to ", ($is_test ? "TEST" : "PRODUCTION"), " system\n";
    }
    $client->close;
}
