#!/usr/bin/env morbo

# -*- mode: perl-mode -*-

# https://mojolicious.org/
# Get morbo with:
# curl -L https://cpanmin.us | perl - -M https://cpan.metacpan.org -n Mojolicious

use Mojolicious::Lite;

get '/' => {text => 'I ♥ Mojolicious!'};

app->start;
