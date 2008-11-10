#!/usr/local/bin/perl -w
use 5.008;
use strict;
use warnings;

use Module::Build;

Module::Build->new(
    module_name       => 'Finance::YahooJPN::Quote',
    dist_version_from => 'Quote.pm',
    dist_author       => 'Masanori HATA <http://www.mihr.net> (Saitama, JAPAN)',
    dist_abstract     => 'to get a quote in Japanese stock market',
    license           => 'perl',
    requires          => {
        'perl'         => '5.8.0',
    },
    recommends        => {
        'perl'         => '5.10.0',
    },
    pm_files          => {
        'Quote.pm'   => 'lib/Finance/YahooJPN/Quote.pm',
    },
    test_files        => ['t/Quote.t'],
    create_readme     => 1,
    )->create_build_script;
