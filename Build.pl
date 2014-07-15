#!/usr/local/bin/perl -w
use 5.16.3;
use warnings;

use Module::Build;

Module::Build->new(
    module_name       => 'Finance::YahooJPN::Quote',
    dist_version_from => 'Quote.pm',
    dist_author       => 'Masanori HATA <http://www.mihr.net/> (Saitama, JAPAN)',
    dist_abstract     => 'Fetch historical stock quotes in Japan from Yahoo! Japan Finance.',
    license           => 'perl',
    requires          => {
        'perl'        => '5.16.3',
        'Math::Round' => '0.06',
    },
    recommends        => {
        'perl'              => '5.20.0',
        'IO::Socket::Socks' => '0.63',
    },
    pm_files          => {
        'Quote.pm' => 'lib/Finance/YahooJPN/Quote.pm',
    },
    test_files        => ['t/Quote.t'],
    create_readme     => 1,
    )->create_build_script;
