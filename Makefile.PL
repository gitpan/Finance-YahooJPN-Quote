use 5.008;
use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'         => 'Finance::YahooJPN::Quote',
	'VERSION_FROM' => 'Quote.pm',
	'PREREQ_PM'    => {
		'LWP::Simple' => '1.36',
	},
	($] >= 5.005 ? (
		'ABSTRACT' => 'fetch a quote of Japanese stock market',
		'AUTHOR'   => 'Masanori HATA <lovewing@geocities.co.jp>',
	) : ()),
);
