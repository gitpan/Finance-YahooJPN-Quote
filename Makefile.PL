use 5.008;
use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'         => 'Finance::YahooJPN::Quote',
	'VERSION_FROM' => 'Quote.pm',
	($] >= 5.005 ? (
		'ABSTRACT' => 'fetch a quote of Japanese stock market',
		'AUTHOR'   => 'Masanori HATA <lovewing@geocities.co.jp>',
	) : ()),
);
