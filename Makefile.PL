use 5.008;
use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'         => 'Finance::YahooJPN::Quote',
	'VERSION_FROM' => 'Quote.pm',
	($] >= 5.005 ? (
		'ABSTRACT' => 'to get a quote in Japanese stock market',
		'AUTHOR'   => 'Masanori HATA <http://www.mihr.net/>',
	) : ()),
);
