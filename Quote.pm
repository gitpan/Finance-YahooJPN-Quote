package Finance::YahooJPN::Quote;

use 5.008;
use strict;
use warnings;
use utf8;

our $VERSION = '0.13'; # 2004-11-13 (since 2001-05-30)

use Carp;
use IO::Socket;
use Encode;

=head1 NAME

Finance::YahooJPN::Quote - fetch a quote of Japanese stock market

=head1 SYNOPSIS

  use Finance::YahooJPN::Quote;
  
  # get the quote of Sony Corp. at Tokyo market.
  my @quote = Finance::YahooJPN::Quote->histrical('6758.t');
  
  my $quote = join "\n", @quote;
  print $quote;

=head1 DESCRIPTION

Historical quote data is basis for analyzing stock market. Here in Japan, standard quote data is indicated as a set of data: four prices (open, high, low, close) and volume for each day. This module provides module user some functions to get historical quote of a company.

=cut

# initialize package global values
my $Japan_Standard_Time = time + (9 * 3600);
my $Today = join '-', (
	                ( gmtime($Japan_Standard_Time) )[5] + 1900 ,
	sprintf('%02d', ( gmtime($Japan_Standard_Time) )[4] + 1   ),
	sprintf('%02d', ( gmtime($Japan_Standard_Time) )[3]       ),
);
undef $Japan_Standard_Time;

my $Server = 'table.yahoo.co.jp';

=head1 METHODS

=over

=item historical($symbol [, 'start' => $start] [, 'noadjust' => 1])

This class method automatically C<new()> and C<scan()> then C<output()> a historical series of quote of the stock which specified with C<$symbol> argument.

See the descriptions about the following methods for the argument and attributes: C<$symbol>, C<start> and C<noadjust>.

=cut

sub historical {
	my($class, $symbol, %option) = @_;
	
	my $self = $class->new($symbol);
	
	foreach my $key (keys %option) {
		my $lowercase = $key;
		$lowercase =~ tr/A-Z/a-z/;
		unless ($lowercase eq 'start' or $lowercase eq 'noadjust') {
			carp "unknown attribute name: $key (will be ignored)";
		}
		$option{$lowercase} = $option{$key};
	}
	
	if ($option{'start'}) {
		$self->scan('start' => $option{'start'});
	}
	else {
		$self->scan();
	}
	
	if ($option{'noadjust'} and $option{'noadjust'} == 1) {
		$self->output('noadjust' => 1);
	}
	else {
		$self->output();
	}
}

=item new($symbol)

Constructor class method. A stock C<$symbol> should be given with 4-digit code number and optionaly followed by a letter extension (dot `.' and an alphabet). (i.e. `6758' or `6758.t')

Japanese stock market use 4-digit code number as a stock symbol. Plus, add an alphabetical letter extention to indicate its exchange market. For example, the stock symbol code of Sony Corp. is '6758' and the letter extention of Tokyo Stock Exchange is '.t'. Hence, the stock quote of Sony Corp. at Tokyo Stock Exchange is specified as '6758.t'.

According to the Yahoo-Japan-Finance's description L<http://help.yahoo.co.jp/help/jp/fin/quote/stock/quote_02.html> the letter extentions of each exchange market are:

 .t: Tokyo   Stock Exchange
 .o: Osaka   Stock Exchange
 .n: Nagoya  Stock Exchange
 .s: Sapporo Stock Exchange
 .f: Fukuoka Stock Exchange
 .q: JASDAQ
 .j: Nippon New Market (Hercules)

Letter extention is omittable. When it was omit, the default exchange market is chosen by Yahoo-Japan-Finance's server. It is not certain but I guess that a default one should be the main exchange market of a stock. Note: since almost symbols should work without letter extention, I have experienced certain problems with a few symbols those which have originally `.j' letter extention. This is of course not for the module but owe to the Yahoo-Japan-Finance server's behavior.

=cut

sub new {
	my($class, $symbol) = @_;
	my $self = {};
	bless $self, $class;
	
	unless ($symbol) {
		croak "'symbol' argument isn't omittable";
	}
	if ($symbol =~ /^\d{4}(\.[a-zA-Z]){0,1}$/) {
		$self->{'symbol'} = $symbol;
	}
	else {
		croak "A stock symbol should be given with four numbers and optionaly followed by a letter extension (dot `.' and an alphabet). (i.e. `6758' or `6758.t' )";	}
	
	return $self;
}

=item scan(['start' => $start])

This object method scans the stock's historical quote pages of Yahoo-Japan-Finance from the C<$start> date to the current date. And picks up quote data of each day from that pages.

A C<$start> date should be given in the format `YYYY-MM-DD' (ex. `2003-08-14'). Be careful, don't forget to quote the word, because bare word 2000-01-01 will be comprehend by Perl as '2000 - 1 - 1 = 1998'. This attribute is omittable. The default value of C<$start> is '1980-01-01'.

You cannot specify the last date. Because, to find the splits you must scan all of the quote from the start date. Without the splits data, estimation of adjustment for the splits cannot be done exactly.

Note that a datetime for this module is based on JST (Japan Standard Time: GMT +09:00).

=cut

sub scan {
	my($self, %term) = @_;
	
	$self->{'start'} = '1980-01-01';
	$self->{'last' } = $Today;
	
	foreach my $key (keys %term) {
		my $lowercase = $key;
		$lowercase =~ tr/A-Z/a-z/;
		unless ($lowercase eq 'start' or $lowercase eq 'last') {
			carp "unknown attribute name: $key (will be ignored)";
		}
		unless ($term{$key} =~ /^\d{4}-\d{2}-\d{2}$/) {
			croak "A date should be given in the format `YYYY-MM-DD'. (ex. `2003-08-14')";
		}
		$self->{$lowercase} = $term{$key};
	}
	
	# estimate term to fetch
	my($year_a, $month_a, $day_a) = split /-/, $self->{'start'};
	my($year_z, $month_z, $day_z) = split /-/, $self->{'last' };
	
	# multi page fetching
	my @remotedoc;
	for (my $page = 0; ; $page++) {
		my $y = $page * 50; # 50rows/1page is max at Yahoo-Japan-Finance
		my $abs_path = "/t?a=$month_a&b=$day_a&c=$year_a&d=$month_z&e=$day_z&f=$year_z&g=d&s=$self->{'symbol'}&y=$y";
		my $remotedoc = decode( 'euc-jp', $self->_fetch($abs_path) );
		
		# testing it is valid symbol or not.
		if ($remotedoc =~ m/のデータはありません。/) {
			last;
		}
		# testing it is valid term or not.
		if ($remotedoc =~ m/この検索期間の価格データはありません。/) {
			last;
		}
		# testing whether it is the final page (with bulk rows) or not
		unless ($remotedoc =~ m|<tr bgcolor="#eeeeee"><th><small>日付</small></th><th><small>始値|) {
			last;
		}
		
		$self->_collect($remotedoc);
	}
	
	return $self;
}

# _fetch($url)
# This private method fetches a web page.
sub _fetch {
	my($self, $abs_path) = @_;
	
	my $sock = IO::Socket::INET->new(
		PeerAddr => $Server,
		PeerPort => 'http(80)',
		Proto    => 'tcp',
	) or die "Couldn't connect to $Server";
	
	print $sock <<"EOF";
GET $abs_path HTTP/1.1
Host: $Server

EOF
	
	my @html = <$sock>;
	close $sock;
	
	my $html = join '', @html;
	# newline character unification
	$html =~ s/\x0D\x0A|\x0D|\x0A/\n/g;
	
	return $html;
}

# _collect($html)
# This private object method collects a stock's historical quote
# of a C<$html> page fetched from Yahoo-Japan-Finance.
sub _collect {
	my($self, $html) = @_;
	
	# split the page to lines
	my @html = split /\n/, $html;
	
	# discard useless lines before the quote rows.
	while (@html) {
		my $line = shift @html;
		last if $line =~ m|^<tr bgcolor="#eeeeee"><th><small>日付</small></th><th><small>始値|;
	}
	
	# discard again the next line
	unless ($html[0] eq "<tr") {
		# this row is information of a split
		$self->_pickup_split($html[0]);
	}
	shift @html;
	
	foreach my $line (@html) {
		if ($line =~ m/^align=right bgcolor="#ffffff"><td><small>\d{4}年/) {
			# this row is a quote
			$self->_pickup($line);
		}
		else {
			last;
		}
	}
	
	$self->_adjustment();
	
	return $self;
}

# _pickup($row)
# This private object method picks up a quote data in a row.
sub _pickup {
	my($self, $row) = @_;
	
	$row =~ m|^align=right bgcolor="#ffffff"><td><small>(\d{4})年(\d{1,2})月(\d{1,2})日</small></td><td><small>(.+?)</small></td><td><small>(.+?)</small></td><td><small>(.+?)</small></td><td><small><b>(.+?)</b></small></td><td><small>(.+?)</small></td><td><small>.+?</small></td>(.+?)$|;
	
	my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
	
	my($open, $high, $low, $close, $volume) = ($4, $5, $6, $7, $8);
	foreach my $num ($open, $high, $low, $close, $volume) {
		$num =~ tr/,//d;
	}
	
	my $extra = $9;
	if ($extra =~ m|<tr bgcolor="#ffffff"><td align=right bgcolor="#ffffff"><font size=-1>\d{4}年|) {
		# this row is information of a split
		$self->_pickup_split($extra);
	}
	
	# store the quote data as a package variable
	$row = join "\t", $date, $open, $high, $low, $close, $volume;
	unshift @{ $self->{'q_noadjust'} }, $row;
	
	return $self;
}

# _pickup_split($row)
# This private object method picks up a split data in a row.
sub _pickup_split {
	my($self, $row) = @_;
	
	# in case split rows are continuous with each other.
	my @string = split m|</td></tr><tr>|, $row;
	
	foreach my $string (@string) {
		$string =~ m|^.*?<font size=-1>(\d{4})年(\d{1,2})月(\d{1,2})日</font></td><td colspan=6 align=center><font size=-1>分割: (.+?)株 -> (.+?)株.*?$|;
		
		my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
		
		my($split_pre, $split_post) = ($4, $5);
		
		# store the split data as a package variable
		$string = join "\t", $date, $split_pre, $split_post;
		push @{ $self->{'splits'} }, $string;
	}
	
	return $self;
}

# _adjustment()
# This private object method calculates a stock's historical quote
# data which is adjusted for splits.
sub _adjustment {
	my $self = shift;
	
	@{ $self->{'q_adjust'} } = @{ $self->{'q_noadjust'} };
	my $last = $#{ $self->{'q_adjust'} };
	
	my $j = $last;
	for (my $k = 0; $k <= $#{ $self->{'splits'} }; $k++) {
		my($split_date, $split_pre, $split_post) =
			split /\t/, $self->{'splits'}->[$k];
		
		for (my $i = $j; $i >= 0; $i--) {
			my $date = ( split /\t/, $self->{'q_adjust'}->[$i] )[0];
			if ($date eq $split_date) {
				$j = $i - 1;
				last;
			}
		}
		
		for (my $i = 0; $i <= $j; $i++) {
			my($date, $open, $high, $low, $close, $volume) =
				split /\t/, $self->{'q_adjust'}->[$i];
			
			foreach my $price ($open, $high, $low, $close) {
				$price = int($price * $split_pre / $split_post + 0.5);
			}
			$volume = int($volume * $split_post / $split_pre + 0.5);
			
			$self->{'q_adjust'}->[$i] =
				"$date\t$open\t$high\t$low\t$close\t$volume";
		}
	}
	
	return 1;
}

=item output(['noadjust' => 1])

This object method returns the collected quote data as a list.

The C<noadjust> option can turn on/off the function of value adjustment for the splits. If you omit this option or set this value '0', adjustment function is effective (default). If you set this value other than '0', adjustment function is ineffective.

The data is formatted as TSV (Tab Separated Values). Each row represents quote of each day in the order with 1)date, 2)open, 3)high, 4)low, 5)close and 6)volume.

=back

=cut

sub output {
	my($self, %noadjust) = @_;
	
	# in case the symbol is invalid (no data exists)
	unless (exists $self->{'q_noadjust'}) {
		return;
	}
	if (%noadjust) {
		foreach my $key (keys %noadjust) {
			my $lowercase = $key;
			$lowercase =~ tr/A-Z/a-z/;
			unless ($lowercase eq 'noadjust') {
				carp "unknown attribute name: $key (will be ignored)";
			}
			if ($noadjust{$key} != 0) {
				return @{ $self->{'q_noadjust'} };
			}
		}
	}
 	
	return @{ $self->{'q_adjust'} };
}

1;
__END__

=head1 NOTES

The mudule calculates adjusted values originally including closing prices. The only adjusted values which Yahoo-Japan-Finance presents are closing prices, and those numbers are not rounded but cut for decimal fractions. For this reason, I have decided to ignore Yahoo-Japan-Finance's adjusted closing prices. That is why some adjusted closing prices are different from Yahoo-Japan-Finance's.

=head1 AUTHOR

Masanori HATA E<lt>lovewing@dream.big.or.jpE<gt> (Saitama, JAPAN)

=head1 COPYRIGHT

Copyright (c)2001-2004 Masanori HATA. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

