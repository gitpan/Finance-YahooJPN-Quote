package Finance::YahooJPN::Quote;

use 5.008;
use strict;
use warnings;
use utf8;

our $VERSION = '0.09'; # 2003-10-03 (since 2001-05-30)

use Carp;
use LWP::Simple;
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

Historical quote data is basis for analyzing stock market. In Japan, standard quote data is indicated as a set of data: four prices (open, high, low, close) and volume of each day. This module provides module user some functions to get historical quote of a company.

=cut

# initialize package global values
my $Japan_Standard_Time = time + (9 * 3600);
my $Today = join '-', (
	                ( gmtime($Japan_Standard_Time) )[5] + 1900 ,
	sprintf('%02d', ( gmtime($Japan_Standard_Time) )[4] + 1   ),
	sprintf('%02d', ( gmtime($Japan_Standard_Time) )[3]       ),
);
undef $Japan_Standard_Time;

=head1 METHODS

=over

=item historical($symbol [, 'start' => $start] [, 'noadjust' => 1])

This class method automatically C<new()>, C<fetch()> and C<extract()> then C<output()>.

See the descriptions about the following methods for the attributes: C<$symbol>, C<start> and C<noadjust>.

=cut

sub historical {
	my($class, $symbol, %option) = @_;
	
	my $self = $class->new($symbol);
	
	foreach my $key (keys %option) {
		my $lowercase = $key;
		$lowercase =~ tr/A-Z/a-z/;
		unless ($lowercase eq 'start' or $lowercase eq 'noadjust') {
			croak "Invalid attribute name: $key";
		}
		$option{$lowercase} = $option{$key};
	}
	
	if ($option{'start'}) {
		$self->fetch('start' => $option{'start'});
	}
	else {
		$self->fetch();
	}
	if ($option{'noadjust'}) {
		$self->extract('noadjust' => $option{'noadjust'});
	}
	else {
		$self->extract();
	}
	$self->output();
}

=item new($symbol)

Constructor class method. A stock C<$symbol> should be given with 4-digit code number and optionaly followed by a letter extension (dot `.' and an alphabet). (i.e. `6758' or `6758.t')

Japanese stock markets use 4-digit code number as stock symbol. Plus, add an alphabetical letter extention to indicate its exchange market. For example, the stock symbol code of Sony Corp. is '6758' and the letter extention of the Tokyo Stock Exchange is '.t'. Hence, the stock quote of the Sony Corp. at Tokyo Stock Exchange is specified as '6758.t'.

According to the Yahoo-Japan-Finance's description L<http://help.yahoo.co.jp/help/jp/fin/quote/stock/quote_02.html> the letter extentions of each exchange market are:

 .t: Tokyo   Stock Exchange
 .o: Osaka   Stock Exchange
 .n: Nagoya  Stock Exchange
 .s: Sapporo Stock Exchange
 .f: Fukuoka Stock Exchange
 .q: JASDAQ
 .j: Nippon New Market (Hercules)

Letter extention is omittable. When it was omit, the default exchange market is chosen by Yahoo-Japan-Finance's server. It is not certain but I guess that a default one should be the main exchange market of a stock.

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

=item fetch(['start' => $start])

This object method fetches the stock's historical quote pages of Yahoo-Japan-Finance from the C<$start> date to the current date.

A C<$start> date should be given in the format `YYYY-MM-DD' (ex. `2003-08-14'). Be careful, don't forget to quote the word, because bare word 2000-01-01 will be comprehend by Perl as '2000 - 1 - 1 = 1998'. This attribute is omittable. The default value of C<$start> is '1980-01-01'.

You cannot specify the last date. Because, to find the splits you must scan all of the quote from the start date. Without the splits data, estimation of adjustment for the splits cannot do exactly.

=cut

sub fetch {
	my($self, %term) = @_;
	
	$self->{'start'} = '1980-01-01';
	$self->{'last' } = $Today;
	
	foreach my $key (keys %term) {
		my $lowercase = $key;
		$lowercase =~ tr/A-Z/a-z/;
		unless ($lowercase eq 'start' or $lowercase eq 'last') {
			croak "Invalid attribute name: $key";
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
		my $url = "http://chart.yahoo.co.jp/t?a=$month_a&b=$day_a&c=$year_a&d=$month_z&e=$day_z&f=$year_z&g=d&s=$self->{'symbol'}&y=$y";
		my $remotedoc = decode('euc-jp', get($url));
		
		# testing it is valid symbol or not.
		if ($remotedoc =~ m/のデータはありません。/) {
			last;
		}
		# testing it is valid term or not.
		if ($remotedoc =~ m/この期間の価格はありません。/) {
			last;
		}
		# testing whether it is the final page (with bulk rows) or not
		if ($remotedoc =~ m|\n<tr bgcolor="#dcdcdc"><th>日付</th><th>始値</th><th>高値</th><th>安値</th><th>終値</th><th>出来高</th><th>調整後終値\*</th></tr>\n</table>\n|) {
			last;
		}
		
		push @remotedoc, $remotedoc;
	}
	
	@{ $self->{'fetched'} } = @remotedoc;
	
	return $self;
}

=item extract(['noadjust' => 1])

This object method extracts the stock's historical quote data from fetched pages of Yahoo-Japan-Finance.

The C<noadjust> option can turn on/off the function of value adjustment for the splits. If you omit this option or set this value '0', adjustment function is effective (default). If you set this value other than '0', adjustment function is ineffective.

=cut

sub extract {
	my($self, %noadjust) = @_;
	
	# in case the symbol is invalid (no data exists)
	unless (@{ $self->{'fetched'} }) {
		return $self;
	}
	
	for (my $i = 0; $i <= $#{ $self->{'fetched'} }; $i++) {
		# split the page to lines
		my @page = split /\n/, $self->{'fetched'}->[$i];
		
		# remove lines before & after the quote data rows.
		my($cut_from_here, $cut_by_here);
		for (my $j = 0; $j <= $#page; $j++) {
			if ($page[$j] =~ m|^<tr bgcolor="#dcdcdc"><th>日付</th><th>始値</th><th>高値</th><th>安値</th><th>終値</th><th>出来高</th><th>調整後終値\*</th></tr>$|)
			{
				$cut_from_here = $j + 2;
				unless ($page[$cut_from_here - 1] =~ m/^<tr$/) {
					# only in the case split row is the top row
					$cut_from_here--;
				}
			}
		}
		for (my $j = $cut_from_here; $j <= $#page; $j++) {
			if ($page[$j] =~ m|</table>|) {
				$cut_by_here = $j;
				last;
			}
		}
		
		# restruct a new list with the quote data rows
		my @table;
		for (my $j = $cut_from_here; $j <= $cut_by_here; $j++) {
			push @table, $page[$j];
		}
		
		# remove needless texts at the head of the lines
		# (except for the top split row)
		foreach my $row (@table) {
			$row =~ s/^align=right><td>//;
		}
		
		foreach my $row (@table) {
			my ($date, $open, $high, $low, $close, $volume, $extra);
			# in the case the row is the top split row
			if ($row =~ m/^<tr><td align=right>/) {
				$row =~ s/<tr><td align=right>/><td align=right>/;
				$row =~ s|</td></tr><tr$||;
				$extra = $row;
			}
			# this case is normal: quote data rows
			else {
				# split the line with </td><td>
				($date, $open, $high, $low, $close, $volume, $extra) =
					split /<\/td><td>/, $row;
				$close =~ s|<b>(.*?)</b>|$1|;
				
				# changing date & numeric formats
				$date =~ s/(\d{4})年(\d{1,2})月(\d{1,2})日/$1-$2-$3/;
				$date =~ s/-(\d)-/-0$1-/;
				$date =~ s/-(\d)$/-0$1/;
				foreach my $number ($open, $high, $low, $close, $volume) {
					$number =~ tr/,//d;
				}
				
				$row = join "\t", ($date, $open, $high, $low, $close, $volume);
				# store the quote data in the style just we've wanted ever!
				push @{ $self->{'quote'} }, $row;
			}
			
			# here it is, another splits infomations...
			# remove the bottom row. you don't worry, because a split
			# row will never appears in the bottom row.
			$extra =~ s|^.*</table>$||;
			# if the row data don't contain the split data, it is
			# converted to a bulk data ('').
			$extra =~ s|^.*?</td></tr><tr||;
			# find the splits!
			unless ($extra eq '') {
				$extra =~ s|><td align=right>(\d{4})年(\d{1,2})月(\d{1,2})日</td><td colspan=6 align=center>分割: (.*?)株 -> (.*?)株.*|$1-$2-$3\t$4\t$5|;
				$extra =~ s/-(\d)-/-0$1-/;
				$extra =~ s/-(\d)\t/-0$1\t/;
				push @{ $self->{'splits'} }, $extra;
			}
		}
	
   	}
	
	if (%noadjust) {
		foreach my $key (keys %noadjust) {
			my $lowercase = $key;
			$lowercase =~ tr/A-Z/a-z/;
			unless ($lowercase eq 'noadjust') {
				croak "Invalid attribute name: $key";
			}
			unless ($noadjust{$key} != 0) {
				$self->_adjustment();
			}
		}
	}
    else {
		$self->_adjustment();
    }
	
	@{ $self->{'quote'} } = reverse @{ $self->{'quote'} };
	
	return $self;
}

sub _adjustment {
	my $self = shift;
	
	my $j = 0;
	for (my $k = 0; $k <= $#{ $self->{'splits'} }; $k++) {
		my ($split_date, $split_pre, $split_post) =
			split /\t/, $self->{'splits'}->[$k];
		for (my $i = $j; $i <= $#{ $self->{'quote'} }; $i++) {
			my $date = ( split /\t/, $self->{'quote'}->[$i] )[0];
			if ($date eq $split_date) {
				$j = $i + 1;
				last;
			}
		}
		for (my $i = $j; $i <= $#{ $self->{'quote'} }; $i++) {
			my($date, $open, $high, $low, $close, $volume) =
				split /\t/, $self->{'quote'}->[$i];
			foreach my $price ($open, $high, $low, $close) {
				$price = int($price * $split_pre / $split_post + 0.5);
			}
			$volume = int($volume * $split_post / $split_pre + 0.5);
			$self->{'quote'}->[$i] =
				"$date\t$open\t$high\t$low\t$close\t$volume";
		}
	}
	
	return 1;
}

=item output()

This object method returns the extracted quote as a list.

=back

=cut

sub output {
	my $self = shift;
	
	# in case the symbol is invalid (no data exists)
	unless (@{ $self->{'fetched'} }) {
		return;
	}
	
	return @{ $self->{'quote'} };
}

1;
__END__

=head1 NOTES

The mudule calculates adjusted values originally including closing price. The only adjusted values which Yahoo presents are closing prices, and those numbers are not rounded but cut for decimal fractions. For this reason, I decided to ignore Yahoo's adjusted values (that's why some adjusted closing prices are different from Yahoo's).

=head1 AUTHOR

Masanori HATA E<lt>lovewing@geocities.co.jpE<gt> (Saitama, JAPAN)

=head1 COPYRIGHT

Copyright (c)2001-2003 Masanori HATA. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

