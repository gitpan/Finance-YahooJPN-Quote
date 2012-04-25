package Finance::YahooJPN::Quote;

use 5.014;
use warnings;
use utf8;

our $VERSION = '1.00'; # 2012-04-26 (since 2001-05-30)

use Carp;
use IO::Socket;
use Encode;

=head1 NAME

Finance::YahooJPN::Quote -- For fetching histrical stock quotes in Japan from Yahoo! Japan Finance.

=head1 SYNOPSIS

  use Finance::YahooJPN::Quote;
  
  # Get quote of Sony Corp. at the Tokyo market.
  my @quote = Finance::YahooJPN::Quote->histrical('6758.t');
  
  print join("\n", @quote);

=head1 DESCRIPTION

Historical quote data is basis for analyzing stock market. Here in Japan, standard quote data is indicated as a set of data: the four prices (open, high, low, close) and the volume of each day. This module provides its user some functions to get historical quote of a company.

=cut

# initialize global variables of this package
my $Japan_Standard_Time = time + 32400; # 9h * 60m * 60s = 32400s
my $Today = join '-', (
                    ( gmtime($Japan_Standard_Time) )[5] + 1900 ,
    sprintf('%02d', ( gmtime($Japan_Standard_Time) )[4] + 1   ),
    sprintf('%02d', ( gmtime($Japan_Standard_Time) )[3]       ),
    );
undef $Japan_Standard_Time;

my $Server = 'table.yahoo.co.jp';

my $Debug = 0;

=head1 METHODS

=over

=item historical($symbol [, 'start' => $start] [, 'noadjust' => 1])

This class method automatically C<new()> and C<scan()> then C<output()> a historical series of quote of the stock which specified with C<$symbol> argument.

See the descriptions about the following methods for the argument and attributes: C<$symbol>, C<start> and C<noadjust>.

=cut

sub debug {
    my($class, $level, $symbol) = @_;
    if ($level !~ m/^[1-5]$/) {
        croak 'You must specify debug level [1-5]';
    }
    my $start = '2000-01-01';
    my $last  = '2000-04-01';
    $Debug = $level;
    
    my $self = $class->new($symbol);
    $self->scan('start' => $start, 'last' => $last);
    print join("\n", $self->output('noadjust' => 1)) if $Debug == 4;
    print join("\n", $self->output(               )) if $Debug == 5;
}

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

Japanese stock markets use 4-digit code numbers for stock symbols. In addtion to that, an alphabetical letter extention is used for indicating its exchanging place. For example, the stock symbol code of Sony Corp. is '6758' and the letter extention of the Tokyo Stock Exchange is '.t'. Hence, the stock quote of Sony Corp. at Tokyo Stock Exchange is specified as '6758.t'.

According to the Yahoo-Japan-Finance's description L<http://help.yahoo.co.jp/help/jp/finance/quote/quote-02.html> the letter extentions of exchanging place are:

 .t: Tokyo   Stock Exchange
 .o: Osaka   Stock Exchange
 .q: JASDAQ
 .n: Nagoya  Stock Exchange
 .s: Sapporo Stock Exchange
 .f: Fukuoka Stock Exchange

Letter extention is omittable. When it is omit, the default exchange market is chosen by the Yahoo-Japan-Finance's server. It is not certain but I guess that the default one should be the main exchange market of the stock. Note: since almost symbols should work without letter extention, I have experienced certain problems with a few symbols those which have originally `.j' letter extention. This is of course not for the module but owe to the Yahoo-Japan-Finance server's behavior.

There is exception for above. A few symbols of index are indicated in 5 to 7 digit code numbers. They are '998405' (TOPIX), '998407' (NIKKEI) and '23337' (JASDAQ). L<http://help.yahoo.co.jp/help/jp/finance/quote/quote-03.html>

=cut

sub new {
    my($class, $symbol) = @_;
    my $self = {};
    bless $self, $class;
    
    unless ($symbol) {
        croak "'symbol' argument can't be omittable";
    }
    if (
           $symbol eq '998405'
        or $symbol eq '998407'
        or $symbol eq '23337'
        or $symbol =~ /^\d{4}(\.[a-zA-Z]){0,1}$/
       ) {
        $self->{'symbol'} = $symbol;
    }
    else {
        croak "Stock symbol must be given in a 4-digit number and optionally which can be\nfollowed by a letter extension (a dot `.' and an alphabet).\n\tFor example: `6758' or `6758.t'\nExcept for these special index codes: 998405, 998407 and 23337.\n\n";	}
    
    return $self;
}

=item scan(['start' => $start])

This object method is for scanning the stock's historical quote pages of Yahoo-Japan-Finance from the C<$start> date to the current date. And for picking up quote data of each day on those pages.

Date of C<$start> must be given in the format `YYYY-MM-DD' (ex. `2003-08-14'). Be careful, don't forget to quote the word, because bare word 2000-01-01 will be comprehend by Perl as '2000 - 1 - 1 = 1998'. This attribute is omittable. The default value of C<$start> is '1980-01-01'.

You cannot specify a date of last day. Because, to find the splits you must scan the quote during whole of the period from the C<$start> day. Without split data, estimation of value adjustment for split cannot be done exactly.

Note that datetime of this module is based on JST (Japan Standard Time: GMT +09:00).

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
        
        # debug level 1 (it should output a raw html)
        if ($Debug == 1) {
            print $remotedoc;
            exit;
        }
        
        # testing it is valid symbol or not.
        if ($remotedoc =~ m/の価格データはありません。/) {
            last;
        }
        # testing it is valid term or not.
        if ($remotedoc =~ m/この検索期間の価格データはありません。/) {
            last;
        }
        # testing whether it is the overrun page (without quote table) or not
        unless ($remotedoc =~ m|<th><small>日付</small></th>\n<th><small>始値</small></th>\n<th><small>高値</small></th>\n<th><small>安値</small></th>\n<th><small>終値</small></th>|) {
            last;
        }
        
        $self->_collect($remotedoc);
    }
    
    return $self;
}

# _fetch($url)
# This private method is for fetching a web page.
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
# This private object method is for collecting historical quote of stock
# on the C<$html> which was fetched from Yahoo-Japan-Finance.
sub _collect {
    my($self, $html) = @_;
    
    # split the page to some lines
    my @html = split /\n/, $html;
    
    # discard useless lines before quote rows.
    while (@html) {
        my $line = shift @html;
        if (   $self->{'symbol'} eq '998405'
            or $self->{'symbol'} eq '998407'
            or $self->{'symbol'} eq '23337'
        ) {
            if ($line =~ m|^<th><small>終値</small></th>|) {
                shift @html;
                last;
            }
        }
        if ($line =~ m|^<th><small>調整後終値\*</small></th>|) {
            shift @html;
            last;
        }
    }
    
    # debug level 2 (it should output an html which has been cut the header part)
    if ($Debug == 2) {
        print encode('utf8', join("\n", @html));
        exit;
    }
    
    if (   $self->{'symbol'} eq '998405'
        or $self->{'symbol'} eq '998407'
        or $self->{'symbol'} eq '23337'
    ) {
        while ($html[0] eq '</tr><tr align=right bgcolor="#ffffff">'
            or $html[0] eq '<tr align=right bgcolor="#ffffff">'
            or $html[0] eq '</tr><tr bgcolor="#ffffff">'
            or $html[0] eq '<tr bgcolor="#ffffff">'
        ) {
            shift @html;
            
            $html[0] =~ m|<td><small>(\d{4})年(\d{1,2})月(\d{1,2})日</small></td>|;
            my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $open = $1;
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $high = $1;
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $low = $1;
            shift @html;
            
            $html[0] =~ m|<small><b>(.+?)</b></small></td>|;
            my $close = $1;
            shift @html;
            
            foreach my $num ($open, $high, $low, $close) {
                $num =~ tr/,//d;
            }
            
            # store the quote data as a package variable
            unshift @{ $self->{'q_noadjust'} }, join("\t", $date, $open, $high, $low, $close);
        }
    }
    else {
        while ($html[0] eq '</tr><tr align=right bgcolor="#ffffff">'
            or $html[0] eq '<tr align=right bgcolor="#ffffff">'
            or $html[0] eq '</tr><tr bgcolor="#ffffff">'
            or $html[0] eq '<tr bgcolor="#ffffff">'
        ) {
            if (   $html[0] eq '</tr><tr bgcolor="#ffffff">'
                or $html[0] eq '<tr bgcolor="#ffffff">'
            ) {
                # this is a split data
                shift @html;
                $html[0] =~ m/(\d{4})年(\d{1,2})月(\d{1,2})日/;
                my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
                shift @html;
                $html[0] =~ m/分割: (.+?)株 -> (.+?)株/;
                my($split_pre, $split_post) = ($1, $2);
                shift @html;
                
                # store this split data as a package variable
                push @{ $self->{'splits'} }, join("\t", $date, $split_pre, $split_post);
                
                next;
            }
            
            shift @html;
            
            $html[0] =~ m|<td><small>(\d{4})年(\d{1,2})月(\d{1,2})日</small></td>|;
            my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $open = $1;
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $high = $1;
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $low = $1;
            shift @html;
            
            $html[0] =~ m|<small><b>(.+?)</b></small></td>|;
            my $close = $1;
            shift @html;
            
            $html[0] =~ m|<td><small>(.+?)</small></td>|;
            my $volume = $1;
            shift @html;
            
            foreach my $num ($open, $high, $low, $close, $volume) {
                $num =~ tr/,//d;
            }
            
            # discard the ajusted closing price
            shift @html;
            
            # store the quote data as a package variable
            unshift @{ $self->{'q_noadjust'} }, join("\t", $date, $open, $high, $low, $close, $volume);
        }
    }
    
    # debug level 3 (it should output the rest (footer) of html)
    if ($Debug == 3) {
        print encode('utf8', join("\n", @html));
        exit;
    }
    
    $self->_adjustment();
    
    return $self;
}

# _adjustment()
# This private object method is for calculating the stock's historical quote
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

This object method is for returning the collected quote data in a list.

By C<noadjust> option you can turn on/off the function of value adjustment for splits. If you omit this option or set this value '0', adjustment function is effective (by default). If you set this value other than '0', adjustment function is ineffective.

Output data is formatted in TSV (Tab Separated Values). Each row represents quote of each day in the order with 1)date, 2)open, 3)high, 4)low, 5)close and 6)volume.

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

This mudule calculates adjusted values (including closing prices) by itself. Though Yahoo-Japan-Finance doesn't give only four prices but also adjusted closing prices, those values are not rounded but cut for decimal fractions (not good). For this reason, I have decided to ignore Yahoo-Japan-Finance's pre-adjusted closing prices. That is why some adjusted closing prices are different from Yahoo-Japan-Finance's.

=head1 AUTHOR

Masanori HATA L<http://www.mihr.net/> (Saitama, JAPAN)

=head1 COPYRIGHT

Copyright © 2001-2012 Masanori HATA. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

