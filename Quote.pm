package Finance::YahooJPN::Quote;

use 5.014;
use warnings;
use utf8;

our $VERSION = '1.04'; # 2014-07-09 (since 2001-05-30)

use Carp;
use IO::Socket;
use Encode;

=head1 NAME

Finance::YahooJPN::Quote -- For fetching historical stock quotes in Japan from Yahoo! Japan Finance.

=head1 SYNOPSIS

  use Finance::YahooJPN::Quote;
  
  # Get the quote of Sony Corp. at the Tokyo stock exchange.
  my @quote = Finance::YahooJPN::Quote->historical('6758.t');
  
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

my $Server = 'info.finance.yahoo.co.jp';

my $Debug = 0;

=head1 METHODS

=over

=item historical($symbol [, 'start' => $start] [, 'noadjust' => 1])

This class method automatically C<new()> and C<scan()> then C<output()> a historical series of quote of the stock which specified with C<$symbol> argument.

See the descriptions about the following methods for the argument and attributes: C<$symbol>, C<start> and C<noadjust>.

=cut

sub debug {
    my($class, $level, $symbol, $start, $last) = @_;
    if ($level !~ m/^[1-6]$/) {
        croak 'You must specify debug level [1-6]';
    }
    $Debug = $level;
    
    my $self = $class->new($symbol);
    $self->scan('start' => $start, 'last' => $last);
    say join("\n", $self->output('noadjust' => 1)) if $Debug == 5;
    say join("\n", $self->output(               )) if $Debug == 6;
}

sub historical {
    my($class, $symbol, %option) = @_;
    my $self = $class->new($symbol);
    
    foreach my $key (keys %option) {
        my $lowercase = $key;
        $lowercase =~ tr/A-Z/a-z/;
        unless ($lowercase eq 'start' or $lowercase eq 'last' or $lowercase eq 'noadjust') {
            carp "unknown attribute name: $key (will be ignored)";
        }
        $option{$lowercase} = $option{$key};
    }
    
    if ($option{'start'}) {
        if ($option{'last'}) {
            $self->scan('start' => $option{'start'}, 'last' => $option{'last'});
        } else {
            $self->scan('start' => $option{'start'});
        }
    } elsif ($option{'last'}) {
            $self->scan('last' => $option{'last'});
    } else {
        $self->scan();
    }
    
    if ($option{'noadjust'} and $option{'noadjust'} == 1) {
        $self->output('noadjust' => 1);
    } else {
        $self->output();
    }
}

=item new($symbol)

Constructor class method. A stock C<$symbol> should be given with 4-digit code number and optionaly followed by a letter extension (dot `.' and an alphabet). (i.e. `6758' or `6758.t')

Japanese stock markets use 4-digit code numbers for stock symbols. In addtion to that, an alphabetical letter extention is used for indicating its exchanging place. For example, the stock symbol code of Sony Corp. is '6758' and the letter extention of the Tokyo Stock Exchange is '.t'. Hence, the stock quote of Sony Corp. at Tokyo Stock Exchange is specified as '6758.t'.

According to the Yahoo-Japan-Finance's description L<http://www.yahoo-help.jp/app/answers/detail/p/546/a_id/45387> the letter extentions of exchanging place are:

 .t: Tokyo   Stock Exchange
 .q: JASDAQ
 .n: Nagoya  Stock Exchange
 .s: Sapporo Stock Exchange
 .f: Fukuoka Stock Exchange

Letter extention is omittable. When it is omit, the default exchange market is chosen by the Yahoo-Japan-Finance's server. It is not certain but I guess that the default one should be the main exchange market of the stock. Note: since almost symbols should work without letter extention, I have experienced certain problems with a few symbols those which have originally `.j' letter extention. This is of course not for the module but owe to the Yahoo-Japan-Finance server's behavior.

There is an exception for above. A few symbols of index are indicated in 5 to 7 digit code numbers. They are '998405' (TOPIX), '998407' (NIKKEI) and '23337' (JASDAQ). L<http://www.yahoo-help.jp/app/answers/detail/p/546/a_id/45388>

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

Date of C<$start> must be given in the format `YYYY-MM-DD' (ex. `2003-08-14'). Be careful, don't forget to quote the word, because bare word 2000-01-01 will be comprehend by Perl as '2000 - 1 - 1 = 1998'. This attribute is omittable. The default value of C<$start> is '1990-01-01'.

You cannot specify a date of last day. Because, to find the splits you must scan the quote during whole of the period from the C<$start> day. Without split data, estimation of value adjustment for split cannot be done exactly.

Note that datetime of this module is based on JST (Japan Standard Time: GMT +09:00).

=cut

sub scan {
    my($self, %term) = @_;
    
    $self->{'start'} = '1990-01-01';
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
    my($yearStart, $monthStart, $dayStart) = split /-/, $self->{'start'};
    my($yearEnd, $monthEnd, $dayEnd) = split /-/, $self->{'last' };
    
    # multi page fetching
    while (1) {
        # 50rows/1page is max at Yahoo-Japan-Finance
        # So fuckin' Yahoo-Japan-Finance's paging algorithm has a certain serious bug that I can't utilize paging method.
        
        my $abs_path;
        if ($yearEnd == $yearStart && $monthEnd == $monthStart) {
            $abs_path = "/history/?code=$self->{'symbol'}&sy=$yearEnd&sm=$monthEnd&sd=$dayStart&ey=$yearEnd&em=$monthEnd&ed=$dayEnd&tm=d";
        } else {
            $abs_path = "/history/?code=$self->{'symbol'}&sy=$yearEnd&sm=$monthEnd&sd=01&ey=$yearEnd&em=$monthEnd&ed=$dayEnd&tm=d";
        }
        my $remotedoc = decode('utf-8', $self->_fetch($abs_path));
        
        # testing whether it is valid symbol or not.
        if ($remotedoc =~ m/該当する銘柄はありません。/) {
            say '該当する銘柄はありません。' if $Debug == 1;
            last;
        }
        # testing whether it is valid term or not.
        if ($remotedoc =~ m/該当する期間のデータはありません。/) {
            say '該当する期間のデータはありません。' if $Debug == 1;
            last;
        }
        # testing whether it is the overrun page (with bulk quote table) or not (currently it is no use withou paging method)
        #if ($remotedoc =~ m|<th width="20%">日付</th>\n<th width="12%">始値</th>\n<th width="12%">高値</th>\n<th width="12%">安値</th>\n<th width="12%">終値</th>\n<th width="12%">出来高</th>\n<th width="20%">調整後終値\*</th>\n</tr></table>|) {
        #    say 'page was overun' if $Debug == 1;
        #    last;
        #}
        
        if ($Debug == 1) {
            # debug level 1 (it should output a raw html)
            utf8::encode($remotedoc);
            say $remotedoc;
        } else {
            if ($self->{'symbol'} eq '998405'
                or $self->{'symbol'} eq '998407'
                or $self->{'symbol'} eq '23337'
                ) {
                # for index
                $self->_collect_for_index($remotedoc);
            } else {
                # for stock
                $self->_collect($remotedoc);
            }
        }
        
        if ($yearEnd == $yearStart && $monthEnd == $monthStart) {
            say 'reached to the last date' if $Debug == 1;
            last;
        }
        
        if ($dayEnd != 31) {
            $dayEnd = 31;
        }
        
        if ($monthEnd == 1) {
            $yearEnd--;
            $monthEnd = 12;
        } else {
            $monthEnd--;
        }
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
Connection: close

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
    
    # find the target line which includes the quote rows.
    my $quoteLine;
    while (@html) {
        my $line = shift @html;
        if ($line =~ m|^<th width="20%">調整後終値\*</th>$|) {
            $quoteLine = shift @html;
            $quoteLine =~ s/^<\/tr>//;
            $quoteLine =~ s/<\/table>$//;
            last;
        }
    }
    
    # debug level 2 (it should output a line of html which has cropped )
    if ($Debug == 2) {
        say encode('utf8', $quoteLine);
        exit;
    }
    
    # split to every quote rows
    $quoteLine =~ s/^<tr>//;
    $quoteLine =~ s/<\/tr>$//;
    my @row = split /<\/tr><tr.*?>/, $quoteLine;
    
    # debug level 3 (it should output multi-line of quote rows)
    if ($Debug == 3) {
        say encode('utf8', join("\n", @row));
        exit;
    }
    
    foreach my $row (@row) {
        if ($row =~ m/class="through"/) {
            # this is a split data
            $row =~ m/<td>(\d{4})年(\d{1,2})月(\d{1,2})日<\/td>/;
            my $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);
            $row =~ m/分割: (.+?)株 -> (.+?)株/;
            my($split_pre, $split_post) = ($1, $2);
            
            # store this split data in a field object
            push @{ $self->{'splits'} }, join("\t", $date, $split_pre, $split_post);
            say join("\t", $date, $split_pre, $split_post) if $Debug == 4;
            
            next;
        }
        
        # split to every columns
        # (the ajusted closing price is discarded)
        $row =~ s/^<td>//;
        $row =~ s/<\/td>$//;
        my($date, $open, $high, $low, $close, $volume, undef) = split /<\/td><td>/, $row;

        # date reformatting
        $date =~ m/(\d{4})年(\d{1,2})月(\d{1,2})日/;
        $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);

	# remove comma signs from each number
        foreach my $num ($open, $high, $low, $close, $volume) {
            $num =~ tr/,//d;
        }
            
        # store the quote data in a field object
        unshift @{ $self->{'q_noadjust'} }, join("\t", $date, $open, $high, $low, $close, $volume);
        
        if ($Debug == 4) {
            say join("\t", $date, $open, $high, $low, $close, $volume);
            exit;
        }
    }
    
    $self->_adjustment();
    
    return $self;
}

# _collect_for_index($html)
# a customized version of _collect($html)
sub _collect_for_index {
    my($self, $html) = @_;
    
    # split the page to some lines
    my @html = split /\n/, $html;
    
    # find the target line which includes the quote rows.
    my $quoteLine;
    while (@html) {
        my $line = shift @html;
        if ($line =~ m|^<th width="12%">終値</th>$|) {
            $quoteLine = shift @html;
            $quoteLine =~ s/^<\/tr>//;
            $quoteLine =~ s/<\/table>$//;
            last;
        }
    }
    
    # debug level 2 (it should output a line of html which has cropped )
    if ($Debug == 2) {
        say encode('utf8', $quoteLine);
        exit;
    }
        
    # split to every quote rows
    $quoteLine =~ s/^<tr>//;
    $quoteLine =~ s/<\/tr>$//;
    my @row = split /<\/tr><tr.*?>/, $quoteLine;
    
    # debug level 3 (it should output multi-line of quote rows)
    if ($Debug == 3) {
        say encode('utf8', join("\n", @row));
        exit;
    }
    
    foreach my $row (@row) {
        # an index has no concept of splitting
    
        # split to every columns (index has no volume data)
        $row =~ s/^<td>//;
        $row =~ s/<\/td>$//;
        my($date, $open, $high, $low, $close) = split /<\/td><td>/, $row;

        # date reformatting
        $date =~ m/(\d{4})年(\d{1,2})月(\d{1,2})日/;
        $date = join '-', $1, sprintf('%02d', $2), sprintf('%02d', $3);

	# remove comma signs from each number and normalize floating point format
        foreach my $num ($open, $high, $low, $close) {
            $num =~ tr/,//d;
            $num = sprintf('%.2f', $num);
        }
            
        # store the quote data in a field object
        unshift @{ $self->{'q_noadjust'} }, join("\t", $date, $open, $high, $low, $close);
        
        if ($Debug == 4) {
            say join("\t", $date, $open, $high, $low, $close);
            exit;
        }
    }
    
    # Since an index has no concept of adjustment, the adjusted equals to the non-adjusted.
    @{ $self->{'q_adjust'} } = @{ $self->{'q_noadjust'} };
    
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

Copyright (c) 2001-2014 Masanori HATA. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

