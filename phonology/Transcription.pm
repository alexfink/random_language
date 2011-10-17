package Transcription;
use strict;

# Right now the only Transcription objects are the standard phonetic alphabets.
# It's my intent to use this also for romanizations designed to fit a particular phonology,
# once creating those is done.

sub load_file {
  my ($filename, $FS) = (shift, shift);
  my $alphabet = YAML::Any::LoadFile($filename);
  $alphabet->{FS} = $FS;

  for my $type (qw/characters ligations/) {
    for my $c (keys %{$alphabet->{$type}}) {
      $alphabet->{$type}{$FS->parse($c)} = $alphabet->{$type}{$c};
      delete $alphabet->{$type}{$c};
    }
  }
  for my $c (keys %{$alphabet->{modifiers}}) {
    my @fs = split / /, $c;
    my $s = $FS->parse($c) . ' ' . $FS->parse($fs[0]);
    $alphabet->{modifiers}{$s} = $alphabet->{modifiers}{$c};
    delete $alphabet->{modifiers}{$c};
  }
  
  bless $alphabet;
}

# In a modifier description, it's only the first phone that the modifier actually spells;
# the rest are just conditions on its applicability.

sub name_phone {
  my ($self, $phone, %args) = (shift, @_);
  my $FS = $self->{FS};
  my %taken_care_of;
  my $s = '##';
  $s = "<abbr title=\"$phone\">$s</abbr>"; #if $use_html; # handy for debugging
  
  for my $x (keys %{$self->{ligations}}) {
    next if $phone !~ /^$x$/;
    my $phone0 = $FS->overwrite($phone, $FS->parse($self->{ligations}{$x}[0]));
    my $phone1 = $FS->overwrite($phone, $FS->parse($self->{ligations}{$x}[1]));
    my ($tc0, $s0) = $self->name_phone($phone0, %args, no_modifiers => 1);
    my ($tc1, $s1) = $self->name_phone($phone1, %args, no_modifiers => 1);
    $s = $self->{ligations}{$x}[2];
    $s =~ s/\[\]/$s0/;
    $s =~ s/\[\]/$s1/;
    %taken_care_of = (%$tc0, %$tc1, 
                      map(($_ => 1), (grep substr($x, $_, 1) ne '.', 0..@{$FS->{features}}-1)) );
    last;
  }

  if ($s =~ /##/) {
    for my $x (keys %{$self->{characters}}) {
      next if $phone !~ /^$x$/;
      $s = $self->{characters}{$x};
      %taken_care_of = map(($_ => 1), (grep substr($x, $_, 1) ne '.', 0..@{$FS->{features}}-1));
      last;
    }
  }

  return (\%taken_care_of, $s) if $args{no_modifiers};
  
  MODIFIER: for my $x (keys %{$self->{modifiers}}) {
    my ($x_all, $x_spells) = split / /, $x;
    next if $phone !~ /^$x_all$/;
    my $redundant = 1;
    for (0..@{$FS->{features}}-1) {
      $redundant = 0 if substr($x_spells, $_, 1) ne '.' and !defined $taken_care_of{$_};
    }
    next if $redundant;
    my $t = $self->{modifiers}{$x};    
    $t =~ s/\[\]/$s/;
    $s = $t;
    %taken_care_of = (%taken_care_of, map(($_ => 1), (grep substr($x_spells, $_, 1) ne '.', 0..@{$FS->{features}}-1)));
  }

  $s;
}

sub spell {
  my ($self, $word, %args) = (shift, @_);
  if ($args{null} and !@$word) {
    return $self->{null};
  }
  join "", map $self->name_phone($_, %args), @$word;
}

# duplicated for efficiency :/
sub spell_spaced_string {
  my ($self, $word, %args) = (shift, @_);
  if ($args{null} and ($word eq '')) {
    return $self->{null};
  }
  join "", map $self->name_phone($_, %args), (split / /, $word);
}

1;
