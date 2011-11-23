package PhoneSet;  # putting this here for now
use strict;

# If $multi is 0, parse into $s a description of a phone set given in $d by {condition}, {except}, {extras}.
# If $multi is 1, parse into $s a hash of the same for multiple indexed phones.
# 
# If $s already has stuff in it, overwrite that rather than trampling it.
sub parse {
  my ($d, $multi, %args) = (shift, shift, shift, @_);
  my $FS = $args{FS};
  my $s = {};
  $s = $args{base} if (defined $args{base});

  my @phones = map $FS->parse($_), split /, */, $d->{condition}, -1;
  for (0..$#phones) {
    if (defined $s->{$_}) {
      $s->{$_}{condition} = $FS->overwrite($s->{$_}{condition}, $phones[$_]);
    } else {
      $s->{$_}{condition} = $phones[$_];
    }
    bless $s->{$_};
  }

  if (defined $d->{except}) {
    # two styles: hash for backward compatibility, string so that specifying single phones is sane
    if (ref $d->{except} eq 'HASH') {
      for my $displ (keys %{$d->{except}}) {
        $s->{$displ}{except} .= ' ' if defined $s->{$displ}{except};
        $s->{$displ}{except} .= join ' ', map $FS->parse($_), split / *\| */, $d->{except}{$displ};
      }
    } else {
      my @exceptions = map $FS->parse($_), split /, */, $d->{except}, -1;
      for my $displ (0..$#exceptions) {
        $s->{$displ}{except} .= ' ' if defined $s->{$displ}{except};
        $s->{$displ}{except} .= join ' ', map $FS->parse($_), split / *\| */, $d->{except}{$displ};
      }
    }
  }

  my $pause_phone;
  @_ = split / +([0-9.]+) */, $d->{pause_phone};
  if (scalar @_ == 1) {
    $pause_phone = $FS->parse($d->{pause_phone}, undefined => 1);
  } elsif (scalar @_ > 1) {
    $pause_phone = $FS->parse(PhonologicalRule::weighted_one_of(@_), undefined => 1);
  }
  # As a corollary of the sort here, '-' assignments follow '+' ones.  TODO: make this saner?
  for my $e (sort keys %{$d->{extras}}) {
    if (rand() < $d->{extras}{$e}) {
      my ($e0, $e1);
      if ($e =~ /^(.*) ([^ ]*)$/) {
        ($e0, $e1) = ($1, $2);
      } else {
        ($e0, $e1) = ($e, 0);
      }
      if ($e0 eq '##') { # ad hoc notation for _only_ at extremum of word
        $s->{$e1}{or_pause} = $pause_phone;
        substr($s->{$e1}{condition}, 0, 1) = 'x'; # ad hoc match prevention
      } elsif ($e0 eq '#') { # end of word _allowed_
        $s->{$e1}{or_pause} = $pause_phone;
      } elsif ($e0 =~ /^!/) {
        $s->{$e1}{except} .= ' ' if defined $s->{$e1}{except};
        $s->{$e1}{except} .= $FS->parse(substr($e0,1));
      } else {
        $s->{$e1}{condition} = $FS->overwrite($s->{$e1}{condition}, $FS->parse($e0));
      }
    }
  }

  # If this wasn't supposed to be multiple phones, lift everything up a level. 
  unless ($multi) {
    $s->{$_} = $s->{0}{$_} for keys %{$s->{0}};
    delete $s->{0};
    bless $s;
  }

  $s;
}

# Test a phoneset against a single phone.
sub matches {
  my ($self, $phone) = (shift, shift);
  if (defined $self->{condition}) {
    return 0 unless $phone =~ /^$self->{condition}$/;
  }
  if (defined $self->{except}) {
    for my $exception (split / /, $self->{except}) {
      return 0 if $phone =~ /^$exception$/;
    }
  }
  return 1;
}

# Simplify a phoneset.
# This could use more testing.
# Possible improvements: consolidate pairs of excepts; handle 'u' values.  
sub simplify {
  my ($self, $FS) = (shift, shift);
  
  if (defined $self->{condition} and $self->{except}) {
    my $condition = $FS->add_entailments($FS->add_requirements($self->{condition}));
    $condition = $FS->intersect($condition, $self->{enriched_condition}) if defined $self->{enriched_condition};
    my @except = split / /, $self->{except};
    { # loop if the condition has changed, so we need to retest the excepts
      my $must_redo = 0;
      for (my $i = 0; $i < @except; ++$i) {
        # x out the condition, and return, if everything's excepted.
        if ($condition =~ /^$except[$i]$/) {
          $self->{condition} = 'x' x @{$FS->{features}};
          delete $self->{except};
          return;
        }

        # Remove excepts that don't matter.
        if (!$FS->compatible($condition, $FS->add_entailments($FS->add_requirements($except[$i])) )) {
          splice @except, $i, 1;
          redo;
        }

        # For excepts that only set one feature further to the condition, remove them and set a feature
        # in the condition if the prerequisites are all there, and a stripping doesn't preclude that.
        my $excess_feature = undef;
        my $just_one = 1;
        for (0..length($condition)-1) {
          if (substr($except[$i], $_, 1) =~ /[01]/ and substr($condition, $_, 1) eq '.') {
            if (defined $excess_feature) {
              $just_one = 0; last;
            } else {
              $excess_feature = $_;
            }
          }
        }
        if ($just_one and defined $excess_feature) {
          $_ = $FS->parse($FS->{features}[$excess_feature]{requires});
          if ($condition =~ /^$_$/) {
            my $cant_be_stripped = 1; 
            for my $str (@{$FS->{strippings}}) {
              $_ = $FS->parse($str->{strip});
              if ($FS->compatible($condition, $str->{condition_parsed})) {
                $cant_be_stripped = 0; last;
              }
            }
            if ($cant_be_stripped) {
              # now we know that things matching the condition will not be 'u' in this position.
              substr($self->{condition}, $excess_feature, 1) = 1 - substr($except[$i], $excess_feature, 1);              
              $must_redo = 1;
              splice @except, $i, 1;
              redo;
            }
          }          
        }
      } # $i

      redo if $must_redo;
    } 
    $self->{except} = join ' ', @except;
    delete $self->{except} unless @except;
  }
}

1;
