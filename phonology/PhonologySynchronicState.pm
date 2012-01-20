package PhonologySynchronicState;
use strict;
use constant INF => 9**9**9; # is there really nothing sensible better?

# This keeps track of certain data in a phonology in a more accessible way than the list of rules
# which define it.  To wit, it currently keeps a list of phones and records which phones resolve
# to others.
# TODO: this is where the frequency table of bigrams will go.

# If there is a conditioned outcome of one of the single phones that are supposed to be resolved,
# that will be stored in {conditional_resolutions}, which is a hash from rule numbers to
# resolutions that have applied up to before when that rule does.

sub initialise {
  my ($pd, $FS, $start, %args) = @_;
  
  my @inventory;
  my %phone_resolutions;
  my %resolution_expiries;
  if (defined @{$args{inventory}}) {
    @inventory = @{$args{inventory}};
  } elsif ($args{start} == $pd->{start_sequences}) {
    @inventory = keys %{$pd->{gen_inventory}};
  } else {
    # FIXME: else we have to run to update the inventory to the starting point
    warn "unimplemented start time";
  }
  @inventory = grep $_, @inventory;
  %phone_resolutions = (map(($_ => $_), @inventory));

  my $s = {
      pd => $pd,
      FS => $FS,
      start => $start,
      inventory => \@inventory, 
      resolutions => \%phone_resolutions,
      resolution_expiries => \%resolution_expiries,
      conditional_resolutions => {},
      conditional_resolution_expiries => {},

      # These next things are data used by the describer, and are updated here on running one rule.
      nearest_outcomes => {},
      frames_examineds => {},
      matcheds => {},
  };
  bless $s;
}



# Methods that deal with a specific rule follow.  Many of these methods can cause the rule
# to be marked with $rule->{pointless} = 1, meaning that the rule can never cause a change
# given the current state of the phonology.



sub clear_rule_data {
  my $self = shift;
  $self->{outcomes} = {};
  $self->{frames_examineds} = {};
  $self->{matcheds} = {};
}

# Given a rule, produce the sets of phones in the current inventory that can match each of its positions.
# If this position can match word boundary, then include the pause_phone.
#
# TODO: put pointless persistent rules into some kind of holding tank,
# to check on creation of new phones.
#
# Actually, quite a lot of stuff for persistent rules potentially needs rewriting 
# when new phones are around.  Ick.  I'm happy to just ignore this for now.
sub find_matches {
  my ($self, $rule) = @_;
  my %matcheds;
  for my $displ ($rule->indices('condition')) {
    $matcheds{$displ} = [grep $rule->{$displ}->matches($_), @{$self->{inventory}}];
    @{$matcheds{$displ}} = grep $rule->{filter}->matches($_), @{$matcheds{$displ}} if defined $rule->{filter};
    push @{$matcheds{$displ}}, $rule->{$displ}{or_pause} if defined $rule->{$displ}{or_pause};
    unless (@{$matcheds{$displ}}) {
      $rule->{pointless} = 1;
    }
  }
  $self->{matcheds} = \%matcheds;
  return %matcheds;
}

# Simplify the rule assuming correctness of the current state.
sub simplify {
  my ($self, $rule, %args) = @_;
  my %matcheds;
  if ($self->{matcheds}) {
    %matcheds = %{$self->{matcheds}};
  } else {
    %matcheds = $self->find_matches($rule);
  }
  my $simplified_rule = $rule->deep_copy_indexed();

  # Drop the assimilatory parts of the rule if there aren't multiple values among the things being
  # assimilated to.
  # It would be at least as sensible to do this one assimilation at a time, in theory,
  # but that would throw off the naming.
  for my $locus ($simplified_rule->indices('effects')) {
    my $effect = $simplified_rule->{$locus}{effects};
    if ($effect =~ /</) {
      my $not_variable = 0;
      my @indices = grep substr($effect, $_, 1) eq '<', 0..length($effect)-1;
      for my $j (@indices) {
        $not_variable = 1, last if grep substr($_, $j, 1) eq '0', @{$matcheds{$locus-1}}
                               and grep substr($_, $j, 1) eq '1', @{$matcheds{$locus-1}};
      }
      unless ($not_variable) {
        for (0..length($effect)-1) {
          substr($effect, $_, 1) = substr($matcheds{$locus-1}[0], $_, 1) if substr($effect, $_, 1) eq '<';
        }
      }
    }
    if ($effect =~ />/) {
      my $not_variable = 0;
      my @indices = grep substr($effect, $_, 1) eq '>', 0..length($effect)-1;
      for my $j (@indices) {
        $not_variable = 1, last if grep substr($_, $j, 1) eq '0', @{$matcheds{$locus+1}}
                               and grep substr($_, $j, 1) eq '1', @{$matcheds{$locus+1}};
      }
      unless ($not_variable) {
        for (0..length($effect)-1) {
          substr($effect, $_, 1) = substr($matcheds{$locus+1}[0], $_, 1) if substr($effect, $_, 1) eq '>';
        }
      }
    }
    if ($simplified_rule->{$locus}{condition} =~ /^$effect$/ and !$simplified_rule->{$locus}{deletions}) {
      $simplified_rule->{pointless} = 1;
    }
    $simplified_rule->{$locus}{effects} = $effect;
  } # $locus

  $simplified_rule;
}

# Update this state to reflect the running of one more rule.  
sub update {
  my ($self, $rule, $i, %args) = @_;
  my %matcheds;
  if ($self->{matcheds}) {
    %matcheds = %{$self->{matcheds}};
  } else {
    %matcheds = $self->find_matches($rule);
  }

  if ($args{record_old_inventory}) {
    $self->{old_inventory} = [@{$self->{inventory}}];
  }

  # There should be no resolutions whose expiry is _strictly less than_ $i.
  # TODO: when this is being done dynamically, one will have to take a different approach to finding these.
  if (defined $self->{resolution_expiries}{$i}) {
    delete $self->{resolutions}{$_} for @{$self->{resolution_expiries}{$i}};
  }
  if (defined $self->{conditional_resolution_expiries}{$i}) {
    delete $self->{conditional_resolutions}{$_} for @{$self->{conditional_resolution_expiries}{$i}};
  }

  my @new_inventory = @{$self->{inventory}};
  my (%outcomes, %frames_examineds);
  for my $locus ($rule->indices('effects'), $rule->indices('deletions')) {
    my $effect = defined $rule->{$locus}{effects} ? $rule->{$locus}{effects} : '.' x @{$self->{FS}{features}}; # might be many phones!
    my %outcome;
    my $pointless = 1;
    my %frames_examined;
    my @template_set;
    push @template_set, @{$matcheds{$locus-1}} if $effect =~ /</;
    push @template_set, @{$matcheds{$locus+1}} if $effect =~ />/;
    push @template_set, '.' x @{$self->{FS}{features}} unless @template_set;
    for my $template (@template_set) {
      my @pieces_of_frame = split / /, $effect;
      for my $frame (@pieces_of_frame) { 
        $frame = $self->{FS}->add_entailments($frame); # why entailed?  for antitheticals?
        my $frame_is_worthwhile = 0;
        for (0..length($frame)-1) {
          # unlikely issue: not right for assimilation of some features in each direction.   and elsewhere
          substr($frame, $_, 1) = substr($template, $_, 1) if substr($frame, $_, 1) =~ /[<>]/;
        }
        next if defined $frames_examined{$frame};
        $frames_examined{$frame} = 1;

        for my $phone (@{$matcheds{$locus}}) {
          my $outcome;
          if (defined $rule->{$locus}{deletions} and $rule->{$locus}{deletions}) {
            $outcome = '';
          } else {
            my $changed = $self->{FS}->add_entailments($self->{FS}->overwrite($phone, $frame));
            if (!defined $self->{resolutions}{$changed}) {
              my $word = [$changed];
              my $expiry = [];
              my $context_dependent = {};
              $self->{pd}->run
                 ($word,
                  cleanup => $i, 
                  change_record => [FeatureSystem::change_record($phone, $changed)],
                  track_expiry => $expiry,
                  nopause => 1,
                  context_dependent => $context_dependent);
              $self->{resolutions}{$changed} = join ' ', @$word;
              push @{$self->{resolution_expiries}{$expiry->[0]}}, $changed if $expiry->[0] < INF;
              while (my ($k, $outcome_before) = each %$context_dependent) {
                next if $k >= $self->{start};
                $self->{conditional_resolutions}{$k}{$changed} = join ' ', @$outcome_before;
                if (defined $self->{pd}{phonology}[$k]{inactive}) {
                  push @{$self->{conditional_resolution_expiries}{$self->{pd}{phonology}[$k]{inactive}}}, $k;
                }
              }
              push @new_inventory, @$word;             
            }

            # For description in rules we want not the ultimate outcome but the nearest one.
            $outcome = $self->{resolutions}{$changed};
            unless ($args{no_old_conditionals}) {
              for my $k (sort {$a <=> $b} keys %{$self->{conditional_resolutions}}) {
                $outcome = $self->{conditional_resolutions}{$k}{$changed}, last 
                    if defined $self->{conditional_resolutions}{$k}{$changed};
              }
            }
          }
          $pointless = 0 unless ($phone eq $outcome and $phone !~ /^$effect$/);
          $outcome{$frame}{$phone} = $outcome;
          $frame_is_worthwhile = 1 if $outcome ne $phone;
        }
        delete $outcome{$frame} unless $frame_is_worthwhile;
      } # $frame (pieces of a split)
    } # $template
    $rule->{pointless} = 1 if $pointless;

    $outcomes{$locus} = \%outcome;
    $frames_examineds{$locus} = \%frames_examined;
  } # $locus

  %_ = map(($_ => 1), @new_inventory);
  @{$self->{inventory}} = keys %_; # uniq

  $self->{outcomes} = \%outcomes;
  $self->{frames_examineds} = \%frames_examineds;
}

1;
