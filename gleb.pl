#!/usr/bin/perl
# Generate random phonologies, featurally and via ordered rules,
# with allophony and the potential for good morphophonology and the works.  (Getting there!)
# Alex Fink, January 2010 -- present.
# Thanks to Marcus Smith <http://smithma.bol.ucla.edu/> for unwitting inspiration,
# and Marcus and UPSID for being proximal sources for various numbers.
# (A greater proportion of the numbers are wholly fabricated, though!)

# List of priorities.
# - Excepts on markeds ought to be quick.
# - Finish aspects of the rule describer.  Inserting passed-over rules when they become applicable,
#   and changing the verb when in fact outcoming sounds always have a different outcome (like deletion), 
#   seem good ideas.
# - Allow consonant inventory tables to merge coronal posterior and palatal columns.
# - Make phone proportions saner?  Perhaps each unlikely distinction should propagate favour up into
#   its prerequisites, or something, in a way that fixes overrare but doesn't exacerbate overcommon.
# - Better extra conditions.

use strict;
use YAML::Any;
use CGI;
use constant INF => 9**9**9; # is there really nothing sensible better?

my $version = '0.2.2';
my $credits = 'Gleb, a phonology generator, by Alex Fink' . 
              (' ' x (29 - length($version))) . # for a total length of 78
              "version $version";

# Phones are specified as strings, with one character for each feature in the order
# in which they occur in the feature definition file.  The principal values of characters:
# '.' is unspecified, 'u' undefined, '0' off, '1' on.
#
# There are various other characters used.  
# In one place the syllable structure uses 'U', which gets converted to 'u' only after the generator runs.
# Effects of sound changes can have '<' and '>', which do progressive and regressive assimilation.

# Univalent features are treated by the code bivalently, just like all the others.
# Their univalence manifests in other ways: 
# - their complement cannot be selected as the conditioning environment of a rule (not in yet);
# - they can't be inserted as part of a repair rule.

# As essentially a hack to achieve reasonable behaviour of certain 3-place continua,
# a feature can be marked as antithetical to another.  Then, whenever the former is
# set +, the latter is automatically set -.

my $FS; # what features there are, what they do
my $phon_descr; # how to describe the features.  the thing that would need to be localised
my %phonetic_alphabets;
my %feature_indices;
my @features_requiring;

my $verbose;
my $debug = 0;
my $use_html;
my $CGI;

# Go from a prototypical prob to an actual one.  Now twice as gentle!
sub fuzz {
  my $p = shift;
  return 0 if $p <= 0;
  return 1 if $p >= 1;
  my $q = rand($p / (1 - $p)) + rand($p / (1 - $p));
  return $q / ($q + rand(1) + rand(1));
}

# When passed a second argument, uses dots for unspecified values instead (esp. for use in regexes).
sub parse_feature_string {
  my $re = defined $_[1];
  my $phone = ($re ? '.' : 'u') x @{$FS->{features}};
  return $phone if !defined $_[0];
  my @a = split / /, $_[0];
  for my $f (@a) {
    if ($f =~ /^([+-])(.*)/) {
      substr($phone, $feature_indices{$2}, 1) = $1 eq '+' ? '1' : '0'; 
    } else {
      substr($phone, $feature_indices{$f}, 1) = 1;
    }
  }
  $phone;
}

# Called with two args, displays undef things.
sub feature_string {
  my $phone = shift; 
  my $fs = '';
  my $c;
  for my $i (0..(length $phone)-1) {
    $fs .= ($fs ? ' ' : '') . 
           ($c eq '1' ? (defined $FS->{features}[$i]{univalent} ? '' : '+') : 
           ($c eq 'u' ? '?' : ($c eq '0' ? '-' : $c))) . 
           $FS->{features}[$i]{name} 
        unless ($c = substr($phone, $i, 1)) eq '.' or ($c eq 'u' and !@_);
  }
  $fs;
}

# Takes a phone and a phone with dots.  Replaces features in the first with non-dots in the second.
sub overwrite {
  my ($a, $b) = @_;
  for my $i (0..(length $b)-1) {
    substr($a, $i, 1) = substr($b, $i, 1) if substr($b, $i, 1) ne '.';
  }
  $a;
}

sub compatible {
  my ($a, $b) = @_;
  for my $i (0..(length $b)-1) {
    return undef unless substr($a, $i, 1) eq '.' or 
                        substr($b, $i, 1) eq '.' or
                        substr($a, $i, 1) eq substr($b, $i, 1);
  }
  return 1;
}

sub add_requirements {
  my $reqd = $_[0];
  for my $i (0..length($_[0])-1) {
    $reqd = overwrite($reqd, parse_feature_string($FS->{features}[$i]{requires}, 1)) 
        if substr($_[0], $i, 1) =~ /[01]/ and defined $FS->{features}[$i]{requires};
  }
  $reqd;
}

sub add_entailments {
  my $phone = shift;
  for my $i (0..length($phone)-1) {
    substr($phone, $feature_indices{$FS->{features}[$i]{antithetical}}, 1) = '0' 
        if substr($phone, $i, 1) eq '1' and defined $FS->{features}[$i]{antithetical};
  }
  for my $i (0..length($phone)-1) {
    if (substr($phone, $i, 1) =~ /[01]/) {
      substr($phone, $_, 1) = 'u' for (@{$features_requiring[1 - substr($phone, $i, 1)][$i]});
    }
  }
  $phone;
}



# Each rule is a hash.  In the simplest case, it has hashes of _preconditions_
# and of _effects_, each of them on a phoneme specified by an index relative
# to this one.  
# Other things there can be:
# _Deletions_ happen after effects.  Deletions should be a list of indices in decreasing order.

# Generator rules also have probabilities of application, and have two effects stored.
# We don't currently actually ever _run_ generator rules, but it's nice to know 
# that level is still there I guess.

=pod
sub generate_syllable {
  my $syllable_structure = shift;
  my @syllable;
  for my $phone (@$syllable_structure) {
    push @syllable, $phone->{features} if (rand() < $phone->{prob});
  }
  @syllable;
}
=cut


# Memoise a rule for the computations performed in feeds(!).
# These things can be totally stripped out once the phonology is finalised. 

sub feed_annotate {
  my $rule = shift;
  for my $displ (keys %{$rule->{precondition}}) {
    $rule->{precondition_ar}{$displ} = add_requirements($rule->{precondition}{$displ});
    if (defined $rule->{effects}{$displ}) {
      $rule->{outcome}{$displ} = overwrite add_requirements($rule->{precondition}{$displ}), $rule->{effects}{$displ};
      $rule->{outcome}{$displ} =~ s/[<>]/./g;
    }
  }
}

# Given two rules ri, rj, can the execution of ri cause rj to be applicable
# where it wasn't before?
#
# One wrinkle is rule priorities. 
# Featuresets have priorities to accommodate strippings: when a phone meets the condition
# of a stripping, the effects of the stripping should remain in effect even though
# rules for generic phones might say otherwise.  Rules also have priorities,
# which should equal the priorities of their preconditions.  
# Priority is zero if not defined.
# There are ramifications:
# (1) a higher-priority rule does not feed a lower-priority rule;
# (2) a lower-priority rule feeds a higher-priority rule if they're both
#     trying to change the same thing, even if they aren't otherwise feedy;
# (3) when a phone might have fallen out of its priority class, we need to rerun all the rules
#     we'd been suppressing.  
# Thankfully, the latter case does not arise all that often, as the only stripping
# we presently use sets features off which are not often turned on.
#
# Heed that strippings do not vary with the language!  They belong transcendingly to the feature system.

# This is somewhat of a bottleneck, still.  Can we speed it up?

# TODO: account for except.

sub feeds {
  my ($ri, $rj, %args) = (shift, shift, @_);

  # Sequence-type rationales overrule ramification (1).
  # TODO: update this as we get new rule types
  # an insertion means we have to look at everything (whereas a fission might be okay with less); etc.
  return 1 if defined $ri->{deletions} and 
      (keys %{$rj->{precondition}} > 1 or defined $rj->{or_pause});

  # ramification (1)
  #return 0 if (defined $ri->{priority} ? $ri->{priority} : 0) >
  #            (defined $rj->{priority} ? $rj->{priority} : 0); 

  for my $i_displ (keys %{$ri->{effects}}) {
    for my $j_displ (keys %{$rj->{precondition}}) {
      # ramification (2)
      #return 1 if (defined $ri->{priority} ? $ri->{priority} : 0) <
      #            (defined $rj->{priority} ? $rj->{priority} : 0) and
      #            defined $rj->{effects}{$j_displ} and
      #            !compatible($ri->{effects}{$i_displ}, $rj->{effects}{$j_displ});
      # ramification (3)
      #for my $str (@{$FS->{strippings}}) {
      #  return 1 if compatible($ri->{precondition}{$i_displ}, $str->{condition_parsed}) and
      #             !compatible($ri->{effects}{$i_displ}, $str->{condition_parsed}); 
      #}
      
      # this is costly enough that it's slightly worth putting it in here.
      feed_annotate $ri if !defined $ri->{precondition_ar};
      feed_annotate $rj if !defined $rj->{precondition_ar};
      next if !compatible($ri->{outcome}{$i_displ}, $rj->{precondition_ar}{$j_displ});

      # The wrinkle-free cases.
      # We might have rules which unnecessarily set features identically
      # to their precondition (antithetical, I'm thinking of you); this can't feed, of course.
      # Either-value assimilation characters can always feed.
      for my $f (0..@{$FS->{features}}-1) {
        if (substr($ri->{effects}{$i_displ}, $f, 1) =~ /[<>]/ and
            substr($rj->{precondition}{$j_displ}, $f, 1) ne '.') {
          return 1 unless defined $args{pairs};
          push @{$args{pairs}}, [$i_displ, $j_displ];
        }
        if (substr($ri->{effects}{$i_displ}, $f, 1) eq 
              substr($rj->{precondition}{$j_displ}, $f, 1) and
            substr($ri->{effects}{$i_displ}, $f, 1) ne 
              substr($ri->{precondition}{$i_displ}, $f, 1) and 
            substr($ri->{effects}{$i_displ}, $f, 1) ne '.') {
          return 1 unless defined $args{pairs};
          push @{$args{pairs}}, [$i_displ, $j_displ];
        }

      }
    }
  }
  
  return 0 unless defined $args{pairs};
  return @{$args{pairs}} ? 1 : 0;
}

# Two rules conflict if they feed each other at the same displacement and their
# outcomes are incompatible.

sub conflict {
  my ($ri, $rj) = (shift, shift);
  my (@pij, @pji);
  return 0 unless feeds($ri, $rj, pairs => \@pij) and feeds($rj, $ri, pairs => \@pji);
  return 1 unless @pij and @pji; # since it's some priority ramification thing.  shouldn't arise
  for my $dij (@pij) {
    for my $dji (@pji) {
      next unless $dij->[0] eq $dji->[1] and $dij->[1] eq $dji->[0];
      return 1 if !compatible($ri->{effects}{$dji->[1]}, $rj->{effects}{$dij->[1]});
    }
  }
  return 0;
}

=doc
sub feedings {
  my $phonology = shift;
  my @fbs;
  for my $i (0..@$phonology-1) {
    my @fed = grep feeds($phonology->[$i], $phonology->[$_]), 0..@$phonology-1;
    push @fbs, {map(($_ => 1), @fed)};
  }
  @fbs;
}
=cut

# Return annotations regarding which rules have which preconditions or excepts.
# Used to optimise which rules we consider rerunning in running the phonology.
# The resulting array is indexed as [$value][$feature], where $value is 0 or 1 or 2, 
# 2 meaning undefined.
# We also use [3] for those rules whose preconditions include a sequence;
# these are those which can be newly triggered after a deletion.

sub which_preconditions {
  my $phonology = shift;
  my %which;
  for my $i (0..@$phonology-1) {
    my $rule = $phonology->[$i];
    # Strippings need to be special-cased: the features they strip out shouldn't be allowed
    # to be turned on.
    if (defined $rule->{tag} and $rule->{tag} =~ /^stripping/) {
      for my $displ (keys %{$rule->{effects}}) {
        for my $j (0..@{$FS->{features}}) {
          if (substr($rule->{effects}{$displ}, $j, 1) eq 'u') {
            push @{$which{0}[$j]}, $i;
            push @{$which{1}[$j]}, $i;
          }
        }
      }
    }

    for my $displ (keys %{$rule->{precondition}}) {
      for my $j (0..@{$FS->{features}}) {
        push @{$which{substr($rule->{precondition}{$displ}, $j, 1)}[$j]}, $i
            if substr($rule->{precondition}{$displ}, $j, 1) =~ /[01]/;
        # Doing undefined features is unnecessary, given the restricted circumstances
        # in which we set features undefined.
        #push @{$which{u}[$j]}, $i
        #   if substr($rule->{precondition}{$displ}, $j, 1) eq 'u'; 
      }
    }

    # Assimilations (to a feature undefined in the target) can be triggered by any change.
    for my $displ (keys %{$rule->{effects}}) {
      for my $j (0..@{$FS->{features}}) {
        if ((substr($rule->{effects}{$displ}, $j, 1) eq '<' and
             substr($rule->{precondition}{$displ-1}, $j, 1) eq '.') or
            (substr($rule->{effects}{$displ}, $j, 1) eq '>' and
             substr($rule->{precondition}{$displ+1}, $j, 1) eq '.')) {
          push @{$which{0}[$j]}, $i;
          push @{$which{1}[$j]}, $i;
        }
      }
    }

    # Again, except never contains undefined features.
    for my $displ (keys %{$rule->{except}}) {
      my @exceptions = split / /, $rule->{except}{$displ};
      for my $phone (@exceptions) {
        for my $j (0..@{$FS->{features}}) {
          push @{$which{1-substr($rule->{except}{$displ}, $j, 1)}[$j]}, $i
            if substr($rule->{except}{$displ}, $j, 1) =~ /[01]/;
        }
      }
    }
    push @{$which{seq}}, $i if keys %{$rule->{precondition}} >= 2;
  }
  \%which;
}

# Drop, from a completed phonology, rules that go inactive too early to ever run.

sub trim_inactive {
  my $pd = shift;
  my @new_indices;
  my $deleted = 0;

  for (my $i = 0; $i < $pd->{start_sequences}-$deleted; $i++) {
    if (defined $pd->{phonology}[$i]{inactive} and $pd->{phonology}[$i]{inactive} <= $pd->{start_sequences}) {
      splice @{$pd->{phonology}}, $i, 1;
      $i--;
      $deleted++;
    }
    push @new_indices, $i;
  }
  # I don't suppose it actually matters when a rule is inactivated if that time is before it runs.
  for my $rule (@{$pd->{phonology}}) {
    $rule->{inactive} = $rule->{inactive} >= $pd->{start_sequences} 
                      ? $rule->{inactive} - $deleted 
                      : $new_indices[$rule->{inactive}]
        if defined $rule->{inactive};
  }
  $pd->{start_sequences} -= $deleted;
}


# Record the changes between from $phone0 to $phone1, as described below.

sub change_record {
  my ($phone0, $phone1) = (shift, shift);
  my @changes;
  for my $i (0..length($phone0)-1) {
    push @changes, "c " . substr($phone1, $i, 1) . " $i" if substr($phone0, $i, 1) ne substr($phone1, $i, 1);
  }
  @changes;
}

# If %args includes a list {changes}, tags describing the particular changes caused
# will be pushed.  These tags are:
# "c $v $f" -- feature $f was changed to value $v
# "d" -- a segment was deleted

sub run_one_rule {
  my ($word, $rule, %args) = (shift, shift, @_);
  my $changed = 0;
  my $syllable_position = defined $args{syllable_position} ? $args{syllable_position} : 0;
  
  # start at -1 for assimilations to word-initial pause
  PHONE: for (my $i = -1; $i < @$word; $i++) {
    for my $displ (keys %{$rule->{precondition}}) {
      next if !$args{nopause} and defined $rule->{or_pause} and defined $rule->{or_pause}{$displ} and 
              ($i + $displ < 0 or $i + $displ >= @$word);
      next PHONE if ($i + $displ < 0 or $i + $displ >= @$word);
      next PHONE if $word->[$i+$displ] !~ /^$rule->{precondition}{$displ}$/;
    }
    if (defined $rule->{except}) {
      for my $displ (keys %{$rule->{except}}) {
        next if ($i + $displ < 0 or $i + $displ >= @$word);
        my @exceptions = split / /, $rule->{except}{$displ};
        for (@exceptions) {
          next PHONE if $word->[$i+$displ] =~ /^$_$/;
        }
      }
    }

    for my $displ (keys %{$rule->{effects}}) {
      next if ($i + $displ < 0 or $i + $displ >= @$word);
      my $effects = $rule->{effects}{$displ};
      if (defined $rule->{prob}) {
        my $r = defined $args{rand_value} ? $args{rand_value} : rand;
        $effects = $rule->{antieffects}{$displ}
            if $r >= $rule->{prob}[$syllable_position]; # randomised rules do their opposite if the dice say no
      }
      my $newphone = overwrite $word->[$i+$displ], $effects;
      
      # Handle the assimilation characters. 
      if ($newphone =~ /[<>]/) {
        my ($next_before, $next_after) = (undef, undef);
        for (keys %{$rule->{precondition}}) { # TODO: use the actual offsets when distance rules exist
          $next_before = $_ if (!defined $next_before or $next_before < $_) and $_ < $displ;
          $next_after = $_ if (!defined $next_after or $next_after > $_) and $_ > $displ;
        }
        while ($newphone =~ /</) {
          my $c = index($newphone, '<');
          substr($newphone, $c, 1) = 
              substr($i+$next_before >= 0 ? $word->[$i+$next_before] : $rule->{pause_phone}, $c, 1);
        }
        while ($newphone =~ />/) {
          my $c = index($newphone, '>');
          substr($newphone, $c, 1) =
              substr($i+$next_after < @$word ? $word->[$i+$next_after] : $rule->{pause_phone}, $c, 1);
        }
        $newphone = add_entailments $newphone;
      }
      
      if ($word->[$i+$displ] ne $newphone) {
        $changed = 1;
        push @{$args{changes}}, change_record($word->[$i+$displ], $newphone) if defined $args{changes};
      }
      $word->[$i+$displ] = $newphone;
    }
    
    if (defined $rule->{deletions}) {
      $changed = 1;
      push @{$args{changes}}, 'd' if defined $args{changes};
      splice @$word, $i+$_, 1 for @{$rule->{deletions}};
      if (defined $args{sources}) {
        splice @{$args{sources}}, $i+$_, 1 for @{$rule->{deletions}};
      }
      $i -= @{$rule->{deletions}};
    }
  } 
  
  $changed;
}

# Persistence is the default state of affairs for a non-generator rule.
# The {inactive} property on a rule is a rule number N, at which point this one
# becomes inactive (it won't run as a resolution when the current rule is >= N).
# A rule can also inactivate itself (they commonly do);
# these still run once.
# If passed generator => 1, the whole thing will run without persistence,
# and it'll do an add_entailments at the end. 

# If passed a list in sources, it will overwrite it with a list of
# positions of source phones of the phones in the result.
# -1 is used for epenthesis, and for the smaller fragment in breakings.
# (It seems a bad idea to use the same source label twice.)

# Regardless of persistence, always run a _single_ rule repeatedly until
# it makes no more changes.  This way things like assimilation will work
# across groups of more than two phones.  It also means we must disallow
# certain rule types (e.g. a single rule to achieve l...l, r...r > l...r).

# Only do rules start..end-1, if these are provided.  
# If passed cleanup => $i, don't even do a single rule but rather just
# fix up the word using persistent rules in force after rule $i runs.

# If passed a list in track_expiry, it puts the expiry time of the derivation
# in the first element (with the same semantics as inactive, i.e. it expires
# just before the given rule number).

use constant STEPS_TO_LOOP => 10;
use constant STEPS_TO_DIE => 30;

sub run_phonology {
  my ($word, $phonology, %args) = (shift, shift, @_);

  my $start = defined $args{start} ? $args{start} : 0;
  my $end = defined $args{end} ? $args{end} : @$phonology;
  ($start, $end) = ($args{cleanup}, $args{cleanup}+1) if defined $args{cleanup};
  my $first_time = 1;
  @{$args{sources}} = 0..@$word-1 if (defined $args{sources} and !@{$args{sources}});
  my $track_expiry;
  $track_expiry = INF if defined $args{track_expiry};

  my @loop_rules;
  my @loop_cessions;
  my %ceders;
  print STDERR "@$word (initially)\n" if $debug >= 1;
  for my $k ($start..$end-1) {
    my %agenda = ($k => 1);
    my $iterations = 0;
    while (keys %agenda) {
      my %new_agenda;
#      print "|" if $iterations == STEPS_TO_LOOP; # debug
      for my $i (sort {$a <=> $b} keys %agenda) {
        next if $i > $k;
        # if this is the first time through, let rules run even if they've marked themselves inactive
        next if $iterations and (defined $phonology->[$i]{inactive} and $k >= $phonology->[$i]{inactive});
        next if ($iterations >= STEPS_TO_LOOP) and defined $ceders{$i};

        my @changes;
        if (($first_time and defined $args{cleanup}) or
            run_one_rule $word, $phonology->[$i], %args, changes => \@changes) {
          if (keys %{$phonology->[$i]{precondition}} > 1) { # an optimization.  helpful?
            1 while run_one_rule $word, $phonology->[$i], %args;
          }
#          print "($i) @changes "; # debug
          print STDERR "@$word (after $i)\n" if $debug >= 1;

          @changes = @{$args{change_record}} if ($first_time and defined $args{change_record});
          $first_time = undef;
          $track_expiry = $phonology->[$i]{inactive} if defined $track_expiry and 
              $phonology->[$i]{inactive} < $track_expiry;

          # This is vile.  We should not be negotiating with <s>terrorists</s> loops.
          # At least it's a respectable algorithm.
          if ($iterations >= STEPS_TO_LOOP and defined $phonology->[$i]{cede}) {
            while (@loop_rules and
                   (($loop_cessions[-1] < $phonology->[$i]{cede}) or
                    ($loop_cessions[-1] == $phonology->[$i]{cede}) and ($loop_rules[-1] < $i))) {
              pop @loop_rules;
              pop @loop_cessions;
            }
            $ceders{$i} = 1 if (@loop_rules and $loop_rules[-1] == $i);
            push @loop_rules, $i;
            push @loop_cessions, $phonology->[$i]{cede};
          }
          if (defined $args{which_preconditions}) {
            # We might need to rerun the rules which have as a precondition a feature
            # this rule has newly acquired.
            my @new_agenda;
            for my $change (@changes) {
              if ($change =~ /^c (.*) (.*)$/) {
                push @new_agenda, @{$args{which_preconditions}{$1}[$2]}
                    if defined $args{which_preconditions}{$1}[$2];
              }
              elsif ($change eq 'd') {
                push @new_agenda, @{$args{which_preconditions}{seq}} if defined $args{which_preconditions}{seq};
              }
            }
            %new_agenda = (%new_agenda, map(($_ => 1), @new_agenda));
          } else {
            %new_agenda = map(($_ => 1), (0..$k));
          }
        }
      }
      %agenda = %new_agenda;
      last if defined $args{generator};
      # fwiw I saw this tripped wrongly once when the bound was 8.
      (print STDERR "*** unceded loop!\n"), last if (++$iterations >= STEPS_TO_DIE); 
    } # while (keys %agenda)
  }
  
  if (defined $args{generator}) {
    $_ = add_entailments($_) for @$word;
  }
  $args{track_expiry}[0] = $track_expiry if defined $track_expiry;
}



# Create two variants of this rule, one persistent, one not.  Weight appropriately.
sub persistence_variants {
  my ($phonology, $base, $persistence_weight, $no_persist, $generable_val) = 
      (shift, shift, shift, shift, shift);
  my ($base_rule, $base_weight) = @$base;
  my @makings;

  for my $persistent (0..1) {
    next if $persistent and $no_persist;
    my $rule = {%$base_rule};
    $rule->{inactive} = scalar @$phonology if !$persistent;

    # Loopbreaks and the like.  Only a worry if you want to be persistent.
    # If an older persistent rule is looping you, there's trouble;
    # in this situation, break and regenerate the older rule.
    my $loopbreak_penalty = 1;
    if ($persistent) {
      # The test for looping we do here was (as of v0.2) the most expensive thing
      # in the phonology generation.  By way of cutting down, only check rules
      # which set something the opposite of this rule.
      my @potential_conflicts;
      for my $displ (keys %{$rule->{effects}}) {
        for my $i (0..@{$FS->{features}}-1) {
          push @potential_conflicts, @{$generable_val->[1-substr($rule->{effects}{$displ}, $i, 1)][$i]}
              if substr($rule->{effects}{$displ}, $i, 1) =~ /[01]/
              and defined($generable_val->[1-substr($rule->{effects}{$displ}, $i, 1)][$i]);
        }
      }
      my %pch = map(($_ => 1), @potential_conflicts);
      @potential_conflicts = keys %pch; 
      for my $j (@potential_conflicts) {
        next if defined $phonology->[$j]{inactive} and $phonology->[$j]{inactive} < @$phonology;
        if (conflict($rule, $phonology->[$j])) {
#              print "$reqd > $effects and\n$jreqd > $phonology->[$j]{effects}{$displ} [$j] clash\n"; # debug
          push @{$rule->{inactivate}}, $j;
          # this is an ugly kluge, but few rules have more than one effect
          if (defined $phonology->[$j]{tag}) {
            my $evitands = join '|', values %{$phonology->[$j]{effects}};
            push @{$rule->{broken_tags}}, $phonology->[$j]{tag} . ' ' . $evitands;
          }
          $loopbreak_penalty *= $phonology->[$j]{recastability} if defined $phonology->[$j]{recastability};
        }
      }
    }

    push @makings, [$rule,
                    $base_weight *
                    ($persistent ? $persistence_weight : 1 - $persistence_weight) *
                    $loopbreak_penalty];
  }
  @makings;
}

# To expand a rule tag:
# - do the chance of extra conditions thing.
# - make all the resolutions, incl. related features, incl. loop-preserving and -breaking forms.
# - make the rules (retaining the tag, for later remaking).  when flipping a feature between 0 and 1, 
# clear features formerly requiring it.
# - repeat to make any necessary new rules for loopbreaks.

# The format of rule tags is "$type $list_index", where $type is one of the values that
# appear herein several times.
sub gen_one_rule {
  my ($phonology, $tag) = (shift, shift); 
  my %args = @_;
  my ($kind, $k, $rest) = split / /, $tag;
  #print "[" . scalar @$phonology . "] tag is $tag\n"; # debug

  if ($kind eq 'default' and !defined $rest) {
    for my $i (0..@{$FS->{features}[$k]{default}}-1) {
      gen_one_rule($phonology, "$tag $i", %args);
    }
    return;
  }

  # Need to have the precondition here so that the extra condition stuff can enrich it.
  my $precondition;
  # Where the data describing this thing is appended.  Used for assimilation so far.
  my $d; 
  if ($kind eq 'stripping') {
    $precondition = $FS->{strippings}[$k]{condition_parsed};
  } elsif ($kind eq 'default') {
    $precondition = parse_feature_string($FS->{features}[$k]{default}[$rest]{condition}, 1);
    $precondition = overwrite $precondition, parse_feature_string($FS->{features}[$k]{requires}, 1)
        if defined $FS->{features}[$k]{requires};
    substr($precondition, $k, 1) = 'u';
    # this avoids a bad situation which antitheticals can cause
    return if defined $args{extra_precondition} and substr($args{extra_precondition}, $k, 1) ne '.';
  } elsif ($kind eq 'repair') {
    $precondition = parse_feature_string($FS->{marked}[$k]{condition}, 1);
  } elsif ($kind =~ /^assimilate/) {
    $d = $FS->{features}[$k]{assimilation}[$rest] if $kind eq 'assimilate';
    $d = $FS->{relations}[$k]{assimilation}[$rest] if $kind eq 'assimilate_related';
    $precondition = join ' ', map parse_feature_string($_, 1), split /, */, $d->{condition}, -1;
  }
  if (defined $args{extra_precondition}) {
    my @precondition_phones = split / /, $precondition;
    my @extra_phones = split / /, $args{extra_precondition};
    for (0..$#precondition_phones) {
      return unless compatible($precondition_phones[$_], add_entailments($extra_phones[$_]));
    }
    $precondition = overwrite $precondition, $args{extra_precondition};
  }

  # Not doing assimilation rules (or strippings) since they can't much come out differently.
  my $threshold;
  if ($kind eq 'default') {
    $threshold = $FS->{features}[$k]{default}[$rest]{value};
  } elsif ($kind eq 'repair') {
    $threshold = $FS->{marked}[$k]{prob};
  } elsif ($kind =~ /^assimilate/) {
    $threshold = $d->{prob};
  }
  $threshold = 0 if !defined $threshold;

  my $initial_threshold = $threshold; # e.g. for things which are more unlikely than marked, in a way that feature choice can't handle
  if ($kind eq 'repair' and $args{initial} and defined $FS->{marked}[$k]{initial_prob}) {
    $initial_threshold = $FS->{marked}[$k]{initial_prob};
  }

  my $skip_me = (rand() > $initial_threshold);
  my $add_a_condition = (rand() < ($skip_me ? 1-$threshold : $threshold)*2/5.0); # magic constant

  # Lame things here: we only ever add an extra condition once; it only ever
  # consists of one feature.  At least we can recurse.
  # TODO: if this isn't the phone-resolution stage, sometimes add the condition
  # to a new phone which didn't previously have one.  This requires a little art
  # to know what good conditions for the adjacent phones are.
  # On the other hand, adding a condition to a phone in the environment that's not
  # changing should be essentially as rare as adding one on a new adjacent phone.
  #
  # This should be predisposed to sensible conditions (roughly, of the correct family.)
  #
  # It should also be predisposed to conditions that avoid a situation which has a
  # persisting rule against it.
  
  if ($add_a_condition and $kind ne 'stripping') {
    my @phones = split / /, $precondition;
    my $r = int rand @phones;
    if (defined $args{generator}) {
      my $full_precondition = add_requirements $phones[$r];
      my @potential_preconditions;
      for my $genrule (@{$args{generator}}) {
        my $i = index($genrule->{effects}{0}, '1');
        # again, prob[0] stands in for all the probabilities
        next unless substr($full_precondition, $i, 1) eq '.' 
             and compatible($full_precondition, $genrule->{precondition}{0})
             and $genrule->{prob}[0] > 0 and $genrule->{prob}[0] < 1;
        push @potential_preconditions, $genrule;
      }
      if (@potential_preconditions) {
        my $rule = $potential_preconditions[int rand @potential_preconditions];
        my $extra_precondition; 
        if (defined $FS->{features}[index($rule->{effects}{0}, '1')]{univalent}) {
          $extra_precondition = $rule->{effects}{0}; 
        } else {
          $extra_precondition = rand(2) > 1 ? $rule->{effects}{0} : $rule->{antieffects}{0};
        }
        $extra_precondition = join ' ', map(($_ == $r ? $extra_precondition : '.' x @{$FS->{features}}), 
                                            0..$#phones);
        gen_one_rule($phonology, $tag, %args, extra_precondition => $extra_precondition);
      }
    }
  }
  
  return if ($kind eq 'repair' or $kind =~ /^assimilate/) and $skip_me;

  my (@resolutions, @weights);
  my $total_base_weight = 0;

  if ($kind eq 'default') {
    for (0..1) {
      my $effects = '.' x @{$FS->{features}}; 
      substr($effects, $k, 1) = $_;
      my $weight = $_ ? $FS->{features}[$k]{default}[$rest]{value} :
                    1 - $FS->{features}[$k]{default}[$rest]{value};
      my $rule = {
        precondition => {0 => $precondition},
        effects => {0 => $effects},
        recastability => 1 - $weight,
      };
      # Default-provision rules shouldn't run where a stripping exists.  
      # This could be made more general later.
      for (@{$FS->{strippings}}) {
        if ($_->{strip} =~ /(^| )$FS->{features}[$k]{name}( |$)/) {
          $rule->{except} = {0 => parse_feature_string($_->{condition}, 1)};
          last;
        }
      }
      push @resolutions, $rule;
      push @weights, $weight;
    }
  } 
  
  elsif ($kind eq 'stripping') {
    for my $s (keys %{$FS->{strippings}[$k]{substitute}}) {
      my $rule = {
        precondition => {0 => overwrite($precondition, parse_feature_string($s, 1))},
        effects => {0 => parse_feature_string($FS->{strippings}[$k]{substitute}{$s}, 1)},
        recastability => 0,
      };
      push @$phonology, $rule;
    }

    my $effects = parse_feature_string($FS->{strippings}[$k]{strip}, 1);
    $effects =~ s/1/u/g;
    my $rule = {
      precondition => {0 => $precondition},
      effects => {0 => $effects},
      recastability => 0,
      priority => $FS->{strippings}[$k]{priority},
      tag => $tag,
    };
    push @$phonology, $rule;
    return;
  } 
  
  elsif ($kind =~ /^assimilate/) {
    my @phones = split / /, $precondition;
    my $target = $d->{target};

    # Randomly pick whether each extra feature is included
    # (rather than actually looping over all 2^n choices).
    my %eselections = map(($_ => 1), grep rand() < (defined $d->{extras}{$_} ? $d->{extras}{$_} : 0), 
                                          keys %{$d->{extras}});

    # Keep this small if you want to finish.  Doesn't work on relations.
    my @bound_features = ($k, map $feature_indices{$_}, split / /, 
        defined($d->{bound_features}) ? $d->{bound_features} : '');

    # The capabilities of this for more than one bound feature are not currently used.
    TV: for my $tv (0..(1<<2*@bound_features)-2) {
      my @t; my $tv_temp = $tv;
      while ($tv_temp > 0) {
        next TV if $tv_temp & 3 == 3;
        # for things like place which are virtually mutually exclusive:
        next TV if $tv_temp & 3 == 1 and defined $d->{exclusive}; 
        push @t, $tv_temp & 3;
        $tv_temp >>= 2; 
      }

      my $effects = '.' x @{$FS->{features}};
      if ($kind eq 'assimilate') {
        substr($effects, $_, 1) = $target ? '<' : '>' for @bound_features; 
      } elsif ($kind eq 'assimilate_related') {
        $effects = overwrite $effects, parse_feature_string($FS->{relations}[$k]{to}, 1);
        $phones[1-$target] = overwrite $phones[1-$target], parse_feature_string($FS->{relations}[$k]{from}, 1);
      }
      if (defined $d->{further_features}) {
        substr($effects, $_, 1) = $target ? '<' : '>' for map $feature_indices{$_}, split / /, $d->{further_features}; 
      }
      my $rule = {
        precondition => {map(($_ => $phones[$_]), 0..$#phones)},
        effects => {$target => $effects},
        recastability => 1 - $d->{prob},
        tag => $tag,
        cede => 1,
      };
      # Both the things being spread from and to need to support this feature.  
      if ($kind eq 'assimilate') {
        $rule->{precondition}{$target} = overwrite $rule->{precondition}{$target}, 
            parse_feature_string($FS->{features}[$k]{requires}, 1);
        $rule->{precondition}{1-$target} = overwrite $rule->{precondition}{1-$target}, 
            parse_feature_string($FS->{features}[$k]{requires}, 1);
      } elsif ($kind eq 'assimilate_related') {
        for (split / /, $FS->{relations}[$k]{to}) {
          /^[+-]?(.*)$/;
          $rule->{precondition}{$target} = overwrite $rule->{precondition}{$target}, 
              parse_feature_string($FS->{features}[$feature_indices{$1}]{requires}, 1);
        }
        for (split / /, $FS->{relations}[$k]{from}) {
          /^[+-]?(.*)$/;
          $rule->{precondition}{1-$target} = overwrite $rule->{precondition}{1-$target}, 
              parse_feature_string($FS->{features}[$feature_indices{$1}]{requires}, 1);
        }
      }
      # But not if it's stripped off.
      for my $str (@{$FS->{strippings}}) {
        for my $displ (0,1) { 
          if ($rule->{precondition}{$displ} =~ /^$str->{condition_parsed}$/) {
            my $effects = parse_feature_string($str->{strip}, 1);
            $effects =~ s/1/a/g; # temporary char
            $rule->{precondition}{$displ} = overwrite $rule->{precondition}{$displ}, $effects;
            $rule->{precondition}{$displ} =~ s/a/./g;
          }
        }
      }
      if (defined $d->{except}) {
        my %except = %{$d->{except}};
        $except{$_} = parse_feature_string($except{$_}, 1) for keys %except;
        $rule->{except} = {%except};
      }

      my $base_weight = 1;
      if ($kind eq 'assimilate' and !$d->{no_value_restriction}) {
        for (0..@bound_features-1) {
          $t[$_] = 0 if !defined $t[$_];
          if ($t[$_] != 2) {
            substr($rule->{precondition}{$target}, $bound_features[$_], 1) = $t[$_]
                if substr($rule->{precondition}{$target}, $bound_features[$_], 1) eq 'u';
            $base_weight *= 1-$threshold;
          }
        }
      }

      my $pause_phone = parse_feature_string($d->{pause_phone});
      # As a corollary of the sort here, '-' assignments follow '+' ones.
      for my $e (sort keys %{$d->{extras}}) {
        if (defined $eselections{$e}) {
          $e =~ /^(.*) ([^ ]*)$/;
          my ($e0, $e1) = ($1, $2);
          if ($e0 eq '#') {
            $rule->{or_pause}{$e1} = 1;
            $rule->{pause_phone} = $pause_phone;
          } elsif ($e0 =~ /^!/) {
            $rule->{except}{$e1} .= ' ' if defined $rule->{except}{$e1};
            $rule->{except}{$e1} .= parse_feature_string(substr($e0,1),1);
          } else {
            $rule->{precondition}{$e1} = overwrite $rule->{precondition}{$e1}, parse_feature_string($e0,1);
          }
        }
      }

      my @variants = persistence_variants $phonology, [$rule, $base_weight], $threshold, 
                                          0, $args{generable_val};
      for (@variants) {
        push @resolutions, $_->[0];
        push @weights, $_->[1];
      }

      last if $kind eq 'assimilate_related';
      last if $kind eq 'assimilate' and $d->{no_value_restriction};
    } # TV
  } # assimilate

  elsif ($kind eq 'repair') {
    my $reqd = add_requirements $precondition;
    
    # Deletion is always available, at low weight, just in case nothing else works.
    my $rule = {
      precondition => {0 => $precondition},
      deletions => [0],
    };
    push @resolutions, $rule;
    push @weights, defined $FS->{marked}[$k]{deletion} ? $FS->{marked}[$k]{deletion} : 0.0001;

    my $i = 0;
    my $resolution_type = 0;
    RESOLUTION_TYPE: while ($resolution_type <= 1) {
      my $effects;
      my $base_weight;
      my $no_persist = 0;
      $no_persist = 1 if defined $FS->{marked}[$k]{phonemic_only};
 
      if ($resolution_type == 0) {
        $i = 0, $resolution_type++, next if $i >= length($precondition);
        $i++, redo if substr($precondition, $i, 1) !~ /[01]/; # only flip actual things in the situation
        $effects = '.' x length($precondition);
        substr($effects, $i, 1) = (substr($reqd, $i, 1) eq '1' ? '0' : '1');
        # don't turn univalents on (unless specially allowed)
        if (substr($reqd, $i, 1) eq '0' and defined $FS->{features}[$i]{univalent}) {
          $i++, redo if !defined $FS->{marked}[$k]{flip}{$FS->{features}[$i]{name}};
          $effects = overwrite($effects, parse_feature_string($FS->{marked}[$k]{univalent_addition}, 1));
          $no_persist = 1;
        }
        $i++, redo if defined $FS->{features}[$i]{no_flip};
        # Weights for flipping individual features: given in {flip}.
        $base_weight = (defined $FS->{marked}[$k]{flip}{$FS->{features}[$i]{name}} ? 
            $FS->{marked}[$k]{flip}{$FS->{features}[$i]{name}} : 1);
      } 

      elsif ($resolution_type == 1) {
        $i = 0, $resolution_type++, next if $i >= @{$FS->{relations}};
        # just bail if we're in a stripping condition.
        for my $str (@{$FS->{strippings}}) {
          my $strip_condition = $str->{condition_parsed};
          $i = 0, $resolution_type++, next RESOLUTION_TYPE if $precondition =~ /^$strip_condition$/;
        }

        $i++, redo if defined $FS->{relations}[$i]{spread_only};
        
        my $from = parse_feature_string($FS->{relations}[$i]{from}, 1);
        $i++, redo if $precondition !~ /^$from$/;
        $effects = add_requirements(parse_feature_string($FS->{relations}[$i]{to}, 1));
        if (compatible(add_entailments($effects), $precondition)) {
          $FS->{relations}[$i]{from} =~ /^([^ ]*)/;
          $_ = parse_feature_string($1, 1);
          y/01/10/;
          $effects = overwrite($effects, add_requirements($_));
        }
        # Weights for doing any complicated feature change: given in {relate_weight},
        # which apply to anything they match.
        $base_weight = $FS->{relations}[$i]{weight};
        if(defined $FS->{marked}[$k]{related_weight}) {
          for my $outcome (keys %{$FS->{marked}[$k]{related_weight}}) {
            my $f = parse_feature_string($outcome, 1);
            $base_weight *= $FS->{marked}[$k]{related_weight}{$outcome} if $effects =~ /^$f$/;
          }
        }
      }

      $total_base_weight += $base_weight;

      my $rule = {
         precondition => {0 => $precondition},
         effects => {0 => $effects},
          # Recastability is filled out below.
         base_weight => $base_weight,
      };
      if ($threshold < 1) {
        $rule->{cede} = 1-$threshold;
      }
      my $persistence_weight = defined $FS->{marked}[$k]{persist} ? $FS->{marked}[$k]{persist} : $threshold;
      my @variants = persistence_variants $phonology, [$rule, $base_weight], $persistence_weight, 
                                          $no_persist, $args{generable_val};
      for (@variants) {
        push @resolutions, $_->[0];
        push @weights, $_->[1];
      }
      $i++;
    } # resolution type
  } # 'repair'
  
  else {
    print STDERR "unknown rule tag: $tag\n";
    return;
  }

  my $selected_rule;
  RESOLVE: {
    $selected_rule = undef;
    my $total_weight = 0;
    $total_weight += $_ for @weights;
    my $w = rand $total_weight;
    my $j;
    for (0..$#weights) {
      $j = $_, $selected_rule = $resolutions[$_], last if (($w -= $weights[$_]) < 0);  
    }
    
    # Decorate the selected resolution by clearing features that now lack their requirements.
    # Do antithetical features.
    for my $displ (keys %{$selected_rule->{effects}}) {
      $selected_rule->{effects}{$displ} = add_entailments $selected_rule->{effects}{$displ};
      
      # If this resolution is to be avoided, try again.
      for my $avoid (@{$args{avoid}}) {
        if ($selected_rule->{effects}{$displ} eq $avoid) {
          splice @resolutions, $j, 1;
          splice @weights, $j, 1;
          redo RESOLVE;
        }
      }
    } # $displ
  } # RESOLVE

  # If any of the preconditions of this rule are not generable by anything coming before,
  # and it's a one-time rule, it's never triggerable; just drop it and don't write it down.
  for my $displ (keys %{$selected_rule->{precondition}}) {
    for my $i (0..@{$FS->{features}}-1) {
      return if substr($selected_rule->{precondition}{$displ}, $i, 1) =~ /[01]/ 
            and !defined($args{generable_val}[substr($selected_rule->{precondition}{$displ}, $i, 1)][$i])
            and defined $selected_rule->{inactive};
    }
  }

  # Adding {except} conditions if this might newly set a feature which a stripping takes out
  # would be nice if it worked, but there are problems if the feature being set is a side effect;
  # we don't want to block the whole rule on its account, then.  So in place of this,
  # we play an underhanded game with which_preconditions.  This is *very very naughty* of us,
  # it means the semantics of which_preconditions aren't straightforward and will
  # likely lead to pain in the future.

  # It's correct for extra condition rules to have no tag, so that they
  # just drop out when regenerated.
  $selected_rule->{tag} = $tag unless defined $args{extra_precondition};
  $selected_rule->{priority} = 0 if !defined $selected_rule->{priority};
  if (defined $selected_rule->{base_weight}) {
    $selected_rule->{recastability} = (1 - $selected_rule->{base_weight} / $total_base_weight);
    $selected_rule->{recastability} = 0 if $selected_rule->{recastability} < 0;
    delete $selected_rule->{base_weight};
  }
  for (@{$selected_rule->{inactivate}}) {
    $phonology->[$_]{inactive} = scalar @$phonology unless defined $phonology->[$_]{inactive}
                                                    and $phonology->[$_]{inactive} < scalar @$phonology;
  }
  delete $selected_rule->{inactivate};

  for my $displ (keys %{$selected_rule->{effects}}) {
    for my $i (0..@{$FS->{features}}-1) {
      if (substr($selected_rule->{effects}{$displ}, $i, 1) =~ /[01]/) {
        push @{$args{generable_val}[substr($selected_rule->{effects}{$displ}, $i, 1)][$i]}, scalar @$phonology;
      }
    }
  }

  # Since extra conditions added to this rule may have come out the same way, delete redundant ones,
  # i.e. rules made since invoking this function whose conditions are narrower than 
  # the one about to be inserted, and which have the same effect, unless they're persistent
  # and we're not.  
  # (If we cut a few too many things, though, not the end of the world.)
  # Only do this to immediately preceding ones, since we might get A in a doubly-special case,
  # B in a special case, A in the general case.
  my $former_length = @$phonology;
  DROP_REDUNDANT: while(1) {
    my $rule = $phonology->[-1];
    for my $displ (keys %{$selected_rule->{precondition}}) {
      last DROP_REDUNDANT unless defined $rule->{precondition}{$displ}
          and $rule->{precondition}{$displ} =~ /^$selected_rule->{precondition}{$displ}$/;
    }
    last DROP_REDUNDANT unless keys %{$rule->{effects}} == keys %{$selected_rule->{effects}};
    for my $displ (keys %{$selected_rule->{effects}}) {
      last DROP_REDUNDANT unless defined $rule->{effects}{$displ}
          and $rule->{effects}{$displ} eq $selected_rule->{effects}{$displ};
    }
    last DROP_REDUNDANT if !defined $rule->{inactive} and defined $selected_rule->{inactive};
    #print STDERR YAML::Any::Dump($rule) . "redounds with\n" . YAML::Any::Dump($selected_rule) . "\n"; # debug
    pop @$phonology;
  }
  if (@$phonology < $former_length) {
    for my $rule (@$phonology) {
      $rule->{inactive} = @$phonology if $rule->{inactive} > @$phonology;
    }
  }

  push @$phonology, $selected_rule;

  # Recurse to replace any other rule which we deactivated; make sure these don't resolve 
  # the same as the bad rule.
  # (Incidentally, we couldn't recurse before the push; it would break rule referencing by number.)
  for my $bt (@{$selected_rule->{broken_tags}}) {
    $bt =~ /^(.*) ([01u.]*)$/;
    my ($tag, $avoid) = ($1, $2);
    my %otherargs = %args;
    delete $otherargs{avoid};
    delete $otherargs{extra_precondition};
#    print "{\n"; # debug
    gen_one_rule($phonology, $tag, avoid => [(split /\|/, $avoid), @{$args{avoid}}], %otherargs);
#    print "}\n"; # debug
  }
  delete $selected_rule->{broken_tags};
}



# A complete generated phonology has the following layers.
#
# (1) General single segment repair and default feature insertion rules.
#     [A few of the default insertion rules may be harmonic in nature.  Aside from this exception,]
#     These are context-independent.
# (2) General cluster resolution, allophony, and the like.  Any time from 
#     the start of this block onward is a sensible affix attachment time.
#
# Alternations will not be implemented by the resolutions of features in
# different contexts alone.  Instead, we'll eventually generate a thing for them:
# perhaps a table with several small dimensions, and for each value of each dimension
# one (or a few?) feature-values from among the contrastive features,
# probably using related features to generate values of the same contrast as we might.  
#
# Things, vowels included, have to be able to alternate with zero!
# In allophony mode (sound change mode is different)
# I won't actually ever generate a new syllabification rule; therefore,
# syllable structure things have to be handled fully in the alternations.
# This means that, to the extent the forms don't fit within the generated syllable structure,
# we'll have to list allowable deletions carefully, and specify epenthetic vowels for the rest.
# (Sonority mistroughs I can handle though.)
#
# To generate forms in an alternation, then, we generate the phone(s) in question
# as normal and overstamp them with the feature values from the alternation somehow.
# This still needs some thought on what to do about features in an alternation
# that don't fulfill their requisites for generation (think also about sound change).  

sub gen_phonology {
  my (@phone_generator, @phonology);
  my @syllable_structure;

  for my $slot (@{$FS->{syllable_template}}) {
    next if rand() >= $slot->{prob};
    do {
      push @syllable_structure, {
        prob => fuzz($slot->{presence}),
        features => parse_feature_string($slot->{features}),
        tag => $slot->{tag},
      };
    } while (defined $slot->{prob_more} and rand() < $slot->{prob_more});
  }
  
  my @generable;
  my @generable_val; # defined($generable_val[$v][$f]) iff the $f-th feature can take value $v \in 0,1.
                     # If it's an empty list, that's ok; that just means the feature can only come up in phone generation.
  my %family_inventories;
  $family_inventories{$_} = { parse_feature_string($FS->{families}{$_}, 1) => 1 }
      for (keys %{$FS->{families}});
  my %special_filling; # which features we're using a U in the syllable structure in
  
  # Sometimes we generate things which we never generate a prerequisite for.  Not a problem though.
  for my $fi (0..@{$FS->{features}}-1) {
    my $f = $FS->{features}[$fi];
    for my $sit (@{$f->{generated}}) {
      if (rand() < $sit->{contrast}) {
        my $requires;
        $requires = parse_feature_string($f->{requires}, 1) if defined $f->{requires};

        my @by_families;
        if (defined $sit->{by_family} and rand() < $sit->{by_family_prob}) {
          for my $phone (keys %{$family_inventories{$sit->{by_family}}}) {
            next if defined $requires and !compatible($phone, $requires);
            push @by_families, $phone if rand() < $sit->{each_family_prob} * 
                sqrt($family_inventories{$sit->{by_family}}{$phone});
          }
          # If we didn't make the contrast anywhere on the first pass, introduce it on the
          # most frequent family category.  I want a rule like this so that the 
          # feature doesn't just fail to appear.  Choosing the
          # most frequent category isn't really motivated, but it serves to spread out
          # probability peaks, and has the right effect for some particular cases 
          # (e.g. lateral).
          if (!@by_families) {
            my ($max_phone, $max_value) = ('', 0);
            for (my ($phone, $value) = each %{$family_inventories{$sit->{by_family}}}) {
              next if defined $f->{requires} and $phone !~ /^$requires$/;
              ($max_phone, $max_value) = ($phone, $value) if $value > $max_value;
            }
            push @by_families, $max_phone;
          }
          # print "doing $f->{name} for @by_families\n"; # debug
        }

        my $precondition = parse_feature_string($sit->{condition}, 1);
        substr($precondition, $feature_indices{$f->{name}}, 1) = 'u';
        $precondition = overwrite $precondition, $requires if defined $f->{requires};
        my %rule = (
          precondition => {0 => $precondition},
          effects => {0 => parse_feature_string($f->{name}, 1)}, 
          antieffects => {0 => parse_feature_string('-' . $f->{name}, 1)}, 
          prob => [map fuzz($sit->{prob}), @syllable_structure],
        );
        substr($rule{effects}{0}, $feature_indices{$f->{antithetical}}, 1) = '0' if (defined $f->{antithetical});
        if (@by_families) {
          for (@by_families) {
            my %rule1 = %rule; 
            $rule1{precondition}{0} = overwrite($precondition, $_);
            # Don't allow a rule inserting f in families to be sensitive to f.
            # (It confuses the inventory-taker.)
            next if index($rule1{precondition}{0}, 'u') == -1;
            push @phone_generator, \%rule1;
          }
        } else {
          push @phone_generator, \%rule;
        }

        $generable[$fi] = 1;
        $generable_val[0][$fi] = [];
        $generable_val[1][$fi] = [];
        
        for my $slot (@syllable_structure) {
          my $phone = $slot->{features};
          $phone =~ s/u/./g;
          next unless compatible $phone, $precondition;
          if (defined $f->{slots}{$slot->{tag}}) {
            my $r = rand();
            if ($r < $f->{slots}{$slot->{tag}}[0]) {
              $slot->{features} = overwrite $slot->{features}, $rule{antieffects}{0};
            } elsif ($r < $f->{slots}{$slot->{tag}}[0] + $f->{slots}{$slot->{tag}}[1]) {
              $slot->{features} = overwrite $slot->{features}, $rule{effects}{0};
              $slot->{features} = overwrite $slot->{features}, parse_feature_string($f->{slot_if_on}, 1)
                  if defined $f->{slot_if_on};
            } elsif ($r < $f->{slots}{$slot->{tag}}[0] + $f->{slots}{$slot->{tag}}[1] +  $f->{slots}{$slot->{tag}}[2]) {
              substr($slot->{features}, $feature_indices{$f->{name}}, 1) = 'U';
              $special_filling{$feature_indices{$f->{name}}} = 1;
            }
          }
        }

        for my $fam (split / /, $f->{families}) {
          $_ = $precondition;
          for my $phone (keys %{$family_inventories{$fam}}) {
            my $s = $_;
            $s =~ s/u/./g;
            next if $phone !~ /^$s$/;
            # Using $rule{prob}[0] here of course isn't especially correct, but it'll do.
            $family_inventories{$fam}{overwrite($phone, $rule{effects}{0})} += 
              $family_inventories{$fam}{$phone} * $rule{prob}[0] if ($rule{prob}[0] > 0);
            $family_inventories{$fam}{overwrite($phone, $rule{antieffects}{0})} += 
              $family_inventories{$fam}{$phone} * (1 - $rule{prob}[0]) if ($rule{prob}[0] < 1);
            delete $family_inventories{$fam}{$phone}; 
          }
        }
      } # rand
    } # situations for generation
    
    if (defined $f->{structural}) {
      $generable[$fi] = 1;
      $generable_val[0][$fi] = [-1]; # -1 to not trip the never triggerable check.
      $generable_val[1][$fi] = [-1]; # hope it doesn't screw up other things
    }
  } # features in the phone generator

  # Choose the order the rules are going to appear in; write down a list of rule tag strings.
  # Default provision rules come in random order; subject to that, repair rules come 
  # as soon as they can (and we lazily haven't randomised them yet).

  my @default_rule_positions = (1..@{$FS->{features}}); # 0 is for earliest repair rules
  for my $i (0..@{$FS->{features}}-1) {
    my $j = $i + int rand(@{$FS->{features}} - $i);
    $_ = $default_rule_positions[$i]; 
    $default_rule_positions[$i] = $default_rule_positions[$j];
    $default_rule_positions[$j] = $_;
  }
  my @repair_rule_tags;
  for my $k (0..@{$FS->{marked}}-1) {
    my $f = parse_feature_string $FS->{marked}[$k]{condition};
    my $when = 0;
    for (0..length($f)-1) {
      $when = $default_rule_positions[$_]
          if substr($f, $_, 1) ne 'u' and !defined $generable[$_] and $default_rule_positions[$_] > $when;
    }
    push @{$repair_rule_tags[$when]}, "repair $k" unless defined $FS->{marked}[$k]{phonemic_only};
  }
  my @assim_tags = ((map "assimilate $_", (0..@{$FS->{features}}-1)), 
                    (map "assimilate_related $_", (0..@{$FS->{relations}}-1)));
  for my $i (0..@assim_tags-1) {
    my $j = $i + int rand(@assim_tags - $i);
    $_ = $assim_tags[$i]; 
    $assim_tags[$i] = $assim_tags[$j];
    $assim_tags[$j] = $_;
  }  

  my @rule_tags;
  push @rule_tags, "stripping $_" for (0..@{$FS->{strippings}}-1);
  push @rule_tags, "default $_" for keys %special_filling;
  for my $i (0..@{$FS->{features}}) {
    push @rule_tags, map "default $_", grep(($default_rule_positions[$_] == $i and !defined $special_filling{$_}), 
                                             0..$#default_rule_positions);
    push @rule_tags, @{$repair_rule_tags[$i]} if defined $repair_rule_tags[$i];
  }
  for my $k (0..@{$FS->{marked}}-1) {
    push @rule_tags, "repair $k" if defined $FS->{marked}[$k]{phonemic_only};
  }
  push @rule_tags, '#'; # false tag for end of phoneme straightening-out
  for my $tag (@assim_tags) {
    my ($kind, $k) = split / /, $tag, 2;
    my $bound = 0;
    if ($kind eq 'assimilate' and defined $FS->{features}[$k]{assimilation}) {
      $bound = @{$FS->{features}[$k]{assimilation}};
    } elsif ($kind eq 'assimilate_related' and defined $FS->{relations}[$k]{assimilation}) {
      $bound = @{$FS->{relations}[$k]{assimilation}};
    }
    push @rule_tags, map "$tag $_", 0..$bound-1;
  }

  my $start_sequences; # end of rules that pertain only to individual segments
  for my $tag (@rule_tags) {
    if ($tag eq '#') {
      print STDERR "on to allophony...\n" if $verbose;
      $start_sequences = @phonology; 
      next;
    } 
    # We pass the generator as a way of specifying what contrasts are available.
    # For sound change purposes we'll need an alternate way to pass this information.
    gen_one_rule \@phonology, $tag, generator => \@phone_generator, generable_val => \@generable_val, initial => 1; 
  }
    
  ($start_sequences, \@syllable_structure, \@phone_generator, \@phonology);
}


# In a modifier description, it's only the first phone that the modifier actually spells;
# the rest are just conditions on its applicability.

sub name_phone {
  my ($phone, %args) = (shift, @_);
  my $pa = defined $args{alphabet} ? $args{alphabet} : 
           $use_html ? $phonetic_alphabets{IPA} : $phonetic_alphabets{CXS};
  my %taken_care_of;
  my $s = '##';
  
  for my $x (keys %{$pa->{ligations}}) {
    next if $phone !~ /^$x$/;
    my $phone0 = overwrite $phone, parse_feature_string($pa->{ligations}{$x}[0], 1);
    my $phone1 = overwrite $phone, parse_feature_string($pa->{ligations}{$x}[1], 1);
    my ($tc0, $s0) = name_phone($phone0, %args, no_modifiers => 1);
    my ($tc1, $s1) = name_phone($phone1, %args, no_modifiers => 1);
    $s = $pa->{ligations}{$x}[2];
    $s =~ s/\[\]/$s0/;
    $s =~ s/\[\]/$s1/;
    %taken_care_of = (%$tc0, %$tc1, 
                      map(($_ => 1), (grep substr($x, $_, 1) ne '.', 0..@{$FS->{features}}-1)) );
    last;
  }

  if ($s eq '##') {
    for my $x (keys %{$pa->{characters}}) {
      next if $phone !~ /^$x$/;
      $s = $pa->{characters}{$x};
      %taken_care_of = map(($_ => 1), (grep substr($x, $_, 1) ne '.', 0..@{$FS->{features}}-1));
      last;
    }
  }

  return (\%taken_care_of, $s) if $args{no_modifiers};
  
  MODIFIER: for my $x (keys %{$pa->{modifiers}}) {
    my ($x_all, $x_spells) = split / /, $x;
    next if $phone !~ /^$x_all$/;
    my $redundant = 1;
    for (0..@{$FS->{features}}-1) {
      $redundant = 0 if substr($x_spells, $_, 1) ne '.' and !defined $taken_care_of{$_};
    }
    next if $redundant;
    my $t = $pa->{modifiers}{$x};    
    $t =~ s/\[\]/$s/;
    $s = $t;
    %taken_care_of = (%taken_care_of, map(($_ => 1), (grep substr($x_spells, $_, 1) ne '.', 0..@{$FS->{features}}-1)));
  }

  $s;
}

sub spell_out {
  my ($word, %args) = (shift, @_);
  if ($args{null} and !@$word) {
    my $pa = defined $args{alphabet} ? $args{alphabet} : 
             $use_html ? $phonetic_alphabets{IPA} : $phonetic_alphabets{CXS};
    return $pa->{null};
  }
  join "", map name_phone($_, %args), @$word;
}


sub add_in {
  my ($inventory, $x, $v) = @_;
  return unless grep $_, @$v;
  if (defined $inventory->{$x}) {
    for my $i (0..@$v-1) {
      $inventory->{$x}[$i] += $v->[$i];
    }
  } else {
    $inventory->{$x} = $v; # shallow copy okay?
  }
}

sub inventory {
  my $phonology_data = shift;
  my ($syllable_structure, $phone_generator, $phonology, $which_preconditions) = 
      @$phonology_data{qw/syllable_structure phone_generator phonology which_preconditions/};
  # This is a hash from phones to lists of probabilities in the various 
  # syllable positions.  The values are inessential for generating the phonology,
  # but we use them to calculate the entropy.  However, we don't take any interactions
  # between phones into account for these numbers, so they're kind of crude.
  my %inventory;

  for my $i (0..@$syllable_structure-1) {
    add_in \%inventory, $syllable_structure->[$i]{features}, 
        [map(($_ == $i ? 1 : 0), (0..@$syllable_structure-1))];
  }

  # Revise this if ever first-pass resolvent rules can cause splits or whatever.
  for my $rule (@$phone_generator) {
    my %inventory2;
    for my $phone (keys %inventory) {
    my @v = @{$inventory{$phone}};
      my @word;
      @word = ($phone);
      run_one_rule \@word, $rule, rand_value => 0;
      add_in \%inventory2, $word[0], [map $v[$_] * $rule->{prob}[$_], 0..@v-1];
      @word = ($phone); 
      run_one_rule \@word, $rule, rand_value => 1;
      add_in \%inventory2, $word[0], [map $v[$_] * (1 - $rule->{prob}[$_]), 0..@v-1];
    }
    %inventory = %inventory2;
  }
  
  # Strip unsupported features at the end, in case the syllable structure put them in.
  for my $phone (keys %inventory) {
    my $stripped = add_entailments $phone;
    $stripped =~ s/U/u/g;
    if ($stripped ne $phone) {
      add_in \%inventory, $stripped, $inventory{$phone};
      delete $inventory{$phone};
    }
  }

  my %prinv;
  my %resolver;

  for my $phone (keys %inventory) {
    my @word = ($phone);
    print STDERR "in:  $phone\n" if $debug >= 1; 
    run_phonology \@word, $phonology, 
                  which_preconditions => $which_preconditions, 
                  end => $phonology_data->{start_sequences};
    my $outcome = join(' ', @word);
    print STDERR "out: $outcome /" . (@word ? name_phone($word[0]) : '') . "/\n" if $debug >= 1;
    $resolver{$phone} = $outcome;
    add_in \%prinv, $outcome, $inventory{$phone};
  }

  # Handle zero specially: its likelihood should not be given by resolutions
  # but should be the likelihood we picked by fiat earlier.
  # In addition, a syllable position where nothing can show up 
  # should just be thrown away.
  my @old_zero_probs = defined $prinv{''} ? @{$prinv{''}} : ((0) x @$syllable_structure);
  $prinv{''} = [map 1 - $syllable_structure->[$_]{prob}, 0..@$syllable_structure-1];
  for (my $i = @$syllable_structure-1; $i >= 0; $i--) {
    if ($old_zero_probs[$i] >= 1 - 1e-8) { # numerical things going on?
      splice @old_zero_probs, $i, 1;
      splice @$syllable_structure, $i, 1;
      splice @$_, $i, 1 for values %prinv;
    }
  }
  for my $phone (keys %prinv) {
    next if $phone eq '';
    for my $i (0..@$syllable_structure-1) {
      $prinv{$phone}[$i] = $prinv{$phone}[$i] / (1 - $old_zero_probs[$i]) * $syllable_structure->[$i]{prob};
    }
  }

  # Record entropies on the syllable structure.  
  for my $i (0..@$syllable_structure-1) {
    my $entropy = 0;
    while (my ($phone, $v) = each %prinv) {
      $entropy += $v->[$i] * log($v->[$i]) / log(0.5) if $v->[$i] > 0;
    }
    $syllable_structure->[$i]{entropy} = $entropy; 
  }

  # We will need to use the resolver when it comes to generating alternations.  
  # It's not necessary to for ordinary stem generation, though; for that the inventory suffices.
  \%prinv;
}



# Put in the false features which our model doesn't contain but which our descriptions do.
# Assumes annotation.
sub add_false_features {
  my ($phone, $str) = (shift, shift);
  while (defined $str->{subtables}) {
    my $subtable = substr($phone, $feature_indices{$str->{subtables}}, 1);
    $str = $str->{$subtable};
  }
  while (my ($k, $v) = each %{$str->{undef_parsed}}) {
    $phone = overwrite $phone, $v if $phone =~ /^$k$/;
  }
  $phone;
}

# Munge a phoneme in the usual order into its table-keyed form.  
sub table_sortkey {
  my ($phone, $str) = (shift, shift);

  # Although tabulate itself doesn't need this, it's handy if we want to use this
  # as an ordering in other places.
  if (defined $str->{subtables}) {
    my $subtable = substr($phone, $feature_indices{$str->{subtables}}, 1);
    return $subtable . table_sortkey($phone, $str->{$subtable});
  }
  
  while (my ($k, $v) = each %{$str->{undef_parsed}}) {
    $phone = overwrite $phone, $v if $phone =~ /^$k$/;
  }
  $phone =~ s/u/0/g;
  my $position = join '', map(substr($phone, $str->{order_i}[$_], 1) =~ /[01]/ ? 
                              (substr($phone, $str->{order_i}[$_], 1) + $str->{order_r}[$_])%2 : 
                              substr($phone, $str->{order_i}[$_], 1),
                              0..@{$str->{order_i}}-1);
  while (my ($k, $v) = each %{$str->{flips_parsed}}) { # does this need keep_undefs?
    substr($position, $v, 1) = 1 - substr($position, $v, 1) 
        if $phone =~ /^$k$/ and substr($position, $v, 1) =~ /[01]/;
  }
  $position;
}

# Add the features which are redundantly present in a list.
sub reenrich {
  my ($k, $inventory) = (shift, shift);
  my $old_k = $k;
  REINSERTION: for my $i (0..length($k)-1) {
    next unless substr($k, $i, 1) eq '.';
    for my $v (0..1) {
      my $test = $old_k;
      substr($test, $i, 1) = 1-$v;
      unless (grep /^$test$/, @$inventory) {
        substr($k, $i, 1) = $v;
        next REINSERTION;
      }
    }
  }
  $k;
}

# Make a label as tabulate needs.

# I guess this kind of duplicates some functionality of name_phone.  
# Maybe I can unify later, if I get zealous.  But right now I'm scared of these things.

sub tabulate_label {
  my ($reenriched, $p, $pmod, $l, $lmod, %args) = @_;
  my $label = '';
  $l = [] if !defined $l;
  $lmod = [] if !defined $lmod;

  my @taken_care_of;
  if (defined $args{significant}) {
    for (0..length($args{significant})-1) {
      $taken_care_of[$_] = 1 if substr($args{significant}, $_, 1) eq '.';
    }
  }
  for my $i (0..@$l-1) {
    if ($reenriched =~ /^$p->[$i]$/) {
      if ($args{nobase}) {
        # if we fail to pick anything from $p in this case, that's okay.
        my $irredundant;
        for (0..length($p->[$i])-1) {
          $irredundant = 1 if substr($p->[$i], $_, 1) ne '.' and !$taken_care_of[$_]; 
        }
        next unless $irredundant;
      }
      $label = $l->[$i];
      for (0..length($p->[$i])) {
        $taken_care_of[$_] = 1 if substr($p->[$i], $_, 1) ne '.';
      }
      last;
    }
  }
  my %modificated_types;
  for my $i (0..@$lmod-1) {
    if ($reenriched =~ /^$pmod->[$i]$/) {
      my $irredundant;
      for (0..length($pmod->[$i])-1) {
        $irredundant = 1 if substr($pmod->[$i], $_, 1) ne '.' and !$taken_care_of[$_];
      }
      next unless $irredundant;
      # Don't use two modificateds from the same class.
      $label =~ /\[(.*)\]/;
      next if defined $modificated_types{$1};
      $modificated_types{$1} = 1 if $1;
      my $old_label = $label;
      $label = $lmod->[$i];
      $label =~ s/\[.*\]/$old_label/;
      for (0..length($pmod->[$i])) {
        $taken_care_of[$_] = 1 if substr($pmod->[$i], $_, 1) ne '.';
      }
    }
  }

  if ($args{nons}) {
    # Spell out even features which take the value not normally given a word.
    # The value of $args{nons} should be the inventory. 
    my %dont_spell;
    my $str = $phon_descr->{table_structure};
    while (defined $str->{subtables}) {
      $dont_spell{$feature_indices{$str->{subtables}}} = 1;
      my $subtable = substr($reenriched, $feature_indices{$str->{subtables}}, 1);
      last if $subtable eq '.';
      $str = $str->{$subtable};
    }

    for my $i (0..length($reenriched)-1) {
      next if $taken_care_of[$i];
      next if substr($reenriched, $i, 1) eq '.';
      next if $dont_spell{$i};
      # TODO: respect_univalent isn't the correct thing; the names in nasal assimilation are still gross
      next if $args{respect_univalent} and substr($reenriched, $i, 1) eq '0' and $FS->{features}[$i]{univalent};
      my $non = '.' x length($reenriched);
      substr($non, $i, 1) = 1 - substr($reenriched, $i, 1);
      my $reenriched_non = reenrich $non, $args{nons}; 
      my $non_label = tabulate_label($reenriched_non, $p, $pmod, $l, $lmod, %args, 
                                     nons => undef, significant => $non, nobase => 1);
      chop $non_label while $non_label =~ / $/; # for nobase
      $non_label = substr($non_label, 1) while $non_label =~ /^ /;
      $label = "non-$non_label $label" if $non_label;
    }
  }

  if ($args{header}) {
    # Make abbreviations for table headers.
    $label =~ s/ or /\//g;
    $label =~ s/\{[^{}]*\}/./g;
  } else {
    $label =~ s/[{}]//g;
  }

  $label;
}

# Return an HTML table of the phonology.  

sub tabulate {
  my ($pd, %args) = (shift, @_);
  my $str = defined $args{structure} ? $args{structure} : $phon_descr->{table_structure};
  my $condition = defined $args{condition} ? $args{condition} : '.' x @{$FS->{features}};

  if (defined $str->{subtables}) {
    my $split_feature = $str->{subtables};
    my $first = 0;
    if ($split_feature =~ /^!/) {
      $split_feature = substr($split_feature, 1);
      $first = 1;
    }
    my $t0 = tabulate($pd, %args, 
                           structure => $str->{0}, 
                           condition => overwrite($condition, parse_feature_string("-$split_feature", 1)));
    my $t1 = tabulate($pd, %args, 
                           structure => $str->{1}, 
                           condition => overwrite($condition, parse_feature_string("+$split_feature", 1)));
    return $first ?
        $t1 . "<br />\n" . $t0 :
        $t0 . "<br />\n" . $t1;
  }

  return "" unless $str->{order}; # may as well keep this

  # annotate with parsed forms
  unless (defined $str->{order_i}) { # i for "indices"
    my @chunks = split /; /, $str->{order};
    $str->{lengths} = [map scalar split(/ /, $_), @chunks];
    my @fs = map split(/ /, $_), @chunks;
    for (0..$#fs) {
      $fs[$_] =~ /^(!?)(.*)$/;
      $str->{order_r}[$_] = $1 ? 1 : 0;
      $str->{order_i}[$_] = $feature_indices{$2};
    }

    while (my ($k, $v) = each %{$str->{undefineds}}) {
      $str->{undef_parsed}{parse_feature_string($k, 1)} = parse_feature_string($v, 1);
    }
    
    while (my ($k, $v) = each %{$str->{flips}}) {
      my @which = grep $str->{order_i}[$_] == $feature_indices{$v}, 0..@{$str->{order_i}}-1;
      $str->{flips_parsed}{parse_feature_string($k, 1)} = $which[0];
    }
    
    @fs = split / /, $str->{collapse};
    for my $f (@fs) {
      my @which = grep $str->{order_i}[$_] == $feature_indices{$f}, 0..@{$str->{order_i}}-1;
      push @{$str->{collapse_i}}, $which[0];
    }
  }

  return if $args{annotate_only};

  # Labels come in a list emulating an ordered hash, so that specific can bleed general.
  # Convert them to sortkey form too.
  # The modifier behaviour here is different to in name_phone: 
  # a modifier will be taken if any one of its features isn't spelled out.
  my %label_phones; 
  my %labels = (rows => [], columns => [], rows_mod => [], columns_mod => []);
  for my $thing (qw/rows columns rows_mod columns_mod/) {
    for (@{$str->{labels}{$thing}}) {
      my ($phone, $label) = split /: */;
      my $position = table_sortkey parse_feature_string($phone, 1), $str, 1;
      push @{$labels{$thing}}, $label;
      if ($thing =~ /^rows/) {
        push @{$label_phones{$thing}}, substr($position, 0, $str->{lengths}[0]);
      } elsif ($thing =~ /^columns/) {
        push @{$label_phones{$thing}}, substr($position, $str->{lengths}[0], $str->{lengths}[1]);
      }
    }
  }

  my (%table, %rows, %columns, %spots);
  for my $phone (keys %{$pd->{gen_inventory}}) {
    next unless $phone =~ /^$condition$/;
    my $name = name_phone($phone, alphabet => $phonetic_alphabets{IPA});
    $table{table_sortkey $phone, $str} = $name;
  }

  my %old_table = %table;
  COLLAPSE: for my $collapse (@{$str->{collapse_i}}) {
    for my $position (keys %table) {
      substr($position, $collapse, 1) = 1 - substr($position, $collapse, 1);
      next COLLAPSE if defined $table{$position};
    }
    my %new_table;
    while (my ($position, $v) = each %table) {
      substr($position, $collapse, 1) = '.';
      $new_table{$position} = $v;
    }
    %table = %new_table;
  }

  for my $position (keys %table) {
    $rows{substr($position, 0, $str->{lengths}[0])} = 1;
    $columns{substr($position, $str->{lengths}[0], $str->{lengths}[1])} = 1;
    $spots{substr($position, $str->{lengths}[0]+$str->{lengths}[1])} = 1;
  }

  # Now reinsert digits where we can, so we can use the most appropriate label.
  # The behaviour of this should be regarded as undefined if a feature
  # is undefined everywhere in the column.
  my @extant_rows = map substr($_, 0, $str->{lengths}[0]), keys %old_table;
  my @extant_columns = map substr($_, $str->{lengths}[0], $str->{lengths}[1]), keys %old_table;
  my (%reenriched_rows, %reenriched_columns);
  $reenriched_rows{$_} = reenrich $_, \@extant_rows for keys %rows;
  $reenriched_columns{$_} = reenrich $_, \@extant_columns for keys %columns;

  my $table = "<table style=\"text-align: center;\">\n<caption>$str->{caption}</caption>\n";
  $table .= '<tr style="vertical-align: bottom;"><th></th>';
  for my $column (sort keys %columns) {
    $table .= '<td></td>'; # empty cells for separation, yeah
    my $label = tabulate_label $reenriched_columns{$column}, 
                               $label_phones{columns}, $label_phones{columns_mod},
                               $labels{columns}, $labels{columns_mod},
                               header => 1;
    $label =~ s/ /<br \/>/g;
    $table .= "<th colspan=\"" . keys(%spots) . "\">" .
              ($label ? "\u$label" : '?') . 
              '</th>'; 
  }

  $table .= "</tr>\n";
  for my $row (sort keys %rows) {
    $table .= '<tr>';
    my $label = tabulate_label $reenriched_rows{$row}, 
                               $label_phones{rows}, $label_phones{rows_mod},
                               $labels{rows}, $labels{rows_mod};
    $table .= "<th style=\"text-align: right;\">" .
              ($label ? "\u$label" : '?') . 
              '</th>'; 

    for my $column (sort keys %columns) {
      $table .= '<td></td>';
      for my $spot (sort keys %spots) {
        $table .= '<td>' .
                  (defined $table{$row . $column . $spot} ? $table{$row . $column . $spot} : '') . 
                  '</td>';
      }
    }
    $table .= "</tr>\n";
  }
  $table .= "</table>\n";
}


# The features of a phone determining what table it's in.
sub str_part {
  my $phone = shift;
  my $skeleton = '.' x @{$FS->{features}};
  my $str = $phon_descr->{table_structure};
  while (defined $str->{subtables}) {
    my $i = $feature_indices{$str->{subtables}};
    my $subtable = substr($phone, $i, 1);
    last if $subtable eq '.';
    substr($skeleton, $i, 1) = substr($phone, $i, 1);
    $str = $str->{$subtable};
  }
  $skeleton;
}

sub English_indefinite {
  my $a = shift;
  return ($a =~ /^[aeiou]/ ? 'an ' : 'a ') . $a;
}

sub English_plural {
  my $a = shift;
  return substr($a, 0, -1) . 'ies' if $a =~ /[^aeiou]y$/;
  return $a .= ($a =~ /[sxz]$|[cs]h$/ ? 'es' : 's');
}

sub name_natural_class {
  my ($phone, $inventory, %args) = (shift, shift, @_);
  #my $debug_bit = '[' . feature_string($phone) . '] '; # debug
  #$debug_bit = '<span style="font-size: small;">' . $debug_bit . '</span>' if $use_html; # debug
  #$debug_bit = '' if defined $args{nodebug}; # debug

  my $reenriched = defined $inventory ? reenrich($phone, $inventory) : $phone;

  if ($args{no_nothing}) {
    return '' unless grep /^$reenriched$/, @$inventory;
  }

  my $str = $args{str};
  my $subtable_index;
  if (!defined $str) {
    $str = $phon_descr->{table_structure};
    while (defined $str->{subtables}) {
      $subtable_index = $feature_indices{$str->{subtables}}; 
      my $subtable = substr($reenriched, $subtable_index, 1);
      last if $subtable eq '.';
      $str = $str->{$subtable};
    }
  }
  $reenriched = add_false_features $reenriched, $str;

  if (defined $str->{subtables}) {
    my $phone0 = $phone;
    substr($phone0, $subtable_index, 1) = '0';
    my $name0 = name_natural_class($phone0, $inventory, %args, str => $str->{0}, nodebug => 1);
    my $phone1 = $phone;
    substr($phone1, $subtable_index, 1) = '1';
    my $name1 = name_natural_class($phone1, $inventory, %args, str => $str->{1}, nodebug => 1);
    my $conjunction = $args{morpho} eq 'plural' ? ' and ' : ' or ';

    return $name0 if ($name0 eq $name1);
    # Factor out a common string of all but at most one word.
    $name0 =~ /^(.* )?([^ ]*)$/;
    my ($fore0, $aft0) = ($1, $2);
    $name1 =~ /^(.* )?([^ ]*)$/;
    my ($fore1, $aft1) = ($1, $2);
    if ($fore0 eq $fore1) {
      my $aft = undef;
      if ($args{morpho} eq 'plural') {
        $aft = (English_plural $str->{name}) if $aft0 eq English_plural($str->{0}{name}) 
                                            and $aft1 eq English_plural($str->{1}{name});
      } else {
        $aft = $str->{name} if $aft0 eq $str->{0}{name} and $aft1 eq $str->{1}{name};
      }
      if (!defined $aft) {
        $aft = $str->{name_first} ?
            $aft1 . $conjunction . $aft0 :
            $aft0 . $conjunction . $aft1;
      }
      return $fore0 . $aft;
    }

    return $str->{name_first} ?
        $name1 . $conjunction . $name0 :
        $name0 . $conjunction . $name1; # $debug_bit here
  }

  my $scheme = defined $args{scheme} ? $args{scheme} : 'labels';
  if (!defined $str->{$scheme}{name_classes}) {
    my (@p, @pmod, @l, @lmod);
    my %modificate = map(($_ => 1), split / /, $str->{$scheme}{modificate});
    for my $thing (qw/pre_other pre_other_mod columns rows other columns_mod rows_mod other_mod/) {
      next unless defined $str->{$scheme}{$thing};
      for (@{$str->{$scheme}{$thing}}) {
        my ($phone, $label) = split /: */;
        $phone = parse_feature_string $phone, 1;
        # TODO: 'and' should be among the scheme data, but then needs better treatment of 'and' below.
        $label .= ' and []' if $args{scheme} eq 'nominalised';
        $label .= " [$thing]" if defined $modificate{$thing};
        if ($label =~ /\[.*\]/) {
          push @pmod, $phone;
          push @lmod, $label;
        } else {
          push @p, $phone;
          push @l, $label;
        }
      }
    }
    push @p, parse_feature_string('', 1);
    push @l, $str->{name}; 
    $str->{$scheme}{name_classes}{p} = \@p;
    $str->{$scheme}{name_classes}{pmod} = \@pmod;
    $str->{$scheme}{name_classes}{l} = \@l;
    $str->{$scheme}{name_classes}{lmod} = \@lmod;
  }

  my $significant = defined $args{significant} ? $args{significant} : $phone;
  my $name = tabulate_label $reenriched,
                            $str->{$scheme}{name_classes}{p}, $str->{$scheme}{name_classes}{pmod},
                            $str->{$scheme}{name_classes}{l}, $str->{$scheme}{name_classes}{lmod},
                            %args,
                            significant => $significant,
                            nons => ($args{bar_nons} ? undef : $inventory),
                            nobase => $args{nobase}; 
  $name = English_indefinite $name if ($args{morpho} eq 'indef');

  if ($args{morpho} eq 'plural' and $name !~ / $/) {
    my @bits = split / or /, $name;
    for (my $i = $#bits; $i >= 0; $i--) {
      $bits[$i] = English_plural $bits[$i];
      last if scalar (split / /, $bits[$i]) > 1; # since then this disjunct is probably in an adj
    }
    $name = join ' or ', @bits;
  }
  chop $name while $name =~ / $/; # nobase 
  $name = substr($name, 1) while $name =~ /^ /; # nobase 
  $name = substr($name, 0, -4) if $args{scheme} eq 'nominalised'; # drop ' and'
  return $name; # debug_bit here
}

# Given a list of phones, figure out a good feature-systematic name for it,
# with the minimum complexity in some heuristic sense.
# Roughly, the model is this: minimise the cost, where
# mentioning a phone (as an inclusion or an exclusion) costs one,
# and mentioning a feature costs one plus epsilon,
# except that table-determining features are free ('cause they're necessary for naming).

# arg 'within' is a class within which we are to describe this one.  (working on it)
# arg 'suppress_ie' suppresses the exemplificatory lists.
# arg 'sort_phones' sorts the input reference to a list of phones in the way the output needs.

# If arg 'extend' is present, this behaves quite differently:
# rather than trying to name the set of phones, it tries to return the analogous
# subset of $args{extend}.
# It's far from perfect at this: it will ignore exceptions.

# FIXME: [?\] is written as [h\_?\] by name_phone from within here.  Dunno why.
sub describe_set {
  my ($orig_phones, $inventory, %args) = (shift, shift, @_);
  my $phones = $orig_phones;
  my $morpho = defined $args{morpho} ? $args{morpho} : 'indef';
  my $extend = defined $args{extend};
  my $lb = $args{etic} ? '[' : '/';
  my $rb = $args{etic} ? ']' : '/';

  my $size = @$phones;
  unless ($extend) {
    return $morpho eq 'plural' ? 'no phones' : 'no phone' if ($size == 0);
    return $lb . name_phone($phones->[0]) . $rb if ($size == 1);
  }

  my $pattern = defined $args{within} ? $args{within} : '.' x @{$FS->{features}};
  my $str = $phon_descr->{table_structure}; 
  while (defined $str->{subtables}) {
    my $subtable;
    $subtable = 1 if !grep substr($_, $feature_indices{$str->{subtables}}, 1) eq '0', @$phones;
    $subtable = 0 if !grep substr($_, $feature_indices{$str->{subtables}}, 1) eq '1', @$phones;
    last unless defined $subtable;
    substr($pattern, $feature_indices{$str->{subtables}}, 1) = $subtable;
    $str = $str->{$subtable};
  }

  $phones = [map add_false_features($_, $str), @$phones];
  $inventory = [map add_false_features($_, $str), @$inventory];
  my %sortkey = map(($_ => table_sortkey($_, $phon_descr->{table_structure})), 
      grep $_, @$inventory);
  @$phones = sort {$sortkey{$a} cmp $sortkey{$b}} @$phones;
  @$inventory = sort {$sortkey{$a} cmp $sortkey{$b}} @$inventory;
  if ($args{sort_phones}) {
    @$orig_phones = sort {$sortkey{add_false_features($a, $str)} cmp $sortkey{add_false_features($b, $str)}} @$orig_phones;
  }

  my @comparanda = grep /^$pattern$/, @$inventory;
  my $str_pattern = $pattern;

  my %phones = map(($_ => 1), @$phones);
  my @complement = grep !defined $phones{$_}, @comparanda;
  my $cosize = @comparanda - @$phones;
  unless ($extend) {
    return name_natural_class($pattern, $inventory, str => $str, morpho => $morpho) if ($cosize == 0);
    return name_natural_class($pattern, $inventory, str => $str, morpho => $morpho) 
        . " other than $lb" . name_phone($complement[0]) . $rb if ($cosize == 1);
  }

  # Try to describe the set as a natural class, or failing that by taking out single-feature
  # natural classes.  Some natural classes that we should allow (e.g. 'stop') have more
  # than one feature.
  my (%usable_value, %usable_positively);
  for my $f (0..@{$FS->{features}}-1) {
    my $has0 = grep substr($_, $f, 1) eq '0', @$phones;
    my $has1 = grep substr($_, $f, 1) eq '1', @$phones;
    $usable_value{$f} = 0 if $has0 and !$has1 and grep substr($_, $f, 1) eq '1', @comparanda;
    $usable_value{$f} = 1 if $has1 and !$has0 and grep substr($_, $f, 1) eq '0', @comparanda;
    $usable_positively{$f} = 1 if !grep substr($_, $f, 1) eq 'u', @$phones;
  }
  my (@usable_extra_classes, @antiusable_extra_classes);
  for (@{$phon_descr->{extra_natural_classes}}) {
    my $extra = parse_feature_string($_, 1);
    push @usable_extra_classes, $extra if !grep !/^$extra$/, @$phones;
    push @antiusable_extra_classes, $extra if !grep /^$extra$/, @$phones;
  }

  my $complexity = 1;
  my @antipatterns;
  my $most_trimmed;
  while (1) {
    my ($best_pattern, $best_antipattern);
    $most_trimmed = 0;
    for my $extra (@usable_extra_classes) {
      next if $pattern =~ /^$extra$/;
      my $new_pattern = overwrite $pattern, $extra; 
      my $trimmed = @comparanda - grep /^$new_pattern$/, @comparanda;
      if ($trimmed > $most_trimmed) {
        ($most_trimmed, $best_pattern, $best_antipattern) = ($trimmed, $new_pattern, undef);
      }      
    }
    for my $extra (@antiusable_extra_classes) {
      next unless grep /^$extra$/, @comparanda;
      my $trimmed = @comparanda - grep !/^$extra$/, @comparanda;
      if ($trimmed > $most_trimmed) {
        ($most_trimmed, $best_pattern, $best_antipattern) = ($trimmed, undef, $extra);
      }      
    }
    for my $f (keys %usable_value) {
      next unless substr($pattern, $f, 1) eq '.';
      if ($usable_positively{$f}) {
        my $new_pattern = $pattern;
        substr($new_pattern, $f, 1) = $usable_value{$f};
        my $trimmed = @comparanda - grep /^$new_pattern$/, @comparanda;
        if ($trimmed > $most_trimmed) {
          ($most_trimmed, $best_pattern, $best_antipattern) = ($trimmed, $new_pattern, undef);
        }
      } 
      # No else; things usable positively are also usable negatively.
      {
        my $new_antipattern = '.' x @{$FS->{features}};
        substr($new_antipattern, $f, 1) = 1 - $usable_value{$f};
        my $trimmed = @comparanda - grep !/^$new_antipattern$/, @comparanda;
        if ($trimmed > $most_trimmed) {
          ($most_trimmed, $best_pattern, $best_antipattern) = ($trimmed, undef, $new_antipattern);
        }
      }
    }

    # Add features while they eliminate at least two phones that would otherwise be considered --
    # if we're talking just one phone, simpler just to name it than to pick a feature for it.
    last if ($most_trimmed < ($extend ? 1 : 2));
    if (defined $best_pattern) {
      $pattern = $best_pattern;
      @comparanda = grep /^$pattern$/, @comparanda;
    }
    if (defined $best_antipattern) {
      push @antipatterns, $best_antipattern;
      @comparanda = grep !/^$best_antipattern$/, @comparanda;
    }

    unless($extend) {
      last if scalar @comparanda <= scalar @$phones;

      $complexity++;
      last if ($size <= $complexity) or ($cosize <= $complexity);
    }
    # I think antipatterns should only be kept if they account for a non-silly proportion of things.
    # This isn't in yet.
  } 

  if ($extend) {
    my @l = grep {
          my $a = add_false_features($_, $str);
          my $f = 0;
          for my $ap (@antipatterns) {
            $f = 1, last if $a =~ /^$ap$/;
          }
          $f ? 0 : $a =~ /^$pattern$/;
        } @{$args{extend}};
    return @l;
  }

  # I don't know why this is wanted.  It used to be in the main loop but that screwed up extend.
  # For now it's voodoo.
  $pattern = add_requirements $pattern; 

  my @detritus = grep !defined $phones{$_}, @comparanda;
  $complexity += @detritus;

  # Trying to name the complement still might be helpful at this point.
  # But if I'm going to change that, I should also think about recognising groups
  # defined by ors of natural classes.  Etc.
  return ($morpho eq 'plural' ? '' : 'one of ') . 
      $lb . join(' ', map name_phone($_), @$phones) . $rb if ($size <= $complexity);
  return name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) . 
      (@complement ? (" other than $lb" .
      join(' ', map name_phone($_), @complement) . $rb) : '') if ($cosize <= $complexity);

  # If not suppressed,
  # list either the examples or the nonexamples, depending on which there are more of.
  my $list_examples = ($size <= $cosize);
  my @excluded_names = map name_natural_class($_, $inventory, str => $str, morpho => $morpho), @antipatterns;
  my $main_name = name_natural_class($pattern, $inventory, str => $str, morpho => $morpho);

  my (@base_caught, @antipatterns_caught);
  if (!$list_examples and !$args{suppress_ie}) {
    my @pool = @complement;
    @base_caught = grep !/^$pattern$/, @pool;
    @pool = grep /^$pattern$/, @pool;
    for my $i (0..$#antipatterns) {
      @{$antipatterns_caught[$i]} = grep /^$antipatterns[$i]$/, @pool;
      @pool = grep !/^$antipatterns[$i]$/, @pool;
      $excluded_names[$i] .= " ($lb" . join(' ', map name_phone($_), @{$antipatterns_caught[$i]}) . "$rb)";
    }
    if (@base_caught) {
      $main_name .= (@detritus or @antipatterns_caught) ? " (i.e. not $lb" : 
          '; i.e. ' . 
          name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) .
          " other than $lb";
      $main_name .= join(' ', map name_phone($_), @base_caught) . $rb;
      $main_name .= ')' if (@detritus or @antipatterns_caught);
    }
    die 'inconsistency in detritus in describe_set' if @pool != @detritus; 
  }

  my $result = $main_name;
  $result .= ' other than ' if @antipatterns or @detritus;
  if (@antipatterns) {
    $result .= (join ' or ', @excluded_names);
    $result .= ' or ' if @detritus;
  }
  $result .= $lb . join(' ', map name_phone($_), @detritus) . $rb if @detritus;
  if ($list_examples and !$args{suppress_ie}) {
    $result .= '; i.e. ' . ($morpho eq 'plural' ? '' : 'one of ') . 
        $lb . join(' ', map name_phone($_), @$phones) . $rb;
  } 
  $result;
}

# This returns a string and a list of strings.
# The first string is the syllable structure itself; 
# the remaining strings are one for each position, describing the further restrictions.
# By the time we call this, gen_syllable contains all the important information
# about the syllable structure, whereas some of what's in syllable_structure is inaccurate.
sub describe_syllable_structure {
  my ($pd, %args) = (shift, @_);
  my @template;
  my @elaborations;
  
  my (%label_uses, %first_pos);
  for my $pos (0..@{$pd->{syllable_structure}}-1) {
    my @phones = grep(($_ and $pd->{gen_inventory}{$_}[$pos]), keys %{$pd->{gen_inventory}});
    my $label;
    for my $slot (@{$phon_descr->{syllable_slots}}) {
      my $phone; 
      ($phone, $label) = split /: */, $slot;
      $phone = parse_feature_string($phone, 1);
      my $mismatch = 0;
      for (@phones) {
        $mismatch = 1 unless /^$phone$/;
      }
      last unless $mismatch;
    }
    # We assume that there's always a catchall in the list, so no special provision for no match.

    if (!defined $label_uses{$label}) {
      $label_uses{$label} = 0;
      $first_pos{$label} = $pos;
    }
    if (++$label_uses{$label} >= 2) {
      # if we will need subscripts after all, go back to put in the _1,
      # _unless_ there's no elaboration (in which case we changed the label)
      if ($label_uses{$label} == 2 and $elaborations[$first_pos{$label}]) {
        $template[$first_pos{$label}] .= ($args{html} ? '<sub>' : '') 
                . '1' 
                . ($args{html} ? '</sub>' : '') 
      }
      $label .= ($args{html} ? '<sub>' : '') 
              . $label_uses{$label} 
              . ($args{html} ? '</sub>' : '') 
    }

    $template[$pos] = $label;

    if (@phones == 1) {
      $template[$pos] = name_phone($phones[0]);
    } else {
      $elaborations[$pos] = describe_set(\@phones, [keys %{$pd->{gen_inventory}}]); 
    }
  }
  
  for (my $pos = $#template; $pos >= 0; $pos--) {
    if ($elaborations[$pos]) {
      $elaborations[$pos] = $template[$pos] . ': ' . $elaborations[$pos];
    } else {
      splice @elaborations, $pos, 1;
    }
    # mark nonrequiredness
    $template[$pos] = '(' . $template[$pos] . ')' if $pd->{gen_inventory}{''}[$pos]; 
  }

  (\@template, \@elaborations);
}


# We describe all the rules at once, since we need to run through keeping track of
# the inventory as we deal with each rule, and since we might wish to fold together 
# certain rule descriptions (depending on style: yes for synchronic presentations,
# no if they occurred apart for diachronic).

# TODO: this needs lots of updating when new rule types arise, for instance
# those that can have multiple effects.
sub describe_rules {
  my ($pd, %args) = (shift, @_);
  my %descriptions;
  my %to_be_numbered;

  # initial and final rules to present
  $args{start} = $pd->{start_sequences} unless defined $args{start};
  $args{end} = @{$pd->{phonology}} unless defined $args{end};

  my @inventory;
  my %phone_resolutions;
  my %resolution_expiries;
  if (defined @{$args{inventory}}) {
    @inventory = @{$args{inventory}};
  } elsif ($args{start} == $pd->{start_sequences}) {
    @inventory = keys %{$pd->{gen_inventory}};
  } else {
    # FIXME: else we have to run to update the inventory to the starting point
    print STDERR "unimplemented start time in describe_rules!\n"
  }
  @inventory = grep $_, @inventory;
  %phone_resolutions = (map(($_ => $_), @inventory));

  my %sortkey = map(($_ => table_sortkey($_, $phon_descr->{table_structure})), @inventory);

  RULE: for my $i ($args{start}..$args{end}-1) {
#print STDERR "rule $i\n"; # debug
    my $rule = $pd->{phonology}[$i];

    # For now, assume there's only one change.  
    if (keys %{$rule->{effects}} > 1) {
      $descriptions{$i}{rule} = '(some rule where multiple phones change at once)';
      next;
    }

    my ($locus, $effect) = each %{$rule->{effects}};
    my $old_effect = $effect;
    my $precondition = $rule->{precondition}{$locus};
    my ($pre, $old_pre, $post, $old_post, $far);
    $old_pre = $pre = $rule->{precondition}{$locus-1} if defined $rule->{precondition}{$locus-1};
    $old_post = $post = $rule->{precondition}{$locus+1} if defined $rule->{precondition}{$locus+1};
    $far = grep(($_ ne $locus-1 and $_ ne $locus and $_ ne $locus+1), keys %{$rule->{precondition}});
    # Try to simplify assimilations, taking advantage of reenrichments.
    for my $i (0..length($effect)-1) {
      substr($effect, $i, 1) = substr($pre, $i, 1)
        if substr($effect, $i, 1) eq '<' and substr($pre, $i, 1) ne '.';
      substr($effect, $i, 1) = substr($post, $i, 1)
        if substr($effect, $i, 1) eq '>' and substr($post, $i, 1) ne '.';
    }

    # TODO: put pointless persistent rules into some kind of holding tank,
    # to check on creation of new phones.
    # (Actually, quite a lot of things potentially need rewriting when new phones are around.  Ick.
    # But this case is enough for the noo.)
    my %matcheds;
    for my $displ (keys %{$rule->{precondition}}) {
      @{$matcheds{$displ}} = grep $_ =~ /^$rule->{precondition}{$displ}$/, @inventory;
      if (defined $rule->{except}{$displ}) {
        my @exceptions = split / /, $rule->{except}{$displ};
        for my $exception (@exceptions) {
          @{$matcheds{$displ}} = grep $_ !~ /^$exception$/, @{$matcheds{$displ}};
        }
      }
      unless (@{$matcheds{$displ}}) {
        $rule->{pointless} = 1;
        next RULE;
      }
    }
    # Drop the assimilatory parts of the rule if there aren't multiple values among the things being
    # assimilated to.
    # It would be at least as sensible to do this one assimilation at a time, in theory,
    # but that would throw off the naming.
    if ($effect =~ /</) {
      my $not_pointless = 0;
      my @indices = grep substr($effect, $_, 1) eq '<', 0..length($effect)-1;
      for my $j (@indices) {
        $not_pointless = 1, last if grep substr($_, $j, 1) eq '0', @{$matcheds{$locus-1}}
                                and grep substr($_, $j, 1) eq '1', @{$matcheds{$locus-1}};
      }
      unless ($not_pointless) {
        $effect =~ s/</./g;
      }
    }
    if ($effect =~ />/) {
      my $not_pointless = 0;
      my @indices = grep substr($effect, $_, 1) eq '>', 0..length($effect)-1;
      for my $j (@indices) {
        $not_pointless = 1, last if grep substr($_, $j, 1) eq '0', @{$matcheds{$locus+1}}
                                and grep substr($_, $j, 1) eq '1', @{$matcheds{$locus+1}};
      }
      unless ($not_pointless) {
        $effect =~ s/>/./g;
      }
    }
    # Drop rules that visibly do nothing, now.
    if ($precondition =~ /^$effect$/) {
      $rule->{pointless} = 1;
      next RULE;
    }

    # There should be no resolutions whose expiry is _strictly less than_ $i.
    if (defined $resolution_expiries{$i}) {
      delete $phone_resolutions{$_} for @{$resolution_expiries{$i}};
    }

    # The big one: get the new inventory.
    my @new_inventory = @inventory;
    my $entailed_effect = add_entailments($effect);
    my %outcome;
    my $pointless = 1;
    my %frames_examined;
    my @template_set;
    push @template_set, @{$matcheds{$locus-1}} if $entailed_effect =~ /</;
    push @template_set, @{$matcheds{$locus+1}} if $entailed_effect =~ />/;
    push @template_set, '.' x length($entailed_effect) unless @template_set;
    for my $template (@template_set) {
      my $frame = $entailed_effect;
      my $frame_is_worthwhile = 0;
      for (0..length($frame)-1) {
        # unlikely issue: not right for both directions at once.   and below
        substr($frame, $_, 1) = substr($template, $_, 1) if substr($frame, $_, 1) =~ /[<>]/;
      }
      next if defined $frames_examined{$frame};
      $frames_examined{$frame} = 1;
 
      for my $phone (@{$matcheds{$locus}}) {
        my $changed = add_entailments overwrite($phone, $frame);
        if (!defined $phone_resolutions{$changed}) {
          my $word = [$changed];
          my $expiry = [];
          run_phonology $word, $pd->{phonology}, 
              which_preconditions => $pd->{which_preconditions},
              cleanup => $i, 
              change_record => [change_record($phone, $changed)],
              track_expiry => $expiry,
              nopause => 1;
          $phone_resolutions{$changed} = join ' ', @$word;
          push @{$resolution_expiries{$expiry->[0]}}, $changed if $expiry->[0] < INF;
          push @new_inventory, @$word; 
          $sortkey{$_} = table_sortkey($_, $phon_descr->{table_structure}) for @$word;
        }
        my $outcome = $phone_resolutions{$changed};
        $pointless = 0 unless ($phone eq $outcome and $phone !~ /^$effect$/);
        $outcome{$frame}{$phone} = $outcome;
        $frame_is_worthwhile = 1 if $outcome ne $phone;
      }
      delete $outcome{$frame} unless $frame_is_worthwhile;
    }

    if ($pointless) {
      $rule->{pointless} = 1; 
      next RULE;
    }
    # If only one frame causes change, rewrite to a non-assimilatory rule.  
    # We need to fix the values of both effect and influencer.
    if (keys %outcome <= 1) {
      $rule->{pointless} = 1, next RULE if keys %outcome <= 0;
      if (keys %frames_examined > 1) {
        my($frame, $dummy) = each %outcome;
        for (0..length($effect)-1) {
          if (substr($effect, $_, 1) =~ /[<>]/) {
            substr($pre, $_, 1) = substr($frame, $_, 1) if substr($effect, $_, 1) eq '<';
            substr($post, $_, 1) = substr($frame, $_, 1) if substr($effect, $_, 1) eq '>';
            substr($effect, $_, 1) = substr($frame, $_, 1);
          }
        }
      }
    }

    # Exceptionality describing time!  We are comparing to add_entailments overwrite($phone, $frame).
    # Note that this doesn't have any particular handling of 
    # "foos do A, except for bar foos, which do B instead".

    # TODO: consolidate multiple frames.
    my @susceptible;
    my $insusceptibles_exist = 0;
    my %dev_distilled; # %dev_distilled maps frames to lists of (condition, phones) pairs
    my %any_nondeviates; # is there any phone which behaves normally?
    if (keys %outcome) {
      for my $frame (keys %outcome) {
        # %deviations maps deviations to the list of sounds that give them
        my %deviations;

        # Collect the deviations.
        for my $phone (@{$matcheds{$locus}}) {
          my $susceptible = 0;
          my $outcome = $outcome{$frame}{$phone};
          if ($outcome =~ / /) { 
            print STDERR "multiple sound outcome in finding deviations!\n"; # FIXME: multiple sound outcomes
          }
          my $changed = add_entailments overwrite($phone, $frame); # duplicative :-/
          if (length($outcome)) { # one phone
            for (0..length($outcome)-1) {
              substr($outcome, $_, 1) = '.' if substr($outcome, $_, 1) eq substr($changed, $_, 1);
            }
            push @{$deviations{$outcome}}, $phone;

            # Only announce the main clause of this rule if there's a nondeviate that actually changes.
            $any_nondeviates{$frame} = 1 if $outcome eq '.' x length($frame) 
                                        and $outcome{$frame}{$phone} ne $phone;
          } else { # no phones: deletion is a deviation
            push @{$deviations{''}}, $phone;              
          }

          $susceptible = 1, push @susceptible, $phone # can't jump out or we might miss exceptionality
              if $outcome{$frame}{$phone} ne $phone;
          $insusceptibles_exist = 1 unless $susceptible;
        } # phone

        # Distill the deviations.  
        #
        # Deviations form a partial order, where D > D' if D makes every change D' makes.
        # For D a deviation running smallest to largest, 
        # as a general rule we want to handle the whole up-set of D, if we can.
        # So we want to just name D within its down-set and transfer that naming to the up-set.
        my $any_deviations;
        do {
          $any_deviations = 0;
          for my $dev (sort {grep(1,($a =~ /[^.]/g)) <=> grep(1,($b =~ /[^.]/g))} keys %deviations) { # D
            next if $dev !~ /[^.]/ and length($dev);
            next unless @{$deviations{$dev}};
            $any_deviations = 1;
            
            my @downset;
            if (length($dev) > 0) { # deviation is not deletion
              for (keys %deviations) {
                push @downset, @{$deviations{$_}} if $dev =~ /^$_$/;
              }
            }
            else { # deviation is deletion
              @downset = @{$matcheds{$locus}};
            }

            my %extension = map(($_ => 1), 
                describe_set $deviations{$dev}, \@downset, extend => $matcheds{$locus});
            
            my @covered;
            for my $dev2 (keys %deviations) { # a member of the up-set
              if ($dev2 =~ /^$dev$/ or length($dev2) == 0) {
                push @covered, grep defined($extension{$_}), @{$deviations{$dev2}};
                if (length($dev2) > 0) { # this aspect of the deviation is handled; don't remark on it again
                  my $stripped_dev2 = $dev2;
                  for (0..length($dev2)-1) {
                    substr($stripped_dev2, $_, 1) = '.' if substr($dev2, $_, 1) eq substr($dev, $_, 1);
                  }
                  push @{$deviations{$stripped_dev2}}, grep defined($extension{$_}), @{$deviations{$dev2}};
                  @{$deviations{$dev2}} = grep !defined($extension{$_}), @{$deviations{$dev2}};
                }
              }
            } # dev2
            delete $deviations{''} if ($dev eq '');
            
            # Throw out distilled deviations which do nothing aside from fill in undefineds.
            my $only_undefineds = 0;
            if ($dev ne '') {
              $only_undefineds = 1;
              ONLY_UNDEF: for my $phone (@covered) {
                for (0..length($phone)-1) {
                  $only_undefineds = 0, last ONLY_UNDEF if substr($dev, $_, 1) =~ /[01]/ and substr($phone, $_, 1) ne 'u';
                }
              }
            }

            push @{$dev_distilled{$frame}}, [$dev, \@covered] unless $only_undefineds;
          } # dev
        } while ($any_deviations);

      } # frame
    }

    # Start to prepare the textual description.  First, what the change does.

    # Some of the features may appear redundant to list in the rule, given the current inventory.
    # But I leave them, just so that there isn't another thing to revise when persistence happens.
    my $simple_effect = $effect;
    $simple_effect =~ y/<>/../;
    my $modified = overwrite $precondition, $simple_effect; 

    my $text;
    if ($insusceptibles_exist) {
      $text = describe_set(\@susceptible, \@inventory, within => $precondition, 
          morpho => 'plural', suppress_ie => 1, etic => 1, sort_phones => 1);
      # maybe break off "other than" here and handle among the exception texts
    } else {
      $text = name_natural_class($precondition, \@inventory, morpho => 'plural');
    }
    my $subject_is_list = ($text =~ /^\[.*\]$/); # klugy
    my @exception_texts;
    if (defined $rule->{except}{$locus}) {
      my @exceptions = split / /, $rule->{except}{$locus};
      @exception_texts = map name_natural_class(overwrite($precondition, $_), 
          \@inventory, morpho => 'plural', significant => $_, no_nothing => 1), @exceptions;
      @exception_texts = grep $_, @exception_texts; # this is the first one
    }
    $text .= ' except for ' . join ' and ', @exception_texts if @exception_texts;
    # It is friendliest not to describe rules which survive till before the _next_ rule as persistent.
    $text .= ' persistently' unless (defined $rule->{inactive} and $rule->{inactive} <= $i + 1);

    my $main_VP = '';
    if ($effect =~ /[01]/) {
      #$main_VP .= ' and' if $main_VP; # this is the first one
      $main_VP .= ' become ';
      my $object;
      # if the subject is a list, make the object one too
      if ($subject_is_list and scalar keys %outcome <= 1) { 
        my ($frame) = keys %outcome; 
        $object = '[' . join(' ', map name_phone($outcome{$frame}{$_}), @susceptible) . ']';
      } else {
        $object = name_natural_class($modified, \@new_inventory, significant => $simple_effect, morpho => 'plural', nobase => 1);
        if ($object =~ / and /) {
          $object .= ', respectively,'; 
        }
      }
      $main_VP .= $object;
    }
    if ($effect =~ /</) {
      $_ = $effect;
      y/01<>/..1./;
      $_ = overwrite(str_part(reenrich($precondition,\@inventory)), $_);
      $main_VP .= ' and' if $main_VP;
      $main_VP .= ' assimilate in ' .
          name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
      if (!defined($old_post) and !$far) {
        $main_VP .= ' to a preceding ' . 
            name_natural_class($pre, \@inventory, morpho => 'bare');
        $pre = undef;
      } else {
        $main_VP .= ' to the previous phone';
      }
      if ($rule->{or_pause}{$locus+1}) { # word-initial
        my $pausal_effect = $effect;
        for (0..length($pausal_effect)-1) {
          substr($pausal_effect, $_, 1) = substr($rule->{pause_phone}, $_, 1)
              if substr($pausal_effect, $_, 1) eq '<';
        }
        my $modified = overwrite $precondition, $pausal_effect; 
        $main_VP .= ' and become ' . 
                    name_natural_class($modified, \@new_inventory, significant => $pausal_effect, morpho => 'plural', nobase => 1) .
                    ' word-initially';
      }
    }
    if ($effect =~ />/) {
      $_ = $effect;
      y/01<>/...1/;
      $_ = overwrite(str_part(reenrich($precondition,\@inventory)), $_);
      $main_VP .= ' and' if $main_VP;
      $main_VP .= ' assimilate in ' .
          name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
      if (!defined($old_pre) and !$far) {
        $main_VP .= ' to a following ' . 
            name_natural_class($post, \@inventory, morpho => 'bare');
        $post = undef;
      } else {
        $main_VP .= ' to the next phone';
      }
      if ($rule->{or_pause}{$locus+1}) { # word-final
        my $pausal_effect = $effect;
        for (0..length($pausal_effect)-1) {
          substr($pausal_effect, $_, 1) = substr($rule->{pause_phone}, $_, 1)
              if substr($pausal_effect, $_, 1) eq '>';
        }
        my $modified = overwrite $precondition, $pausal_effect; 
        $main_VP .= ' and become ' . 
                    name_natural_class($modified, \@new_inventory, significant => $pausal_effect, morpho => 'plural', nobase => 1) .
                    ' word-finally';
      }
    }

    # FIXME: for unknown reasons the [lz] > [ll] type rule is being misdescribed as if not assimilatory.

    # Things TODO: 
    # - Implement a non-heavy behaviour wrt lists of sounds.  
    #     I think a method with three, four examples ... chosen for their salience is good.  
    # - Do better with the verb.  Include exceptions in this.
    # - Describe deletions.
    # - Understand syllable position.
    # - The inventory should shrink when changes are unconditioned.  (Actually, branch off inventory tracking.)
    # - Persistent rules might need their statements recast when the inventory enlarges,
    #   given the above.  But I'll probably ignore this.

    $text .= $main_VP;
    my ($pre_text, $post_text);
    if (defined $pre) {
      $pre_text = name_natural_class($pre, \@inventory, morpho => 'indef');
      $pre_text .= ' or word-initially' if $rule->{or_pause}{$locus-1};
      my @exceptions = split / /, $rule->{except}{$locus-1};
      my @exception_texts = map name_natural_class(overwrite($precondition, $_), 
          \@inventory, morpho => 'plural', significant => $_, no_nothing => 1), @exceptions;
      @exception_texts = grep $_, @exception_texts;
      $text .= ' except for ' . join ' and ', @exception_texts if @exception_texts;
    }
    if (defined $post) {
      $post_text = name_natural_class($post, \@inventory, morpho => 'indef');
      $post_text .= ' or word-finally' if $rule->{or_pause}{$locus+1};
      my @exceptions = split / /, $rule->{except}{$locus+1};
      my @exception_texts = map name_natural_class(overwrite($precondition, $_), 
          \@inventory, morpho => 'plural', significant => $_, no_nothing => 1), @exceptions;
      @exception_texts = grep $_, @exception_texts;
      $text .= ' except for ' . join ' and ', @exception_texts if @exception_texts;
    }
    if (defined $pre and defined $post) {
      $text .= " between $pre_text and $post_text";
    } elsif (defined $pre) {
      $text .= " after $pre_text";
    } elsif (defined $post) {
      $text .= " before $post_text";
    }
    if ($far) {
      $text .= ' under some conditions on nonadjacent phones'; # FIXME
    }
    # Again, rules which survive one rule shouldn't be described as persistent.
    if (defined $rule->{inactive} and $rule->{inactive} > $i + 1) {
      $to_be_numbered{$i} = 1;
      push @{$descriptions{$rule->{inactive}}{pre}}, "rule ($i) becomes inactive";
    }

    # Describe the deviations.
    for my $frame (keys %dev_distilled) {
      # really this 'before' / 'after' should I guess remention pause, so as not to suggest pause is special-cased
      my $frame_text = '';
      if ($effect =~ /[<>]/) {
        $frame_text = $effect !~ />/ ? 'After ' : 
                        ($effect !~ /</ ? 'Before ' : 'Assimilating to ');
        $frame_text .= name_natural_class(overwrite(($effect !~ />/ ? $old_pre : 
                        ($effect !~ /</ ? $old_post : '.' x @{$FS->{features}})), $frame), 
            \@inventory, morpho => 'indef', bar_nons => 1) . ', '; # disallowing nons isn't right, but it makes the thing readable
      }

      my $keep_frame = 0;
      for (@{$dev_distilled{$frame}}) {
        my ($deviation, $all_deviants) = @$_;
        next unless my @deviants = grep $outcome{$frame}{$_} ne $_, @$all_deviants;
#        $frame_text .= '[' . join(' ', map(($_ . ':' . $outcome{$frame}{$_} . '<>' . add_entailments overwrite($_, $frame)), @deviants)) . ']'; # debugdebug
        my $subject = describe_set(\@deviants, \@inventory, within => $precondition, 
            morpho => 'plural', suppress_ie => 1, etic => 1, sort_phones => 1);
        $frame_text .= $subject;
        $keep_frame = 1;
        if (length($deviation) > 0) {
          $frame_text .= ' become ';
          # if the subject is a list, make the object one too
          if ($subject =~ /^\[.*\]$/) { # klugy
            $frame_text .= '[' . join(' ', map name_phone($outcome{$frame}{$_}), @deviants) . ']';
          } else {
            $_ = name_natural_class(overwrite(overwrite($precondition, $frame), $deviation), 
                \@new_inventory,
                significant => $deviation, morpho => 'plural', nobase => 1, bar_nons => 1); 
            $frame_text .= ($_ ? $_ : "GD".feature_string(overwrite(overwrite($precondition, $frame), $deviation)));
          }
        }
        else {
          $frame_text .= ' are deleted';
        }
        $frame_text .= '; ';
      }
      $frame_text = substr($frame_text, 0, -2) if $frame_text =~ /; $/; # eh
      
      if ($keep_frame) {
        $text .= ". " . ucfirst $frame_text;
        $text .= ' (all deviants!)' unless $any_nondeviates{$frame}; # temporary!!
      }
    }
    
# TEMPORARY!!! 
    my $table = '';
    if (1) {
      if ($use_html) {
        $table = "Cases:\n<table>";
        my @frames = keys %outcome;
        my $one_frame = 0;
        if (@frames <= 1) {
          $one_frame = 1;
          unshift @frames, '';
        }
        for my $frame (@frames) {
          # Heuristically, use nons if there are only two categories (these are probably foo
          # vs. nonfoo) but not with three or more (which is probably something like place assimilation).
          $table .= '<tr><th align="right">' . 
              ($frame ? ($one_frame ? 'Outcome' : 
                  ucfirst name_natural_class(overwrite($precondition, $frame), 
                  \@new_inventory, significant => $frame, nobase => 1, bar_nons => (@frames >= 3)))
                  : 'Original') . 
              '</th>';
          my %exceptional = map(($_ => 1), @{$matcheds{$locus}});
          my @exceptional = sort {$sortkey{$a} cmp $sortkey{$b}} keys %exceptional;
          my %done;
          for my $phone (@exceptional) {
            # don't make the exact same column twice.  fortuitously this matches up across columns
            my $nonassim = $phone;
            for (0..length($effect)-1) {
              substr($nonassim, $_, 1) = '.' if substr($effect, $_, 1) =~ /[<>]/;
            }
            next if $done{$nonassim};
            $done{$nonassim} = 1;
            
            my $changed = $frame ? $outcome{$frame}{$phone} : $phone;
            $table .= '<td'; 
            $table .= '>' . spell_out([split / /, $changed], null => 1) . '</td>'; 
          }
          $table .= '</tr>'; 
        }
        $table .= '</table>';
      }
    }


    $descriptions{$i}{rule} = ucfirst $text . '. ' . $table;

    my %new_inventory = map(($_ => 1), @new_inventory);
    @inventory = keys %new_inventory;
  }

  for my $i (keys %to_be_numbered) {
    $descriptions{$i}{rule} = "($i) $descriptions{$i}{rule}" if (defined $descriptions{$i}{rule});
  }
  
  my @final_list;
  for (sort keys %descriptions) {
    push @final_list, @{$descriptions{$_}{pre}} if defined $descriptions{$_}{pre};
    push @final_list, $descriptions{$_}{rule} if defined $descriptions{$_}{rule};
    push @final_list, @{$descriptions{$_}{post}} if defined $descriptions{$_}{post};
  }
  \@final_list;
}



sub generate_form {
  my ($target_entropy, $pd) = (shift, shift);

  my $normal = 0;
  $normal += rand(1/4.0) for 1..8; # std dev sqrt(2/3) eh
  my $entropy = $normal * $target_entropy; 

  # This was the old way, which would now have problems with the 'U' value
  # and the multiple probabilities on generator rules, and presumably other things.
  #
  #   my @syl;
  #   @syl = (@syl, generate_syllable $syllable_structure) for (1..$num_syllables);
  #   run_phonology \@syl, $phone_generator,generator => 1;
  #   run_phonology \@syl, $phonology, which_preconditions => (whatever);
  #
  # But the below is more direct.
  
  my @form;
  # The form of this loop will very much be changing when we start asking for
  # forms that aren't made of whole syllables.
  my $total_entropy = 0;
  while ($total_entropy < $entropy) {
    for my $i (0..@{$pd->{syllable_structure}}-1) {
      # next if $pd->{syllable_structure}[$i]{nonzero_prob} == 0; # these cases are eliminated.

      $total_entropy += $pd->{syllable_structure}[$i]{entropy};
      next if rand() >= $pd->{syllable_structure}[$i]{prob};

      my $rand = rand (1 - $pd->{gen_inventory}{''}[$i]);
      my $selected_phone;
      # only generate structural zeroes in a form, not resolvent zeroes 
      # (though we've corrected the probabilities anyhow)
      for (keys %{$pd->{gen_inventory}}) {
        $selected_phone = $_, last if $_ ne '' and ($rand -= $pd->{gen_inventory}{$_}[$i]) < 0;
      }
      push @form, split / /, $selected_phone;
    }
  }

  \@form;
}

# Simplify the phonemic presentation of a form.
# E.g. if we generate /agsa/ but there's compulsory regressive voice assimilation in that situation,
# and /aksa/ consists of extant phonemes and has the same outcome, we may as well present it as /aksa/.
# Returns (outcome, canonicalised); we may as well, since we end up with both.

# TODO: when morphology gets here, respect it.  Also new types of change?

sub canonicalise_phonemic_form {
  my ($word, $pd) = (shift, shift);
  my @canonical_word = @$word; 
  my @current_word = @$word;
  my @sources = 0..@$word-1;
  
  for my $k ($pd->{start_sequences}..@{$pd->{phonology}}-1) {
#    print "before $k /" . spell_out(\@canonical_word) . "/ [" . spell_out(\@current_word) . "]\n"; # debug
    my @old_sources = @sources;
    my @old_word = @current_word;
    my (@prov_canonical_word, @prov_current_word);
    my $changed;
    run_phonology \@current_word, $pd->{phonology}, 
        which_preconditions => $pd->{which_preconditions},
        start => $k,
        end => $k+1,
        sources => \@sources;
#    print "target [" . spell_out(\@current_word) . "]\n"; # debug
    
    { # block for redo
      $changed = 0;

      # check for new deletions
      for my $source (@old_sources) {
        unless (grep $_ == $source, @sources) {
          @prov_canonical_word = @canonical_word;
          splice @prov_canonical_word, $source, 1;
          @prov_current_word = @prov_canonical_word;
          run_phonology \@prov_current_word, $pd->{phonology}, 
              which_preconditions => $pd->{which_preconditions},
              start => $pd->{start_sequences},
              end => $k+1;
          if (scalar @prov_current_word == scalar @current_word and
              !grep $prov_current_word[$_] != $current_word[$_], 0..$#current_word) {
            $changed = 1;
            @canonical_word = @prov_canonical_word;
            @sources = map $_ > $source ? $_-1 : $_, @sources;
          }
        }
      } # deletions
      
      # check for featural changes.
      # Try out every underlying phoneme that is Hamming-between this phone in the old and the new words.
      # (Maybe special-case place, i.e.\ features that are bound, for e.g. /mk/ vs. /nk/ when there's no /N/.)
      # What about syllable position restrictions?  I think it's more conventional to ignore them.
      CHANGE_SOURCE: for my $i (0..$#sources) {
        next if $sources[$i] == -1; # insertions
        my $old_i;
        for (0..$#old_sources) {
          $old_i = $_, last if $old_sources[$_] == $sources[$i];
        }
        # no need to continue unless there was a change in this sound
        next if $old_word[$old_i] eq $current_word[$i]; 
        
        my @varying_features = grep substr($canonical_word[$sources[$i]], $_, 1) ne substr($current_word[$i], $_, 1), 
                                    0..length($current_word[$i])-1;
#        print "@varying_features vary\n" if @varying_features; # debug
        my @prov_varied_features = @varying_features;
        while (@prov_varied_features) {
          @prov_canonical_word = @canonical_word;
          substr($prov_canonical_word[$sources[$i]], $_, 1) = substr($current_word[$i], $_, 1) 
              for @prov_varied_features;
          if (defined $pd->{gen_inventory}{$prov_canonical_word[$sources[$i]]}) {
#          print "trying out " . name_phone($prov_canonical_word[$sources[$i]]) . " at $i\n"; # debug
            @prov_current_word = @prov_canonical_word;
            run_phonology \@prov_current_word, $pd->{phonology}, 
                which_preconditions => $pd->{which_preconditions},
                start => $pd->{start_sequences},
                end => $k+1;
            if (scalar @prov_current_word == scalar @current_word and
                !grep $prov_current_word[$_] != $current_word[$_], 0..$#current_word) {
              $changed = 1;
              @canonical_word = @prov_canonical_word;
              next CHANGE_SOURCE;
            }
          }
          
          # loop increment
          my $was_last = pop @prov_varied_features;
          my $t;
          for ($t = -1; $varying_features[$t] != $was_last; $t--) { }
          push @prov_varied_features, @varying_features[$t+1..-1] if $t < -1; # stupid negatives convention
        } # prov_varied_features
      }
      
      # In order not to miss cases of form /0a 0b 1a 1b/ [1b 1b 1a 1b], we need to iterate
      # the description simplification, *not* the sound changes.
      redo if $changed;
    } 
  }
  (\@current_word, \@canonical_word);
}




my $seed =  time ^ $$ ^ $$<<15; 
my $outfile;
my $humane_output;
my $infile;
my $show_inventory;
my $show_all;
my $num_words = 0;
my $phone_to_interpret;
my $canonicalise;

sub die_with_usage {
  print STDERR <<USAGE;
$credits

Usage: $0 [options]

-o <filename>   Phonology output file.  Defaults to no output.  The output is a 
                YAML-formatted collection of the data needed to run the 
                phonology generator.  It's not the human-readable form;
                that comes on standard output.
-O <filename>   As above, with a little extra annotation for 
                readability, like translations of the internal phone notation.
-i <filename>   Input the phonology from the named file, rather than generating
                a new one.
-I              Produce a segmental inventory, with frequencies of appearance
                in each syllable position.
-d              Produce English descriptions of the phonology's rules, etc.
-w N            Generate N random words.
-c              When generating random words, also compute canonical phonemic 
                representations, which don't require unnecessary rules.
-h              Use HTML.
-p <string>     Do some conversions between phone formats.  Do nothing else.
-r N            Use N as the random seed.
-v              Verbose.  Show progress and a few other things.
-D              Debug.

USAGE
  exit 1;
}

sub parse_args {
  my $arg;
  while ($arg = shift) {
    if ($arg eq '-D') {
      $debug++;
    }
    elsif ($arg eq '-r') {
      $seed = shift;
      die "-r expects an integer argument\n" if !defined $seed or ($seed !~ /^\-?[0-9]+$/);
    }
    elsif ($arg =~ /^-[oO]$/) {
      $outfile = shift;
      $humane_output = 1 if $arg eq '-O';
      die "$arg expects a filename argument\n" if !defined $outfile;
    }
    elsif ($arg eq '-i') {
      $infile = shift;
      die "-i expects a filename argument\n" if !defined $infile;
    }
    elsif ($arg eq '-I') {
      $show_inventory = 1;
    }
    elsif ($arg eq '-d') {
      $show_inventory = $show_all = 1;
    }
    elsif ($arg eq '-v') {
      $verbose = 1;
    }
    elsif ($arg eq '-w') {
      $num_words = shift;
      die "-w expects an integer argument\n" if !defined $num_words or ($num_words !~ /^\-?[0-9]+$/);
    }
    elsif ($arg eq '-p') {
      $phone_to_interpret = shift;
      die "-p expects an argument\n" if !defined $phone_to_interpret;
    }
    elsif ($arg eq '-c') {
      $canonicalise = 1;
    }
    elsif ($arg eq '-h') {
      $use_html = 1;
    }
    else {
      die_with_usage;
    }
  }
}

my $yamlimpl = YAML::Any->implementation;
if ($yamlimpl ne 'YAML::XS' and $yamlimpl ne 'YAML::Syck') {
  print STDERR <<END;
Warning: your YAML implementation might not like this.
YAML::Syck works, and I expect YAML::XS too.
END
}

if ($0 =~ /\.cgi$/) {
  $CGI = CGI->new;

  $use_html = 1;
  # lazily not taking genuine query arguments for now
  $show_inventory = $show_all = 1;
  $num_words = 30;

  print $CGI->header;
}

parse_args @ARGV;

if ($use_html) {
  my $style = <<END; # put everything in an IPAish font for now
* {
  font-family: Gentium, 'Doulos SIL', 'Lucida Grande', serif;
}
END
  print CGI::start_html(-title => 'Random phonology', 
                         -style => {-code => $style});
} 
else {
  $" = $, = ", ";
}
# David wanted a link to an IPA chart, but I hope that a humane rule description
# would render that unnecessary.  We'll see.

$FS = YAML::Any::LoadFile('features.yml');

$feature_indices{$FS->{features}[$_]{name}} = $_ for (0..@{$FS->{features}}-1);
for my $i (0..@{$FS->{features}}-1) {
  if (defined $FS->{features}[$i]{requires}) {
    my $s = parse_feature_string($FS->{features}[$i]{requires});
    for (0..length($s)-1) {
      push @{$features_requiring[substr($s, $_, 1)][$_]}, $i if (substr($s, $_, 1) =~ /[01]/);
    }
  }
}
for my $str (@{$FS->{strippings}}) {
  $str->{condition_parsed} = parse_feature_string($str->{condition}, 1);
}
my @otherway_relations;
for my $rel (@{$FS->{relations}}) {
  if (defined $rel->{twoway}) {
    my %flipped = %$rel;
    $flipped{from} = $rel->{to};
    $flipped{to} = $rel->{from};
    delete $flipped{assimilation};
    $flipped{assimilation} = $rel->{otherway_assimilation} if defined $rel->{otherway_assimilation};
    push @otherway_relations, \%flipped;
  }
  delete $rel->{otherway_assimilation};
}
push @{$FS->{relations}}, @otherway_relations;

$phonetic_alphabets{CXS} = YAML::Any::LoadFile('CXS.yml') if -f 'CXS.yml';
$phonetic_alphabets{IPA} = YAML::Any::LoadFile('IPA_HTML.yml') if -f 'IPA_HTML.yml';
for my $alphabet (values %phonetic_alphabets) {
  for my $type (qw/characters ligations/) {
    for my $c (keys %{$alphabet->{$type}}) {
      $alphabet->{$type}{parse_feature_string $c, 1} = $alphabet->{$type}{$c};
      delete $alphabet->{$type}{$c};
    }
  }
  for my $c (keys %{$alphabet->{modifiers}}) {
    my @fs = split / /, $c;
    my $s = parse_feature_string($c, 1) . ' ' . parse_feature_string($fs[0], 1);
    $alphabet->{modifiers}{$s} = $alphabet->{modifiers}{$c};
    delete $alphabet->{modifiers}{$c};
  }
}

if (defined $phone_to_interpret) {
  $phone_to_interpret = parse_feature_string($phone_to_interpret) unless $phone_to_interpret =~ /^[.01u]*$/;
  print '[' . name_phone($phone_to_interpret) . '] ' . feature_string($phone_to_interpret);
  $phone_to_interpret =~ /[01]/g;
  print '   ' . (pos($phone_to_interpret) - 1) if defined pos($phone_to_interpret);
  print "\n";
  exit 0;
}

print STDERR "seed $seed\n" if $verbose; 
srand $seed; 

my $phonology_data;

if (defined $infile) {
  $phonology_data = YAML::Any::LoadFile($infile);
} 

else {
  print STDERR "generating phonology...\n" if $verbose;
  my ($start_sequences, $syllable_structure, $phone_generator, $phonology) = gen_phonology;
  $phonology_data = {
    syllable_structure => $syllable_structure,
    phone_generator => $phone_generator, 
    phonology => $phonology,
    which_preconditions => which_preconditions($phonology),
    start_sequences => $start_sequences,
  };
  print STDERR "computing inventory...\n" if $verbose;
  $phonology_data->{gen_inventory} = inventory $phonology_data; # base inventory for generation
  delete $phonology_data->{phone_generator}; # now this is needless
  if ($debug < 1) {
    trim_inactive $phonology_data; 
  } else {
    print STDERR "pruning of inactive rules skipped\n";
  }
  $phonology_data->{which_preconditions} = which_preconditions($phonology) # since the numbers are changed
}

$phon_descr = YAML::Any::LoadFile('phon_descr.yml');

if ($show_inventory) {
  if ($use_html) {
    print CGI::h2('Phonemic inventory'),
          tabulate $phonology_data;
  } 
  
  else {
    my %things_named;
    print "phonemic inventory:\n"; 
    for my $p (sort keys %{$phonology_data->{gen_inventory}}) { 
      my $n = join '', map name_phone($_), split / /, $p;
      print "/" . ($n !~ /\#\#/ ? $n : feature_string($p)) . "/\t@{$phonology_data->{gen_inventory}{$p}}\n";
      push @{$things_named{$n}}, $p if ($n !~ /\#\#/);
    }
    for my $name (keys %things_named) { # left in in case it crops up
      if (@{$things_named{$name}} >= 2) {
        print "duplicate [$name]\n";
        print "$_\n" for @{$things_named{$name}};
      }
    }
    # subtract 1 for the empty phone
    print "that's " . ((keys %{$phonology_data->{gen_inventory}}) - 1) . " phonemes\n";
  }
}

if (defined $outfile) {
  if (defined $humane_output) {
    for my $rule (@{$phonology_data->{phone_generator}}, @{$phonology_data->{phonology}}) {
      for my $displ (keys %{$rule->{precondition}}) {
        $rule->{precondition_humane}{$displ} = feature_string $rule->{precondition}{$displ}, 1;
      }
      for my $displ (keys %{$rule->{effects}}) {
        $rule->{effects_humane}{$displ} = feature_string $rule->{effects}{$displ}, 1;
      }
    }
    for my $rule (@{$phonology_data->{syllable_structure}}) {
        $rule->{features_humane} = feature_string $rule->{features};
    }
    $phonology_data->{phonology}[$_]{number} = $_ for 0..@{$phonology_data->{phonology}}-1;
  }
  YAML::Any::DumpFile($outfile, $phonology_data);
}

if ($show_all) {
  tabulate $phonology_data, annotate_only => 1;
  my ($template, $elaborations) = describe_syllable_structure $phonology_data, html => $use_html;
  if ($use_html) { 
    print CGI::h2('Syllable structure'),
          CGI::p(join '', @$template),
          CGI::p(join '<br />', @$elaborations);
  } else {
    print "\nSyllable structure: " . join('', @$template) . "\n"; # not well formatted at present, eh
    print join "\n", @$elaborations;
    print "\n\n";
  }

  my $rules = describe_rules $phonology_data;
  if ($use_html) {
    print CGI::h2('Allophony'),
          CGI::ul(CGI::li($rules));
  } else {
    print "Allophony:\n";
    print join "\n", @$rules;
    print "\n\n";
  }
}

if ($use_html and $num_words > 0) {
  print CGI::h2('Some words'),
        CGI::start_table();
}

for (1..$num_words) {
  my $word = generate_form 12, $phonology_data; # magic entropy value
  my $surface_word;
  my $generated_word = [@$word];
  if (defined $canonicalise) {
    ($surface_word, $word) = canonicalise_phonemic_form $generated_word, $phonology_data;
  } else {
    $surface_word = [@$word];
    run_phonology $surface_word, $phonology_data->{phonology}, 
        which_preconditions => $phonology_data->{which_preconditions},
        start => $phonology_data->{start_sequences}; 
  }

  if ($use_html) {
    print '<tr>';
    print "<td>//", 
          spell_out($generated_word),
          "//</td>" 
        if defined $canonicalise;
    print "<td>/", 
          spell_out($word), 
          "/</td><td>[",
          spell_out($surface_word),
          "]</td></tr>\n";
  } else {
    print "//" . spell_out($generated_word). "//\t" if defined $canonicalise;
    print "/" . spell_out($word) . "/\t[" . spell_out($surface_word) . "]\n";
    for my $phone (@$surface_word) {
      $_ = name_phone($phone);
      print feature_string($phone), "\n" if /\#\#/;
    }
  }
}

if ($use_html and $num_words > 0) {
  print CGI::end_table();
}

if ($use_html) {
  print CGI::p({-style => 'font-size: small;'},
                "Generated by <a href=\"http://000024.org/conlang/random-language.html#gleb\">Gleb</a>",
                "version $version / $FS->{version} ",
                $infile ? "from the file $infile." : "with seed $seed.");
  print CGI::end_html;
}




