#!/usr/bin/perl
# Generate random phonologies, featurally and via ordered rules with persistence,
# with allophony and the potential for good morphophonology and the works.  (Getting there!)
# Alex Fink, January 2010 -- present.
# Thanks to Marcus Smith <http://smithma.bol.ucla.edu/> for unwitting inspiration,
# and Marcus and UPSID for being proximal sources for various numbers.
# (A greater proportion of the numbers are wholly fabricated, though!)

# Proximal plan for cleanliness:
# - Refactoring: make some classes, perhaps do some encapsulation, split files out.
# Short-term plan for features:
# - Our new general sequence rules yield a framework for general constraints against sequences 
#   (e.g. trapped resonant, /tl/, increasing sonority sequences in V_V, ...)
# - Presence of certain contrasts influencing chances of certain assimilations.
#   (I'm thinking of resonant voice assimilation, and V frontness assim to C.)
#   (also markednesses?  e.g. /b_< b_<_k/ shd be collapsed, and often /v\ w/.
#    can kluge that with a change in one direction though)
# - Implement some of the assimilations we already have code support for.
# - Do something to get rid of syllable structures where the coda can have two resonants 
#   but never an obstruent (3327079296 presently).
# - Coronals shouldn't front vowels, nor labials round them, etc., unless more obvious sources do too.
# - We can simplify ``a phone or word-initially''.
# > 0.3.1.  
# - For surface filters that redistribute a phoneme, we need rules which
#   bar running of earlier persistent rules after first passing them in applying sound changes.
#   This way, the second always runs immediately after the first.
#   Other than this, these clusters of rules need no unusual properties
#   (e.g. they can be pushed back normally.)
#
#   After that, should we privilege 
# (a) advanced inventory tracking, with the bigram transition matrix stuff; or
# (b) new phonology?  (long-distance rules; syllable tracking > moraic stuff)

use strict;
use YAML::Any;
use CGI;
use constant INF => 9**9**9; # is there really nothing sensible better?
use constant TWOPI => 6.28318530717958647688;

my $version = '0.3.0b';
my $credits = 'Gleb, a phonology generator, by Alex Fink' . 
              (' ' x (29 - length($version))) . # for a total length of 78
              "version $version";

# Phones are specified as strings, with one character for each feature in the order
# in which they occur in the feature definition file.  The principal values of characters:
# '.' is unspecified, 'u' undefined, '0' off, '1' on.
# Only [u01] appear in actual phones; '.' appears in classes (where it matches everything)
# and sound change outcomes and the like.

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
# TODO: this likely needs generalisation for e.g. tone.

my $FS; # what features there are, what they do
my $phon_descr; # how to describe the features.  the thing that would need to be localised
my %phonetic_alphabets;

my $verbose;
my $debug = 0;
my $debug2 = 0; # for the use of tracking things down with temporary diagnostic prints.  will leave for now
my $use_html;
my $CGI;
my $seed =  time ^ $$ ^ $$<<15; 

# Choose randomly from a hash giving weight distribution.  (Not called everywhere it might be, yet.)
sub weighted_one_of {
  my $sum = 0; 
  $sum += $_[2*$_+1] for 0..@_/2-1;
  $sum = rand $sum;
  while (@_) { 
    my ($a, $b) = (shift, shift);
    return $a if ($sum -= $b) < 0;
  }
}

package Phonology;

# Go from a prototypical prob to an actual one.  Now twice as gentle!
sub fuzz {
  my $p = shift;
  return 0 if $p <= 0;
  return 1 if $p >= 1;
  my $q = rand($p / (1 - $p)) + rand($p / (1 - $p));
  return $q / ($q + rand(1) + rand(1));
}

package main;

# Box-Muller.  I wonder whether this is faster than sum of 12 uniforms.
sub std_normal {
  return sqrt(-2*log rand(1)) * cos(rand(TWOPI));
}

package FeatureSystem;
# TODO: (proximal, with OOifying) put a reference to the feature system in the phonology.  
# But be certain not to include it in phonologies saved to files!  That would inflate them ridiculously.

# Uses dots for unspecified values, unless $args{undefined} is true when it uses 'u'.
sub parse {
  my ($self, $fs, %args) = (shift, shift, @_);
  my $re = !defined $args{undefined};
  my $phone = ($re ? '.' : 'u') x @{$self->{features}};
  return $phone if !defined $fs;
  my @a = split / /, $fs;
  for my $f (@a) {
    if ($f =~ /^([^\w\s])(.*)/) {
      substr($phone, $self->{feature_index}{$2}, 1) = $1 eq '+' ? '1' : $1 eq '-' ? '0' : $1; 
    } else {
      substr($phone, $self->{feature_index}{$f}, 1) = '1';
    }
  }
  $phone;
}

# Called with two args, displays undef things.
sub feature_string {
  my $self = shift;
  my $phone = shift; 
  my $fs = '';
  my $c;
  for my $i (0..(length $phone)-1) {
    $fs .= ($fs ? ' ' : '') . 
           ($c eq '1' ? (defined $self->{features}[$i]{univalent} ? '' : '+') : 
           ($c eq 'u' ? '?' : ($c eq '0' ? '-' : $c))) . 
           $self->{features}[$i]{name} 
        unless ($c = substr($phone, $i, 1)) eq '.' or ($c eq 'u' and !@_);
  }
  $fs;
}

sub load_file {
  my $filename = shift;
  my $FS = YAML::Any::LoadFile($filename);
  bless $FS;

  $FS->{feature_index}{$FS->{features}[$_]{name}} = $_ for (0..@{$FS->{features}}-1);
  # {features_requiring}[v][f] is the list of features which are only defined if feature f takes value v
  for my $i (0..@{$FS->{features}}-1) {
    if (defined $FS->{features}[$i]{requires}) {
      my $s = $FS->parse($FS->{features}[$i]{requires}, undefined => 1);
      for (0..length($s)-1) {
        push @{$FS->{features_requiring}[substr($s, $_, 1)][$_]}, $i if (substr($s, $_, 1) =~ /[01]/);
      }
    }
  }
  for my $str (@{$FS->{strippings}}) {
    $str->{condition_parsed} = $FS->parse($str->{condition}, 1);
  }
  my @otherway_relations;
  for my $rel (@{$FS->{relations}}) {
    if (defined $rel->{twoway}) {
      my %flipped = %$rel;
      $flipped{from} = $rel->{to};
      $flipped{to} = $rel->{from};
      push @otherway_relations, \%flipped;
    }
  }
  push @{$FS->{relations}}, @otherway_relations;
  
  return $FS;
}

# Phones ought to be objects themselves (then they could carry around their excepts and tiers &c), 
# but as they're just strings right now this will be a large rewrite.  
# So for now operations on phones, like the four below, will live in the feature system. 

# Takes a phone and a phone with dots.  Replaces features in the first with non-dots in the second.
sub overwrite {
  my ($self, $a, $b) = @_;
  for my $i (0..(length $b)-1) {
    substr($a, $i, 1) = substr($b, $i, 1) if substr($b, $i, 1) ne '.';
  }
  $a;
}

sub compatible {
  my ($self, $a, $b) = @_;
  for my $i (0..(length $b)-1) {
    return undef unless substr($a, $i, 1) eq '.' or 
                        substr($b, $i, 1) eq '.' or
                        substr($a, $i, 1) eq substr($b, $i, 1);
  }
  return 1;
}

sub add_requirements {
  my ($self, $reqd) = @_;
  for my $i (0..length($reqd)-1) {
    $reqd = $self->overwrite($reqd, $self->parse($self->{features}[$i]{requires})) 
        if substr($reqd, $i, 1) =~ /[01]/ and defined $self->{features}[$i]{requires};
  }
  $reqd;
}

sub add_entailments {
  my ($self, $phone) = @_;
  for my $i (0..length($phone)-1) {
    substr($phone, $self->{feature_index}{$self->{features}[$i]{antithetical}}, 1) = '0' 
        if substr($phone, $i, 1) eq '1' and defined $self->{features}[$i]{antithetical};
  }
  for my $i (0..length($phone)-1) {
    if (substr($phone, $i, 1) =~ /[01]/) {
      substr($phone, $_, 1) = 'u' for (@{$self->{features_requiring}[1 - substr($phone, $i, 1)][$i]});
    }
  }
  $phone;
}

package main;



# Each rule is a hash.  In the simplest case, it has hashes of _preconditions_
# and of _effects_, each of them on a phoneme specified by an index relative
# to this one.  
# Other things there can be:
# _Deletions_ happen after effects.  Deletions should be a list of indices in decreasing order.


# Memoise a rule for the computations performed in feeds(!).
# These things can be totally stripped out once the phonology is finalised. 

sub feed_annotate {
  my $rule = shift;
  for my $displ (keys %{$rule->{precondition}}) {
    $rule->{precondition_ar}{$displ} = $FS->add_requirements($rule->{precondition}{$displ});
    if (defined $rule->{effects}{$displ}) {
      $rule->{outcome}{$displ} = $FS->overwrite($FS->add_requirements($rule->{precondition}{$displ}), $rule->{effects}{$displ});
      $rule->{outcome}{$displ} =~ s/[<>]/./g;
    }
  }
}

sub strip_feed_annotation {
  my $rule = shift;
  delete $rule->{precondition_ar};
  delete $rule->{outcome};
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

# This had been somewhat of a bottleneck, still.  Can we speed it up?

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
      #            !$FS->compatible($ri->{effects}{$i_displ}, $rj->{effects}{$j_displ});
      # ramification (3)
      #for my $str (@{$FS->{strippings}}) {
      #  return 1 if $FS->compatible($ri->{precondition}{$i_displ}, $str->{condition_parsed}) and
      #             !$FS->compatible($ri->{effects}{$i_displ}, $str->{condition_parsed}); 
      #}
      
      # this is costly enough that it's slightly worth putting it in here.
      feed_annotate $ri if !defined $ri->{precondition_ar};
      feed_annotate $rj if !defined $rj->{precondition_ar};
      next if !$FS->compatible($ri->{outcome}{$i_displ}, $rj->{precondition_ar}{$j_displ});

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
      return 1 if !$FS->compatible($ri->{effects}{$dji->[1]}, $rj->{effects}{$dij->[1]});
    }
  }
  return 0;
}



package Phonology;

# Return annotations regarding which rules have which preconditions or excepts.
# Used to optimise which rules we consider rerunning in running the phonology.
# The resulting array is indexed as {$value}[$feature], where $value is '0' or '1' or 'u', 
# We also use {seq} for those rules whose preconditions include a sequence;
# these are those which can be newly triggered after a deletion.

sub annotate_with_preconditions {
  my $self = shift; 
  my %which;
  for my $i (0..@{$self->{phonology}}-1) {
    my $rule = $self->{phonology}[$i];
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
        # Not so!  We now use this for forcibly_unmark.
        push @{$which{u}[$j]}, $i
           if substr($rule->{precondition}{$displ}, $j, 1) eq 'u'; 
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

  $self->{which_preconditions} = \%which;
}

# Drop, from a completed phonology, rules that go inactive too early to ever run.

sub trim_inactive {
  my $self = shift;
  my @new_indices;
  my $deleted = 0;

  for (my $i = 0; $i < $self->{start_sequences}-$deleted; $i++) {
    if (defined $self->{phonology}[$i]{inactive} and $self->{phonology}[$i]{inactive} <= $self->{start_sequences}) {
      splice @{$self->{phonology}}, $i, 1;
      $i--;
      $deleted++;
    }
    push @new_indices, $i;
  }
  # I don't suppose it actually matters when a rule is inactivated if that time is before it runs.
  for my $rule (@{$self->{phonology}}) {
    $rule->{inactive} = $rule->{inactive} >= $self->{start_sequences} 
                      ? $rule->{inactive} - $deleted 
                      : $new_indices[$rule->{inactive}]
        if defined $rule->{inactive};
  }
  $self->{start_sequences} -= $deleted;
}

package main;

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
            if $r >= $rule->{prob}[$syllable_position]; # OBSOLETE: randomised rules do their opposite if the dice say no
      }
      
      # Handle the assimilation characters. 
      if ($effects =~ /[<>]/) {
        my ($next_before, $next_after) = (undef, undef);
        for (keys %{$rule->{precondition}}) { # TODO: use the actual offsets when distance rules exist
          $next_before = $_ if (!defined $next_before or $next_before < $_) and $_ < $displ;
          $next_after = $_ if (!defined $next_after or $next_after > $_) and $_ > $displ;
        }
        while ($effects =~ /</) {
          my $c = index($effects, '<');
          substr($effects, $c, 1) = 
              substr($i+$next_before >= 0 ? $word->[$i+$next_before] : $rule->{pause_phone}, $c, 1);
        }
        while ($effects =~ />/) {
          my $c = index($effects, '>');
          substr($effects, $c, 1) =
              substr($i+$next_after < @$word ? $word->[$i+$next_after] : $rule->{pause_phone}, $c, 1);
        }
        # We must entail the effects, not just the overwritten phone, since otherwise
        # jumps over the middle point on an antithetical scale won't always work.
        $effects = $FS->add_entailments($effects);
      }
      my $newphone = $FS->overwrite($word->[$i+$displ], $effects);
      
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

package Phonology;

# Persistent rules implement so-called surface filters.  

# Persistence is the default state of affairs for a non-generator rule.
# The {inactive} property on a rule is a rule number N, at which point this one
# becomes inactive (it won't run as a resolution when the current rule is >= N).
# A rule can also inactivate itself (they commonly do);
# these still run once.

# If passed a list in sources, it will overwrite it with a list of
# positions of source phones of the phones in the result.
# -1 is used for epenthesis, and for the smaller fragment in breakings.
# (It seems a bad idea to use the same source label twice.)

# Regardless of persistence, always run a _single_ rule repeatedly until
# it makes no more changes.  This way things like assimilation will work
# across groups of more than two phones.  It also means we must disallow
# certain rule types (e.g. a single rule to achieve l...l, r...r > l...r).
# TODO: whether LtR or RtL needs to be an option here.  

# Only do rules start..end-1, if these are provided.  
# If passed cleanup => $i, don't even do a single rule but rather just
# fix up the word using persistent rules in force after rule $i runs.

# If passed a list in track_expiry, it puts the expiry time of the derivation
# in the first element (with the same semantics as inactive, i.e. it expires
# just before the given rule number).

use constant STEPS_TO_LOOP => 10;
use constant STEPS_TO_DIE => 30;

sub run {
  my ($self, $word, %args) = (shift, shift, @_);
  my $phonology = $self->{phonology};

  my $start = defined $args{start} ? $args{start} : 0;
  my $end = defined $args{end} ? $args{end} : @$phonology;
  ($start, $end) = ($args{cleanup}, $args{cleanup}+1) if defined $args{cleanup};
  my $first_time = 1;
  @{$args{sources}} = 0..@$word-1 if (defined $args{sources} and !@{$args{sources}});
  my $track_expiry;
  $track_expiry = main::INF if defined $args{track_expiry};

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
            main::run_one_rule $word, $phonology->[$i], %args, changes => \@changes) {
          if (keys %{$phonology->[$i]{precondition}} > 1) { # an optimization.  helpful?
            1 while main::run_one_rule $word, $phonology->[$i], %args;
          }
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
          if (defined $self->{which_preconditions}) {
            # We might need to rerun the rules which have as a precondition a feature
            # this rule has newly acquired.
            my @new_agenda;
            for my $change (@changes) {
              if ($change =~ /^c (.*) (.*)$/) {
                push @new_agenda, @{$self->{which_preconditions}{$1}[$2]}
                    if defined $self->{which_preconditions}{$1}[$2];
              }
              elsif ($change eq 'd') {
                push @new_agenda, @{$self->{which_preconditions}{seq}} if defined $self->{which_preconditions}{seq};
              }
            }
            %new_agenda = (%new_agenda, map(($_ => 1), @new_agenda));
          } else {
            %new_agenda = map(($_ => 1), (0..$k));
          }
        }
      }
      %agenda = %new_agenda;
      # fwiw I saw this tripped wrongly once when the bound was 8.
      (print STDERR "*** unceded loop!\n"), last if (++$iterations >= STEPS_TO_DIE); 
    } # while (keys %agenda)
  }
  
  if (defined $args{generator}) {
    $_ = $FS->add_entailments($_) for @$word;
  }
  $args{track_expiry}[0] = $track_expiry if defined $track_expiry;
}

package main;


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
    } # if $persistent

    push @makings, [$rule,
                    $base_weight *
                    ($persistent ? $persistence_weight : 1 - $persistence_weight) *
                    $loopbreak_penalty];
  }
  @makings;
}

# Generate an extra condition for a given rule.

sub gen_extra_condition {
  my ($rule, %args) = (shift, @_);
  my (%resolution_keys, %resolutions);
  my $global_res_count = 0;

  for my $locus (keys %{$rule->{or_pause}}) {
    # Restriction to word-extremal, and away from it.
    my %rule1 = %$rule;
    $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
    substr($rule1{precondition}{$locus}, 0, 1) = 'x';
    $resolution_keys{$global_res_count} = 0.5; # magic weight
    $resolutions{$global_res_count++} = \%rule1;
    
    my %rule2 = %$rule;
    $rule2{or_pause} = { %{$rule->{or_pause}} }; # deep copy this part
    delete $rule2{or_pause}{$locus};
    $resolution_keys{$global_res_count} = 0.5; # magic weight
    $resolutions{$global_res_count++} = \%rule2;
  }

  for my $locus (keys %{$rule->{effects}}) {
    my $effect = $rule->{effects}{$locus};

    # Conditions of the same family as the effects (which we don't have stored in a special structure).
    %_ = map(($_ => 1), map split(/ /, $FS->{features}[$_]{families}), 
        grep substr($effect, $_, 1) ne '.', 0..length($effect)-1);
    my @families = grep $FS->compatible($FS->parse($FS->{families}{$_}), $rule->{precondition}{$locus}),
        grep $_, keys %_;
    my @family_features = grep {
      my $i = $_;
      grep $FS->{features}[$i]{families} =~ /\b$_\b/, @families;
    } grep((defined $args{generable_val}[0][$_] && @{$args{generable_val}[0][$_]} && 
            defined $args{generable_val}[1][$_] && @{$args{generable_val}[1][$_]}), 
        0..length($effect)-1);
    # TODO: handle this when there's no generable_val.  also, a more uniform way of dropping ungenerables for the later types
    for my $f (@family_features) {
      next if substr($rule->{precondition}{$locus}, $f, 1) ne '.';
      for my $v (0..1) {
        next if $v == 0 and $FS->{features}[$f]{univalent};
        my %rule1 = %$rule;
        $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
        substr($rule1{precondition}{$locus}, $f, 1) = $v;
        $resolution_keys{$global_res_count} = $FS->{features}[$f]{univalent} ? 1.0 : 0.5; # magic factor
          # equiprobable on features, aot on their values
        $resolutions{$global_res_count++} = \%rule1;
      }
    }
    
    # Conditions related to the outcome.
    my $outcome = $FS->overwrite($rule->{precondition}{$locus}, $effect);
    $outcome =~ s/[<>]/./;
    for my $rel (@{$FS->{relations}}) {
      next if $rel->{spread_only};
      $_ = $FS->parse($rel->{to});
      next unless $outcome =~ /^$_$/;
      
      my %rule1 = %$rule;
      $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
      my $extra = $FS->parse($rel->{from});
      next unless $FS->compatible($rule1{precondition}{$locus}, $extra);
      $rule1{precondition}{$locus} = $FS->overwrite($rule1{precondition}{$locus}, $extra);
      next if $rule1{precondition}{$locus} == $rule->{precondition}{$locus};
      $resolution_keys{$global_res_count} = $rel->{weight}; # magic factor
      $resolutions{$global_res_count++} = \%rule1;
    }

    # Conditions to which the outcome is a (possibly related) assimilation.
    # TODO: once preconditions can have equality w/out fixed values, allow it here in special cases (like homorganicity).
    # Also once assimilations can be long distance, allow that in those cases here -- but that should be automatic.
    unless ($args{bar_sequences}) {
      for my $f (0..$#{$FS->{features}}) {
        next if substr($effect, $f, 1) eq '.';

        EF_ASSIM: for my $d (@{$FS->{features}[$f]{assimilation}}) {
          my @condition = map $FS->parse($_), split /, */, $d->{condition}, -1;
          next unless $outcome =~ /^$condition[$d->{target}]$/;

          my %rule1 = %$rule;
          $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
          $rule1{or_pause} = { %{$rule->{or_pause}} }; # deep copy this part
          for my $displ (0..$#condition) {
            my $l = $locus + $displ - $d->{target};
            if (!defined $rule1{precondition}{$l}) {
              $_ = $FS->parse($FS->{generic_pause_phone});
              $rule1{or_pause}{$l} = 1 if $_ =~ /^$condition[$displ]$/;
              $rule1{precondition}{$l} = '.' x length($effect);
            }
            next EF_ASSIM unless $FS->compatible($rule1{precondition}{$l}, $condition[$displ]);
            $rule1{precondition}{$l} = $FS->overwrite($rule1{precondition}{$l}, $condition[$displ]);
          }
          $_ = '.'  x length($effect);
          substr($_, $f, 1) = substr($rule1{precondition}{$locus}, $f, 1); 
          next unless $FS->compatible(substr($rule1{precondition}{$locus + 1 - 2*$d->{target}}, $f, 1), $_);
          substr($rule1{precondition}{$locus + 1 - 2*$d->{target}}, $f, 1) =
              $FS->overwrite(substr($rule1{precondition}{$locus + 1 - 2*$d->{target}}, $f, 1), $_); # impose the actual assimilation
          
          $resolution_keys{$global_res_count} = ($d->{prob} >= 1/24.0 ? 1/24.0 : $d->{prob}) * 48; # magic factor
          $resolutions{$global_res_count++} = \%rule1;
        }
      }

      # pretty duplicative
      for my $r (@{$FS->{relations}}) {
        $_ = $FS->parse($r->{to});
        next if $effect !~ /^$_$/;

        EF_ASSIMR: for my $d (@{$r->{assimilation}}) {
          my @condition = map $FS->parse($_), split /, */, $d->{condition}, -1;
          next unless $outcome =~ /^$condition[$d->{target}]$/;

          my %rule1 = %$rule;
          $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
          $rule1{or_pause} = { %{$rule->{or_pause}} }; # deep copy this part
          for my $displ (0..$#condition) {
            my $l = $locus + $displ - $d->{target};
            if (!defined $rule1{precondition}{$l}) {
              $_ = $FS->parse($FS->{generic_pause_phone});
              $rule1{or_pause}{$l} = 1 if $_ =~ /^$condition[$displ]$/;
              $rule1{precondition}{$l} = '.' x length($effect);
            }
            next EF_ASSIMR unless $FS->compatible($rule1{precondition}{$l}, $condition[$displ]);
            $rule1{precondition}{$l} = $FS->overwrite($rule1{precondition}{$l}, $condition[$displ]);
          }
          $_ = $FS->parse($r->{from});
          next unless $FS->compatible($rule1{precondition}{$locus + 1 - 2*$d->{target}}, $_);
          $rule1{precondition}{$locus + 1 - 2*$d->{target}} = 
              $FS->overwrite($rule1{precondition}{$locus + 1 - 2*$d->{target}}, $_); # impose the actual assimilation
          
          $resolution_keys{$global_res_count} = ($d->{prob} >= 1/24.0 ? 1/24.0 : $d->{prob}) * 48; # magic factor
          $resolutions{$global_res_count++} = \%rule1;
        }
      }
    }

    # Conditions that avoid a marked situation changed by a previous rule.
    for my $old_rule (@{$args{phonology}}) {
      next if defined $old_rule->{inactive};
      next if keys %{$old_rule->{precondition}} >= 2 and $args{bar_sequences};
      for my $old_locus (keys %{$old_rule->{effects}}) {
        my $old_precondition = $old_rule->{precondition}{$old_locus};
        next if $old_precondition =~ /u/;
        next unless $FS->compatible($old_precondition, $outcome);
        my $old_effect = $old_rule->{effects}{$old_locus};
        # We take a rule to avoid markedness if its effect 
        # is incompatible with its precondition.
        for (0..length($old_effect)-1) {
          substr($old_effect, $_, 1) = substr($old_rule->{effects}{$old_locus+1}, $_, 1)
            if substr($old_effect, $_, 1) eq '>';
          substr($old_effect, $_, 1) = substr($old_rule->{effects}{$old_locus-1}, $_, 1)
            if substr($old_effect, $_, 1) eq '<';
        }
        next if $FS->compatible($old_effect, $old_precondition);

        # We let how good this thing is as a condition to avoid depend on how many features have to be added.  
        # (We just perform this subtraction on the old precondition, direcly.)
        my %rule1 = %$rule;
        $rule1{precondition} = { %{$rule->{precondition}} }; # deep copy this part
        $rule1{except} = { %{$rule->{except}} } if defined $rule->{except}; # deep copy this part

        my $num_convergences = scalar grep substr($old_precondition, $_, 1) ne '.', 0..length($old_precondition)-1;
        for (0..length($old_precondition)-1) {
          substr($old_precondition, $_, 1) = '.'
              if substr($old_precondition, $_, 1) eq substr($outcome, $_, 1);
        }
        my $num_divergences = scalar grep substr($old_precondition, $_, 1) ne '.', 0..length($old_precondition)-1;
        $num_convergences -= $num_divergences; # num_convergences is for magic weights
        if ($num_divergences <= 0) {
          substr($rule1{precondition}{$locus}, 0, 1) = 'x'; # nothing is left to match!
        } elsif ($num_divergences <= 1) {
          $old_precondition =~ y/01/10/;
          $rule1{precondition}{$locus} = $FS->overwrite($rule1{precondition}{$locus}, $old_precondition);
        } else {
          $rule1{except}{$locus} .= ' ' if defined $rule1{except}{$locus};
          $rule1{except}{$locus} .= $old_precondition;
        }
        $resolution_keys{$global_res_count} = 
            $num_convergences / ($num_divergences * ($num_divergences - 1) / 2 + 1); # much magic :/
        $resolutions{$global_res_count++} = \%rule1;
      }
    }
  } # locus

  # print STDERR YAML::Any::Dump(\%resolutions), "\n\n";
  if (keys %resolutions) {
    my $i = weighted_one_of %resolution_keys;
    return $resolutions{$i};
  }
  return $rule;
}

# To expand a rule tag:
# - make all the resolutions, incl. related features, incl. loop-preserving and -breaking forms.
# - make the rules (retaining the tag, for later remaking).  When flipping a feature between 0 and 1, 
#   clear features formerly requiring it.
# - do the chance of extra conditions thing.
# - repeat to make any necessary new rules for loopbreaks.

# The format of rule tags is "$kind $list_index", where $kind is one of the values that
# appear herein several times.
sub gen_one_rule {
  my ($phonology, $tag) = (shift, shift); 
  my %args = @_;
  my ($kind, $k, $rest) = split / /, $tag;
  # print STDERR "[" . scalar @$phonology . "] tag is $tag\n"; # debug

  if ($kind eq 'default' and !defined $rest) {
    for my $i (0..@{$FS->{features}[$k]{default}}-1) {
      gen_one_rule($phonology, "$tag $i", %args);
    }
    return;
  }

  # Not doing assimilation rules (or strippings) since they can't much come out differently.
  my $threshold = 1;
  if ($kind eq 'default') {
    $threshold = $FS->{features}[$k]{default}[$rest]{value};
  } elsif ($kind eq 'repair') {
    $threshold = $FS->{marked}[$k]{prob};
  } elsif ($kind eq 'assimilation') {
    $threshold = $FS->{assimilations}[$k]{prob};
  }

  my $initial_threshold = $threshold; # e.g. for things which are more unlikely than marked, in a way that feature choice can't handle
  if ($kind eq 'repair' and $args{initial} and defined $FS->{marked}[$k]{initial_prob}) {
    $initial_threshold = $FS->{marked}[$k]{initial_prob};
  }

  my $skip_me = 0;
  if (!$args{dont_skip} and ($kind eq 'repair' or $kind eq 'assimilation')) {
    $skip_me = (rand() > $initial_threshold);
  }
  my $add_a_condition = 0;
  if ($kind ne 'stripping') {
    $add_a_condition = (rand() < ($skip_me ? $threshold * 2/5.0 : 1/15.0)); # magic constants
  }
  return if $skip_me and !$add_a_condition;

  
  my (@resolutions, @weights);
  my $total_base_weight = 0;

  if ($kind eq 'default') {
    my $precondition = $FS->parse($FS->{features}[$k]{default}[$rest]{condition});
    $precondition = $FS->overwrite($precondition, $FS->parse($FS->{features}[$k]{requires}))
        if defined $FS->{features}[$k]{requires};
    substr($precondition, $k, 1) = 'u';

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
          $rule->{except} = {0 => $FS->parse($_->{condition})};
          last;
        }
      }
      push @resolutions, $rule;
      push @weights, $weight;
    }
  } 
  
  elsif ($kind eq 'stripping') {
    my $precondition = $FS->{strippings}[$k]{condition_parsed};

    for my $s (@{$FS->{strippings}[$k]{substitute}}) {
      $s =~ /^(.*) *: *(.*)$/;
      my $rule = {
        precondition => {0 => $FS->overwrite($precondition, $FS->parse($1))},
        effects => {0 => $FS->parse($2)},
        recastability => 0,
      };
      push @$phonology, $rule;
    }

    my $effects = $FS->parse($FS->{strippings}[$k]{strip});
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
  
  elsif ($kind =~ /^assimilation/ or $kind =~ /^repair/) { # TESTING
    my $unsplit_d;
    if ($kind =~ /^assimilation/) {
      $unsplit_d = $FS->{assimilations}[$k];
    } elsif ($kind =~ /^repair/) {
      $unsplit_d = $FS->{marked}[$k];
    }
    my $d;
    if ($kind =~ /_split$/) {
      $d = $unsplit_d->{split}[$rest];
    } else {
      $d = $unsplit_d;
    }

    # If there are split resolutions, recurse to handle them.  
    # Recursive splits don't in fact work, as this is currently implemented.
    if (defined $d->{split}) {
      for my $i (0..$#{$d->{split}}) {
        gen_one_rule($phonology, "${kind}_split $k $i", %args, dont_skip => 1) 
            if rand() < $d->{split}[$i]{prob};
      }
    }

    # the condition on a split is further to the condition on the parent
    my @unsplit_phones = map $FS->parse($_), split /, */, $unsplit_d->{condition}, -1;
    my @phones = map $FS->parse($_), split /, */, $d->{condition}, -1;
    $phones[$_] = $FS->overwrite($unsplit_phones[$_], $phones[$_]) for 0..$#phones;
    my %base_rule = (
      precondition => {map(($_ => $phones[$_]), 0..$#phones)},
      recastability => 1 - $d->{prob},
      tag => $tag,
      cede => 1 - $threshold,
    ); 

    if (defined $d->{except}) {
      my %except = %{$d->{except}};
      for my $displ (keys %except) {
        $except{$displ} = join ' ', map $FS->parse($_), split /, */, $d->{except}{$displ};
      }
      $base_rule{except} = {%except};
    }
    if ($kind =~ /_split$/ and defined $unsplit_d->{except}) {
      for my $displ (keys %{$unsplit_d->{except}}) {
        $base_rule{except}{$displ} .= ' ' if defined $base_rule{except}{$displ};
        $base_rule{except}{$displ} .= join ' ', map $FS->parse($_), split /, */, $unsplit_d->{except}{$displ};
      }
    }

    my $pause_phone;
    @_ = split / +([0-9.]+) */, $d->{pause_phone};
    if (scalar @_ == 1) {
      $pause_phone = $FS->parse($d->{pause_phone}, undefined => 1);
    } elsif (scalar @_ > 1) {
      $pause_phone = $FS->parse(weighted_one_of @_, undefined => 1);
    }
    # As a corollary of the sort here, '-' assignments follow '+' ones.  TODO: make this saner?
    for my $e (sort keys %{$d->{extras}}) {
      if (rand() < $d->{extras}{$e}) {
        $e =~ /^(.*) ([^ ]*)$/;
        my ($e0, $e1) = ($1, $2);
        if ($e0 eq '##') { # ad hoc notation for _only_ at extremum of word
          $base_rule{or_pause}{$e1} = 1;
          $base_rule{pause_phone} = $pause_phone;
          substr($base_rule{precondition}{$e1}, 0, 1) = 'x'; # ad hoc match prevention
        } elsif ($e0 eq '#') { # end of word _allowed_
          $base_rule{or_pause}{$e1} = 1;
          $base_rule{pause_phone} = $pause_phone;
        } elsif ($e0 =~ /^!/) {
          $base_rule{except}{$e1} .= ' ' if defined $base_rule{except}{$e1};
          $base_rule{except}{$e1} .= $FS->parse(substr($e0,1));
        } else {
          $base_rule{precondition}{$e1} = $FS->overwrite($base_rule{precondition}{$e1}, $FS->parse($e0));
        }
      }
    }

    # {resolve} is a weight-hash of possible resolutions, whose keys are of the form "$operation $argument".
    # 
    # If the resolution part isn't written, we will resolve phone 0 freely.
    # (This is intended for marked single phoneme rules.  In particular, the last-resort deletion
    # that these rules once had is now no more.)
    my %resolutions;
    %resolutions = %{$d->{resolve}} if defined %{$d->{resolve}};
    $resolutions{'free 0'} = 1 unless keys %resolutions;

    while (($_, my $weight) = each %resolutions) {
      /^([^ ]*) +(.*)$/;
      my ($reskind, $arg) = ($1, $2);

      my %rule = %base_rule; 
      my @variants = (); # where to put the generated rules

      # resolve as specified
      if ($reskind eq 'r') {
        my @effects_strings = split /, +/, $arg;
        my %effects = ();
        for (@effects_strings) {
          /^(.*) +([0-9]*)$/;
          my ($effect, $target) = ($1, $2);
          my $parsed_effect = $FS->parse($effect);

          for (0..length($effects{$target})-1) {
            if (substr($parsed_effect, $_, 1) =~ /[{}]/) {
              my $restriction = rand(2.0 + 4.0/(1-$threshold)); # 4 is a magic factor
              if ($restriction < 2.0) {
                substr($rule{precondition}{$target}, $_, 1) = int($restriction)
                    if substr($rule{precondition}{$target}, $_, 1) eq 'u';
              }
            }
          }
          $parsed_effect =~ y/{}/<>/;

          # In case of assimilation, both the things being spread from and to need to support the feature,
          # unless assimilation in that feature as well assures that this is unnecessary.  
          for (0..length($parsed_effect)-1) {
            if (substr($parsed_effect, $_, 1) ne '.') {
              my $requirements = $FS->parse($FS->{features}[$_]{requires});
              for my $i (0..length($requirements)-1) {
                substr($requirements, $i, 1) = '.'
                    if substr($parsed_effect, $_, 1) =~ /[<>]/ 
                    and substr($parsed_effect, $i, 1) eq substr($parsed_effect, $_, 1);
              }
              $rule{precondition}{$target} = $FS->overwrite($rule{precondition}{$target}, $requirements);
              if (substr($parsed_effect, $_, 1) =~ /[<>]/) {
                my $source = (substr($parsed_effect, $_, 1) eq '>') ? $target + 1 : $target - 1;
                $rule{precondition}{$source} = $FS->overwrite($rule{precondition}{$source}, $requirements);
              }
            }
          }
          
          $effects{$target} = $parsed_effect;
        }

        # But not if it's stripped off.
        for my $str (@{$FS->{strippings}}) {
          for my $displ (keys %{$rule{precondition}}) { 
            if ($rule{precondition}{$displ} =~ /^$str->{condition_parsed}$/) {
              my $effect = $FS->parse($str->{strip});
              $effect =~ s/1/a/g; # temporary char
              $rule{precondition}{$displ} = $FS->overwrite($rule{precondition}{$displ}, $effect);
              $rule{precondition}{$displ} =~ s/a/./g;
            }
          }
        }

        $rule{effects} = \%effects;
        push @variants, persistence_variants $phonology, [\%rule, 1], $threshold, 
                                             0, $args{generable_val};
      } #r

      elsif ($reskind eq 'delete') {
        push @{$rule{deletions}}, $arg;
        push @variants, [\%rule, 1];
      } #delete

      # Resolve the named phone in the ways listed in {flip} and {related_weight}.
      # In {flip} is a hash of single features to be flipped, with multiplicative weights;
      # in {related_weight} is a hash of multiplicative weights applying to resolutions via related features.
      # For essentially historical reasons, {flip} and {related_weight} belong to the whole constraint,
      # not the resolution.  If entries in {flip} or keys in {related_weight} are followed by
      # a number, they apply only to the phone of that index, else they apply to all phones.
      elsif ($reskind eq 'free') {
        my $resolvend = $phones[$arg];
        my $reqd = $FS->add_requirements($resolvend);
        my $i = 0;
        my $resolution_type = 0;
        RESOLUTION_TYPE: while ($resolution_type <= 1) {
          my $effects;
          my $base_weight = 0;
          my $no_persist = 0;
          $no_persist = 1 if defined $d->{phonemic_only};

          %rule = %base_rule; 

          if ($resolution_type == 0) {
            $i = 0, $resolution_type++, next if $i >= length($resolvend);
            unless (defined $d->{flip}{$FS->{features}[$i]{name}}) {
              $i++, redo if substr($unsplit_phones[$arg], $i, 1) !~ /[01]/; # only flip actual things in the *base* situation
              $i++, redo if defined $FS->{features}[$i]{structural};
            }
            $effects = '.' x length($resolvend);
            substr($effects, $i, 1) = (substr($reqd, $i, 1) eq '1' ? '0' : '1');
            # don't turn univalents on (unless specially allowed)
            if (substr($reqd, $i, 1) eq '0' and defined $FS->{features}[$i]{univalent}) {
              $i++, redo if !defined $d->{flip}{$FS->{features}[$i]{name}};
              $effects = $FS->overwrite($effects, $FS->parse($d->{univalent_addition}));
                  # this still needs to have multiple phones enabled on it
            }
            # Weights for flipping individual features: given in {flip}.
            $base_weight = (defined $d->{flip}{$FS->{features}[$i]{name} . " $arg"} ? 
                $d->{flip}{$FS->{features}[$i]{name} . " $arg"} : 
                (defined $d->{flip}{$FS->{features}[$i]{name}} ? 
                $d->{flip}{$FS->{features}[$i]{name}} : 1));
          } 

          elsif ($resolution_type == 1) {
            $i = 0, $resolution_type++, next if $i >= @{$FS->{relations}};
            # just bail if we're in a stripping condition. --- why did I do this?
            for my $str (@{$FS->{strippings}}) {
              my $strip_condition = $str->{condition_parsed};
              $i = 0, $resolution_type++, next RESOLUTION_TYPE if $resolvend =~ /^$strip_condition$/;
            }

            $i++, redo if defined $FS->{relations}[$i]{spread_only};
            
            my $from = $FS->parse($FS->{relations}[$i]{from});
            $i++, redo if $resolvend !~ /^$from$/;
            $effects = $FS->add_requirements($FS->parse($FS->{relations}[$i]{to}));
            if ($FS->compatible($FS->add_entailments($effects), $resolvend)) {
              # This is the place where we get the first word.  That's problematic.
              $FS->{relations}[$i]{from} =~ /^([^ ]*)/;
              $_ = $FS->parse($1);
              y/01/10/;
              $effects = $FS->overwrite($effects, $FS->add_requirements($_));
            }
            # Weights for doing any complicated feature change: given in {relate_weight},
            # which apply to anything they match.
            $base_weight = $FS->{relations}[$i]{weight};
            if(defined $d->{related_weight}) {
              for my $outcome (keys %{$d->{related_weight}}) {
                if ($outcome =~ /^(.*) ([0-9]*)$/) {
                  next unless $arg == $2;
                  $outcome = $1;
                }
                my $f = $FS->parse($outcome);
                $base_weight *= $d->{related_weight}{$outcome} if $effects =~ /^$f$/;
              }
            }
          }

          $total_base_weight += $base_weight;

          $rule{effects}{$arg} = $effects;
          # This base_weight is used to fill out recastability, below.
          $rule{base_weight} = $base_weight; 

          my $persistence_weight = defined $d->{persist} ? $d->{persist} : $threshold;
          push @variants, persistence_variants $phonology, [\%rule, $base_weight], $persistence_weight, 
                                              $no_persist, $args{generable_val};
          for (@variants) {
            push @resolutions, $_->[0];
            push @weights, $_->[1];
          }
          $i++;
        } # resolution type
      } # free

      my $total_weight = 0;
      $total_weight += $_->[1] for @variants;
      for (@variants) {
        push @resolutions, $_->[0];
        push @weights, $_->[1] * $weight / $total_weight;
      }
    }
  } # assimilation

  else {
    warn "unknown rule tag: $tag";
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
      $selected_rule->{effects}{$displ} = $FS->add_entailments($selected_rule->{effects}{$displ});

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

  # Adorn the rule with extra conditions, if we decided to before.
  if ($add_a_condition) {
    $selected_rule = gen_extra_condition $selected_rule, phonology => $phonology, %args;
  }

  # If any of the preconditions of this rule are not generable by anything coming before,
  # and it's a one-time rule, it's never triggerable; just drop it and don't write it down.
  for my $displ (keys %{$selected_rule->{precondition}}) {
    for my $i (0..@{$FS->{features}}-1) {
      return if substr($selected_rule->{precondition}{$displ}, $i, 1) =~ /[01]/ 
            and !defined($args{generable_val}[substr($selected_rule->{precondition}{$displ}, $i, 1)][$i])
            and defined $selected_rule->{inactive};
    }
  }

  # Heed forcibly_unmark: take out changes setting a certain feature
  # to a value other than undefined, except on default rules.
  # Moreover, if a rule changes a feature on which the defaults of that feature continge,
  # explicitly force it back to undefined.
  # (This is for before start_sequences, and is meant to be a mechanism by which e.g.
  # we can avoid the stupid /n_a/ : /n_m/ contrasts.)
  #
  # Get rid of effectses if we can.
  if (defined $args{forcibly_unmark}) {
    for my $i (keys %{$args{forcibly_unmark}}) {
      for my $displ (keys %{$selected_rule->{effects}}) {
        if ($kind ne 'default' and $kind ne 'stripping') {
          substr($selected_rule->{effects}{$displ}, $i, 1) = '.'
              if substr($selected_rule->{effects}{$displ}, $i, 1) =~ /[01]/;
          delete $selected_rule->{effects}{$displ}, next unless $selected_rule->{effects}{$displ} =~ /[^.]/;
        }
        for (@{$args{forcibly_unmark}{$i}}) { 
          substr($selected_rule->{effects}{$displ}, $i, 1) = 'u', last 
              if substr($selected_rule->{effects}{$displ}, $_, 1) ne '.';
        }
      }
    }
  }

  # Abandon this ruls if it does nothing now.
  return unless keys %{$selected_rule->{effects}} or defined $selected_rule->{deletions}; # TODO: update as needed

  # Adding {except} conditions if this might newly set a feature which a stripping takes out
  # would be nice if it worked, but there are problems if the feature being set is a side effect;
  # we don't want to block the whole rule on its account, then.  So in place of this,
  # we play an underhanded game with which_preconditions.  This is *very very naughty* of us,
  # it means the semantics of which_preconditions aren't straightforward and will
  # likely lead to pain in the future.

  # It's correct for extra condition rules to have no tag, so that they
  # just drop out when regenerated.
  $selected_rule->{tag} = $tag unless $add_a_condition;
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
    delete $otherargs{dont_skip};
#    print "{\n"; # debug
    gen_one_rule($phonology, $tag, avoid => [(split /\|/, $avoid), @{$args{avoid}}], %otherargs);
#    print "}\n"; # debug
  }
  delete $selected_rule->{broken_tags};

  # If this is the added-condition version of a rule which we also wanted to generate unadorned,
  # recurse to do the unadorned form.
  if ($add_a_condition and !$skip_me) {
    gen_one_rule($phonology, $tag, %args, dont_skip => 1);
  }
}



package Phonology;

# A complete generated phonology has the following layers.
#
# (1) General single segment repair and default feature insertion rules.
#     [It is planned that a few of the default insertion rules may be harmonic in nature.  
#     Aside from this exception:] These are context-independent.
# (2) General cluster resolution, allophony, and the like.  Any time from 
#     the start of this block onward is a sensible affix attachment time.
#
# The start of (2) is called start_sequences.  Phonemes are regarded as being those phones
# which can be extant at the start of (2).

# Alternations (whose implementaiton is probably not to be soon, comparatively...)
# will not be implemented by the resolutions of features in
# different contexts alone.  Instead, we'll eventually generate a thing for them:
# perhaps a table with several small dimensions, and for each value of each dimension
# one (or a few?) feature-values from among the contrastive features,
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
#
# To make good deeper alternations probably requires using related features and stuff
# to retcon some extra history.  But that seems hard.

sub generate {
  print STDERR "generating phonology...\n" if $verbose;
  my $pd = Phonology::generate_preliminary();
  $pd->annotate_with_preconditions();
  print STDERR "computing inventory...\n" if $verbose;
  $pd->compute_inventory(); # base inventory for generation
  $pd->postprocess_inventory();
  delete $pd->{phone_generator}; # now this is needless
  if ($debug < 1) {
    $pd->trim_inactive(); 
  } else {
    print STDERR "pruning of inactive rules skipped\n";
  }
  $pd->annotate_with_preconditions(); # since the numbers are changed
  for (@{$pd->{phonology}}) {
    main::strip_feed_annotation $_;
  }
  return $pd;
}

sub generate_preliminary {
  my (@phone_generator, @phonology);
  my @syllable_structure;

  # The most general allowable form of a syllable position features specification
  # consists of feature strings alternated with weights.  Each probability
  # associates to the feature string before it; a missing final weight will be chosen
  # to make the sum 1.
  for my $slot (@{$FS->{syllable_template}}) {
    next if rand() >= $slot->{prob};
    do {
      # Prepare syllable structure for the cases where there are alternates.
      # Fuzz the probabilities.
      my @featureses = split / +([0-9.]+) */, $slot->{features};
      my %featureses;
      my $remaining_weight = 1; # pre-fuzz weight
      my $fuzzed_weight = 0; # post-fuzz weight
      while (@featureses) {
        my $phone = $FS->parse(shift @featureses, undefined => 1);
        $remaining_weight -= @featureses[0] if @featureses;
        $_ = (@featureses ? shift @featureses : $remaining_weight) * (rand() + rand());
        $fuzzed_weight += $_;
        $featureses{$phone} = $_;
      }
      $featureses{$_} /= $fuzzed_weight for (keys %featureses);

      my $rslot = {
        prob => fuzz($slot->{presence}),
        features => \%featureses,
        tag => $slot->{tag},
      };
      $rslot->{bend} = $slot->{bend} if defined $slot->{bend};
      $rslot->{reprune} = 1 if defined $slot->{reprune} and rand() < $slot->{reprune};
      if (defined $slot->{except}) {
        while (my ($k, $v) = each %{$slot->{except}}) {
          push @{$rslot->{except}}, $FS->parse($k) if rand() < $v;
        }
      }
      push @syllable_structure, $rslot;
    } while (defined $slot->{prob_more} and rand() < $slot->{prob_more});
  }
  
  my @generable; # is this feature generated as contrastive in any situation?
  my @generable_val; # defined($generable_val[$v][$f]) iff the $f-th feature can take value $v \in 0,1.
                     # If it's an empty list, that's ok; that just means the feature can only come up in phone generation.
  my %family_inventories;
  $family_inventories{$_} = { $FS->parse($FS->{families}{$_}) => 1 }
      for (keys %{$FS->{families}});
  my %special_filling; # which features we're using a U in the syllable structure in
  my %prevent_marked; # when we look through the markeds, which ones we don't do
  
  # Sometimes we generate things which we never generate a prerequisite for.  Not a problem though.
  for my $fi (0..@{$FS->{features}}-1) {
    my $f = $FS->{features}[$fi];
    for my $sit (@{$f->{generated}}) {
      if (rand() < $sit->{contrast}) {
        my $requires;
        $requires = $FS->parse($f->{requires}) if defined $f->{requires};

        my @by_families;
        if (defined $sit->{by_family} and rand() < $sit->{by_family_prob}) {
          for my $phone (keys %{$family_inventories{$sit->{by_family}}}) {
            next if defined $requires and !$FS->compatible($phone, $requires);
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
        }

        my $precondition = $FS->parse($sit->{condition});
        substr($precondition, $FS->{feature_index}{$f->{name}}, 1) = 'u';
        $precondition = $FS->overwrite($precondition, $requires) if defined $f->{requires};
        my %rule = (
          precondition => {0 => $precondition},
          effects => {0 => $FS->parse($f->{name})}, # er, rework this
          antieffects => {0 => $FS->parse('-' . $f->{name})},
          prob => [map fuzz($sit->{prob}), @syllable_structure],
        );
        substr($rule{effects}{0}, $FS->{feature_index}{$f->{antithetical}}, 1) = '0' if (defined $f->{antithetical});
        if (@by_families) {
          for (@by_families) {
            my %rule1 = %rule; 
            $rule1{precondition}{0} = $FS->overwrite($precondition, $_);
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
          my $r = rand(); 
          while (my ($phone, $weight) = each %{$slot->{features}}) {
            $_ = $phone;
            s/u/./g;
            next unless $FS->compatible($_, $precondition);

            delete $slot->{features}{$phone};
            if (defined $f->{slots}{$slot->{tag}}) {
              if ($r < $f->{slots}{$slot->{tag}}[0]) {
                $phone = $FS->overwrite($phone, $rule{antieffects}{0});
              } elsif ($r < $f->{slots}{$slot->{tag}}[0] + $f->{slots}{$slot->{tag}}[1]) {
                $phone = $FS->overwrite($phone, $rule{effects}{0});
                $phone = $FS->overwrite($phone, $FS->parse($f->{slot_if_on}))
                    if defined $f->{slot_if_on};
              } elsif ($r < $f->{slots}{$slot->{tag}}[0] + $f->{slots}{$slot->{tag}}[1] +  $f->{slots}{$slot->{tag}}[2]) {
                substr($phone, $FS->{feature_index}{$f->{name}}, 1) = 'U';
                $special_filling{$FS->{feature_index}{$f->{name}}} = 1;
              }
            }
            if (defined $slot->{features}{$phone}) {
              $slot->{features}{$phone} += $weight;
            } else {
              $slot->{features}{$phone} = $weight;
            }
          } # each %{$slot->{features}}
        } # @syllable_structure

        for my $fam (split / /, $f->{families}) {
          $_ = $precondition;
          for my $phone (keys %{$family_inventories{$fam}}) {
            my $s = $_;
            $s =~ s/u/./g;
            next if $phone !~ /^$s$/;
            # Using $rule{prob}[0] here of course isn't especially correct, but it'll do.
            $family_inventories{$fam}{$FS->overwrite($phone, $rule{effects}{0})} += 
              $family_inventories{$fam}{$phone} * $rule{prob}[0] if ($rule{prob}[0] > 0);
            $family_inventories{$fam}{$FS->overwrite($phone, $rule{antieffects}{0})} += 
              $family_inventories{$fam}{$phone} * (1 - $rule{prob}[0]) if ($rule{prob}[0] < 1);
            delete $family_inventories{$fam}{$phone}; 
          }
        }
        
        # If the same string appears as the value of key 'prevent_marked' on both a
        # generable situation and a marked, then -- if the generable situation is chosen,
        # the marked will never be.   
        if (defined $sit->{prevent_marked}) {
          $prevent_marked{$sit->{prevent_marked}} = 1;
        }
      } # rand() < $sit->{contrast}
    } # situations for generation
    
    if (defined $f->{structural}) {
      $generable[$fi] = 1;
      $generable_val[0][$fi] = [-1]; # -1 to not trip the never triggerable check.
      $generable_val[1][$fi] = [-1]; # hope it doesn't screw up other things
    }
  } # features in the phone generator

  # Map features to the things on which their defaults depend, including stripping situations.
  my %forcibly_unmark;
  for my $i (0..@{$FS->{features}}-1) {
    if (!$generable[$i] and defined $FS->{features}[$i]{forcibly_unmark}
                        and rand() < $FS->{features}[$i]{forcibly_unmark}) {
      my @l = ();
      for my $default (@{$FS->{features}[$i]{default}}) {
        my $phone = $FS->parse($default->{condition});
        for (0..@{$FS->{features}}-1) {
          push @l, $_ if substr($phone, $_, 1) ne '.';
        }
      }
      FUSTRIP: for my $stripping (@{$FS->{strippings}}) {
        my $trigger = $FS->parse($stripping->{strip});
        for my $i (@l) {
          if (substr($trigger, $i, 1) ne '.') {
              my $phone = $FS->parse($stripping->{condition});
              for (0..@{$FS->{features}}-1) {
                push @l, $_ if substr($phone, $_, 1) ne '.';
              }            
            next FUSTRIP;
          }
        }
      }
      $forcibly_unmark{$i} = \@l;
    }
  }  

  # Choose the order the rules are going to appear in, and write down a list of rule tag strings.

  # Marked single phones and sequences are handled by rules of the same type.  
  # If the constraint is against a sequence of length one, the rule is placed before 
  # the point defining what the phonemes ar.  Sequences of greater length are placed after,
  # and correspond to allophony.

  # Default provision rules come in a random order; contrastive features are more likely to 
  # come early; among uncontrastive features the unlikely to have been contrastive are biased to come late.

  # Subject to that, repair rules come as soon as they can; we have taken occasional 
  # advantage of the fact that they are not further randomized.

  my @feature_at_position; # do first feature in this list first, etc.
  my @position_of_feature; # the inverse of this, plus 1 (so do the $i such that $p_o_f[$i] is 1 first)
  my @sortkey;
  for my $i (0..@{$FS->{features}}-1) {
    $sortkey[$i] = main::std_normal();
    unless ($generable[$i]) {
      my $max_generation = 1e-6; # zero is scary
      for (@{$FS->{features}[$i]{generated}}) {
        $max_generation = $_->{contrast} if $max_generation < $_->{contrast};
      }
      $sortkey[$i] += log($max_generation); # there is a hidden multiplicative magic constant of 1 here
    }
  }
  @feature_at_position = sort {$sortkey[$b] <=> $sortkey[$a]} (0..@{$FS->{features}}-1);
  $position_of_feature[$feature_at_position[$_]] = 1 + $_ for (0..@{$FS->{features}}-1);

  my @single_repair_indices = grep $FS->{marked}[$_]{condition} !~ /,/, 0..$#{$FS->{marked}};
  my @sequence_repair_indices = grep $FS->{marked}[$_]{condition} =~ /,/, 0..$#{$FS->{marked}};

  my @repair_rule_tags;
  for my $k (@single_repair_indices) {
    # How should {prevented_by} be generalised?
    next if defined $FS->{marked}[$k]{prevented_by} and $prevent_marked{$FS->{marked}[$k]{prevented_by}};
    my $f = $FS->parse($FS->{marked}[$k]{condition}, undefined => 1);
    my $when = 0;
    for (0..length($f)-1) {
      $when = $position_of_feature[$_]
          if substr($f, $_, 1) ne 'u' and !defined $generable[$_] and $position_of_feature[$_] > $when;
    }
    push @{$repair_rule_tags[$when]}, "repair $k" unless defined $FS->{marked}[$k]{phonemic_only};
  }
  my @assim_tags = ((map "repair $_", (@sequence_repair_indices)));
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
    push @rule_tags, "default $feature_at_position[$i-1]" unless $i <= 0 or defined $special_filling{$feature_at_position[$i-1]};
    push @rule_tags, @{$repair_rule_tags[$i]} if defined $repair_rule_tags[$i];
  }
  for my $k (@single_repair_indices) {
    next if defined $FS->{marked}[$k]{prevented_by} and $prevent_marked{$FS->{marked}[$k]{prevented_by}};
    push @rule_tags, "repair $k" if defined $FS->{marked}[$k]{phonemic_only};
  }
  push @rule_tags, '#'; # false tag for end of phoneme straightening-out
  push @rule_tags, @assim_tags;

  my $start_sequences; # end of rules that pertain only to individual segments
  for my $tag (@rule_tags) {
    if ($tag eq '#') {
      print STDERR "on to allophony...\n" if $verbose;
      $start_sequences = @phonology; 
      next;
    } 
    # We pass the generator as a way of specifying what contrasts are available.
    # For sound change purposes we'll need an alternate way to pass this information.
    main::gen_one_rule \@phonology, $tag, 
        generator => \@phone_generator, 
        generable_val => \@generable_val, 
        initial => 1,
        syllable_structure => \@syllable_structure, # used only by extra conditions, presently
        bar_sequences => defined $start_sequences ? undef : 1,
        forcibly_unmark => defined $start_sequences ? undef : \%forcibly_unmark; 
  }
    
  my $self = {
    syllable_structure => \@syllable_structure,
    phone_generator => \@phone_generator, 
    phonology => \@phonology,
    start_sequences => $start_sequences,
  };
  bless $self;
}

package main;



# In a modifier description, it's only the first phone that the modifier actually spells;
# the rest are just conditions on its applicability.

sub name_phone {
  my ($phone, %args) = (shift, @_);
  my $pa = defined $args{alphabet} ? $args{alphabet} : 
           $use_html ? $phonetic_alphabets{IPA} : $phonetic_alphabets{CXS};
  my %taken_care_of;
  my $s = '##';
  $s = "<abbr title=\"$phone\">$s</abbr>" if $use_html; # handy for debugging
  
  for my $x (keys %{$pa->{ligations}}) {
    next if $phone !~ /^$x$/;
    my $phone0 = $FS->overwrite($phone, $FS->parse($pa->{ligations}{$x}[0]));
    my $phone1 = $FS->overwrite($phone, $FS->parse($pa->{ligations}{$x}[1]));
    my ($tc0, $s0) = name_phone($phone0, %args, no_modifiers => 1);
    my ($tc1, $s1) = name_phone($phone1, %args, no_modifiers => 1);
    $s = $pa->{ligations}{$x}[2];
    $s =~ s/\[\]/$s0/;
    $s =~ s/\[\]/$s1/;
    %taken_care_of = (%$tc0, %$tc1, 
                      map(($_ => 1), (grep substr($x, $_, 1) ne '.', 0..@{$FS->{features}}-1)) );
    last;
  }

  if ($s =~ /##/) {
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

# duplicated for efficiency :/
sub spell_out_spaces {
  my ($word, %args) = (shift, @_);
  if ($args{null} and ($word eq '')) {
    my $pa = defined $args{alphabet} ? $args{alphabet} : 
             $use_html ? $phonetic_alphabets{IPA} : $phonetic_alphabets{CXS};
    return $pa->{null};
  }
  join "", map name_phone($_, %args), (split / /, $word);
}

package Phonology;

# vectors, whee.
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

# The inventory this returns is raw, and needs a post-processing stage.

sub compute_inventory {
  my $self = shift;
  my ($syllable_structure, $phone_generator, $phonology, $which_preconditions) = 
      @$self{qw/syllable_structure phone_generator phonology which_preconditions/};
  # This is a hash from phones to lists of probabilities in the various syllable positions.  
  # We use these for generation and to calculate the entropy.  However, we don't take any interactions
  # between phones into account for these numbers, so they're kind of crude.
  my %inventory;

  for my $i (0..@$syllable_structure-1) {
    for my $phone (keys %{$syllable_structure->[$i]{features}}) {
      add_in \%inventory, $phone, 
          [map(($_ == $i ? $syllable_structure->[$i]{features}{$phone} : 0), 
               (0..@$syllable_structure-1))];
    }
  }

  # TODO: Revise this if ever pre-sequences resolvent rules can cause breakings or whatever.
  for my $rule (@$phone_generator) {
    my %inventory2;
    for my $phone (keys %inventory) {
    my @v = @{$inventory{$phone}};
      my @word;
      @word = ($phone);
      main::run_one_rule \@word, $rule, rand_value => 0;
      add_in \%inventory2, $word[0], [map $v[$_] * $rule->{prob}[$_], 0..@v-1];
      @word = ($phone); 
      main::run_one_rule \@word, $rule, rand_value => 1;
      add_in \%inventory2, $word[0], [map $v[$_] * (1 - $rule->{prob}[$_]), 0..@v-1];
    }
    %inventory = %inventory2;
  }
  
  # Strip unsupported features at the end, in case the syllable structure put them in.
  for my $phone (keys %inventory) {
    my $stripped = $FS->add_entailments($phone);
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
    $self->run(\@word, end => $self->{start_sequences});
    my $outcome = join(' ', @word);
    print STDERR "out: $outcome /" . (@word ? main::name_phone($word[0]) : '') . "/\n" if $debug >= 1;
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

  # We will need to use the resolver when it comes to generating alternations.  
  # It's not necessary to for ordinary stem generation, though; for that the inventory suffices.
  $self->{gen_inventory} = \%prinv;
}

# Do some artificial thing meant to stop a lot of the frequency mass from being concentrated
# in a few phones.  Fairly harsh.
# Class method.
sub bend_frequencies {
  my ($gi, $i, $threshold) = (shift, shift, shift);
  my $n = scalar keys %$gi;
  my $sum = 0;
  for my $phone (keys %$gi) {
    if ($gi->{$phone}[$i] > $threshold / $n) {
      $gi->{$phone}[$i] = (log($gi->{$phone}[$i] * $n / $threshold) + 1) * $threshold / $n;
    }
    $sum += $gi->{$phone}[$i];
  }
  for my $phone (keys %$gi) {
    $gi->{$phone}[$i] /= $sum;
  }
}

# Make some tweaks to the inventory of the sort that're problematic to do in initial generation.
# There is some icky duplication in here.
sub postprocess_inventory {
  my $self = shift;

  for (my $i = $#{$self->{syllable_structure}}; $i >= 0; --$i) {
    # bend frequencies
    if (defined $self->{syllable_structure}[$i]{bend}) {
      bend_frequencies $self->{gen_inventory}, $i, $self->{syllable_structure}[$i]{bend};
      delete $self->{syllable_structure}[$i]{bend};
    }

    # drop nonmatches if we want to force matches
    if (defined $self->{syllable_structure}[$i]{reprune}) {
      my $sum = 0;
      for my $phone (keys %{$self->{gen_inventory}}) {
        $self->{gen_inventory}{$phone}[$i] = 0 if $phone and !grep {s/u/./; $phone =~ /^$_$/;} keys %{$self->{syllable_structure}[$i]{features}};
        $sum += $self->{gen_inventory}{$phone}[$i];
        unless (grep $_ > 0, @{$self->{gen_inventory}{$phone}}) {
          delete $self->{gen_inventory}{$phone};
        }
      }
      for my $phone (keys %{$self->{gen_inventory}}) {
        $self->{gen_inventory}{$phone}[$i] /= $sum;
      }
      delete $self->{syllable_structure}[$i]{reprune};
    }

    # drop excepted things
    if (defined $self->{syllable_structure}[$i]{except}) {
      my $sum = 0;
      for my $phone (keys %{$self->{gen_inventory}}) {
        $self->{gen_inventory}{$phone}[$i] = 0 if grep $phone =~ /^$_$/, @{$self->{syllable_structure}[$i]{except}};        
        $sum += $self->{gen_inventory}{$phone}[$i];
        unless (grep $_ > 0, @{$self->{gen_inventory}{$phone}}) {
          delete $self->{gen_inventory}{$phone};
        }
      }
      for my $phone (keys %{$self->{gen_inventory}}) {
        $self->{gen_inventory}{$phone}[$i] /= $sum;
      }
      delete $self->{syllable_structure}[$i]{except};
    }

    # eliminate positions with nothing in them
    if ($self->{gen_inventory}{''}[$i] >= 1) {
      for my $phone (keys %{$self->{gen_inventory}}) {
        splice @{$self->{gen_inventory}{$phone}}, $i, 1;
      }
      splice @{$self->{syllable_structure}}, $i, 1;
    }
  }

  # Now that we're done playing with it, record entropies in bits on the syllable structure.  
  for my $i (0..@{$self->{syllable_structure}}-1) {
    my $entropy = 0;
    while (my ($phone, $v) = each %{$self->{gen_inventory}}) {
      $entropy += $v->[$i] * log($v->[$i]) / log(0.5) if $v->[$i] > 0;
    }
    $self->{syllable_structure}[$i]{entropy} = $entropy; 
  }
}

sub generate_form {
  my ($self, $target_entropy) = (shift, shift);

  my $normal = 0;
  $normal += rand(1/4.0) for 1..8; # std dev sqrt(2/3) eh
  my $entropy = $normal * $target_entropy; 

  # Form generation was once done by rules with probabilistic effects.  But that is long obsolete.
  # TODO: remove this.  (It's marked OBSOLETE: above.)  I think {antieffects} is eliminable too.
  
  my @form;
  # The form of this loop will very much be changing when we start asking for
  # forms that aren't made of whole syllables.
  my $total_entropy = 0;
  while ($total_entropy < $entropy) {
    for my $i (0..@{$self->{syllable_structure}}-1) {
      # next if $self->{syllable_structure}[$i]{nonzero_prob} == 0; # these cases are eliminated.

      $total_entropy += $self->{syllable_structure}[$i]{entropy};
      next if rand() >= $self->{syllable_structure}[$i]{prob};

      my $rand = rand (1 - $self->{gen_inventory}{''}[$i]);
      my $selected_phone;
      # only generate structural zeroes in a form, not resolvent zeroes 
      # (though we've corrected the probabilities anyhow)
      for (keys %{$self->{gen_inventory}}) {
        $selected_phone = $_, last if $_ ne '' and ($rand -= $self->{gen_inventory}{$_}[$i]) < 0;
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
  my ($self, $word) = (shift, shift);
  my @canonical_word = @$word; 
  my @current_word = @$word;
  my @sources = 0..@$word-1;
  
  for my $k ($self->{start_sequences}..@{$self->{phonology}}-1) {
#    print "before $k /" . spell_out(\@canonical_word) . "/ [" . spell_out(\@current_word) . "]\n"; # debug
    my @old_sources = @sources;
    my @old_word = @current_word;
    my (@prov_canonical_word, @prov_current_word);
    my $changed;
    $self->run(\@current_word, 
               start => $k,
               end => $k+1,
             sources => \@sources);
#    print "target [" . spell_out(\@current_word) . "]\n"; # debug
    
    { # block for redo
      $changed = 0;

      # check for new deletions
      for my $source (@old_sources) {
        unless (grep $_ == $source, @sources) {
          @prov_canonical_word = @canonical_word;
          splice @prov_canonical_word, $source, 1;
          @prov_current_word = @prov_canonical_word;
          $self->run(\@prov_current_word, 
                     start => $self->{start_sequences},
                     end => $k+1);
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
          if (defined $self->{gen_inventory}{$prov_canonical_word[$sources[$i]]}) {
#          print "trying out " . name_phone($prov_canonical_word[$sources[$i]]) . " at $i\n"; # debug
            @prov_current_word = @prov_canonical_word;
            $self->run(\@prov_current_word, 
                       start => $self->{start_sequences},
                       end => $k+1);
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

package main;

# TODO: this belongs in the describer class
sub describe_inventory {
  my ($pd, %args) = @_;
  my $buffer = '';

  if ($args{html}) {
    $buffer .= CGI::h2('Phonemic inventory') . tabulate($pd); # HERE
  } else {
    my %things_named;
    $buffer .= "phonemic inventory:\n"; 
    for my $p (sort keys %{$pd->{gen_inventory}}) { 
      my $n = join '', map name_phone($_), split / /, $p;
      $buffer .= "/" . ($n !~ /\#\#/ ? $n : $FS->feature_string($p)) . "/\t@{$pd->{gen_inventory}{$p}}\n";
      push @{$things_named{$n}}, $p if ($n !~ /\#\#/);
    }
    for my $name (keys %things_named) { # left in in case it crops up
      if (@{$things_named{$name}} >= 2) {
        warn "duplicate [$name]\n";
        warn "$_\n" for @{$things_named{$name}};
      }
    }
    # subtract 1 for the empty phone
    $buffer .= "that's " . ((keys %{$pd->{gen_inventory}}) - 1) . " phonemes\n";
  }
}



####### Here genuine phonology code ends, and description-writing code begins.

# Put in the false features which our model doesn't contain but which our descriptions do.
# Assumes annotation.
sub add_false_features {
  my ($phone, $str) = (shift, shift);
  while (defined $str->{subtables}) {
    my $subtable = substr($phone, $FS->{feature_index}{$str->{subtables}}, 1);
    $str = $str->{$subtable};
  }
  while (my ($k, $v) = each %{$str->{undef_p}}) {
    $phone = $FS->overwrite($phone, $v) if $phone =~ /^$k$/;
  }
  $phone;
}

# Munge a phoneme in the usual order into its table-keyed form.  
sub table_sortkey {
  my ($phone, $str) = (shift, shift);

  # Although tabulate itself doesn't need this, it's handy if we want to use this
  # as an ordering in other places.
  if (defined $str->{subtables}) {
    my $subtable = substr($phone, $FS->{feature_index}{$str->{subtables}}, 1);
    return $subtable . table_sortkey($phone, $str->{$subtable});
  }
  
  while (my ($k, $v) = each %{$str->{undef_p}}) {
    $phone = $FS->overwrite($phone, $v) if $phone =~ /^$k$/;
  }
  $phone =~ s/u/0/g;
  my $position = join '', map(substr($phone, $str->{order_i}[$_], 1) =~ /[01]/ ? 
                              (substr($phone, $str->{order_i}[$_], 1) + $str->{order_r}[$_])%2 : 
                              substr($phone, $str->{order_i}[$_], 1),
                              0..@{$str->{order_i}}-1);
  while (my ($k, $v) = each %{$str->{flips_p}}) { # does this need keep_undefs?
    substr($position, $v, 1) = 1 - substr($position, $v, 1) 
        if $phone =~ /^$k$/ and substr($position, $v, 1) =~ /[01]/;
  }
  $position;
}

# Add the features which are redundantly present in a list.
sub enrich {
  my ($k, $inventory) = (shift, shift);
  my $old_k = $k;
  my $test;
  my @absent;
  REINSERTION: for my $i (0..length($k)-1) {
    next unless substr($k, $i, 1) eq '.';
    for my $v (0..2) {
      $test = $old_k;
      substr($test, $i, 1) = $v == 2 ? 'u' : $v;
      $absent[$v] = !grep /^$test$/, @$inventory;
    }
    substr($k, $i, 1) = '0', next REINSERTION if $absent[1] and $absent[2];
    substr($k, $i, 1) = '1', next REINSERTION if $absent[0] and $absent[2];
    substr($k, $i, 1) = 'u', next REINSERTION if $absent[0] and $absent[1];
  }
  $k;
}

# The table that a given phone is in (which I call $str throughout).
# This code is duplicated in several places, with various side-effects :/
# REFACTOR: have a structure to hold this
sub get_str {
  my $enriched = shift;
  my $str = $phon_descr->{table_structure};
  while (defined $str->{subtables}) {
    my $subtable = substr($enriched, $FS->{feature_index}{$str->{subtables}}, 1);
    last if $subtable eq '.';
    $str = $str->{$subtable};
  }
  return $str;
}

# The features of a phone determining what table it's in.
sub str_part {
  my $phone = shift;
  my $skeleton = '.' x @{$FS->{features}};
  my $str = $phon_descr->{table_structure};
  while (defined $str->{subtables}) {
    my $i = $FS->{feature_index}{$str->{subtables}};
    my $subtable = substr($phone, $i, 1);
    last if $subtable eq '.';
    substr($skeleton, $i, 1) = substr($phone, $i, 1);
    $str = $str->{$subtable};
  }
  $skeleton;
}

# Make a label as tabulate needs.  @$p are the phones to match for bases, @$pmod for modifiers;
# @$l and @$lmod the labels for these, respectively.

# If $args{repeat_base} is true, multiple bases will be used if they match irredundantly, 
# strings joined by its value.  $args{repeat_mod} is a hash doing similarly for modificated things.

# If $args{taken_care_of} exists, it should be a reference to an array; it is filled with
# the features that were really taken care of (not just blotted out by $args{significant}).

sub tabulate_label {
  my ($enriched, $p, $pmod, $l, $lmod, %args) = @_;
  my $label = '';
  $l = [] if !defined $l;
  $lmod = [] if !defined $lmod;

  my @taken_care_of;
  if (defined $args{significant}) {
    for (0..length($args{significant})-1) {
      $taken_care_of[$_] = 2 if substr($args{significant}, $_, 1) eq '.';
    }
  }
  for my $i (0..@$l-1) {
    if ($enriched =~ /^$p->[$i]$/) {
      if ($args{nobase} or $args{repeat_base}) {
        # if we fail to pick anything from $p in this case, that's okay.
        my $irredundant;
        for (0..length($p->[$i])-1) {
          $irredundant = 1 if substr($p->[$i], $_, 1) ne '.' and !$taken_care_of[$_]; 
        }
        next unless $irredundant;
      }
      $label .= $args{repeat_base} if $label and $args{repeat_base};
      $label .= $l->[$i];
      if (defined $args{entailed_p}) { # geh structure
        for (0..length($args{entailed_p}[$i])-1) {
          $taken_care_of[$_] = 1 if substr($args{entailed_p}[$i], $_, 1) ne '.';
        }
      } else {
        for (0..length($p->[$i])-1) {
          $taken_care_of[$_] = 1 if substr($p->[$i], $_, 1) ne '.';
        }
      }
      last unless $args{repeat_base};
    }
  }
  my $base = $label;

  $label = '[]';
  my %repeated;
  for my $i (0..@$lmod-1) {
    if ($enriched =~ /^$pmod->[$i]$/) {
      my $irredundant;
      for (0..length($pmod->[$i])-1) {
        $irredundant = 1 if substr($pmod->[$i], $_, 1) ne '.' and !$taken_care_of[$_];
      }
      next unless $irredundant;
      $lmod->[$i] =~ /\[(.*)\]/;
      my $thing = $1;
      # if repeat_mod isn't defined, don't allow repeating
      next if $thing and $repeated{$thing} and !$args{repeat_mod}{$thing};
      my $inner_label = $lmod->[$i];
      $label =~ s/ \[/$args{repeat_mod}{$thing}\[/ if $args{repeat_mod}{$thing} and $repeated{$thing};
      $repeated{$thing} = 1 if $thing;
      $label =~ s/\[.*\]/$inner_label/;

      if (defined $args{entailed_pmod}) {
        for (0..length($args{entailed_pmod}[$i])-1) {
          $taken_care_of[$_] = 1 if substr($args{entailed_pmod}[$i], $_, 1) ne '.';
        }
      } else {
        for (0..length($pmod->[$i])-1) {
          $taken_care_of[$_] = 1 if substr($pmod->[$i], $_, 1) ne '.';
        }
      }
      if (defined $args{eliminate}{$pmod->[$i]}) {
        for (0..length($pmod->[$i])-1) {
          $taken_care_of[$_] = 1 if substr($args{eliminate}{$pmod->[$i]}, $_, 1) ne '.';
        }
      }
    }
  }
  $label =~ s/\[.*\]/$base/;

   if ($args{nons}) {
    # Spell out even features which take the value not normally given a word.
    # The value of $args{nons} should be the inventory. 
    my %dont_spell;
    my $str = $phon_descr->{table_structure};
    while (defined $str->{subtables}) {
      $dont_spell{$FS->{feature_index}{$str->{subtables}}} = 1;
      my $subtable = substr($enriched, $FS->{feature_index}{$str->{subtables}}, 1);
      last if $subtable eq '.';
      $str = $str->{$subtable};
    }

    for my $i (0..length($enriched)-1) {
      next if $taken_care_of[$i];
      next if substr($enriched, $i, 1) eq '.';
      next if $dont_spell{$i};
      next if $args{respect_univalent} and substr($enriched, $i, 1) eq '0' and $FS->{features}[$i]{univalent};
      my $non = '.' x length($enriched); 
      substr($non, $i, 1) = 1 - substr($enriched, $i, 1);
      my $enriched_non = enrich $FS->overwrite(str_part($enriched), $non), $args{non_inventory}; 
      my $non_label = tabulate_label($enriched_non, $p, $pmod, $l, $lmod, %args, 
                                     nons => undef, significant => $non, nobase => 1);
      chop $non_label while $non_label =~ / $/; # for nobase
      $non_label = substr($non_label, 1) while $non_label =~ /^ /;
      if ($non_label) {
        if (defined $args{negate} and defined $args{negate}{$non_label}) {
          $label = "$args{negate}{$non_label} $label";
        } else {
          $label = "non-$non_label $label";
        }
        my $ae = '.' x length($enriched);
        substr($ae, $i, 1) = substr($enriched, $i, 1);
        $ae = $FS->add_entailments($ae);
        for (0..length($enriched)-1) {
          $taken_care_of[$_] = 1 if substr($ae, $_, 1) ne '.';
        }
      }
    } # $i
  }

  if ($args{header}) {
    # Make abbreviations for table headers.
    if ($use_html) {
      $label =~ s/ or /&nbsp;\/ /g; 
    } else {
      $label =~ s/ or / \/ /g; 
    }
    $label =~ s/\{[^{}]*\}/./g;
  } else {
    $label =~ s/[{}]//g;
  }

  if ($args{taken_care_of}) {
    @{$args{taken_care_of}} = map $_ != 2, @taken_care_of;    
  }

  $label;
}

# Return an HTML table of the phonology.  

# TODO: this belongs in the describer class
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
                           condition => $FS->overwrite($condition, $FS->parse("-$split_feature")));
    my $t1 = tabulate($pd, %args, 
                           structure => $str->{1}, 
                           condition => $FS->overwrite($condition, $FS->parse("+$split_feature")));
    return $first ?
        $t1 . "<br />\n" . $t0 :
        $t0 . "<br />\n" . $t1;
  }

  # annotate with parsed forms
  unless (defined $str->{order_i}) { # i for "indices"
    my @chunks = split /; /, $str->{order};
    $str->{lengths} = [map scalar split(/ /, $_), @chunks];
    my @fs = map split(/ /, $_), @chunks;
    for (0..$#fs) {
      $fs[$_] =~ /^(!?)(.*)$/;
      $str->{order_r}[$_] = $1 ? 1 : 0;
      $str->{order_i}[$_] = $FS->{feature_index}{$2};
    }

    while (my ($k, $v) = each %{$str->{undefineds}}) {
      $str->{undef_p}{$FS->parse($k)} = $FS->parse($v); # p for 'parsed'
    }
    
    while (my ($k, $v) = each %{$str->{flips}}) {
      my @which = grep $str->{order_i}[$_] == $FS->{feature_index}{$v}, 0..@{$str->{order_i}}-1;
      $str->{flips_p}{$FS->parse($k)} = $which[0];
    }
    
    @fs = split / /, $str->{collapse};
    for my $f (@fs) {
      if (defined $str->{named_collapses}{$f}) {
        push @{$str->{collapse_i}}, $f;
        my $ae = $FS->add_entailments($FS->parse($str->{named_collapses}{$f}{to}));
        my %undefinenda = map(($_ => 1), grep substr($ae, $_, 1) eq 'u', 0..length($ae)-1);
        %{$str->{named_collapses_p}{$f}} = (
            from => table_sortkey($FS->parse($str->{named_collapses}{$f}{from}), $str),
            to => table_sortkey($FS->parse($str->{named_collapses}{$f}{to}), $str),
            undefine => [grep $undefinenda{$str->{order_i}[$_]}, 0..@{$str->{order_i}}-1],
            avoid_unless => table_sortkey($FS->parse($str->{named_collapses}{$f}{avoid_unless}), $str),
        );
      } else {
        my @which = grep $str->{order_i}[$_] == $FS->{feature_index}{$f}, 0..@{$str->{order_i}}-1;
        push @{$str->{collapse_i}}, $which[0];
      }
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
      my $position = table_sortkey $FS->parse($phone), $str, 1;
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

  my @modified_phones = sort keys %table;
  my (%row_moves, %column_moves);
  COLLAPSE: for my $collapse (@{$str->{collapse_i}}) {
    my %new_table;
    if ($collapse =~ /^[0-9]*$/) { # simple collapse
      for my $position (keys %table) {
        substr($position, $collapse, 1) = 1 - substr($position, $collapse, 1);
        next COLLAPSE if defined $table{$position};
      }
      while (my ($position, $v) = each %table) {
        substr($position, $collapse, 1) = '.';
        $new_table{$position} = $v;
      }
    } else { # named collapse
      # don't collapse if:
      # (a) the resulting column doesn't combine anything;
      # (b) there is an obvious contrast;
      # (c) there is a contrast among the features which are set undefined by this
      @_ = grep /^$str->{named_collapses_p}{$collapse}{avoid_unless}$/, keys %table;
      next COLLAPSE unless @_; # (a)
      for my $position (keys %table) { # can't use each, there's a keys inside
        my $v = $table{$position};
        if ($position =~ /^$str->{named_collapses_p}{$collapse}{from}$/) {
          $position = $FS->overwrite($position, $str->{named_collapses_p}{$collapse}{to});
          my $template = $position;
          next COLLAPSE if defined $table{$position}; # (b)

          for my $i (0..length($str->{named_collapses_p}{$collapse}{from})-1) {
            next unless substr($str->{named_collapses_p}{$collapse}{from}, $i, 1) ne '.'
                    and substr($str->{named_collapses_p}{$collapse}{to}, $i, 1) eq 1 - substr($str->{named_collapses_p}{$collapse}{from}, $i, 1);
            for my $jf (@{$FS->{features_requiring}[substr($str->{named_collapses_p}{$collapse}{from}, $i, 1) ^ $str->{order_r}[$i]]
                                             [$str->{order_i}[$i]]}) {
              for my $j (0..$#{$str->{order_i}}) {
                substr($template, $j, 1) = '.', last if $str->{order_i}[$j] == $jf;
              }
            }
            next COLLAPSE if grep(/^$template$/, keys %table) >= 2; # (c)
          }
        }
        $new_table{$position} = $v;
      }

      @modified_phones = map {
        if (/^$str->{named_collapses_p}{$collapse}{from}$/) {          
          my $a = $FS->overwrite($_, $str->{named_collapses_p}{$collapse}{to});
          my $b = $_;
          for my $i (0..length($b)-1) {
            substr($b, $i, 1) = '.' if substr($str->{named_collapses_p}{$collapse}{to}, $i, 1) eq '.';
          }
          $row_moves{substr($a, 0, $str->{lengths}[0])} = substr($b, 0, $str->{lengths}[0])
            unless substr($_, 0, $str->{lengths}[0]) eq substr($a, 0, $str->{lengths}[0]);
          $column_moves{substr($a, $str->{lengths}[0], $str->{lengths}[1])} = substr($b, $str->{lengths}[0], $str->{lengths}[1])
            unless substr($_, $str->{lengths}[0], $str->{lengths}[1]) eq substr($a, $str->{lengths}[0], $str->{lengths}[1]);
          $a;
        } else {
          $_;
        }
      } @modified_phones;
    } # named collapse
    %table = %new_table;
  } # COLLAPSE

  for my $position (keys %table) {
    $rows{substr($position, 0, $str->{lengths}[0])} = 1;
    $columns{substr($position, $str->{lengths}[0], $str->{lengths}[1])} = 1;
    $spots{substr($position, $str->{lengths}[0]+$str->{lengths}[1])} = 1;
  }

  my (%genuine_rows, %genuine_columns);
  for my $row (keys %rows) {
    $genuine_rows{$row} = scalar grep $_ =~ /^$condition$/ && 
        substr(table_sortkey($_, $str), 0, $str->{lengths}[0]) =~ /^$row$/, keys %{$pd->{gen_inventory}};
  }
  for my $column (keys %columns) {
    $genuine_columns{$column} = scalar grep $_ =~ /^$condition$/ &&
        substr(table_sortkey($_, $str), $str->{lengths}[0], $str->{lengths}[1]) =~ /^$column$/, keys %{$pd->{gen_inventory}};
  }

  # Now reinsert digits where we can, so we can use the most appropriate label.
  # (The behaviour of this should be regarded as undefined if a feature
  # is undefined everywhere in the column.  But is it?)
  my @extant_rows = map substr($_, 0, $str->{lengths}[0]), @modified_phones;
  my @extant_columns = map substr($_, $str->{lengths}[0], $str->{lengths}[1]), @modified_phones;
  my $base_enrichment;

  my $table = "<table style=\"text-align: center;\">\n<caption>$str->{caption}</caption>\n";
  $table .= '<tr style="vertical-align: bottom;"><th></th>';
  for my $column (sort keys %columns) {
    $table .= '<td></td>'; # empty cells for separation, yeah
    $base_enrichment = enrich($column, \@extant_columns);
    my $label = '';
    $label = tabulate_label($base_enrichment, 
                            $label_phones{columns}, $label_phones{columns_mod},
                            $labels{columns}, $labels{columns_mod},
                            header => 1,
                            repeat_base => $str->{labels}{repeat_columns})
        if $genuine_columns{$column};
    my %moves_stated; # have we already named this thing being moved in?
    while (my ($k, $v) = each %column_moves) {
      if ($k =~ /^$column$/ and !$moves_stated{$v}) {
        $label .= '&nbsp;/ ' if $label; 
        $label .= tabulate_label($FS->overwrite($base_enrichment, $v),
                                 $label_phones{columns}, $label_phones{columns_mod},
                                 $labels{columns}, $labels{columns_mod},
                                 header => 1,
                                 repeat_base => $str->{labels}{repeat_columns});        
        $moves_stated{$v} = 1;
      }
    }
    $label =~ s/ /<br \/>/g;
    $table .= "<th colspan=\"" . keys(%spots) . "\">" .
              ($label ? "\u$label" : '?') . 
              '</th>'; 
  }

  # code duplication, ick
  $table .= "</tr>\n";
  for my $row (sort keys %rows) {
    $table .= '<tr>';
    $base_enrichment = enrich($row, \@extant_rows);
    my $label = '';
    $label = tabulate_label($base_enrichment, 
                            $label_phones{rows}, $label_phones{rows_mod},
                            $labels{rows}, $labels{rows_mod},
                            repeat_base => $str->{labels}{repeat_rows})
        if $genuine_rows{$row};
    my %moves_stated;
    while (my ($k, $v) = each %row_moves) {
      if ($k =~ /^$row$/ and !$moves_stated{$v}) {
        $label .= '&nbsp;/ ' if $label;
        $label .= tabulate_label($FS->overwrite($base_enrichment, $v),
                                 $label_phones{rows}, $label_phones{rows_mod},
                                 $labels{rows}, $labels{rows_mod},
                                 repeat_base => $str->{labels}{repeat_rows});
        $moves_stated{$v} = 1;
      }
    }
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

  unless ($args{accept_nothing}) {
    return $args{no_nothing} ? '' : ($args{morpho} eq 'plural' ? 'no phones' : 'no phone') 
        if defined $inventory and !grep /^$phone$/, @$inventory;
  }

  $inventory = undef if $args{no_enrich}; # hackish
  my $enriched = defined($inventory) ? enrich($phone, $inventory) : $phone;

  my $str = $args{str};
  my $subtable_index;
  if (!defined $str) {
    $str = $phon_descr->{table_structure};
    while (defined $str->{subtables}) {
      $subtable_index = $FS->{feature_index}{$str->{subtables}}; 
      my $subtable = substr($enriched, $subtable_index, 1);
      if ($subtable eq '.') {
        last unless $args{use_dominant_str};
        $subtable = $str->{dominant};
      }
      $str = $str->{$subtable};
    }
  }
  $enriched = add_false_features $enriched, $str;

  if (defined $str->{subtables}) {
    # We prevent enriching again since it makes the names produces for e.g. classes of 
    # vowels and semivowels be the same, when otherwise all the semivowels might be high 
    # yielding a more specific and thus different name.
    my $phone0 = $enriched;
    substr($phone0, $subtable_index, 1) = '0';
    my $name0 = name_natural_class($phone0, $inventory, %args, str => $str->{0}, no_nothing => 1, no_enrich => 1, significant => $phone);
    my $phone1 = $enriched;
    substr($phone1, $subtable_index, 1) = '1';
    my $name1 = name_natural_class($phone1, $inventory, %args, str => $str->{1}, no_nothing => 1, no_enrich => 1, significant => $phone);

    return $name0 unless $name1;
    return $name1 unless $name0;
    return $name0 if ($name0 eq $name1);

    my $conjunction = $args{morpho} eq 'plural' ? ' and ' : ' or ';

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
        $name0 . $conjunction . $name1; 
  }

  my $scheme = defined $args{scheme} ? $args{scheme} : 'labels';
  # Memoise.
  if (!defined $str->{$scheme}{name_classes}) {
    my (@p, @pmod, @entailed_p, @entailed_pmod, @l, @lmod);
    my %modificate = map(($_ => 1), split / /, $str->{$scheme}{modificate});
    for my $thing (qw/pre_other pre_other_mod columns rows other columns_mod rows_mod other_mod/) {
      next unless defined $str->{$scheme}{$thing};
      $str->{$scheme}{name_classes}{repeat_mod}{$thing} = $str->{$scheme}{'repeat_' . $thing} if $str->{$scheme}{'repeat_' . $thing};
      for (@{$str->{$scheme}{$thing}}) {
        my ($phone, $label) = split /: */;
        $phone = $FS->parse($phone);
        $label .= " [$thing]" if $modificate{$thing};
        if ($label =~ /\[.*\]/) {
          push @pmod, $phone;
          push @entailed_pmod, $FS->add_entailments($phone);
          push @lmod, $label;
        } else {
          push @p, $phone;
          push @entailed_p, $FS->add_entailments($phone);
          push @l, $label;
        }
      }
    }
    $_ = $FS->parse('');
    push @p, $_;
    push @entailed_p, $_;
    push @l, $str->{name}; 
    $str->{$scheme}{name_classes}{p} = \@p;
    $str->{$scheme}{name_classes}{pmod} = \@pmod;
    $str->{$scheme}{name_classes}{entailed_p} = \@entailed_p;
    $str->{$scheme}{name_classes}{entailed_pmod} = \@entailed_pmod;
    $str->{$scheme}{name_classes}{l} = \@l;
    $str->{$scheme}{name_classes}{lmod} = \@lmod;
    while (my ($k, $v) = each %{$str->{$scheme}{eliminate}}) {
      $str->{$scheme}{name_classes}{eliminate}{$FS->parse($k)} = 
          $FS->parse($v);
    }
  }

  # Our handling of "sonorant", for now, is just by swapping it in as a different base name.
  my @l = @{$str->{$scheme}{name_classes}{l}};
  @l[-1] = $args{base} if $args{base};

  my $significant = defined $args{significant} ? $args{significant} : $phone;
  $significant =~ s/u/./g; # undefineds are not significant
  # REFACTOR: there should probably be a single data structure to hold this all.
  my $name = tabulate_label $enriched,
                            $str->{$scheme}{name_classes}{p}, $str->{$scheme}{name_classes}{pmod},
                                                        \@l , $str->{$scheme}{name_classes}{lmod},
                            %args,
                            entailed_p => $str->{$scheme}{name_classes}{entailed_p},
                            entailed_pmod => $str->{$scheme}{name_classes}{entailed_pmod},
                            significant => $significant,
                            nons => !$args{bar_nons},
                            non_inventory => ($args{bar_nons} ? undef : $inventory),
                            negate => $str->{$scheme}{negate},
                            repeat_mod => $str->{$scheme}{name_classes}{repeat_mod},
                            eliminate => $str->{$scheme}{name_classes}{eliminate}; 
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
  return $name; 
}

# Given a list of phones, figure out a good feature-systematic name for it,
# with the minimum complexity in some heuristic sense.
# Roughly, the model is this: minimise the cost, where
# mentioning a phone (as an inclusion or an exclusion) costs one,
# and mentioning a feature costs one plus epsilon,
# except that table-determining features are free ('cause they're necessary for naming).

# $args{within} is a class within which we are to describe this one.
# $args{suppress_ie} suppresses the exemplificatory lists.
# $args{sort_phones} sorts the input reference to a list of phones in the way the output needs.

# If $args{extend} is present, this behaves quite differently:
# rather than trying to name the set of phones, it tries to return the analogous
# subset of $args{extend}.
# It's far from perfect at this: it will ignore exceptions.

sub describe_set {
  my ($orig_phones, $inventory, %args) = (shift, shift, @_);
  my $phones = $orig_phones;
  my $morpho = defined $args{morpho} ? $args{morpho} : 'indef';
  my $extend = defined $args{extend};
  my $lb = $args{etic} ? '[' : '/';
  my $rb = $args{etic} ? ']' : '/';
#print STDERR join(' ', map(name_phone($_,alphabet=>$phonetic_alphabets{CXS}), sort @$orig_phones)) . '  within  ' . 
#             join(' ', map(name_phone($_,alphabet=>$phonetic_alphabets{CXS}), sort @$inventory)) . "\n";
  $args{get_str_pattern}->[0] = '.' x @{$FS->{features}} if defined $args{get_str_pattern}; # extra return hack

  my $pattern = defined $args{within} ? $args{within} : '.' x @{$FS->{features}};
  my $str = $phon_descr->{table_structure}; 
  while (defined $str->{subtables}) {
    my $subtable;
    $subtable = 1 if !grep substr($_, $FS->{feature_index}{$str->{subtables}}, 1) eq '0', @$phones;
    $subtable = 0 if !grep substr($_, $FS->{feature_index}{$str->{subtables}}, 1) eq '1', @$phones;
    last unless defined $subtable;
    substr($pattern, $FS->{feature_index}{$str->{subtables}}, 1) = $subtable;
    $str = $str->{$subtable};
  }
  $args{get_str_pattern}->[0] = $pattern if defined $args{get_str_pattern};

  my $size = @$phones;
  unless ($extend) {
    return $morpho eq 'plural' ? 'no phones' : 'no phone' if ($size == 0);
    return $lb . name_phone($phones->[0]) . $rb if ($size == 1);
  }

  # add_false_features causes a few things, like [?\], to be named wrongly.  
  my %remove_false_features = map((add_false_features($_, $str) => $_), @$inventory);

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
        . " other than $lb" . name_phone($remove_false_features{$complement[0]}) . $rb if ($cosize == 1);
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
    my $extra = $FS->parse($_);
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
      my $new_pattern = $FS->overwrite($pattern, $extra); 
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
      my $new_antipattern = '.' x @{$FS->{features}};
      substr($new_antipattern, $f, 1) = 1 - $usable_value{$f};
      my $trimmed = @comparanda - grep !/^$new_antipattern$/, @comparanda;
      if ($trimmed > $most_trimmed) {
        ($most_trimmed, $best_pattern, $best_antipattern) = ($trimmed, undef, $new_antipattern);
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
  } # main describing loop

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

  if ($size <= $complexity) {
    return ($morpho eq 'plural' ? '' : 'one of ') . 
        $lb . join(' ', map name_phone($remove_false_features{$_}), @$phones) . $rb;
  }
  if ($cosize <= $complexity) {
    return name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) . 
        (@complement ? (" other than $lb" .
          join(' ', map name_phone($remove_false_features{$_}), @complement) . $rb) : ''); 
  }

  # Special negation.  Currently just for sonorant.
  my $base = '';
  my $base_antipattern;
  while (my ($phone, $neg) = each %{$str->{labels}{special_negate}}) {
    for (@antipatterns) {
      ($base, $base_antipattern) = ($neg, $_) if $_ eq $FS->parse($phone);
    }
  }


  my @detritus = grep !defined $phones{$_}, @comparanda;
  # For now, just to avoid egregiosities with negative lists, admit defeat and fall back to
  # a big list if there's too much detritus.  
  if (@detritus > @$phones / 4) { # magic linear function
    return ($morpho eq 'plural' ? '' : 'one of ') . 
        $lb . join(' ', map name_phone($remove_false_features{$_}), @$phones) . $rb;
  }

  my $significant = $pattern;
  if ($args{insignificant}) {
    for (0..length($pattern)-1) {
      substr($significant, $_, 1) = '.' if substr($args{insignificant}, $_, 1) ne '.';
    }
  }

  my $main_name = name_natural_class($pattern, $inventory, str => $str, morpho => $morpho, 
      base => $base, significant => $significant);
  # Get rid of the antipattern in question if it made it in as the base.
  @antipatterns = grep $_ != $base_antipattern, @antipatterns if $base and $main_name =~ /\b$base/; # kluge!

  @_ = grep $pattern, @$inventory; # name antipatterns within matches to the pattern
  my @excluded_names = map name_natural_class($_, \@_, str => $str, morpho => $morpho), @antipatterns;

  # If not suppressed,
  # list either the examples or the nonexamples, depending on which there are more of.
  my $list_examples = ($size <= $cosize);
  my (@base_caught, @antipatterns_caught);
  if (!$list_examples and $args{ie}) {
    my @pool = @complement;
    @base_caught = grep !/^$pattern$/, @pool;
    @pool = grep /^$pattern$/, @pool;
    for my $i (0..$#antipatterns) {
      @{$antipatterns_caught[$i]} = grep /^$antipatterns[$i]$/, @pool;
      @pool = grep !/^$antipatterns[$i]$/, @pool;
      $excluded_names[$i] .= " ($lb" . join(' ', map name_phone($remove_false_features{$_}), @{$antipatterns_caught[$i]}) . "$rb)";
    }
    if (@base_caught) {
      $main_name .= (@detritus or @antipatterns_caught) ? " (i.e. not $lb" : 
          '; i.e. ' . 
          name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) .
          " other than $lb";
      $main_name .= join(' ', map name_phone($remove_false_features{$_}), @base_caught) . $rb;
      $main_name .= ')' if (@detritus or @antipatterns_caught);
    }
    warn 'inconsistency in detritus in describe_set' if @pool != @detritus; 
  }

  my $result = $main_name;
  $result .= ' other than ' if @antipatterns or @detritus;
  if (@antipatterns) {
    $result .= (join ' or ', @excluded_names);
    $result .= ' or ' if @detritus;
  }

  # Trying to name the detritus, or indeed the complement, still might be helpful at this point.
  # But if I'm going to change that, I should also think about recognising groups
  # defined by ors of natural classes, etc.  It's all a bit of a mess.
  # E.g. try the stored examples "horrid(er)-lists".
  #
  # For now, we just tack on the name of all the detritus, if it's simple.
  if (@detritus) {
    my $detritus_name = '';
    if (@detritus > 2 and !$args{bottom_out}) {
      $detritus_name = describe_set([map $remove_false_features{$_}, @detritus], $inventory, %args, 
          get_str_pattern => undef, insignificant => $pattern, bottom_out => 1,
          morpho => ($args{morpho} eq 'bare' ? 'indef' : $args{morpho}), ie => undef);
      $detritus_name = '' if $detritus_name =~ /^[[\/]/ or $detritus_name =~ /other than/; # big kluge
    }
    if ($detritus_name) {
      $detritus_name .= " ($lb" . join(' ', map name_phone($remove_false_features{$_}), @detritus) . "$rb)"
          if (!$list_examples and $args{ie}); # duplicative
      $result .= $detritus_name;
    } else {
      $result .= $lb . join(' ', map name_phone($remove_false_features{$_}), @detritus) . $rb;
    }
  }

  if ($list_examples and $args{ie}) {
    $result .= '; i.e. ' . ($morpho eq 'plural' ? '' : 'one of ') . 
        $lb . join(' ', map name_phone($remove_false_features{$_}), @$phones) . $rb;
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
      $phone = '(' . join(')|(', map($FS->parse($_), split(/\|/, $phone))) . ')';
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
      $elaborations[$pos] = describe_set(\@phones, [keys %{$pd->{gen_inventory}}], ie => 1); 
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
# Not that we do any such folding yet.

# TODO: this needs lots of updating when new rule types arise, for instance
# those that can have multiple effects.

# More current issues:
# - There is a misstatement in 11598455.  It is a fundamental one: the rule as stated is
#   _always_ wrong, given that an assimilation to the _same_ environment interferes with
#   what would otherwise be the resolution of the resulting segment.  
# - In 634146154, nasality spreads across high V but doesn't latch on.  Describe that when it happens?

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
    warn "unimplemented start time in describe_rules!";
  }
  @inventory = grep $_, @inventory;
  %phone_resolutions = (map(($_ => $_), @inventory));

  my %sortkey = map(($_ => table_sortkey($_, $phon_descr->{table_structure})), @inventory);

  RULE: for my $i ($args{start}..$args{end}-1) {
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
    # Try to simplify assimilations, taking advantage of enrichments.
    for my $i (0..length($effect)-1) {
      substr($effect, $i, 1) = substr($pre, $i, 1)
        if substr($effect, $i, 1) eq '<' and substr($pre, $i, 1) ne '.';
      substr($effect, $i, 1) = substr($post, $i, 1)
        if substr($effect, $i, 1) eq '>' and substr($post, $i, 1) ne '.';
    }

    # TODO: put pointless persistent rules into some kind of holding tank,
    # to check on creation of new phones.
    #
    # Actually, quite a lot of stuff for persistent rules potentially needs rewriting 
    # when new phones are around.  Ick.  I'm happy to just ignore this for now.
    #
    # Some rules are being missed; is it this thing's fault?
    my %matcheds;
    for my $displ (keys %{$rule->{precondition}}) {
      @{$matcheds{$displ}} = grep $_ =~ /^$rule->{precondition}{$displ}$/, @inventory;
      if (defined $rule->{except}{$displ}) {
        my @exceptions = split / /, $rule->{except}{$displ};
        for my $exception (@exceptions) {
          @{$matcheds{$displ}} = grep $_ !~ /^$exception$/, @{$matcheds{$displ}};
        }
      }
      # Rules aren't pointless if they trigger _only_ at word boundary.
      unless (@{$matcheds{$displ}} or $rule->{or_pause}{$displ}) {
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
        for (0..length($effect)-1) {
          substr($effect, $_, 1) = substr($matcheds{$locus-1}[0], $_, 1) if substr($effect, $_, 1) eq '<';
        }
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
        for (0..length($effect)-1) {
          substr($effect, $_, 1) = substr($matcheds{$locus+1}[0], $_, 1) if substr($effect, $_, 1) eq '>';
        }
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
    
    # FIXME: this way about it has a great flaw: there may be persistent sequence rules
    # that *always* run after a given rule, but this won't notice them at all.
    my @new_inventory = @inventory;
    my $entailed_effect = $FS->add_entailments($effect);
    my %outcome;
    my $pointless = 1;
    my %frames_examined;
    my @template_set;
    push @template_set, @{$matcheds{$locus-1}} if $entailed_effect =~ /</;
    push @template_set, @{$matcheds{$locus+1}} if $entailed_effect =~ />/;
    push @template_set, '.' x length($entailed_effect) unless @template_set;
    for my $template (@template_set) {
      my $frame = $entailed_effect; # why?  this seems only to be important for antitheticals
      my $frame_is_worthwhile = 0;
      for (0..length($frame)-1) {
        # unlikely issue: not right for both directions at once.   and below
        substr($frame, $_, 1) = substr($template, $_, 1) if substr($frame, $_, 1) =~ /[<>]/;
      }
      next if defined $frames_examined{$frame};
      $frames_examined{$frame} = 1;
 
      for my $phone (@{$matcheds{$locus}}) {
        my $changed = $FS->add_entailments($FS->overwrite($phone, $frame));
        if (!defined $phone_resolutions{$changed}) {
          my $word = [$changed];
          my $expiry = [];
          $pd->run($word,
                   cleanup => $i, 
                   change_record => [change_record($phone, $changed)],
                   track_expiry => $expiry,
                   nopause => 1);
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

    # Exceptionality describing time!  We are comparing to $FS->add_entailments $FS->overwrite($phone, $frame).
    # Note that this doesn't have any particular handling of 
    # "foos do A, except for bar foos, which do B instead".

    my @susceptible;
    my $insusceptibles_exist = 0;
    my %any_nondeviates; # is there any phone which behaves normally?
    my %dev_distilled; # %dev_distilled maps frames to maps from conditions to lists of phones.
    for my $frame (keys %outcome) {
      # %deviations maps deviations to the list of sounds that give them
      my %deviations;

      # Collect the deviations.
      for my $phone (@{$matcheds{$locus}}) {
        my $susceptible = 0;
        my $outcome = $outcome{$frame}{$phone};
        if ($outcome =~ / /) { 
          warn "multiple sound outcome in finding deviations!"; # FIXME: multiple sound outcomes
        }
        my $changed = $FS->add_entailments($FS->overwrite($phone, $frame)); # duplicative :-/
        if (length($outcome)) { # one phone
          # Just filling in undefineds isn't good enough here, since it's not good enough below.
          for (0..length($outcome)-1) {
            substr($outcome, $_, 1) = '.' if substr($outcome, $_, 1) eq substr($changed, $_, 1)
                                          or substr($changed, $_, 1) eq 'u';
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
          for my $dev2 (keys %deviations) { # a member of the up-set of D
            if ($dev2 =~ /^$dev$/ or length($dev2) == 0) {
              push @covered, grep defined($extension{$_}), @{$deviations{$dev2}};
              if (length($dev2) > 0) { 
                # This aspect of the deviation is handled.  Don't remark on it again.
                my $stripped_dev2 = $dev2;
                for my $f (0..length($dev2)-1) {
                  if (substr($dev2, $f, 1) eq substr($dev, $f, 1)) {
                    substr($stripped_dev2, $f, 1) = '.';
                  }
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

          push @{$dev_distilled{$frame}{$dev}}, @covered unless $only_undefineds;
        } # dev
      } while ($any_deviations);

    } # frame

    # Merge deviations with identical effects across frames.  
    # We merge two at a time, but since the feature system is binary we can get away with that.
    my @frames_to_merge = keys %dev_distilled;
    for (my $i = 1; $i < @frames_to_merge; $i++) {
      my $f0 = $frames_to_merge[$i];
      next unless keys %{$dev_distilled{$f0}};
      MERGE_DEV_J: for (my $j = 0; $j < $i; $j++) {
        my $f1 = $frames_to_merge[$j];
        next unless keys %{$dev_distilled{$f1}};

        my $union = $f0;
        for (0..length($f1)-1) {
          substr($union, $_, 1) = '.' if substr($union, $_, 1) ne substr($f1, $_, 1);
        }
        # Don't merge if it would falsely subsume other frames.
        for my $f (grep /^$union$/, keys %outcome) { 
          next MERGE_DEV_J unless $f =~ /^$f0$/ or $f =~ /^$f1$/;
        }

        my $added_this_merge = 0;
        for my $dev (keys %{$dev_distilled{$f0}}) {
          next unless defined $dev_distilled{$f1}{$dev};
          for (my $k = $#{$dev_distilled{$f0}{$dev}}; $k >= 0; --$k) {
            my $phone = $dev_distilled{$f0}{$dev}[$k];
            if (grep $_ eq $phone, @{$dev_distilled{$f1}{$dev}}) {
              unless ($added_this_merge) {
                push @frames_to_merge, $union;
                $added_this_merge = 1;
              }
              push @{$dev_distilled{$union}{$dev}}, $phone;
              @{$dev_distilled{$f1}{$dev}} = grep $_ ne $phone, @{$dev_distilled{$f1}{$dev}};
              splice @{$dev_distilled{$f0}{$dev}}, $k, 1;
            }
          }
          delete $dev_distilled{$f0}{$dev} unless @{$dev_distilled{$f0}{$dev}};
          delete $dev_distilled{$f1}{$dev} unless @{$dev_distilled{$f1}{$dev}}; 
        }
      } # j
    } # i
    for (keys %dev_distilled) {
      delete $dev_distilled{$_} unless keys %{$dev_distilled{$_}};
    }

    # Start to prepare the textual description.  First, what the change does.

    # Some of the features may appear redundant to list in the rule, given the current inventory.
    # But I leave them, just so that there isn't another thing to revise when persistence happens.
    my $simple_effect = $effect;
    $simple_effect =~ y/<>/../;
    my $modified = $FS->add_entailments($FS->overwrite($precondition, $simple_effect)); 
    # $FS->add_entailments there seems necessary to wipe out things that are forced undefined 
    # when deviations allow them to be defined again.  I hope it doesn't break anything else.

    my $text = '';

    # Describe the deviations.
    my $deviation_texts = '';
    my $frames_start_with_PP = ($effect =~ /[<>]/);
    my $all_all_deviates = 1;
    my %kept_deviations; # maps frame to a list
    for my $frame (keys %outcome) {
      $all_all_deviates = 0 if $any_nondeviates{$frame};
    } 
    for my $frame (keys %dev_distilled) {
      keys %{$dev_distilled{$frame}}; # reset each()
      while (my ($deviation, $all_deviants) = each %{$dev_distilled{$frame}}) {
        next unless grep $outcome{$frame}{$_} ne $_, @$all_deviants;
        push @{$kept_deviations{$frame}}, $deviation;
      }
    }

    # If all frames have all deviates, we don't want to use the default complement.
    # If there is just one deviation, use it instead.
    # If there is more than one, even after frame-merging and list-consolidation,
    # it is best just not to have a main VP or subject, just the environment PP there.
    my $no_main_VP = 0;
    if (keys %kept_deviations and $all_all_deviates) {
      if (keys %kept_deviations <= 1) {
        @_ = keys %kept_deviations;
        my $frame = $_[0];
        if (@{$kept_deviations{$frame}} <= 1) {
          my $dev = $kept_deviations{$frame}[0];
          if ($dev ne '') { # deviation can be deletion
            $modified = $FS->overwrite($modified, $dev);
          } else {
            $modified = '';
          }
        } else {
          $no_main_VP = 1;
        }
      } else {
        $no_main_VP = 1;
      }
    }

    for my $frame (keys %kept_deviations) {
      # This frame might be a consolidated one.  We need to make an actual representative of it
      # to look things up in %outcome.
      my $frame_representative;
      for (keys %outcome) {
        $frame_representative = $_, last if /^$frame$/;
      }

      my $frame_text = '';
      if ($effect =~ /[<>]/) {
        # $frame can contain constant features from the change, as well as features from
        # assimilation.  We only want the latter here.  
        # Modifying the below to handle bidirectional assimilation is straightforward;
        # I just haven't bothered since we generate no bidirectional assimilation yet.
        $frame_text = $effect !~ />/ ? 'After ' : 
                        ($effect !~ /</ ? 'Before ' : 'Assimilating to ');
        my $phone = $frame;
        for (0..length($phone)-1) {
          substr($phone, $_, 1) = '.' unless substr($effect, $_, 1) =~ /[<>]/;
        }
        my $phone = $FS->overwrite(($effect !~ />/ ? $old_pre : 
                        ($effect !~ /</ ? $old_post : '.' x @{$FS->{features}})), $phone);
        @_ = grep /^$phone$/, @inventory;
        my @exceptions;
        push @exceptions, split / /, $rule->{except}{$locus-1} if $effect =~ /</;
        push @exceptions, split / /, $rule->{except}{$locus+1} if $effect =~ />/;
        for my $phone (@exceptions) {
            @_ = grep $_ !~ /^$phone$/, @_;
        }
        $frame_text .= describe_set(\@_, \@inventory, morpho => 'indef', bar_nons => 1, etic => 1); 
            # disallowing nons isn't right, but it makes the thing readable
        # okay, so just defined($rule->{pause_phone}) isn't what I look for elsewhere, but it should hackwork
        $frame_text .= ' or pause' if defined $rule->{pause_phone} and $rule->{pause_phone} =~ /^$frame$/;
        $frame_text .= ', ';
      }

      my %appeared_in_a_list = ();
      DEVIATION: for my $deviation (sort {@{$dev_distilled{$frame}{$b}} <=> @{$dev_distilled{$frame}{$a}}} 
                             @{$kept_deviations{$frame}}) {
        my $all_deviants = $dev_distilled{$frame}{$deviation};
        my @deviants = grep $outcome{$frame_representative}{$_} ne $_, @$all_deviants;
        my @get_str_pattern;

        my @undescribed_deviants = grep !defined $appeared_in_a_list{$_}, @deviants;
        next DEVIATION unless @undescribed_deviants;

        my $subject = describe_set(\@deviants, $no_main_VP ? \@inventory : \@susceptible,
            morpho => 'plural', etic => 1, sort_phones => 1, get_str_pattern => \@get_str_pattern);
        # if the subject is a list, redo, dropping things that have already appeared in some list
        my $subject_is_list = ($subject =~ /^\[.*\]$/); # klugy
        if ($subject_is_list) { 
          @deviants = @undescribed_deviants;
          $subject = describe_set(\@deviants, $no_main_VP ? \@inventory : \@susceptible,
              morpho => 'plural', etic => 1, sort_phones => 1, get_str_pattern => \@get_str_pattern);
              # duplicated code, ick
        }
        $frame_text .= $subject;

        if (length($deviation) > 0) {
          # Check whether some assimilation is left, because frames have been merged.
          my $framed_effect = $FS->overwrite($effect, $frame);
          my $assimilation_left = ($framed_effect =~ /[<>]/);

          # If the subject is a list, make the object one too, unless it's entirely deletions.
          # For deviations, it's less offputting to make the lists long.
          if ($subject_is_list and !$assimilation_left) { 
            %appeared_in_a_list = (%appeared_in_a_list, map(($_ => 1), @deviants));

            if (grep $outcome{$frame_representative}{$_}, @deviants) {
              $frame_text .= ' become [' . join(' ', map spell_out_spaces($outcome{$frame_representative}{$_}, null => 1), @deviants) . ']';
            } else {
              $frame_text .= ' are deleted';
            }
          } else {
            # Here again we aren't handling bidirectional assimilation right.
            my $assimilation_text = '';
            if ($assimilation_left) {
              my @taken_care_of;
              $_ = $effect;
              y/01u<>/...11/;
              my $significant = $framed_effect;
              $significant =~ y/01u<>/...11/;              
              $assimilation_text = ' and assimilate in ' .
                  name_natural_class($_, undef, scheme => 'nominalised', nobase => 1, 
                      str => get_str($get_str_pattern[0]),
                      significant => $significant, taken_care_of => \@taken_care_of); 

              # This in particular rids $framed_effect of all <>s.
              for (0..length($frame)-1) {
                substr($framed_effect, $_, 1) = '.' if $taken_care_of[$_];
              }
            }

            # Reintroduce parts of the deviation that necessarily occurred for everything in this set,
            # but have been distilled out.  It seems a waste to do this here,
            # but I didn't manage to figure out how to do this correctly during distillation
            # (since we do want to genuinely lose the things we lose there).
            my $necessary_deviations = '.' x length($deviation);
            for my $deviation2 (@{$kept_deviations{$frame}}) {
              unless (grep {
                my $a = $_;
                !grep $_ eq $a, @{$dev_distilled{$frame}{$deviation2}}
              } @deviants) {
                $necessary_deviations = $FS->overwrite($necessary_deviations, $deviation2);
              }
            }
            
            # We need accept_nothing => 1 so that weird things in the frame don't make the
            # namer think impossible things are going on.  
            my $phone = $FS->overwrite($FS->overwrite($FS->overwrite($precondition, $framed_effect), 
                $get_str_pattern[0]), $necessary_deviations);

            # Constant features of the frame which came from assimilations in the effect are still insignificant.
            my $significant;
            if ($no_main_VP) {
              $significant = $FS->overwrite($precondition, $framed_effect);
              for (0..length($significant)-1) {
                substr($significant, $_, 1) = '.' if substr($effect, $_, 1) =~ /[<>]/;
              }
              $significant = $FS->overwrite($FS->overwrite($significant, $get_str_pattern[0]), $necessary_deviations);
            } else {
              $significant = $deviation;
            }

            $_ = name_natural_class($phone, \@new_inventory,
                significant => $significant,
                morpho => 'plural', nobase => 1, accept_nothing => 1, use_dominant_str => 1); 
            $frame_text .= " become $_";
            $frame_text .= $assimilation_text if $assimilation_text;
          }
        } else { # deviation is length 0
          $frame_text .= ' are deleted';
        }
        $frame_text .= '; ';
      }
      $frame_text = substr($frame_text, 0, -2) if $frame_text =~ /; $/; # eh
      
      $deviation_texts .= '. ' . ucfirst $frame_text;
    } # frame

    my $main_clause = '';
    # It is friendliest not to describe rules which survive till before the _next_ rule as persistent.
    # (Is my interpretation subject to an  off-by-one error?)
    my $persistent = !(defined $rule->{inactive} and $rule->{inactive} <= $i + 1);
    my @get_str_pattern;

    # For impersistent rules, no point favouring a featural description to a list.
    if ($insusceptibles_exist or !$persistent) {
      # Note that sounds excluded by {except}{$locus} are already outside of \@susceptible.
      $main_clause .= describe_set(\@susceptible, \@inventory, within => $precondition, 
          morpho => 'plural', etic => 1, sort_phones => 1, get_str_pattern => \@get_str_pattern);
    } else {
      $main_clause .= name_natural_class($precondition, \@inventory, morpho => 'plural');
      $get_str_pattern[0] = str_part $precondition;
    }
    $modified = $FS->overwrite($get_str_pattern[0], $modified);
    my $subject_is_list = ($main_clause =~ /^\[.*\]$/); # klugy
    my $both_are_lists = 0; # don't need deviations if both subj and obj are lists
    my @example_sounds;
    my $example_ellipsis = '';
    unless ($subject_is_list) {
      # Prepare the examples.
      # Give all the examples if there are at most 4, and 3 representative ones otherwise.
      # To consider: if there are enough deviates, just give everything as an example
      # and skip the deviate descriptions?
      %_ = ();
      @susceptible = map $_{$_} ? () : ($_{$_} = 1 && $_), @susceptible; # uniq, keeping first instances
      if (@susceptible <= 4) {
        @example_sounds = @susceptible;
      } else {
        # Can we be more representative here?  Should we avoid naming deviates?
        @example_sounds = @susceptible[0, @susceptible/3, 2*@susceptible/3]; 
        $example_ellipsis = ' ...';
      }
      my $braces = ($main_clause =~ /\]/);
      $main_clause .= ', i.e.' if $braces;
      $main_clause .= ' [' . join(' ', map name_phone($_), @example_sounds)
          . "$example_ellipsis]";
      $main_clause .= ',' if $braces;
    }
    
    $main_clause .= ' persistently' if $persistent;

    if ($modified eq '') { # this only happens if overwritten by a deviation
      $main_clause .= ' are deleted';
    } else {
      my $main_VP;
      if ($effect =~ /[01]/) {
        my $any_nondeletions = 0;
        FRAME_ANY_NONDELETIONS: for my $frame (keys %outcome) {
          for my $phone (@susceptible) {
            $any_nondeletions = 1, last FRAME_ANY_NONDELETIONS if $outcome{$frame}{$phone} ne '';
          }
        }
        unless ($any_nondeletions) {
          $main_VP .= ' are deleted';
        } else {
          my ($frame) = keys %outcome;        
          $main_VP .= ' become ';
          # if the subject is a _short_ list, make the complement one too
          if ($subject_is_list and @susceptible <= 2 and scalar keys %outcome <= 1) { 
            $both_are_lists = 1;
            $main_VP .= '[' . join(' ', map spell_out([split ' ', $outcome{$frame}{$_}], null => 1), @susceptible) . ']';
          } else {
            if ($main_VP =~ / and /) {
              $main_VP .= ' respectively'; 
            }
            # There might be dots in the frame which have come from assimilations in the effect
            # after frame mergers.  We want these to stomp on $modified.
            for (0..length($modified)-1) {
              substr($modified, $_, 1) = '.' if substr($frame, $_, 1) eq '.' and substr($effect, $_, 1) =~ /[<>]/;
            }
            $main_VP .= name_natural_class($modified, \@new_inventory, significant => $simple_effect, 
                morpho => 'plural', nobase => 1, accept_nothing => 1, use_dominant_str => 1);
          }
          # examples
          if (keys %outcome <= 1 and !$subject_is_list) {
            $main_VP .= ' [' . join(' ', map spell_out_spaces($outcome{$frame}{$_}, null => 1), @example_sounds)
                . "$example_ellipsis]";
          }
        }
      } # $effect =~ /[01]/
      if ($effect =~ /</) {
        $_ = $effect;
        y/01u<>/...1./;
        $_ = $FS->overwrite(str_part(enrich($precondition,\@inventory)), $_);
        $main_VP .= ' and' if $main_VP;
        $main_VP .= ' assimilate in ' .
            name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
        if (!defined($old_post) and !$far) {
          $main_VP .= ' to a preceding ';
          @_ = grep /^$pre$/, @inventory;
          for my $phone (split / /, $rule->{except}{$locus-1}) {
            @_ = grep $_ !~ /^$phone$/, @_;
          }
          $main_VP .= describe_set(\@_, \@inventory, morpho => 'bare', etic => 1);
          $pre = undef;
        } else {
          $main_VP .= ' to the previous phone';
        }
        if ($rule->{or_pause}{$locus-1}) { # word-initial
          my $pausal_effect = $effect;
          for (0..length($pausal_effect)-1) {
            substr($pausal_effect, $_, 1) = substr($rule->{pause_phone}, $_, 1)
                if substr($pausal_effect, $_, 1) eq '<';
          }
          my $modified = $FS->overwrite($precondition, $pausal_effect); 
          $modified =~ s/u/./g;
          $main_VP .= ' and become ' . 
                      name_natural_class($modified, \@new_inventory, significant => $pausal_effect, morpho => 'plural', nobase => 1) .
                      ' word-initially';
        }
      }
      if ($effect =~ />/) {
        $_ = $effect;
        y/01u<>/....1/;
        $_ = $FS->overwrite(str_part(enrich($precondition,\@inventory)), $_);
        $main_VP .= ' and' if $main_VP;
        $main_VP .= ' assimilate in ' .
            name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
        if (!defined($old_pre) and !$far) {
          $main_VP .= ' to a following ';
          @_ = grep /^$post$/, @inventory;
          for my $phone (split / /, $rule->{except}{$locus+1}) {
            @_ = grep $_ !~ /^$phone$/, @_;
          }
          $main_VP .= describe_set(\@_, \@inventory, morpho => 'bare', etic => 1);
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
          my $modified = $FS->overwrite($precondition, $pausal_effect); 
          $modified =~ s/u/./g;
          $main_VP .= ' and become ' . 
                      name_natural_class($modified, \@new_inventory, significant => $pausal_effect, morpho => 'plural', nobase => 1) .
                      ' word-finally';
        }
      }
      $main_clause .= $main_VP;
    } # modified is not deletion

    my $environment_text = '';
    my ($pre_text, $post_text);
    my ($no_segmental_pre, $no_segmental_post);
    # the 'or word-finally' aren't quite right, since the main rule might be a between.
    if (defined $pre) {
      $pre_text = name_natural_class($pre, \@inventory, morpho => 'indef', no_nothing => 1);
      $no_segmental_pre = 1 unless $pre_text;
      my @exceptions = split / /, $rule->{except}{$locus-1};
      my @exception_texts = map name_natural_class($FS->overwrite($precondition, $_), 
              \@inventory, significant => $_, no_nothing => 1, morpho => 'indef'), 
          # don't state exceptions that don't actually exclude anything
          grep { my $a = $_; grep /^$a$/ && /^$pre$/, @inventory; } @exceptions;
      @exception_texts = grep $_, @exception_texts;
      $pre_text .= ' except for ' . join ' and ', @exception_texts if @exception_texts;
      $pre_text .= ',' if (scalar @exception_texts) and $rule->{or_pause}{$locus-1};
      $pre_text .= ($pre_text ? ' or ' : '') . 'word-initially' if $rule->{or_pause}{$locus-1};
    }
    if (defined $post) {
      $post_text = name_natural_class($post, \@inventory, morpho => 'indef', no_nothing => 1);
      $no_segmental_post = 1 unless $post_text;
      my @exceptions = split / /, $rule->{except}{$locus+1};
      my @exception_texts = map name_natural_class($FS->overwrite($precondition, $_), 
              \@inventory, significant => $_, no_nothing => 1, morpho => 'indef'), 
          # don't state exceptions that don't actually exclude anything
          grep { my $a = $_; grep /^$a$/ && /^$post$/, @inventory; } @exceptions;
      @exception_texts = grep $_, @exception_texts;
      $post_text .= ' except for ' . join ' and ', @exception_texts if @exception_texts;
      $post_text .= ',' if (scalar @exception_texts) and $rule->{or_pause}{$locus+1};
      $post_text .= ($post_text ? ' or ' : '') . 'word-finally' if $rule->{or_pause}{$locus+1};
    }
    if ($no_segmental_pre) {
      $environment_text .= " $pre_text";
      $pre = undef;
    }
    if ($no_segmental_post) {
      $environment_text .= " $post_text";
      $post = undef;
    }
    if (defined $pre and defined $post) {
      $environment_text .= " between $pre_text and $post_text";
    } elsif (defined $pre) {
      $environment_text .= " after $pre_text";
    } elsif (defined $post) {
      $environment_text .= " before $post_text";
    }
    if ($far) {
      $environment_text .= ' under some conditions on nonadjacent phones'; # FIXME
    }
    # Again, rules which survive one rule shouldn't be described as persistent.
    if (defined $rule->{inactive} and $rule->{inactive} > $i + 1) {
      $to_be_numbered{$i} = 1;
      push @{$descriptions{$rule->{inactive}}{pre}}, "Rule ($i) becomes inactive.";
    }

    # FIXME: "persistently" is lost if $no_main_VP
    $text .= $main_clause unless $no_main_VP;
    $text .= $environment_text unless $no_main_VP and $frames_start_with_PP;
    if ($deviation_texts) {
      if ($no_main_VP) {
        $deviation_texts = ($text ? ', ' : '') . lcfirst substr($deviation_texts, 2);
      }
      $text .= $deviation_texts unless !$no_main_VP and ($all_all_deviates or $both_are_lists);
    }

    $text =~ s/^ *//;
    $descriptions{$i}{rule} = ucfirst $text . '. ';

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
-D              Show some debugging output.

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
unless (grep $_ eq $yamlimpl, qw(YAML::XS YAML::Syck YAML::Perl)) {
  print STDERR <<END;
Warning: your YAML implementation might not like the data files.
YAML::Syck and YAML::XS work.  So does YAML::Perl, though it's mighty slow.
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

$FS = FeatureSystem::load_file('features.yml');

$phonetic_alphabets{CXS} = YAML::Any::LoadFile('CXS.yml') if -f 'CXS.yml';
$phonetic_alphabets{IPA} = YAML::Any::LoadFile('IPA_HTML.yml') if -f 'IPA_HTML.yml';
for my $alphabet (values %phonetic_alphabets) {
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
}

if (defined $phone_to_interpret) {
  $phone_to_interpret = $FS->parse($phone_to_interpret, undefined => 1) unless $phone_to_interpret =~ /^[.01u]*$/;
  print '[' . name_phone($phone_to_interpret) . '] ' . $FS->feature_string($phone_to_interpret);
  $phone_to_interpret =~ /[01]/g;
  print '   ' . (pos($phone_to_interpret) - 1) if defined pos($phone_to_interpret);
  print "\n";
  exit 0;
}

print STDERR "seed $seed\n" if $verbose; 
srand $seed; 

my $pd;

if (defined $infile) {
  $pd = YAML::Any::LoadFile($infile);
} else {
  $pd = Phonology::generate;
}

$phon_descr = YAML::Any::LoadFile('phon_descr.yml');

if ($show_inventory) {
  print describe_inventory($pd, html => $use_html); 
}

if (defined $outfile) {
  if (defined $humane_output) {
    for my $rule (@{$pd->{phone_generator}}, @{$pd->{phonology}}) {
      for my $displ (keys %{$rule->{precondition}}) {
        $rule->{precondition_humane}{$displ} = $FS->feature_string($rule->{precondition}{$displ}, 1);
      }
      for my $displ (keys %{$rule->{effects}}) {
        $rule->{effects_humane}{$displ} = $FS->feature_string($rule->{effects}{$displ}, 1);
      }
    }
    $pd->{phonology}[$_]{number} = $_ for 0..@{$pd->{phonology}}-1;
  }
  YAML::Any::DumpFile($outfile, $pd);
}

if ($show_all) {
  tabulate($pd, annotate_only => 1); # should this be given a name of its own?
  my ($template, $elaborations) = describe_syllable_structure $pd, html => $use_html;
  if ($use_html) { 
    print CGI::h2('Syllable structure'),
          CGI::p(join '', @$template),
          CGI::p(join '<br />', @$elaborations);
  } else {
    print "\nSyllable structure: " . join('', @$template) . "\n"; # not well formatted at present, eh
    print join "\n", @$elaborations;
    print "\n\n";
  }

  print STDERR "describing rules...\n" if $verbose;
  my $rules = describe_rules $pd;
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
  my $word = $pd->generate_form(12); # magic entropy value
  my $surface_word;
  my $generated_word = [@$word];
  if (defined $canonicalise) {
    ($surface_word, $word) = $pd->canonicalise_phonemic_form($generated_word);
  } else {
    $surface_word = [@$word];
    $pd->run($surface_word, start => $pd->{start_sequences}); 
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
      print $FS->feature_string($phone), "\n" if /\#\#/;
    }
  }
}

if ($use_html and $num_words > 0) {
  print CGI::end_table();
}

if ($use_html) {
  print CGI::p({-style => 'font-size: small;'},
                "Generated by <a href=\"https://github.com/alexfink/random_language/tree/master/phonology\">Gleb</a>",
                "version $version / $FS->{version} ",
                $infile ? "from the file $infile." : "with seed $seed.");
  print CGI::end_html;
}




