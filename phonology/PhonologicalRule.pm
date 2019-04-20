package PhonologicalRule;
use strict;
use constant INF => 9**9**9; # is there really nothing sensible better?

# Each rule is a hash.  In the simplest case, it has hashes that
# contain hashes with keys including {condition} and {effects},
# giving the before and after of the rule in this position.
# The top-level keys are relative indices into the word, and these are always integers
# starting from 0 and sequentially increasing.  (So e.g. $rule->{0}{condition} exists.)
#
# Hashes with keys {condition} and {except} are used elsewhere to specify phone classes too.
#
# Other kinds of effects include {deletions}.

# Dump this rule without the feature system.
sub debug_dump {
  my $self = shift;
  my $a = { %$self };
  delete $a->{FS};
  YAML::Any::Dump($a);
}

# Return the indices for this rule.  If passed an argument, return the indices that have that datum.
sub indices {
  my $self = shift;
  if (@_) {
    return grep((/^-?[0-9]*$/ and defined $self->{$_}{$_[0]}), keys %$self);
  } else {
    return grep /^-?[0-9]*$/, keys %$self;
  }
}

# Make a copy of this rule which deeply copies the indexed parts.
sub deep_copy_indexed {
  my $self = shift;
  my $a = { %$self };
  bless $a;
  for my $i (grep /^-?[0-9]*$/, keys %$self) {
    $a->{$i} = { %{$self->{$i}} };
    bless $a->{$i}, 'PhoneSet';
    delete $a->{$i}{condition_ar};
    delete $a->{$i}{outcome};
  }
  if (defined $self->{filter}) {
    $a->{filter} = { %{$self->{filter}} };
    bless $a->{filter}, 'PhoneSet';
  }
  delete $a->{broken_tags};
  $a;
}

# Choose randomly from a hash giving weight distribution.  (Not called everywhere it might be, yet.)
# Class method.
sub weighted_one_of {
  my $sum = 0; 
  $sum += $_[2*$_+1] for 0..@_/2-1;
  $sum = rand $sum;
  # Sort again, just in case the argument really was a hash
  # (because it'll be randomly ordered, breaking determinism when the seed is constant).
  my %distribution = @_;
  for my $a (sort keys %distribution) {
    return $a if ($sum - $distribution{$a}) < 0;
  }
}



# Return a skeletal rule with essentially nothing in it.  Class method.
sub skeletal_rule {
  my $FS = shift;
  my $rule = {
    0 => {condition => $FS->parse(''), effects => $FS->parse('')},
    FS => $FS,
  };
  bless $rule;
}

# Memoise a rule for the computations performed in feeds(!).
# These things can be totally stripped out once the phonology is finalised. 
sub feed_annotate {
  my $rule = shift;
  my $FS = $rule->{FS};
  for my $displ ($rule->indices('condition')) {
    $rule->{$displ}{condition_ar} = $FS->add_requirements($rule->{$displ}{condition});
    if (defined $rule->{$displ}{effects}) {
      $rule->{$displ}{outcome} = $FS->overwrite($FS->add_requirements($rule->{$displ}{condition}), $rule->{$displ}{effects});
      $rule->{$displ}{outcome} =~ s/[<>]/./g;
    }
  }
}

sub strip_feed_annotation {
  my $rule = shift;
  for my $i ($rule->indices()) {
    delete $rule->{$i}{condition_ar};
    delete $rule->{$i}{outcome};
  }
}

# Given two rules ri, rj, can the execution of ri cause rj to be applicable
# where it wasn't before?  This can yield false positives.

# This had been somewhat of a bottleneck, but I haven't checked for a long time.

# TODO: account for except.

sub feeds {
  my ($ri, $rj, %args) = (shift, shift, @_);
  my $FS = $ri->{FS}; # shd check that they're the same

  # TODO: update this as we get new rule types
  # an insertion means we have to look at everything (whereas a fission might be okay with less); etc.
  return 1 if scalar $ri->indices('deletions') and 
      (scalar $ri->indices('condition') > 1) or scalar $ri->indices('or_pause');

  for my $i_displ ($ri->indices('effects')) {
    for my $j_displ ($rj->indices('condition')) {
      # this is costly enough that it's slightly worth putting it in here.  klugily, assume index 0 has a condition
      $ri->feed_annotate() if !defined $ri->{0}{condition_ar};
      $rj->feed_annotate() if !defined $rj->{0}{condition_ar};
      next if !$FS->compatible($ri->{$i_displ}{outcome}, $rj->{$j_displ}{condition_ar});

      # We might have rules which unnecessarily set features identically
      # to their precondition (antithetical, I'm thinking of you); this can't feed, of course.
      for my $f (0..@{$FS->{features}}-1) {
        if (substr($ri->{$i_displ}{effects}, $f, 1) =~ /[<>]/ and
            substr($rj->{$j_displ}{condition}, $f, 1) ne '.') {
          # Don't count this assimilation as feeding another rule if it can't create the precondition
          # _in a word that lacked it before_.  
          my $effects = $ri->{$i_displ}{effects};
          if ($effects =~ />/ and $effects =~ /</) {
            return 1 unless defined $args{pairs};
            push @{$args{pairs}}, [$i_displ, $j_displ]; next;
          }
          my $direction = ($effects =~ />/) ? 1 : -1;
          my $condition = $rj->{$j_displ}{condition};
          my $trigger = '.' x @{$FS->{features}};
          for (0..$#{$FS->{features}}) {
            substr($trigger, $_, 1) = substr($condition, $_, 1) if substr($effects, $_, 1) =~ /[<>]/;
          }
          $trigger = $FS->add_entailments($FS->add_requirements($FS->intersect($trigger, $ri->{$i_displ+$direction}{condition})));
          next if $trigger =~ /^$condition$/;

          return 1 unless defined $args{pairs};
          push @{$args{pairs}}, [$i_displ, $j_displ]; next;
        }
        # or, this rule could force the assimilation to apply again.  kluge out undefineds
        if (substr($rj->{$j_displ}{effects}, $f, 1) =~ /[<>]/ and
            substr($ri->{$i_displ}{condition}, $f, 1) !~ /[.u]/) {
          return 1 unless defined $args{pairs};
          push @{$args{pairs}}, [$i_displ, $j_displ]; next;
        }
        if (substr($ri->{$i_displ}{effects}, $f, 1) eq 
              substr($rj->{$j_displ}{condition}, $f, 1) and
            substr($ri->{$i_displ}{effects}, $f, 1) ne 
              substr($ri->{$i_displ}{condition}, $f, 1) and 
            substr($ri->{$i_displ}{effects}, $f, 1) ne '.') {
          return 1 unless defined $args{pairs};
          push @{$args{pairs}}, [$i_displ, $j_displ]; next;
        }

      }
    }
  }
  
  return 0 unless defined $args{pairs};
  return @{$args{pairs}} ? 1 : 0;
}

# Two rules conflict if they feed each other at the same displacement and their
# outcomes are incompatible.

sub conflicts_with {
  my ($ri, $rj, %args) = (shift, shift, @_);
  my $FS = $ri->{FS}; # shd check if they're the same
  my (@pij, @pji);
  return 0 unless $ri->feeds($rj, pairs => \@pij) and $rj->feeds($ri, pairs => \@pji);
  for my $dij (@pij) {
    for my $dji (@pji) {
      my ($i, $j) = ($dij->[0], $dji->[0]);
      next unless $i eq $dji->[1] and $j eq $dij->[1];
      @{$args{indices}} = ($i, $j) if defined $args{indices};
      return 1 if !$FS->compatible($ri->{$i}{effects}, $rj->{$j}{effects});
    }
  }
  return 0;
}



# Test a hash of phonesets against a word, at given displacement $i.
#
# If $args{context_dependent} is defined (assumed to be a hash), 
# and this word could come to match (or fail to) if it were prefixed 
# or suffixed, record this fact there, under the key ''.  
# (Assumed to be use with nopause.)

sub matches_word {
  my ($self, $word, $i, %args) = (shift, shift, shift, @_);
  my $context_dependent = undef;
  for my $displ ($self->indices()) {
    if ($i + $displ < 0 or $i + $displ >= @$word) {
      next if !$args{nopause} and defined $self->{$displ}{or_pause};
      if (defined $self->{$displ}{condition}) {
        return 0 unless defined $args{context_dependent} and 
          !(defined $self->{$displ}{effects} or defined $self->{$displ}{deletions});
        $context_dependent = 1;
      }
      next;
    }
    return 0 unless PhoneSet::matches($self->{$displ}, $word->[$i+$displ]);
  }
  if ($context_dependent) { # we haven't already returned thanks to a mismatch...
    $args{context_dependent}{''} = [@$word] if scalar $self->indices() >= 2;
    return 0;
  }
  return 1;
}



# Return a left-right reversed copy of this rule.
# TODO: this should reverse outcomes internally.
sub reverse_rule {
  my $self = shift;
  my $a = { %$self };
  bless $a;
  my @indices = $self->indices();
  my $max = -1 * INF;
  for (@indices) {
    delete $a->{$_};
    $max = $_ if $_ > $max;
  }
  for (@indices) {
    $a->{$max - $_} = $self->{$_};
  }
  for ($a->indices()) {
    if (defined $a->{$_}{effects}) {
      $a->{$_}{effects} =~ y/<>/></;
    }
  }
  if (defined $self->{direction}) {
    $a->{direction} = -$self->{direction};
  } else {
    $a->{direction} = -1;
  }
  $a;
}



# If %args includes a list {changes}, tags describing the particular changes caused
# will be pushed.  These tags are:
# "c $v $f" -- feature $f was changed to value $v
# "d" -- a segment was deleted
# "r" -- a segment was replicated (as prelude to some kind of breaking or metathesis)

# HERE: the current big thing in progress is implementing changes which epenthesise.
# I currently envision doing these entirely by multiplying the outcomes of a single phone.
# After this is finished, don't forget the various collision-detecting machinery!
# Furrther down the line, can deletions be eliminated as a special kind of thing?

sub run {
  my ($rule, $unfiltered_word, %args) = (shift, shift, @_);
  my $changed = 0;

  my $original_word = [@$unfiltered_word];
  my $reversed = $rule->{bidirectional} ? 1 : 0;
  {
    my $word = $unfiltered_word;
    my $operating_on_subword = 0;
    my @inverse_filter = 0..@$unfiltered_word-1; 
    if ($reversed) {
      $operating_on_subword = 1;
      @inverse_filter = reverse @inverse_filter;
      $word = [reverse @$word];
    }
    if (defined $rule->{filter}) {
      $operating_on_subword = 1;
      @inverse_filter = grep $rule->{filter}->matches($unfiltered_word->[$_]), @inverse_filter;
      $word = [grep $rule->{filter}->matches($_), @$word];
    }
    my @surviving = (1,) x @$word; # actually a count

    # iterate in the direction specified
    my @displs = -1..@$word;   # start at -1 for assimilations to word-initial pause, and end at @$word for word-final with {-1,0}
        # will need to be changed if there can be rules whose indices are all strictly of the same sign
    @displs = reverse @displs if (defined $rule->{direction} and $rule->{direction} < 0);
    for my $i (@displs) {
      next unless $rule->matches_word($word, $i, 
          nopause => $args{nopause}, 
          context_dependent => $args{context_dependent});

      for my $displ ($rule->indices('effects')) {
        next if ($i + $displ < 0 or $i + $displ >= @$word);
        my @effects = split / /, $rule->{$displ}{effects};
        if (defined $args{alternate_effects}) {
          @effects = $rule->{$displ}{alternate_effects} if $args{alternate_effects}; # for generation only?
        }
        if (@effects != 1) {
          $changed = 1;
          push @{$args{changes}}, ('r') x (@effects - 1);
        }
        
        for my $j (0..$#effects) {
          my $effects = $effects[$j];
          # Handle the assimilation characters.  This is still okay for multi-phone resolutions;
          # there is no reason to use an assimilation character except out across an edge.
          if ($effects =~ /[<>]/) {
            my ($next_before, $next_after) = (undef, undef);
            for ($rule->indices('condition')) {
              $next_before = $_ if (!defined $next_before or $next_before < $_) and $_ < $displ;
              $next_after = $_ if (!defined $next_after or $next_after > $_) and $_ > $displ;
            }
            while ($effects =~ /</) {
              my $c = index($effects, '<');
              substr($effects, $c, 1) = 
                  substr($i+$next_before >= 0 ? $word->[$i+$next_before] : $rule->{$next_before}{or_pause}, $c, 1);
            }
            while ($effects =~ />/) {
              my $c = index($effects, '>');
              substr($effects, $c, 1) =
                  substr($i+$next_after < @$word ? $word->[$i+$next_after] : $rule->{$next_after}{or_pause}, $c, 1);
            }
            # We must entail the effects, not just the overwritten phone, since otherwise
            # jumps over the middle point on an antithetical scale won't always work.
            $effects = $rule->{FS}->add_entailments($effects);
          }
          my $newphone = $rule->{FS}->overwrite($word->[$i+$displ], $effects);
          $effects[$j] = $newphone;
          
          if ($word->[$i+$displ] ne $newphone) {
            $changed = 1;
            push @{$args{changes}}, FeatureSystem::change_record($word->[$i+$displ], $newphone) if defined $args{changes};
          }
        } # $j
        
        #TODO: (proximal) reverse internally if necessary.
        splice @$word, $i+$displ, 1, @effects;
        @surviving[$i+$displ] = scalar @effects;
      }
      
      if (scalar $rule->indices('deletions')) {
        $changed = 1;
        push @{$args{changes}}, 'd' if defined $args{changes};
        splice @$word, $i+$_, 1 for sort {$b <=> $a} $rule->indices('deletions');
        if ($operating_on_subword) {
          $surviving[$i+$_] = 0 for grep(($i+$_ >= 0 and $i+$_ < @surviving), sort {$b <=> $a} $rule->indices('deletions'));
        } else {
          # can only do this if it's not a subword
          if (defined $args{sources}) {
            splice @{$args{sources}}, $i+$_, 1 for sort {$b <=> $a} $rule->indices('deletions');
          }
        }
        $i -= scalar $rule->indices('deletions');
      }
    } 

    if ($operating_on_subword) {
      # Assume @inverse_filter is either ordered or reverse-ordered.
      my @indices = (0..$#inverse_filter);
      @indices = reverse @indices if (@inverse_filter >= 2 and $inverse_filter[0] < $inverse_filter[1]);
      my $a = 0;
      my @splice_ranges = (0, map $a += $_, @surviving);
      my @word = @$word;

      for my $i (@indices) {
        splice @$unfiltered_word, $inverse_filter[$i], 1, (@word[($splice_ranges[$i])..($splice_ranges[$i+1]-1)]);
        if (defined $args{sources}) {
          splice @{$args{sources}}, $inverse_filter[$i], 1, (($args{sources}[$inverse_filter[$i]]) x $surviving[$i]);
        }
      } # i
    }
    
    if ($rule->{bidirectional} and $reversed) {
      $reversed = 0;
      redo;
    }
  } # block for reverse iteration

  # Cases in which we might have found changes that weren't actually changes.
  if ($rule->{bidirectional}) {
    $changed = 0 if join(' ', @$original_word) eq join(' ', @$unfiltered_word); #kluge
  }

  $changed;
}

# Record on this rule that rule $rj, whose number is $j, should be inactivated if this one is chosen.
sub mark_to_inactivate {
  my ($self, $rj, $j) = (shift, shift, shift);
  push @{$self->{inactivate}}, $j;
  # For now, don't regenerate split-off pieces of split rules, since they'll come back without the base part of their condition.
  # this is an ugly kluge, but few rules have more than one effect
  if (defined $rj->{tag} and $rj->{tag} !~ /_split/) {
    my $evitands = join '|', map($rj->{$_}{effects}, $rj->indices('effects'));
    push @{$self->{broken_tags}}, $rj->{tag} . ' ' . $evitands;
  }
}

# Create persistent and impersistent variants of this rule.  Weight appropriately.
# In fact, there are two kinds of persistent variants; one tries to redo every rule it conflicts with,
# while one takes on their conditions as excepts in many cases (if this rule is recastable enough).
sub persistence_variants {
  my ($self, $base_weight, $pd, $persistence_weight, $no_persist, $generable_val) = 
      (shift, shift, shift, shift, shift, shift);
  my $phonology = $pd->{phonology};
  my @makings;

  my $impersistent = $self->deep_copy_indexed();
  $impersistent->{inactive} = scalar @$phonology;
  push @makings, [$impersistent, $base_weight * (1 - $persistence_weight)];

  unless ($no_persist) {
    my $redo = $self->deep_copy_indexed();
    my $loopbreak_penalty = 1 - $self->{recastability};
    my $redo_and_except = $self->deep_copy_indexed();
    my $loopbreak_penalty_and_except = $self->{recastability};

    # The test for looping we do here was at one point the most expensive thing
    # in the phonology generation.  By way of cutting down, only check rules
    # which set something the (potential) opposite of this rule.
    my @potential_conflicts;
    for my $displ ($redo->indices('effects')) {
      for my $i (0..@{$pd->{FS}{features}}-1) {
        push @potential_conflicts, @{$generable_val->[1-substr($redo->{$displ}{effects}, $i, 1)][$i]}
            if substr($redo->{$displ}{effects}, $i, 1) =~ /[01]/
                and defined($generable_val->[1-substr($redo->{$displ}{effects}, $i, 1)][$i]);
        if (substr($redo->{$displ}{effects}, $i, 1) =~ /[<>]/) {
          push @potential_conflicts, @{$generable_val->[0][$i]} if defined($generable_val->[0][$i]);
          push @potential_conflicts, @{$generable_val->[1][$i]} if defined($generable_val->[1][$i]);
        }
      }
    }
    my %pch = map(($_ => 1), @potential_conflicts);
    @potential_conflicts = sort keys %pch; # uniq

    my @conflict_indices;
    for my $j (@potential_conflicts) {
      next if defined $phonology->[$j]{inactive} and $phonology->[$j]{inactive} < @$phonology;
      if ($self->conflicts_with($phonology->[$j], indices => \@conflict_indices)) {
        #print STDERR "clash of $self->{tag} with $j\n"; #debug
        my $recastability = 1;
        $recastability = $phonology->[$j]{recastability} if defined $phonology->[$j]{recastability};
        
        $redo->mark_to_inactivate($phonology->[$j], $j);
        $loopbreak_penalty *= $recastability;
        
        if (1) {
          my $clash = $pd->{FS}->overwrite($phonology->[$j]{$conflict_indices[1]}{condition}, $phonology->[$j]{$conflict_indices[1]}{effects});
          $clash =~ s/u/./g; # in case e.g. of forcing undefined
          $redo_and_except->{$conflict_indices[0]}{except} .= ' ' if defined $redo_and_except->{$conflict_indices[0]}{except};
          $redo_and_except->{$conflict_indices[0]}{except} .= $clash;
        } else {
          $redo_and_except->mark_to_inactivate($phonology->[$j], $j);
          $loopbreak_penalty_and_except *= $recastability;
        }
      }
    }
    push @makings, [$redo, $base_weight * $persistence_weight * $loopbreak_penalty];
    push @makings, [$redo_and_except, $base_weight * $persistence_weight * $loopbreak_penalty_and_except];
  } # unless ($no_persist)

  @makings;
}

# Generate an extra condition for this rule.

sub gen_extra_condition {
  my ($self, %args) = (shift, @_);
  my $FS = $self->{FS};
  my (%resolution_keys, %resolutions);
  my $global_res_count = 0;

  for my $locus ($self->indices('or_pause')) {
    # Restriction to word-extremal, and away from it.
    my $rule1 = $self->deep_copy_indexed();
    substr($rule1->{$locus}{condition}, 0, 1) = 'x';
    $resolution_keys{$global_res_count} = 0.5; # magic weight
    $resolutions{$global_res_count++} = $rule1;
    
    my $rule2 = $self->deep_copy_indexed();
    delete $rule2->{$locus}{or_pause};
    $resolution_keys{$global_res_count} = 0.5; # magic weight
    $resolutions{$global_res_count++} = $rule2;
  }

  for my $locus ($self->indices('effects')) {
    my $effect = $self->{$locus}{effects};

    # Conditions of the same family as the effects (which we don't have stored in a special structure).
    %_ = map(($_ => 1), map split(/ /, $FS->{features}[$_]{families}), 
        grep substr($effect, $_, 1) ne '.', 0..length($effect)-1);
    my @families = grep $FS->compatible($FS->parse($FS->{families}{$_}), $self->{$locus}{condition}),
        grep $_, keys %_;
    my @family_features = grep {
      my $i = $_;
      grep $FS->{features}[$i]{families} =~ /\b$_\b/, @families;
    } grep((defined $args{generable_val}[0][$_] && @{$args{generable_val}[0][$_]} && 
            defined $args{generable_val}[1][$_] && @{$args{generable_val}[1][$_]}), 
        0..length($effect)-1);
        # TODO: handle this when there's no generable_val.  also, a more uniform way of dropping ungenerables for the later types
    for my $f (@family_features) {
      next if substr($self->{$locus}{condition}, $f, 1) ne '.';
      for my $v (0..1) {
        next if $v == 0 and $FS->{features}[$f]{univalent};
        my $rule1 = $self->deep_copy_indexed();
        substr($rule1->{$locus}{condition}, $f, 1) = $v;
        $resolution_keys{$global_res_count} = $FS->{features}[$f]{univalent} ? 1.0 : 0.5; # magic factor
          # equiprobable on features, aot on their values
        $resolutions{$global_res_count++} = $rule1;
      }
    }
    
    # Conditions related to the outcome.
    my $outcome = $FS->overwrite($self->{$locus}{condition}, $effect);
    $outcome =~ s/[<>]/./;
    for my $rel (@{$FS->{relations}}) {
      next if $rel->{spread_only};
      $_ = $FS->parse($rel->{to});
      next unless $outcome =~ /^$_$/;
      
      my $rule1 = $self->deep_copy_indexed();
      my $extra = $FS->parse($rel->{from});
      next unless $FS->compatible($rule1->{$locus}{condition}, $extra);
      $rule1->{$locus}{condition} = $FS->overwrite($rule1->{$locus}{condition}, $extra);
      next if $rule1->{$locus}{condition} == $self->{$locus}{condition};
      $resolution_keys{$global_res_count} = $rel->{weight}; # magic factor
      $resolutions{$global_res_count++} = $rule1;
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

          my $rule1 = $self->deep_copy_indexed();
          for my $displ (0..$#condition) {
            my $l = $locus + $displ - $d->{target};
            if (!defined $rule1->{$l}{condition}) {
              $_ = $FS->parse($FS->{generic_pause_phone});
              $rule1->{$l}{or_pause} = $_ if /^$condition[$displ]$/;
              $rule1->{$l}{condition} = '.' x length($effect);
            }
            next EF_ASSIM unless $FS->compatible($rule1->{$l}{condition}, $condition[$displ]);
            $rule1->{$l}{condition} = $FS->overwrite($rule1->{$l}{condition}, $condition[$displ]);
          }
          $_ = '.'  x length($effect);
          substr($_, $f, 1) = substr($rule1->{$locus}{condition}, $f, 1); 
          next unless $FS->compatible(substr($rule1->{$locus + 1 - 2*$d->{target}{condition}}, $f, 1), $_);
          substr($rule1->{$locus + 1 - 2*$d->{target}{condition}}, $f, 1) =
              $FS->overwrite(substr($rule1->{$locus + 1 - 2*$d->{target}{condition}}, $f, 1), $_); # impose the actual assimilation
          
          $resolution_keys{$global_res_count} = ($d->{prob} >= 1/24.0 ? 1/24.0 : $d->{prob}) * 48; # magic factor
          $resolutions{$global_res_count++} = $rule1;
        }
      }

      # pretty duplicative :/
      for my $r (@{$FS->{relations}}) {
        $_ = $FS->parse($r->{to});
        next if $effect !~ /^$_$/;

        EF_ASSIMR: for my $d (@{$r->{assimilation}}) {
          my @condition = map $FS->parse($_), split /, */, $d->{condition}, -1;
          next unless $outcome =~ /^$condition[$d->{target}]$/;

          my $rule1 = $self->deep_copy_indexed();
          for my $displ (0..$#condition) {
            my $l = $locus + $displ - $d->{target};
            if (!defined $rule1->{$l}->{condition}) {
              $_ = $FS->parse($FS->{generic_pause_phone});
              $rule1->{$l}{or_pause} = $_ if /^$condition[$displ]$/;
              $rule1->{$l}{condition} = '.' x length($effect);
            }
            next EF_ASSIMR unless $FS->compatible($rule1->{$l}{condition}, $condition[$displ]);
            $rule1->{$l}{condition} = $FS->overwrite($rule1->{$l}{condition}, $condition[$displ]);
          }
          $_ = $FS->parse($r->{from});
          next unless $FS->compatible($rule1->{$locus + 1 - 2*$d->{target}{condition}}, $_);
          $rule1->{$locus + 1 - 2*$d->{target}{condition}} = 
              $FS->overwrite($rule1->{$locus + 1 - 2*$d->{target}{condition}}, $_); # impose the actual assimilation
          
          $resolution_keys{$global_res_count} = ($d->{prob} >= 1/24.0 ? 1/24.0 : $d->{prob}) * 48; # magic factor
          $resolutions{$global_res_count++} = $rule1;
        }
      }
    }

    # Conditions that avoid a marked situation changed by a previous rule.
    for my $old_rule (@{$args{phonology}{phonology}}) {
      next if defined $old_rule->{inactive};
      next if scalar $old_rule->indices('condition') >= 2 and $args{bar_sequences};
      for my $old_locus ($old_rule->indices('effects')) {
        my $old_precondition = $old_rule->{$old_locus}{condition};
        next if $old_precondition =~ /u/;
        next unless $FS->compatible($old_precondition, $outcome);
        my $old_effect = $old_rule->{$old_locus}{effects};
        # We take a rule to avoid markedness if its effect 
        # is incompatible with its precondition.
        for (0..length($old_effect)-1) {
          substr($old_effect, $_, 1) = substr($old_rule->{$old_locus+1}{effects}, $_, 1)
            if substr($old_effect, $_, 1) eq '>';
          substr($old_effect, $_, 1) = substr($old_rule->{$old_locus-1}{effects}, $_, 1)
            if substr($old_effect, $_, 1) eq '<';
        }
        next if $FS->compatible($old_effect, $old_precondition);

        # We let how good this thing is as a condition to avoid depend on how many features have to be added.  
        # (We just perform this subtraction on the old precondition, direcly.)
        my $rule1 = $self->deep_copy_indexed();

        my $num_convergences = scalar grep substr($old_precondition, $_, 1) ne '.', 0..length($old_precondition)-1;
        for (0..length($old_precondition)-1) {
          substr($old_precondition, $_, 1) = '.'
              if substr($old_precondition, $_, 1) eq substr($outcome, $_, 1);
        }
        my $num_divergences = scalar grep substr($old_precondition, $_, 1) ne '.', 0..length($old_precondition)-1;
        $num_convergences -= $num_divergences; # num_convergences is for magic weights
        if ($num_divergences <= 0) {
          substr($rule1->{$locus}{condition}, 0, 1) = 'x'; # nothing is left to match!
        } elsif ($num_divergences <= 1) {
          $old_precondition =~ y/01/10/;
          $rule1->{$locus}{condition} = $FS->overwrite($rule1->{$locus}{condition}, $old_precondition);
        } else {
          $rule1->{$locus}{except} .= ' ' if defined $rule1->{$locus}{except};
          $rule1->{$locus}{except} .= $old_precondition;
        }
        $resolution_keys{$global_res_count} = 
            $num_convergences / ($num_divergences * ($num_divergences - 1) / 2 + 1); # much magic :/
        $resolutions{$global_res_count++} = $rule1;
      }
    }
  } # locus

  # print STDERR YAML::Any::Dump(\%resolutions), "\n\n";
  if (keys %resolutions) {
    my $i = weighted_one_of(%resolution_keys);
    return $resolutions{$i};
  }
}


# To expand a rule tag:
# - make all the resolutions, incl. related features, incl. loop-preserving and -breaking forms.
# - make the rules (retaining the tag, for later remaking).  When flipping a feature between 0 and 1, 
#   clear features formerly requiring it.
# - do the chance of extra conditions thing.
# - repeat to make any necessary new rules for loopbreaks.

# The format of rule tags is "$kind $list_index", where $kind is one of the values that
# appear herein several times.

sub generate {
  my ($tag, %args) = (shift, @_);
  my ($kind, $k, $rest) = split / /, $tag;
  my $FS = $args{phonology}{FS};
  #print STDERR "[" . scalar @{$args{phonology}{phonology}} . "] tag is $tag\n"; # debug

  # Not doing assimilation rules (or strippings) since they can't much come out differently.
  my $threshold = 1;
  if ($kind eq 'default') {
    $threshold = $FS->{features}[$k]{default}[$rest]{value};
  } elsif ($kind eq 'repair') {
    $threshold = $FS->{marked}[$k]{prob};
    if (defined $FS->{marked}[$k]{contrast_probs}) {
      while (my ($cp, $prob) = each %{$FS->{marked}[$k]{contrast_probs}}) {
        my ($cp_pre, $cp_outcome) = split /; */, $cp;
        $cp_pre = $FS->parse($cp_pre);
        $cp_outcome = $FS->parse($cp_outcome);
        $cp_outcome = $FS->add_entailments($FS->overwrite($cp_pre, $cp_outcome));
        if ($args{initial} and $args{phonology}->generatedly_contrast($cp_pre, $cp_outcome)) {
          $threshold = $prob;
          last;
        }
      }
    }
  }

  my $initial_threshold = $threshold; # e.g. for things which are more unlikely than marked, in a way that feature choice can't handle
  if ($kind eq 'repair' and $args{initial} and defined $FS->{marked}[$k]{initial_prob}) {
    $initial_threshold = $FS->{marked}[$k]{initial_prob};
  }

  # Catch the default rules which are really repair rules.
  if ($kind eq 'default' and defined $FS->{features}[$k]{default}[$rest]{repair}) {
    $kind = 'repair_default';
    $threshold = 1;
    $initial_threshold = $FS->{features}[$k]{default}[$rest]{repair}{prob};
  }

  my $skip_me = 0;
  if (!$args{dont_skip} and ($kind eq 'repair' or $kind eq 'repair_default')) {
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

    # Check for the case where defaults are different if a contrast exists.
    my $expected_value = $FS->{features}[$k]{default}[$rest]{value};
    my ($precondition0, $precondition1) = ($precondition, $precondition);
    substr($precondition0, $k, 1) = '0'; substr($precondition1, $k, 1) = '1'; 
    if (defined $FS->{features}[$k]{default}[$rest]{contrast_value} and 
        $args{phonology}->generatedly_contrast($precondition0, $precondition1)) {
      $expected_value = $FS->{features}[$k]{default}[$rest]{contrast_value};
    }

    for (0..1) {
      my $effects = '.' x @{$FS->{features}}; 
      substr($effects, $k, 1) = $_;
      my $weight = $_ ? $expected_value : 1 - $expected_value;
      my $rule = {
        0 => {condition => $precondition, effects => $effects},
        recastability => 1 - $weight,
      };
      bless $rule->{0}, 'PhoneSet';
      # Default-provision rules shouldn't run where a stripping exists.  
      for (@{$FS->{strippings}}) {
        if ($_->{strip} =~ /(^| )$FS->{features}[$k]{name}( |$)/) {
          $rule->{0}{except} = $FS->parse($_->{condition});
          last;
        }
      }
      push @resolutions, $rule;
      push @weights, $weight;
    }
  } 
  
  # this short-circuits a bunch of stuff.
  elsif ($kind eq 'stripping') {
    my $precondition = $FS->{strippings}[$k]{condition_parsed};

    if (defined $rest) {
      my $s = $FS->{strippings}[$k]{substitute}[$rest];
      $s =~ /^(.*) *: *(.*)$/;
      my $rule = {
        0 => {condition => $FS->overwrite($precondition, $FS->parse($1)), effects => $FS->parse($2)},
        recastability => 0,
        FS => $FS,
      };
      return bless $rule;
    }

    my $effects = $FS->parse($FS->{strippings}[$k]{strip});
    $effects =~ s/1/u/g;
    my $rule = {
      0 => {condition => $precondition, effects => $effects},
      recastability => 0,
      tag => $tag,
      FS => $FS,
    };
    return bless $rule;
  } 
  
  elsif ($kind =~ /^repair/) {
    my ($unsplit_d, $d); 
    if ($kind eq 'repair_default') {
      $d = $unsplit_d = $FS->{features}[$k]{default}[$rest]{repair};

    } else {
      $unsplit_d = $FS->{marked}[$k];
      if ($kind =~ /_split$/) {
        $d = $unsplit_d->{split}[$rest];
      } else {
        $d = $unsplit_d;
      }
    }

    my $base_rule = PhoneSet::parse($d, 1, FS => $FS, 
        base => defined $args{unsplit_rule} ? $args{unsplit_rule}->deep_copy_indexed() : undef);
    bless $base_rule;
    $base_rule->{recastability} = 1 - $d->{prob};
    $base_rule->{tag} = $tag;
    $base_rule->{cede} = 1 - $threshold;
    $base_rule->{FS} = $FS;

    if (defined $d->{filter}) {
      $base_rule->{filter} = PhoneSet::parse($d->{filter}, 0, FS => $FS);
    }

    # Invalidate previous resolutions if directed.  This is intended to be used for cases
    # where typical epenthetic segments differ from typical phonemic segments.
    if ($kind eq 'repair_default' and defined $FS->{features}[$k]{default}[$rest]{inactivate_previous}) {
      for my $i (0..$#{$args{phonology}{phonology}}) {
        if ($args{phonology}{phonology}[$i]{tag} =~ /^default $k\W/) {
          my $extant_cd = $args{phonology}{phonology}[$i]{0}{condition};
          $extant_cd =~ y/u/./;
          my $new_cd = $base_rule->{0}{condition};
          $new_cd =~ y/u/./;
          push @{$base_rule->{inactivate}}, $i if $extant_cd =~ /^$new_cd$/;
        }
      }
      $threshold = 1;
    }

    my @unsplit_phones = map $FS->parse($_), split /, */, $unsplit_d->{condition}, -1;

    # Do choice of direction outside of the resolution selection for now, for laziness.
    if (defined $d->{direction}) {
      my $direction = weighted_one_of %{$d->{direction}};
      if ($direction eq 'reverse') {
        $base_rule->{reverse} = 1;
      } elsif ($direction eq 'both') {
        $base_rule->{bidirectional} = 1;
      }
    }

    # {resolve} is a weight-hash of possible resolutions, whose keys are of the form "$operation $argument".
    # 
    # If the resolution part isn't written, we will resolve phone 0 freely.
    # (This is intended for marked single phoneme rules.  In particular, the last-resort deletion
    # that these rules once had is now no more.)
    my %resolutions;
    %resolutions = %{$d->{resolve}} if defined $d->{resolve};
    $resolutions{'free 0'} = 1 unless keys %resolutions;

    my @resolution_keys = sort keys %resolutions;
    for (@resolution_keys) {
      my $weight = $resolutions{$_};
      /^([^ ]*) +(.*)$/;
      my ($reskind, $arg) = ($1, $2);

      my $rule = $base_rule->deep_copy_indexed();
      my @variants = (); # where to put the generated rules

      # resolve as specified
      if ($reskind eq 'r') {
        my @effects_strings = split /, +/, $arg;
        my %effects = ();
        for (@effects_strings) {
          /^(.*) +(-?[0-9]*)$/;
          my ($effect, $target) = ($1, $2);
          my $parsed_effect = $FS->parse($effect);

          for (0..length($effects{$target})-1) {
            if (substr($parsed_effect, $_, 1) =~ /[{}]/) {
              my $restriction = rand(2.0 + 4.0/(1-$threshold)); # 4 is a magic factor
              if ($restriction < 2.0) {
                substr($rule->{$target}{condition}, $_, 1) = int($restriction)
                    if substr($rule->{$target}{condition}, $_, 1) eq 'u';
              }
            }
          }
          $parsed_effect =~ y/{}/<>/;

          # In case of assimilation, both the things being spread from and to need to support the feature,
          # unless assimilating or setting that feature as well assures that this is unnecessary.  
          for (0..length($parsed_effect)-1) {
            if (substr($parsed_effect, $_, 1) ne '.') {
              my $requirements = $FS->parse($FS->{features}[$_]{requires});
              for my $i (0..length($requirements)-1) {
                substr($requirements, $i, 1) = '.'
                    if substr($parsed_effect, $_, 1) =~ /[<>]/ 
                    and substr($parsed_effect, $i, 1) eq substr($parsed_effect, $_, 1);
                substr($requirements, $i, 1) = '.' if substr($parsed_effect, $i, 1) eq substr($requirements, $i, 1);
              }
              $rule->{$target}{condition} = '.' x @{$FS->{features}} unless defined $rule->{$target}{condition};
              $rule->{$target}{condition} = $FS->intersect($rule->{$target}{condition}, $requirements);
              if (substr($parsed_effect, $_, 1) =~ /[<>]/) {
                my $source = (substr($parsed_effect, $_, 1) eq '>') ? $target + 1 : $target - 1;
                $rule->{$source}{condition} = $FS->intersect($rule->{$source}{condition}, $requirements);
              }
            }
          }
          
          $effects{$target} = $parsed_effect;
        }

        # But not if it's stripped off.
        for my $str (@{$FS->{strippings}}) {
          for my $displ ($rule->indices('condition')) { 
            if ($rule->{$displ}{condition} =~ /^$str->{condition_parsed}$/) {
              my $effect = $FS->parse($str->{strip});
              $effect =~ s/1/a/g; # temporary char
              $rule->{$displ}{condition} = $FS->overwrite($rule->{$displ}{condition}, $effect);
              $rule->{$displ}{condition} =~ s/a/./g;
            }
          }
        }

        $rule->{$_}{effects} = $effects{$_} for keys %effects;
        push @variants, $rule->persistence_variants(1, $args{phonology}, $threshold, 
                                             0, $args{generable_val});
      } #r

      elsif ($reskind eq 'delete') {
        $rule->{$arg}{deletions} = 1;
        push @variants, [$rule, 1];
      } #delete

      # Resolve the named phone in the ways listed in {flip} and {related_weight}.
      # In {flip} is a hash of single features to be flipped, with multiplicative weights;
      # in {related_weight} is a hash of multiplicative weights applying to resolutions via related features.
      # For essentially historical reasons, {flip} and {related_weight} belong to the whole constraint,
      # not the resolution.  If entries in {flip} or keys in {related_weight} are followed by
      # a number, they apply only to the phone of that index, else they apply to all phones.
      elsif ($reskind eq 'free') {
        my $resolvend = $base_rule->{$arg}{condition};
        my $reqd = $FS->add_requirements($resolvend);
        my $i = 0;
        my $resolution_type = 0;
        RESOLUTION_TYPE: while ($resolution_type <= 1) {
          my $effects;
          my $base_weight = 0;
          my $no_persist = 0;
          $no_persist = 1 if defined $d->{phonemic_only};

          my $rule = $base_rule->deep_copy_indexed();

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
                if ($outcome =~ /^(.*) (-?[0-9]*)$/) {
                  next unless $arg == $2;
                  $outcome = $1;
                }
                my $f = $FS->parse($outcome);
                $base_weight *= $d->{related_weight}{$outcome} if $effects =~ /^$f$/;
              }
            }
          }

          $total_base_weight += $base_weight;

          $rule->{$arg}{effects} = $effects;
          # This base_weight is used to fill out recastability, below.
          $rule->{base_weight} = $base_weight; 

          my $persistence_weight = defined $d->{persist} ? $d->{persist} : $threshold;
          push @variants, $rule->persistence_variants($base_weight, $args{phonology}, $persistence_weight, 
                                              $no_persist, $args{generable_val});
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
    } # @resolution_keys

    # Record which split resolutions we will need to do.
    # Recursive splits don't in fact work, as this is currently implemented.
    if (defined $d->{split}) {
      for my $rule (@resolutions) {
        for my $i (0..$#{$d->{split}}) {
          next unless rand() < $d->{split}[$i]{prob};
          if (defined $d->{split}[$i]{if}) {
            $d->{split}[$i]{if} =~ /^(.*) +(-?[0-9]*)$/;
            my ($condition, $target) = ($1, $2);
            $condition = $FS->parse($condition);
            next unless $rule->{$target}{effects} =~ /^$condition$/;
          }
          if (defined $d->{split}[$i]{unless_deletion}) { # code duplication...
            next if $rule->{$d->{split}[$i]{unless_deletion}}{deletions};
          }
          next if grep $_ eq "${kind}_split $k $i", @{$rule->{splits}}; #kluge? 
          push @{$rule->{splits}}, "${kind}_split $k $i";
        }
      }
    }
  } # repair

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

    return unless $selected_rule;
    bless $selected_rule;
    
    # Decorate the selected resolution by clearing features that now lack their requirements.
    # Do antithetical features.
    for my $displ ($selected_rule->indices('effects')) {
      $selected_rule->{$displ}{effects} = $FS->add_entailments($selected_rule->{$displ}{effects});

      # If this resolution is to be avoided, try again.
      for my $avoid (@{$args{avoid}}) {
        if ($selected_rule->{$displ}{effects} eq $avoid) {
          splice @resolutions, $j, 1;
          splice @weights, $j, 1;
          redo RESOLVE;
        }
      }
    } # $displ
  } # RESOLVE

  $selected_rule->{FS} = $FS;
  
  # Adorn the rule with extra conditions, if we decided to before.
  if ($add_a_condition) {
    $selected_rule->gen_extra_condition(%args); # {phonology} is passed through
  }

  # If any of the preconditions of this rule are not generable by anything coming before,
  # and it's a one-time rule, it's never triggerable; just drop it and don't write it down.
  for my $displ ($selected_rule->indices('condition')) {
    for my $i (0..@{$FS->{features}}-1) {
      return if substr($selected_rule->{$displ}{condition}, $i, 1) =~ /[01]/ 
            and !defined($args{generable_val}[substr($selected_rule->{$displ}{condition}, $i, 1)][$i])
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
      for my $displ ($selected_rule->indices('effects')) {
        if ($kind ne 'default' and $kind ne 'stripping') {
          substr($selected_rule->{$displ}{effects}, $i, 1) = '.'
              if substr($selected_rule->{$displ}{effects}, $i, 1) =~ /[01]/;
          delete $selected_rule->{$displ}{effects}, next unless $selected_rule->{$displ}{effects} =~ /[^.]/;
        }
        for (@{$args{forcibly_unmark}{$i}}) { 
          substr($selected_rule->{$displ}{effects}, $i, 1) = 'u', last 
              if substr($selected_rule->{$displ}{effects}, $_, 1) ne '.';
        }
      }
    }
  }

  # Abandon this rule if it does nothing now.
  # TODO: update these tests for rules that do nothing as needed
  return unless scalar $selected_rule->indices('effects') or scalar $selected_rule->indices('deletions'); 

  # Reverse this rule if it needs that.
  if ($selected_rule->{reverse}) {
    delete $selected_rule->{reverse};
    $selected_rule = $selected_rule->reverse_rule();
  }

  # Choose an application direction.
  $selected_rule->{direction} = rand(2) >= 1.0 ? -1 : 1;

  # It's correct for extra condition rules to have no tag, so that they
  # just drop out when regenerated.
  $selected_rule->{tag} = $tag unless $add_a_condition;
  $selected_rule->{run_again} = 1 if ($add_a_condition and !$skip_me);
  if (defined $selected_rule->{base_weight}) {
    $selected_rule->{recastability} = (1 - $selected_rule->{base_weight} / $total_base_weight);
    $selected_rule->{recastability} = 0 if $selected_rule->{recastability} < 0;
    delete $selected_rule->{base_weight};
  }

  $selected_rule;
}


1;
