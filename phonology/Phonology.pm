package Phonology;
use strict;
use constant INF => 9**9**9; # is there really nothing sensible better?

our $debug_alphabet; # used for printing phones for debugging only
our $verbose = 0;
our $debug = 0;

# Go from a prototypical prob to an actual one.  Now twice as gentle!
sub fuzz {
  my $p = shift;
  return 0 if $p <= 0;
  return 1 if $p >= 1;
  my $q = rand($p / (1 - $p)) + rand($p / (1 - $p));
  return $q / ($q + rand(1) + rand(1));
}

# Box-Muller.  I wonder whether this is faster than sum of 12 uniforms.
use constant TWOPI => 6.28318530717958647688;
sub std_normal {
  return sqrt(-2*log rand(1)) * cos(rand(TWOPI));
}

# To save space, we currently trim the feature system out of a phonology before saving it.
sub load_file {
  my ($infile, $FS) = (shift, shift);
  my $pd = YAML::Any::LoadFile($infile);
  $pd->{FS} = $FS;
  bless $pd, 'Phonology';
  for my $rule (@{$pd->{phonology}}) {
    $rule->{FS} = $FS;
    bless $rule, 'PhonologicalRule';
  }
  bless $pd;
}

sub dump_file {
  my ($self, $outfile, $annotate) = (shift, shift, shift);

  my $FS = $self->{FS};
  my $pd = YAML::Any::Load(YAML::Any::Dump($self)); # kluge deep copy
  delete $pd->{FS};
  for my $rule (@{$pd->{phonology}}) {
    delete $rule->{FS};
  }
  if ($annotate) {
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

# Return annotations regarding which rules have which preconditions or excepts.
# Used to optimise which rules we consider rerunning in running the phonology.
# The resulting array is indexed as {$value}[$feature], where $value is '0' or '1' or 'u', 
# We also use {seq} for those rules whose preconditions include a sequence;
# these are those which can be newly triggered after a deletion.

sub annotate_with_preconditions {
  my $self = shift; 
  my $FS = $self->{FS};
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
            $phonology->[$i]->run($word, %args, changes => \@changes)) {
          if (keys %{$phonology->[$i]{precondition}} > 1) { # an optimization.  helpful?
            1 while $phonology->[$i]->run($word, %args);
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
    $_ = $self->{FS}->add_entailments($_) for @$word;
  }
  $args{track_expiry}[0] = $track_expiry if defined $track_expiry;
}

# Generates a new rule with tag $tag, and appends it to the phonology, 
# making the other changes that this may entail.
sub generate_new_rule {
  my ($self, $tag, %args) = (shift, shift, @_); 
  my $phonology = $self->{phonology};

  # Strippings and defaults are clusters of rules to be generated all at once.
  # Other tags, just generate once.
  my @tag_suffixes = ('');
  if ($tag =~ /^default /) {
    my ($kind, $k) = split / /, $tag;
    @tag_suffixes = map " $_", 0..@{$self->{FS}{features}[$k]{default}}-1;
  } elsif ($tag =~ /^stripping /) {
    my ($kind, $k) = split / /, $tag;
    @tag_suffixes = map " $_", 0..@{$self->{FS}{strippings}[$k]{substitute}}-1;
    push @tag_suffixes, '';
  }

  for my $tag_suffix (@tag_suffixes) {
    my $rule = PhonologicalRule::generate($tag.$tag_suffix, %args, phonology => $self);
    next unless $rule;

    if ($tag =~ /^stripping /) {
      push @$phonology, $rule;
      next;
    }

    for (@{$rule->{inactivate}}) {
      $phonology->[$_]{inactive} = scalar @$phonology unless defined $phonology->[$_]{inactive}
                                                      and $phonology->[$_]{inactive} < scalar @$phonology;
    }
    delete $rule->{inactivate};

    for my $displ (keys %{$rule->{effects}}) {
      for my $i (0..@{$self->{FS}{features}}-1) {
        if (substr($rule->{effects}{$displ}, $i, 1) =~ /[01]/) {
          push @{$args{generable_val}[substr($rule->{effects}{$displ}, $i, 1)][$i]}, scalar @$phonology;
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
      my $rule1 = $phonology->[-1];
      for my $displ (keys %{$rule->{precondition}}) {
        last DROP_REDUNDANT unless defined $rule1->{precondition}{$displ}
            and $rule1->{precondition}{$displ} =~ /^$rule->{precondition}{$displ}$/;
      }
      last DROP_REDUNDANT unless keys %{$rule1->{effects}} == keys %{$rule->{effects}};
      for my $displ (keys %{$rule->{effects}}) {
        last DROP_REDUNDANT unless defined $rule1->{effects}{$displ}
            and $rule1->{effects}{$displ} eq $rule->{effects}{$displ};
      }
      if (defined $rule1->{deletions}) {
        last DROP_REDUNDANT if !defined $rule->{deletions};
        last DROP_REDUNDANT unless scalar @{$rule1->{deletions}} == scalar @{$rule->{deletions}};
        for my $displ (@{$rule1->{deletions}}) {
          last DROP_REDUNDANTE unless grep $_ == $displ, @{$rule->{deletions}};
        }
      } else {
        last DROP_REDUNDANT if defined $rule->{deletions};
      }
      last DROP_REDUNDANT if !defined $rule1->{inactive} and defined $rule->{inactive};
      #print STDERR YAML::Any::Dump($rule1) . "redounds with\n" . YAML::Any::Dump($rule) . "\n"; # debug
      pop @$phonology;
    }
    if (@$phonology < $former_length) {
      for my $rule1 (@$phonology) {
        $rule1->{inactive} = @$phonology if $rule1->{inactive} > @$phonology;
      }
    }

    push @$phonology, $rule;

    # Recurse to replace any other rule which we deactivated; make sure these don't resolve 
    # the same as the bad rule.
    # (Incidentally, we couldn't recurse before the push; it would break rule referencing by number.)
    for my $bt (@{$rule->{broken_tags}}) {
      $bt =~ /^(.*) ([01u.]*)$/;
      my ($tag, $avoid) = ($1, $2);
      my %otherargs = %args;
      delete $otherargs{avoid};
      delete $otherargs{dont_skip};
  #    print "{\n"; # debug
      $args{avoid} = [] if !defined $args{avoid};
      $self->generate_new_rule($tag, avoid => [(split /\|/, $avoid), @{$args{avoid}}], %otherargs);
  #    print "}\n"; # debug
    }
    delete $rule->{broken_tags};

    # If this is the added-condition version of a rule which we also wanted to generate unadorned,
    # recurse to do the unadorned form.
    if (defined $rule->{run_again}) {
      delete $rule->{run_again};
      $self->generate_new_rule($tag, %args, dont_skip => 1);
    }
  }
}



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
  my $pd = Phonology::generate_preliminary(shift);
  $pd->annotate_with_preconditions();
  print STDERR "computing inventory...\n" if $verbose;
  $pd->compute_inventory(); # base inventory for generation
  $pd->postprocess();
  delete $pd->{phone_generator}; # now this is needless
  if ($debug < 1) {
    $pd->trim_inactive(); 
  } else {
    print STDERR "pruning of inactive rules skipped\n";
  }
  $pd->annotate_with_preconditions(); # since the numbers are changed
  for (@{$pd->{phonology}}) {
    $_->strip_feed_annotation();
  }
  return $pd;
}

sub generate_preliminary {
  my $FS = shift;
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
          prob => [map fuzz($sit->{prob}), @syllable_structure],
          FS => $FS,
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
        bless $_, 'PhonologicalRule' for @phone_generator;

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
    $sortkey[$i] = std_normal();
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

  my $self = {
    syllable_structure => \@syllable_structure,
    phone_generator => \@phone_generator, 
    phonology => \@phonology,
    FS => $FS,
  };
  bless $self;

  for my $tag (@rule_tags) {
    if ($tag eq '#') {
      print STDERR "on to allophony...\n" if $verbose;
      $self->{start_sequences} = @phonology; # end of rules that pertain only to individual segments
      next;
    } 
    # We pass the generator as a way of specifying what contrasts are available.
    # For sound change purposes we'll need an alternate way to pass this information.
    $self->generate_new_rule($tag, 
        generator => \@phone_generator, 
        generable_val => \@generable_val, 
        initial => 1,
        syllable_structure => \@syllable_structure, # used only by extra conditions, presently
        bar_sequences => defined $self->{start_sequences} ? undef : 1,
        forcibly_unmark => defined $self->{start_sequences} ? undef : \%forcibly_unmark); 
  }
    
  $self;
}

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
      $rule->run(\@word, rand_value => 0);
      add_in \%inventory2, $word[0], [map $v[$_] * $rule->{prob}[$_], 0..@v-1];
      @word = ($phone); 
      $rule->run(\@word, rand_value => 1);
      add_in \%inventory2, $word[0], [map $v[$_] * (1 - $rule->{prob}[$_]), 0..@v-1];
    }
    %inventory = %inventory2;
  }
  
  # Strip unsupported features at the end, in case the syllable structure put them in.
  for my $phone (keys %inventory) {
    my $stripped = $self->{FS}->add_entailments($phone);
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
    print STDERR "out: $outcome /" . (@word ? $debug_alphabet->name_phone($word[0]) : '') . "/\n" if $debug >= 1;
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
# Prominent among these are the forcing of the phoneme frequencies in the main slots not to have
# certain phonemes anomalously common. 

# There is some icky duplication in here.
sub postprocess {
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
#    print "before $k /" . $debug_alphabet->spell(\@canonical_word) . "/ [" . $debug_alphabet->spell(\@current_word) . "]\n"; # debug
    my @old_sources = @sources;
    my @old_word = @current_word;
    my (@prov_canonical_word, @prov_current_word);
    my $changed;
    $self->run(\@current_word, 
               start => $k,
               end => $k+1,
             sources => \@sources);
#    print "target [" . $debug_alphabet->spell(\@current_word) . "]\n"; # debug
    
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
#          print "trying out " . $debug_alphabet->name_phone($prov_canonical_word[$sources[$i]]) . " at $i\n"; # debug
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

1;
