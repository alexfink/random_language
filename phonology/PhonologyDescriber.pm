package PhonologyDescriber;
use strict;
use constant INF => 9**9**9; # is there really nothing sensible better?

# The only reason I currently see to instantiate describers is to hold references to
# the phonetic alphabet and the phone-class naming data.
# (The latter defines words like "stop", "uvular", etc.; it is the file that would need localisation.)
sub new {
  my $pdes = {phonetic_alphabet => shift, classes => shift, use_html => shift};
  $pdes->{FS} = $pdes->{phonetic_alphabet}{FS};
  bless $pdes;
}

sub describe_inventory {
  my ($self, $pd, %args) = @_;
  my $buffer = '';

  if ($args{html}) {
    $buffer .= CGI::h2('Phonemic inventory') . $self->tabulate($pd); 
  } else {
    my %things_named;
    $buffer .= "phonemic inventory:\n"; 
    for my $p (sort keys %{$pd->{gen_inventory}}) { 
      my $n = join '', map $self->{phonetic_alphabet}->name_phone($_), split / /, $p;
      $buffer .= "/" . ($n !~ /\#\#/ ? $n : $self->{FS}->feature_string($p)) . "/\t@{$pd->{gen_inventory}{$p}}\n";
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

# Put in the false features which our model doesn't contain but which our descriptions do.
# Assumes annotation.
sub add_false_features {
  my ($self, $phone, $str) = (shift, shift);
  while (defined $str->{subtables}) {
    my $subtable = substr($phone, $self->{FS}{feature_index}{$str->{subtables}}, 1);
    $str = $str->{$subtable};
  }
  while (my ($k, $v) = each %{$str->{undef_p}}) {
    $phone = $self->{FS}->overwrite($phone, $v) if $phone =~ /^$k$/;
  }
  $phone;
}

# Munge a phoneme in the usual order into its table-keyed form.  
sub table_sortkey {
  my ($self, $phone, $str) = (shift, shift, shift);

  # Although tabulate itself doesn't need this, it's handy if we want to use this
  # as an ordering in other places.
  if (defined $str->{subtables}) {
    my $subtable = substr($phone, $self->{FS}{feature_index}{$str->{subtables}}, 1);
    return $subtable . $self->table_sortkey($phone, $str->{$subtable});
  }
  
  while (my ($k, $v) = each %{$str->{undef_p}}) {
    $phone = $self->{FS}->overwrite($phone, $v) if $phone =~ /^$k$/;
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
  my ($self, $enriched) = (shift, shift);
  my $str = $self->{classes}{table_structure};
  while (defined $str->{subtables}) {
    my $subtable = substr($enriched, $self->{FS}{feature_index}{$str->{subtables}}, 1);
    last if $subtable eq '.';
    $str = $str->{$subtable};
  }
  return $str;
}

# The features of a phone determining what table it's in.
sub str_part {
  my ($self, $phone) = (shift, shift);
  my $skeleton = '.' x @{$self->{FS}{features}};
  my $str = $self->{classes}{table_structure};
  while (defined $str->{subtables}) {
    my $i = $self->{FS}{feature_index}{$str->{subtables}};
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
  my ($self, $enriched, $p, $pmod, $l, $lmod, %args) = @_;
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
    my $str = $self->{classes}{table_structure};
    while (defined $str->{subtables}) {
      $dont_spell{$self->{FS}{feature_index}{$str->{subtables}}} = 1;
      my $subtable = substr($enriched, $self->{FS}{feature_index}{$str->{subtables}}, 1);
      last if $subtable eq '.';
      $str = $str->{$subtable};
    }

    for my $i (0..length($enriched)-1) {
      next if $taken_care_of[$i];
      next if substr($enriched, $i, 1) eq '.';
      next if $dont_spell{$i};
      next if $args{respect_univalent} and substr($enriched, $i, 1) eq '0' and $self->{FS}{features}[$i]{univalent};
      my $non = '.' x length($enriched); 
      substr($non, $i, 1) = 1 - substr($enriched, $i, 1);
      my $enriched_non = enrich($self->{FS}->overwrite($self->str_part($enriched), $non), $args{non_inventory}); 
      my $non_label = $self->tabulate_label($enriched_non, $p, $pmod, $l, $lmod, %args, 
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
        $ae = $self->{FS}->add_entailments($ae);
        for (0..length($enriched)-1) {
          $taken_care_of[$_] = 1 if substr($ae, $_, 1) ne '.';
        }
      }
    } # $i
  }

  if ($args{header}) {
    # Make abbreviations for table headers.
    if ($self->{use_html}) {
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

sub tabulate {
  my ($self, $pd, %args) = (shift, @_);
  my $FS = $self->{FS};
  my $str = defined $args{structure} ? $args{structure} : $self->{classes}{table_structure};
  my $condition = defined $args{condition} ? $args{condition} : '.' x @{$FS->{features}};

  if (defined $str->{subtables}) {
    my $split_feature = $str->{subtables};
    my $first = 0;
    if ($split_feature =~ /^!/) {
      $split_feature = substr($split_feature, 1);
      $first = 1;
    }
    my $t0 = $self->tabulate($pd, %args, 
                           structure => $str->{0}, 
                           condition => $FS->overwrite($condition, $FS->parse("-$split_feature")));
    my $t1 = $self->tabulate($pd, %args, 
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
            from => $self->table_sortkey($FS->parse($str->{named_collapses}{$f}{from}), $str),
            to => $self->table_sortkey($FS->parse($str->{named_collapses}{$f}{to}), $str),
            undefine => [grep $undefinenda{$str->{order_i}[$_]}, 0..@{$str->{order_i}}-1],
            avoid_unless => $self->table_sortkey($FS->parse($str->{named_collapses}{$f}{avoid_unless}), $str),
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
      my $position = $self->table_sortkey($FS->parse($phone), $str, 1);
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
    my $name = $self->{phonetic_alphabet}->name_phone($phone);
    $table{$self->table_sortkey($phone, $str)} = $name;
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
        substr($self->table_sortkey($_, $str), 0, $str->{lengths}[0]) =~ /^$row$/, keys %{$pd->{gen_inventory}};
  }
  for my $column (keys %columns) {
    $genuine_columns{$column} = scalar grep $_ =~ /^$condition$/ &&
        substr($self->table_sortkey($_, $str), $str->{lengths}[0], $str->{lengths}[1]) =~ /^$column$/, keys %{$pd->{gen_inventory}};
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
    $label = $self->tabulate_label($base_enrichment, 
                            $label_phones{columns}, $label_phones{columns_mod},
                            $labels{columns}, $labels{columns_mod},
                            header => 1,
                            repeat_base => $str->{labels}{repeat_columns})
        if $genuine_columns{$column};
    my %moves_stated; # have we already named this thing being moved in?
    while (my ($k, $v) = each %column_moves) {
      if ($k =~ /^$column$/ and !$moves_stated{$v}) {
        $label .= '&nbsp;/ ' if $label; 
        $label .= $self->tabulate_label($FS->overwrite($base_enrichment, $v),
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
    $label = $self->tabulate_label($base_enrichment, 
                            $label_phones{rows}, $label_phones{rows_mod},
                            $labels{rows}, $labels{rows_mod},
                            repeat_base => $str->{labels}{repeat_rows})
        if $genuine_rows{$row};
    my %moves_stated;
    while (my ($k, $v) = each %row_moves) {
      if ($k =~ /^$row$/ and !$moves_stated{$v}) {
        $label .= '&nbsp;/ ' if $label;
        $label .= $self->tabulate_label($FS->overwrite($base_enrichment, $v),
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
  my ($self, $phone, $inventory, %args) = (shift, shift, @_);
  my $FS = $self->{FS};

  unless ($args{accept_nothing}) {
    return $args{no_nothing} ? '' : ($args{morpho} eq 'plural' ? 'no phones' : 'no phone') 
        if defined $inventory and !grep /^$phone$/, @$inventory;
  }

  $inventory = undef if $args{no_enrich}; # hackish
  my $enriched = defined($inventory) ? enrich($phone, $inventory) : $phone;

  my $str = $args{str};
  my $subtable_index;
  if (!defined $str) {
    $str = $self->{classes}{table_structure};
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
  $enriched = $self->add_false_features($enriched, $str);

  if (defined $str->{subtables}) {
    # We prevent enriching again since it makes the names produces for e.g. classes of 
    # vowels and semivowels be the same, when otherwise all the semivowels might be high 
    # yielding a more specific and thus different name.
    my $phone0 = $enriched;
    substr($phone0, $subtable_index, 1) = '0';
    my $name0 = $self->name_natural_class($phone0, $inventory, %args, str => $str->{0}, no_nothing => 1, no_enrich => 1, significant => $phone);
    my $phone1 = $enriched;
    substr($phone1, $subtable_index, 1) = '1';
    my $name1 = $self->name_natural_class($phone1, $inventory, %args, str => $str->{1}, no_nothing => 1, no_enrich => 1, significant => $phone);

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
  my $name = $self->tabulate_label($enriched,
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
                            eliminate => $str->{$scheme}{name_classes}{eliminate}); 
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

# As name_natural_class, but for a phoneset structure. 
sub name_phoneset {
  my ($self, $phoneset, $inventory, %args) = (shift, shift, @_);
  my $FS = $self->{FS};

  my $condition = defined $phoneset->{condition} ? $phoneset->{condition} : ('.' x @{$FS->{features}});
  my $name = $self->name_natural_class($condition, $inventory, %args);
  return $name if (!$name) or ($name =~ /^no phones?$/); # kluge
  my @exceptions = split / /, $phoneset->{except};
  my @exception_texts = map $self->name_natural_class($FS->overwrite($condition, $_), 
          $inventory, %args, significant => $_, no_nothing => 1), 
      # don't state exceptions that don't actually exclude anything
      grep { my $a = $_; grep /^$a$/ && /^$condition$/, @$inventory; } @exceptions;
  @exception_texts = grep $_, @exception_texts;
  my $conj = $args{morpho} eq 'indef' ? ' or ' : ' and ';
  $name .= ' except ' . join $conj, @exception_texts if @exception_texts;
  $name;
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

# TODO: shd nobase be a kind of morpho?

sub describe_set {
  my ($self, $orig_phones, $inventory, %args) = (shift, shift, @_);
  my $FS = $self->{FS};
  my $phones = $orig_phones;
  my $morpho = defined $args{morpho} ? $args{morpho} : 'indef';
  my $extend = defined $args{extend};
  my $lb = $args{etic} ? '[' : '/';
  my $rb = $args{etic} ? ']' : '/';
#print STDERR join(' ', map($debug_alphabet->name_phone($_), sort @$orig_phones)) . '  within  ' . 
#             join(' ', map($debug_alphabet->name_phone($_), sort @$inventory)) . "\n";
  $args{get_str_pattern}->[0] = '.' x @{$FS->{features}} if defined $args{get_str_pattern}; # extra return hack

  my $pattern = defined $args{within} ? $args{within} : '.' x @{$FS->{features}};
  my $str = $self->{classes}{table_structure}; 
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
    return $lb . $self->{phonetic_alphabet}->name_phone($phones->[0]) . $rb if ($size == 1);
  }

  # add_false_features causes a few things, like [?\], to be named wrongly.  
  my %remove_false_features = map(($self->add_false_features($_, $str) => $_), @$inventory);

  $phones = [map $self->add_false_features($_, $str), @$phones];
  $inventory = [map $self->add_false_features($_, $str), @$inventory];
  my %sortkey = map(($_ => $self->table_sortkey($_, $self->{classes}{table_structure})), 
      grep $_, @$inventory);
  @$phones = sort {$sortkey{$a} cmp $sortkey{$b}} @$phones;
  @$inventory = sort {$sortkey{$a} cmp $sortkey{$b}} @$inventory;
  if ($args{sort_phones}) {
    @$orig_phones = sort {$sortkey{$self->add_false_features($a, $str)} cmp $sortkey{$self->add_false_features($b, $str)}} @$orig_phones;
  }

  my @comparanda = grep /^$pattern$/, @$inventory;
  my $str_pattern = $pattern;

  my %phones = map(($_ => 1), @$phones);
  my @complement = grep !defined $phones{$_}, @comparanda;
  my $cosize = @comparanda - @$phones;
  unless ($extend) {
    return $self->name_natural_class($pattern, $inventory, str => $str, morpho => $morpho) if ($cosize == 0);
    return $self->name_natural_class($pattern, $inventory, str => $str, morpho => $morpho) 
        . " other than $lb" . $self->{phonetic_alphabet}->name_phone($remove_false_features{$complement[0]}) . $rb if ($cosize == 1);
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
  for (@{$self->{classes}{extra_natural_classes}}) {
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
          my $a = $self->add_false_features($_, $str);
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
        $lb . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @$phones) . $rb;
  }
  if ($cosize <= $complexity) {
    return $self->name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) . 
        (@complement ? (" other than $lb" .
          join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @complement) . $rb) : ''); 
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
        $lb . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @$phones) . $rb;
  }

  my $significant = $pattern;
  if ($args{insignificant}) {
    for (0..length($pattern)-1) {
      substr($significant, $_, 1) = '.' if substr($args{insignificant}, $_, 1) ne '.';
    }
  }

  my $main_name = $self->name_natural_class($pattern, $inventory, %args, str => $str, morpho => $morpho, 
      base => $base, significant => $significant);
  # Get rid of the antipattern in question if it made it in as the base.
  @antipatterns = grep $_ != $base_antipattern, @antipatterns if $base and $main_name =~ /\b$base/; # kluge!

  @_ = grep $pattern, @$inventory; # name antipatterns within matches to the pattern
  my @excluded_names = map $self->name_natural_class($_, \@_, str => $str, morpho => $morpho), @antipatterns;

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
      $excluded_names[$i] .= " ($lb" . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @{$antipatterns_caught[$i]}) . "$rb)";
    }
    if (@base_caught) {
      $main_name .= (@detritus or @antipatterns_caught) ? " (i.e. not $lb" : 
          '; i.e. ' . 
          $self->name_natural_class($str_pattern, $inventory, str => $str, morpho => $morpho) .
          " other than $lb";
      $main_name .= join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @base_caught) . $rb;
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
      $detritus_name = $self->describe_set([map $remove_false_features{$_}, @detritus], $inventory, %args, 
          get_str_pattern => undef, insignificant => $pattern, bottom_out => 1,
          morpho => ($args{morpho} eq 'bare' ? 'indef' : $args{morpho}), ie => undef);
      $detritus_name = '' if $detritus_name =~ /^[[\/]/ or $detritus_name =~ /other than/; # big kluge
    }
    if ($detritus_name) {
      $detritus_name .= " ($lb" . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @detritus) . "$rb)"
          if (!$list_examples and $args{ie}); # duplicative
      $result .= $detritus_name;
    } else {
      $result .= $lb . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @detritus) . $rb;
    }
  }

  if ($list_examples and $args{ie}) {
    $result .= '; i.e. ' . ($morpho eq 'plural' ? '' : 'one of ') . 
        $lb . join(' ', map $self->{phonetic_alphabet}->name_phone($remove_false_features{$_}), @$phones) . $rb;
  } 
  $result;
}

# This returns a string and a list of strings.
# The first string is the syllable structure itself; 
# the remaining strings are one for each position, describing the further restrictions.
# By the time we call this, gen_syllable contains all the important information
# about the syllable structure, whereas some of what's in syllable_structure is inaccurate.
sub describe_syllable_structure {
  my ($self, $pd, %args) = (shift, @_);
  my @template;
  my @elaborations;
  
  my (%label_uses, %first_pos);
  for my $pos (0..@{$pd->{syllable_structure}}-1) {
    my @phones = grep(($_ and $pd->{gen_inventory}{$_}[$pos]), keys %{$pd->{gen_inventory}});
    my $label;
    for my $slot (@{$self->{classes}{syllable_slots}}) {
      my $phone; 
      ($phone, $label) = split /: */, $slot;
      $phone = '(' . join(')|(', map($self->{FS}->parse($_), split(/\|/, $phone))) . ')';
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
      $template[$pos] = $self->{phonetic_alphabet}->name_phone($phones[0]);
    } else {
      $elaborations[$pos] = $self->describe_set(\@phones, [keys %{$pd->{gen_inventory}}], ie => 1); 
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

# TODO: update for these rule types and features:
# - LtR vs. RtL
# - {except} without a {condition} (unused probably)
# - (future) formation of longs for moraic reasons.  Be sure to track this in the inventory.

# FIXME: More current issues:
# - There is a misstatement in 11598455.  It is a fundamental one: the rule as stated is
#   _always_ wrong, given that an assimilation to the _same_ environment interferes with
#   what would otherwise be the resolution of the resulting segment.  
# - In 634146154, nasality spreads across high V but doesn't latch on.  Describe that when it happens?

sub describe_rule {
  my ($self, $unsimplified_rule, $sstate, %args) = @_;
  my $FS = $self->{FS};

  # Perhaps we discard pointless things too aggressively, given the chance for remaining active.
  $sstate->find_matches($unsimplified_rule);
  my $rule = $sstate->simplify($unsimplified_rule);
  return '' if $rule->{pointless};
  $sstate->update($rule, $args{i}, record_old_inventory => 1);
  return '' if $rule->{pointless};

  # For now, assume there's only one change.  
  my @loci = ($rule->indices('effects'), $rule->indices('deletions'));
  if (@loci > 1) {
    return '(some rule where multiple phones change at once)';
  }

  my $locus = $loci[0];
  my %outcome = %{$sstate->{outcomes}{$locus}}; # HERE
  my %frames_examined = %{$sstate->{frames_examineds}{$locus}};
  my %matcheds = %{$sstate->{matcheds}};
  my $effect = defined $rule->{$locus}{effects} ? $rule->{$locus}{effects} : '.' x @{$FS->{features}};
  my $old_effect = $effect;
  my $precondition = $rule->{$locus}{condition};
  my ($pre, $old_pre, $post, $old_post);
  $old_pre = $pre = $rule->{$locus-1}{condition} if defined $rule->{$locus-1} and defined $rule->{$locus-1}{condition};
  $old_post = $post = $rule->{$locus+1}{condition} if defined $rule->{$locus+1} and defined $rule->{$locus+1}{condition};
  my $far = grep(($_ != $locus-1 and $_ != $locus and $_ != $locus+1), $rule->indices('condition'));

  if (defined $rule->{filter}) {
    $precondition = $FS->intersect($rule->{filter}{condition}, $precondition);
    $old_pre = $pre = $FS->intersect($rule->{filter}{condition}, $pre) if defined $pre;
    $old_post = $post = $FS->intersect($rule->{filter}{condition}, $post) if defined $post;
  }

  # whether or_pause specifications in the rule can actually happen
  my ($pre_pause, $post_pause) = (defined $rule->{$locus-1}{or_pause}, defined $rule->{$locus+1}{or_pause});
  # If only one frame causes change, rewrite to a non-assimilatory rule.  
  # We need to fix the values of both effect and influencer.
  if (keys %outcome <= 1) {
    $rule->{pointless} = 1, return undef if keys %outcome <= 0 and !defined $rule->{$locus}{deletions};
    if (keys %frames_examined > 1) {
      my($frame, $dummy) = each %outcome;
      my ($pre_frame, $post_frame) = ($frame, $frame);
      for (0..length($effect)-1) {
        substr($pre_frame, $_, 1) = '.' if substr($effect, $_, 1) ne '<';
        substr($post_frame, $_, 1) = '.' if substr($effect, $_, 1) ne '>';
        substr($effect, $_, 1) = substr($frame, $_, 1);
      }
      $pre = $FS->intersect($pre, $pre_frame);
      $post = $FS->intersect($post, $post_frame);
      $pre_pause = undef unless $FS->compatible($rule->{$locus-1}{or_pause}, $pre_frame);
      $post_pause = undef unless $FS->compatible($rule->{$locus+1}{or_pause}, $post_frame);
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
            $self->describe_set($deviations{$dev}, \@downset, extend => $matcheds{$locus}));
        
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
          $modified = ''; # this has a special meaning
        }
      } else {
        $no_main_VP = 1;
      }
    } else {
      $no_main_VP = 1;
    }
  }


  my $filter_text;
  if (defined $rule->{filter}) {
    $filter_text = $self->name_phoneset($rule->{filter}, $sstate->{old_inventory});
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
      my $filter_insig = undef;
      if (defined $rule->{filter}) {
        $frame_text = $rule->{bidirectional} ? "When the nearest $filter_text to either side is " :
                        ($effect !~ />/ ? "When the previous $filter_text is " : 
                        ($effect !~ /</ ? "When the next $filter_text is " : 'Assimilating to '));
        $filter_insig = $rule->{filter}{condition};
      } else {
        $frame_text = $rule->{bidirectional} ? 'Next to ':
                        ($effect !~ />/ ? 'After ' : 
                        ($effect !~ /</ ? 'Before ' : 'Assimilating to '));
      }
      my $phone = $frame;
      for (0..length($phone)-1) {
        substr($phone, $_, 1) = '.' unless substr($effect, $_, 1) =~ /[<>]/;
      }
      my $phone = $FS->overwrite(($effect !~ />/ ? $old_pre : 
                      ($effect !~ /</ ? $old_post : '.' x @{$FS->{features}})), $phone);
      @_ = grep /^$phone$/, @{$sstate->{old_inventory}};
      my @exceptions;
      push @exceptions, split / /, $rule->{$locus-1}{except} if $effect =~ /</;
      push @exceptions, split / /, $rule->{$locus+1}{except} if $effect =~ />/;
      for my $phone (@exceptions) {
          @_ = grep $_ !~ /^$phone$/, @_;
      }
      $frame_text .= $self->describe_set(\@_, $sstate->{old_inventory}, morpho => defined($filter_insig) ? 'bare' : 'indef', 
          bar_nons => 1, etic => 1,
          nobase => defined($filter_insig), insignificant => $filter_insig); 
          # disallowing nons isn't right, but it makes the thing readable
      if ($effect =~ /</) {
        $frame_text .= ' or pause' if $pre_pause and $rule->{or_pause} =~ /^$frame$/;
      }
      if ($effect =~ />/) {
        $frame_text .= ' or pause' if $post_pause and $rule->{or_pause} =~ /^$frame$/;
      }
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

      my $subject = $self->describe_set(\@deviants, $no_main_VP ? $sstate->{old_inventory} : \@susceptible,
          morpho => 'plural', etic => 1, sort_phones => 1, get_str_pattern => \@get_str_pattern);
      # if the subject is a list, redo, dropping things that have already appeared in some list
      my $subject_is_list = ($subject =~ /^\[.*\]$/); # klugy
      if ($subject_is_list) { 
        @deviants = @undescribed_deviants;
        $subject = $self->describe_set(\@deviants, $no_main_VP ? $sstate->{old_inventory} : \@susceptible,
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
            $frame_text .= ' become [' . join(' ', map $self->{phonetic_alphabet}->spell_spaced_string($outcome{$frame_representative}{$_}, null => 1), @deviants) . ']';
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
                $self->name_natural_class($_, undef, scheme => 'nominalised', nobase => 1, 
                    str => $self->get_str($get_str_pattern[0]),
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

          $_ = $self->name_natural_class($phone, $sstate->{inventory},
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
  

  my $environment_text = '';
  my ($pre_text, $post_text);
  my ($pre_phoneset, $post_phoneset);
  my ($pre_filter_nontrivial, $post_filter_nontrivial);
  # the 'or word-finally' aren't quite right, since the main rule might be a between.
  # FIXME: there is too much 'no phone' for the word-extremal stuff.
  if (defined $pre) {
    $pre_phoneset = { %{$rule->{$locus-1}} };
    $pre_phoneset->{condition} = $pre;
    $pre_phoneset->{enriched_condition} = enrich($pre, $sstate->{old_inventory});
    bless $pre_phoneset, 'PhoneSet';
    $pre_phoneset->simplify($FS);
    $pre_text = $self->name_phoneset($pre_phoneset, $sstate->{old_inventory}, morpho => 'indef', no_nothing => 1);
  }
  if (defined $post) {
    $post_phoneset = { %{$rule->{$locus+1}} };
    $post_phoneset->{condition} = $post;
    $post_phoneset->{enriched_condition} = enrich($post, $sstate->{old_inventory});
    bless $post_phoneset, 'PhoneSet';
    $post_phoneset->simplify($FS);
    $post_text = $self->name_phoneset($post_phoneset, $sstate->{old_inventory}, morpho => 'indef', no_nothing => 1);
  }
  if (defined $rule->{filter}) {
    $pre_filter_nontrivial = grep((!$pre_phoneset->matches($_) and $rule->{filter}->matches($_)), @{$sstate->{old_inventory}})
        if defined $pre;
    $post_filter_nontrivial = grep((!$post_phoneset->matches($_) and $rule->{filter}->matches($_)), @{$sstate->{old_inventory}})
        if defined $post;
  }
  

  my $main_clause = '';
  # It is friendliest not to describe rules which survive till before the _next_ rule as persistent.
  # (Is my interpretation subject to an  off-by-one error?)
  my $persistent = !(defined $rule->{inactive} and $rule->{inactive} <= $args{i} + 1);
  my @get_str_pattern;

  # For impersistent rules, no point favouring a featural description to a list.
  if ($insusceptibles_exist or !$persistent) {
    # Note that sounds excluded by {$locus}{except} are already outside of \@susceptible.
    $main_clause .= $self->describe_set(\@susceptible, $sstate->{old_inventory}, within => $precondition, 
        morpho => 'plural', etic => 1, sort_phones => 1, get_str_pattern => \@get_str_pattern);
  } else {
    $main_clause .= $self->name_natural_class($precondition, $sstate->{old_inventory}, morpho => 'plural');
    $get_str_pattern[0] = $self->str_part($precondition);
  }
  $modified = $FS->overwrite($get_str_pattern[0], $modified) if length($modified);
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
    $main_clause .= ' [' . join(' ', map $self->{phonetic_alphabet}->name_phone($_), @example_sounds)
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
          $main_VP .= '[' . join(' ', map $self->{phonetic_alphabet}->spell([split ' ', $outcome{$frame}{$_}], null => 1), @susceptible) . ']';
        } else {
          if ($main_VP =~ / and /) {
            $main_VP .= ' respectively'; 
          }
          # There might be dots in the frame which have come from assimilations in the effect
          # after frame mergers.  We want these to stomp on $modified.
          for (0..length($modified)-1) {
            substr($modified, $_, 1) = '.' if substr($frame, $_, 1) eq '.' and substr($effect, $_, 1) =~ /[<>]/;
          }
          $main_VP .= $self->name_natural_class($modified, $sstate->{inventory}, significant => $simple_effect, 
              morpho => 'plural', nobase => 1, accept_nothing => 1, use_dominant_str => 1);
        }
        # examples
        if (keys %outcome <= 1 and !$subject_is_list) {
          $main_VP .= ' [' . join(' ', map $self->{phonetic_alphabet}->spell_spaced_string($outcome{$frame}{$_}, null => 1), @example_sounds)
              . "$example_ellipsis]";
        }
      }
    } # $effect =~ /[01]/
    # FIXME: 'or word-finally' etc. is wrong when an assimilation only has one frame.  also, ' and become foo word-finally' has been wrong
    if ($effect =~ /</) {
      $_ = $effect;
      y/01u<>/...1./;
      $_ = $FS->overwrite($self->str_part(enrich($precondition, $sstate->{old_inventory})), $_);
      $main_VP .= ' and' if $main_VP;
      $main_VP .= ' assimilate in ' .
          $self->name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
      if (!defined($old_post) and !$far) {
        my $filter_insig = undef;
        my $appendage = '';
        if (defined $rule->{filter}) {
          if ($pre_filter_nontrivial) {
            $main_VP .= ' to the ' . 
                ($rule->{bidirectional} ? 'nearest' : 'previous') . 
                " $filter_text on either side when it is ";
            $filter_insig = $rule->{filter}{condition};
          } else {
            $main_VP .= ' to the ' . 
                ($rule->{bidirectional} ? 'nearest ' : 'previous ');
            $appendage .= ' on either side' if $rule->{bidirectional};
          }
        } else {
          $main_VP .= ' to a' . ($rule->{bidirectional} ? 'n adjacent ' : ' preceding ');
        }
        @_ = grep /^$pre$/, @{$sstate->{old_inventory}};
        for my $phone (split / /, $rule->{$locus-1}{except}) {
          @_ = grep $_ !~ /^$phone$/, @_;
        }
        $main_VP .= $self->describe_set(\@_, $sstate->{old_inventory}, morpho => 'bare', etic => 1, 
            nobase => defined($filter_insig), insignificant => $filter_insig);
        $main_VP .= $appendage;
        $pre = undef;
      } else {
        $main_VP .= ' to the ' . 
            ($rule->{bidirectional} ? 'nearest ' : 'previous ') . 
            defined $rule->{filter} ? $filter_text : 'phone';
      }
      if ($pre_pause) { # word-initial
        my $pausal_effect = $effect;
        for (0..length($pausal_effect)-1) {
          substr($pausal_effect, $_, 1) = substr($rule->{$locus-1}{or_pause}, $_, 1)
              if substr($pausal_effect, $_, 1) eq '<';
        }
        my $modified = $FS->overwrite($precondition, $pausal_effect); 
        $modified =~ s/u/./g;
        $main_VP .= ' and become ' . 
                    $self->name_natural_class($modified, $sstate->{inventory}, significant => $pausal_effect, morpho => 'plural', nobase => 1);
        $main_VP .= defined $rule->{filter} ? (" if no $filter_text " . ($rule->{bidirectional} ? 'is present' : 'precedes')) 
            : ($rule->{bidirectional} ? ' word-extremally' : ' word-initially');
      }
    }
    if ($effect =~ />/) {
      $_ = $effect;
      y/01u<>/....1/;
      $_ = $FS->overwrite($self->str_part(enrich($precondition, $sstate->{old_inventory})), $_);
      $main_VP .= ' and' if $main_VP;
      $main_VP .= ' assimilate in ' .
          $self->name_natural_class($_, undef, scheme => 'nominalised', nobase => 1);
      if (!defined($old_pre) and !$far) {
        my $filter_insig = undef;
        my $appendage = '';
        if (defined $rule->{filter}) {
          if ($pre_filter_nontrivial) {
            $main_VP .= ' to the ' . 
                ($rule->{bidirectional} ? 'nearest' : 'next') . 
                " $filter_text on either side when it is ";
            $filter_insig = $rule->{filter}{condition};
          } else {
            $main_VP .= ' to the ' . 
                ($rule->{bidirectional} ? 'nearest ' : 'next ');
            $appendage .= ' on either side' if $rule->{bidirectional};
          }
        } else {
          $main_VP .= ' to a' . ($rule->{bidirectional} ? 'n adjacent ' : ' following ');
        }
        @_ = grep /^$post$/, @{$sstate->{old_inventory}};
        for my $phone (split / /, $rule->{$locus+1}{except}) {
          @_ = grep $_ !~ /^$phone$/, @_;
        }
        $main_VP .= $self->describe_set(\@_, $sstate->{old_inventory}, morpho => 'bare', etic => 1,
            nobase => defined($filter_insig), insignificant => $filter_insig);  
        $main_VP .= $appendage;
        $post = undef;
      } else {
        $main_VP .= ' to the ' . 
            ($rule->{bidirectional} ? 'nearest ' : 'next ') . 
            defined $rule->{filter} ? $filter_text : 'phone';
      }
      if ($post_pause) { # word-final
        my $pausal_effect = $effect;
        for (0..length($pausal_effect)-1) {
          substr($pausal_effect, $_, 1) = substr($rule->{$locus+1}{or_pause}, $_, 1)
              if substr($pausal_effect, $_, 1) eq '>';
        }
        my $modified = $FS->overwrite($precondition, $pausal_effect); 
        $modified =~ s/u/./g;
        $main_VP .= ' and become ' . 
                    $self->name_natural_class($modified, $sstate->{inventory}, significant => $pausal_effect, morpho => 'plural', nobase => 1);
        $main_VP .= defined $rule->{filter} ? (" if no $filter_text " . ($rule->{bidirectional} ? 'is present' : 'follows')) 
            : ($rule->{bidirectional} ? ' word-extremally' : ' word-finally');
      }
    }
    $main_clause .= $main_VP;
  } # modified is not deletion


  my $envbit;
  if (defined $pre) {
    $envbit = '';
    if ($pre_text) {
      if (defined $rule->{filter}) {
        if ($pre_filter_nontrivial) {
          $environment_text .= ' if ' . 
              ($rule->{bidirectional} ? "the nearest $filter_text on either side" : "the previous $filter_text") . 
              ' is';
          # recompute, for significant
          $pre_text = $self->name_phoneset($pre_phoneset, $sstate->{old_inventory}, morpho => 'bare', no_nothing => 1, 
              nobase => 1, significant => $FS->subtract_features($pre_phoneset->{condition}, $rule->{filter}{condition}) );
        } else {
          $environment_text .= ' if there is ';
          $envbit .= ($rule->{bidirectional} ? ' earlier' : ' elsewhere') . ' in the word';
        }
      } else {
        $environment_text .= ($post_text ? ' between' : ($rule->{bidirectional} ? ' next to' : ' after'));
      }
      $environment_text .= " $pre_text" . $envbit;
    } else {
      $pre = undef;
    }
    if ($pre_pause) {
      if (defined $rule->{filter}) {
        if ($pre_text) {
          $environment_text .= ' or there is none';
        } else {
          $environment_text .= ' when it is the ' . 
              ($rule->{bidirectional} ? 'only' : 'first') . 
              " $filter_text in the word";
        }
      } else {
        $environment_text .= ' or' if $pre_text;
        $environment_text .= $rule->{bidirectional} ? ' word-extremally' : ' word-initially';
      }
    }
  } 
  if (defined $post) {
    $envbit = '';
    if ($post_text) {
      if (defined $rule->{filter}) {
        if ($post_filter_nontrivial) {
          # FIXME: "... is high or high semivowel".  Also: be less hung up on excepts!
          $environment_text .= ' if ' . 
              ($rule->{bidirectional} ? "the nearest $filter_text on either side" : "the next $filter_text") . 
              ' is';
          # recompute, for significant
          $post_text = $self->name_phoneset($post_phoneset, $sstate->{old_inventory}, morpho => 'bare', no_nothing => 1, 
              nobase => 1, significant => $FS->subtract_features($post_phoneset->{condition}, $rule->{filter}{condition}) );
        } else {
          $environment_text .= ' if there is ';
          $envbit .= ($rule->{bidirectional} ? ' later' : ' elsewhere') . ' in the word';
        }
      } else {
        $environment_text .= ($pre_text ? ' and' : ($rule->{bidirectional} ? ' next to' : ' before'));
      }
      $environment_text .= " $post_text" . $envbit;
      $environment_text .= ' in either order' if ($pre_text and $rule->{bidirectional} and !defined $rule->{filter});
    } else {
      $post = undef;
    }
    if ($post_pause) {
      if (defined $rule->{filter}) {
        if ($pre_text) {
          $environment_text .= ' or there is none';
        } else {
          $environment_text .= ' when it is the ' . 
              ($rule->{bidirectional} ? 'only' : 'last') . 
              " $filter_text in the word";
        }
      } else {
        $environment_text .= ' or' if $post_text;
        $environment_text .= $rule->{bidirectional} ? ' word-extremally' : ' word-finally';
      }
    }
  }
  if ($far) {
    $environment_text .= ' under some conditions on nonadjacent phones'; # FIXME
  }

  $text .= $main_clause unless $no_main_VP;
  $text .= ' persistently' if $no_main_VP and $persistent;
  $text .= $environment_text unless $no_main_VP and $frames_start_with_PP;
  if ($deviation_texts) {
    if ($no_main_VP) {
      $deviation_texts = ($text ? ', ' : '') . lcfirst substr($deviation_texts, 2);
    }
    $text .= $deviation_texts unless !$no_main_VP and ($all_all_deviates or $both_are_lists);
  }

  $text =~ s/^ *//;
  return ucfirst $text . '. ';
}

sub describe_rules {
  my ($self, $pd, %args) = @_;
  my %descriptions;
  my %to_be_numbered;
  my %old_conditional_resolutions = ();

  # initial and final rules to present
  $args{start} = $pd->{start_sequences} unless defined $args{start};
  $args{end} = @{$pd->{phonology}} unless defined $args{end};

  my $sstate = PhonologySynchronicState::initialise($pd, $self->{FS}, $args{start}, %args);

  RULE: for my $i ($args{start}..$args{end}-1) {
    printf STDERR "describing rule $i\n" if $Phonology::debug;
    my $rule = $pd->{phonology}[$i];

    $descriptions{$i}{rule} = $self->describe_rule($rule, $sstate, %args, i => $i);

    # Catch and describe the old conditional rules.
    for my $j (keys %{$sstate->{conditional_resolutions}}) {
      next if defined $old_conditional_resolutions{$j};      
      push @{$descriptions{$i}{post}}, "(TEMPORARY $j) " . 
          $self->describe_rule($pd->{phonology}[$j], $sstate, %args, i => $j);
    }
    %old_conditional_resolutions = %{$sstate->{conditional_resolutions}};

    # Rules which survive one rule shouldn't be described as persistent.
    if (defined $rule->{inactive} and $rule->{inactive} > $i + 1) {
      $to_be_numbered{$i} = 1;
      push @{$descriptions{$rule->{inactive}}{pre}}, "Rule ($i) becomes inactive.";
    }
    # TODO: (proximal) describe what happens to the remaining ones.
  }

  for my $i (keys %to_be_numbered) {
    $descriptions{$i}{rule} = "($i) $descriptions{$i}{rule}" if (defined $descriptions{$i}{rule});
  }
  
  my @final_list;
  for (sort keys %descriptions) {
    push @final_list, @{$descriptions{$_}{pre}} if defined $descriptions{$_}{pre};
    push @final_list, $descriptions{$_}{rule} if $descriptions{$_}{rule};
    push @final_list, @{$descriptions{$_}{post}} if defined $descriptions{$_}{post};
  }
  \@final_list;
}


1;
