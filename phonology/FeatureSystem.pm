package FeatureSystem;
use strict;

# There is no package for phones, since they are just strings.  The FeatureSystem object 
# is the one that knows how to handle its phones.

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

# Record the changes between from $phone0 to $phone1, as described below.
sub change_record {
  my ($phone0, $phone1) = (shift, shift);
  my @changes;
  for my $i (0..length($phone0)-1) {
    push @changes, "c " . substr($phone1, $i, 1) . " $i" if substr($phone0, $i, 1) ne substr($phone1, $i, 1);
  }
  @changes;
}


1;
