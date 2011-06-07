#!/usr/bin/perl
# Generate random systems of morphological contrasts, such as case. 
# Alex Fink, July 2009, after an algorithm of Jim Henry
# http://listserv.brown.edu/archives/cgi-bin/wa?A2=ind0907b&L=conlang&P=11401
# Thanks also to Jim Henry for suggestions.

use strict;
use ReadDotGraphs;

my $debug = 0;

# Later: Add unmarkedness to nodes, incl. special unmarkedness in products,
# for use in the morphological form generation.  
# (It seems to me that the unmarkedness should be high if the seed_weight is high,
# but not likewise for low; and the unmarkedness should be low if the
# probability of retention is low.)

# Also probably boundness for contrasts (what things are usually clitics,
# what tightly woven in?)

# Things that were on my whiteboard:
# - what should be done about that e.g. inanimate agents aren't likely?
# - sporadically having a value have multiple expressions
# - how to handle optionality?  here?  in syntax?  both?

srand;

my $categories = {};

=pod 

C<weighted_one_of> randomly chooses a string (without its weight suffix) from the given list, 
with items suffixed by ":I<n>" for a number n weighted by n and others weighted by 1.
C<decolonize> parses this notation and returns (weight, token with weight stripped).
I've used the colon syntax for conformance with John Fisher's words.pl. 

=cut

sub weighted_one_of {
  my $sum = 0; 
  $sum += /:(.*)$/ ? $1 : 1 for @_;
  $sum = rand $sum;
  for (@_) { 
    # an assumption here about number formats
    if (/^(.*):([.0-9e-]*)$/) { 
      return $1 if ($sum -= $2) < 0;
    } else {
      return $_ if ($sum -= 1) < 0;
    }
  }
}

sub decolonize {
  local $_ = shift;
  /^(.*):(.*)$/ ? ($2, $1) : (1, $_);
}

=pod

The graph describing any given category is specified as a hash 
from nodes to hashes with the following keys.
These graphs are read from DOT files, with their edges expressed in
the usual DOT way and the other items below expressed as node attributes.

=over

=item C<edges>

The edges from this node.  Weights can be specified by appending ":I<n>" as usual.
Graphs are taken as directed, so undirected edges should be specified from both ends. 

=item C<seed_weight>

The weight of this node for seeding the subgraphs initially.

=item C<retention>

A measure of how likely this node is to be retained after doing the subgraphing,
ranging from 0 for never to 1 for always -- but not quite a probability.
The node is retained if the product of a C<1 + rand> for the category
and a C<rand> for the node exceeds its retention.  This is a cheap
and not especially good
way of having all the retentions for one category be correlated. 
This is the behaviour we want in the general case: languages tend
to systemically vary in how many contrasts of a given type they show.

=item C<uncorrelated_retention>

Like C<retention>, but without the correlation to anything else.  
The value is just a probability.  

=item C<seed_retention>

A probability of dropping (initial) seeds that are placed here, default 1.
Set less than 1 for values that don't usually form contrastive units on their own.

=item C<product_with>

A list of names of other categories, controlling the behaviour in products
(i.e. conflationary expressions of several categories).  If some nodes
of the graph for category A mention category B in C<product_with>,
only these nodes will be split apart in the product; but as the default behaviour,
if no nodes of category A mention category B they I<all> will be split.  

This is used e.g. for spatial relations in the case graph: it only makes sense
to split the local cases.  

=item C<no_product_with>

A list of names of other categories.  If any of the named categories
appear in a product with this category, this value will not appear at all.
(Misleadingly not just the negative of C<product_with>.)

=item C<syncretism>

A hash from categories to numbers.  If the product is taken with a category
appearing as a key, the edges which lie over this node in the factor
have their weight multiplied by the specified value.  A value exceeding 1
means that this node is likely to show syncretism, less than 1 that
this node is likelier than usual to make the distinction.

If the empty string appears among the keys, its value is taken as
the default syncretism for all categories not otherwise specified.

=item C<replaces>

A list of names of values in any categories.  If this value exists in
the ultimate generated language, the values listed in its C<replaces>
will always be dropped.  This is meant to be used for cases where an interaction
of categories would produce two differently-labelled values with the same meaning
(e.g. "state_entered" and "goal state_relation").

=item C<necessitates>

The opposite semantics to C<replaces>: if this value exists, the
values listed in C<replaces> will never be dropped.  

=back

=cut

sub equivalence_classes {
  my $head = shift;
  my @subgraphs;
  my %indices;
  # not the best shuffle ever, but it'll do
  for my $x (sort {rand >= 0.5} keys %$head) {
    my @g;
    if (@g = grep {$head->{$_} eq $x} keys %$head) {
      $indices{$x} = @subgraphs;
      push @subgraphs, \@g;
    }
  }
  \%indices, \@subgraphs; 
}

=pod

C<subgraphize> splits a graph (passed by reference) into a randomly generated number of subgraphs, 
at least 2.  (Why at least 2?  I'm assuming we only call this if we've already decided
our language will mark the contrast represented in the graph passed.  Postprocessing
might reduce the number of contrasts to one, though.)

If passed only one argument, C<subgraphize> returns a list of lists;
if passed two, it returns a hash instead (whose values happen to be node names,
though that shouldn't be depended upon).

=cut

sub subgraphize {
  my $graph = shift;
  # number of subgraphs: 1 + exp of a nonnegative number >= 2; upper bound okay
  my $assigned = 1 + int exp rand log keys %$graph; 
  my %head; # head{$x} is the node representing the subgraph $x is in
  my $head_only = shift; 
  
  for (1..$assigned) { 
    my $seed;
    do {
      $seed = weighted_one_of map {"$_:".($graph->{$_}{seed_weight} || 1)} keys %$graph 
    } while defined $head{$seed}; 
    $head{$seed} = $seed;
  }
  for (keys %head) {
    delete $head{$_} if (defined $graph->{$_}{seed_retention} and rand >= $graph->{$_}{seed_retention});
  }
  $assigned = keys %head;

  # Choosing from among all available edges with their weights is the most intuitive (and correct) thing.
  while ($assigned != keys %$graph) {
    my @fringe;
    for my $x (keys %$graph) {
      next if !defined $head{$x};
      push @fringe, "$x:$_" for (grep {!defined $head{decolonize $_}} @{$graph->{$x}{edges}});
    }
    my ($x, $y) = split /:/, weighted_one_of @fringe;
    if ($x) {
      $head{$y} = $head{$x};
    } else {
      # The graph must be disconnected; start a new category in a connected component.
      do {
        $x = weighted_one_of map {"$_:".($graph->{$_}{seed_weight} || 1)} keys %$graph 
      } while defined $head{$x};   
      $head{$x} = $x;
    }
    $assigned++;
  }

  return %head if $head_only; 
  equivalence_classes \%head;
}

=pod

C<remake_graph>(I<subgraphed graph>, I<graph>)
recasts the subgraphed graph, provided as a hash output by C<subgraphize>,
into a graph, so that the reduced set of categories can be used for products etc.
The second graph is the one that the first is a subgraphing of.

The new node names are the values of the hash.

The behaviour correct from the point of view of emulating how products behaved
before this says that 
edge weights add, and syncretisms also add, I<even if these are the default 1>.
The former says that ragged subdivisions which are bad cuts
have a greater chance of being unified across in subsequent products; 
I think that's okay, so we do it.
The latter says that large categories are more likely to syncretise; 
I think that's actually undesired.  So we take the mean syncretism instead.

I don't think seed_weight carries over well here; I'll also make it the mean.

seed_retention I don't model at all.  Using the average or something would
punish whatever value a node with low seed_retention gets lumped in with,
and we don't want that.  

no_product_with appears only if everything in this node has it.  
product_with appears if something in this node has it, but that's a poor
solution, as it means some categories lacking the product_with might still appear
in two different places; accordingly the expansion has to take care.

=cut

sub remake_graph {
  my ($graph, $head) = @_;
  my %new_graph;

  my %edges;
  for my $x (keys %$graph) {
    for my $y (@{$graph->{$x}{edges}}) {
      my($y_weight, $y_name) = decolonize $y;
      next if $head->{$y_name} eq $head->{$x};
        if (defined $edges{$head->{$x}}{$head->{$y_name}}) {
          $edges{$head->{$x}}{$head->{$y_name}} += $y_weight;
        } else {
          $edges{$head->{$x}}{$head->{$y_name}} = $y_weight;
        }
    }
  }
  for my $x (keys %edges) {
    while ((my $y_name, my $y_weight) = each %{$edges{$x}}) {
      push @{$new_graph{$x}{edges}}, "$y_name:$y_weight";
    }
  }

  my %total;
  $total{$head->{$_}}++ for keys %$graph;
  
  my %num_no_product_with;
  
  for my $x (keys %$graph) {
    for my $c (keys %{$graph->{$x}{syncretism}}) {
      $new_graph{$head->{$x}}{syncretism}{$c} = 1
          if !defined $new_graph{$head->{$x}}{syncretism}{$c};
      $new_graph{$head->{$x}}{syncretism}{$c} += ($graph->{$x}{syncretism}{$c} - 1) / $total{$head->{$x}}
          if defined $graph->{$x}{syncretism}{$c};
    }

    $new_graph{$head->{$x}}{seed_weight} = 1
        if !defined $new_graph{$head->{$x}}{seed_weight};
    $new_graph{$head->{$x}}{seed_weight} += ($graph->{$x}{seed_weight} - 1) / $total{$head->{$x}}
        if defined $graph->{$x}{seed_weight};

    for my $c (@{$graph->{$x}{product_with}}) {
      push @{$new_graph{$head->{$x}}{product_with}}, $c 
          if !grep $_ eq $c, @{$new_graph{$head->{$x}}{product_with}};
    }
    
    $num_no_product_with{$head->{$x}}{$_}++ for @{$graph->{$x}{no_product_with}};
  }
  
  for my $k (keys %new_graph) {
    @{$new_graph{$k}{no_product_with}} = 
        grep $num_no_product_with{$k}{$_} == $total{$k}, keys %{$num_no_product_with{$k}};
  }

  \%new_graph;
}

=pod

C<product>(I<name of graph 0>, I<ref to graph 0>, I<name of graph 1>, I<ref to graph 1>) 
constructs graph products.  The names are necessary since graphs refer to each other by name.

=cut

sub product {
  my($cat0name, $cat0, $cat1name, $cat1) = @_;
  my %graph;
  
  # Whether any (well, how many) nodes in each graph say product_with the other.
  # This is necessary for unfurling the default behaviour, and ugly.
  # Also grr scoping.
  # (It's also redundant with annotate_with_product_with below, but if it ain't broke.)
  my(%prod01, %prod10);
  for my $c (split / /, $cat1name) {
    $prod01{$c} = grep {grep $_ eq $c, @{$cat0->{$_}{product_with}};} keys %$cat0; 
  }
  for my $c (split / /, $cat0name) {
    $prod10{$c} = grep {grep $_ eq $c, @{$cat1->{$_}{product_with}};} keys %$cat1;  
  }
  sub use0 {
    my($x, $cat0, $cat1name) = (shift, shift, shift);
    for my $c (split / /, $cat1name) {
      return 0 if (defined $cat0->{$x}{no_product_with} and 
          grep $_ eq $c, @{$cat0->{$x}{no_product_with}});
    }
    return 1;
  }
  sub use1 {
    my($y, $cat1, $cat0name) = (shift, shift, shift);
    for my $c (split / /, $cat0name) {
      return 0 if (defined $cat1->{$y}{no_product_with} and 
          grep $_ eq $c, @{$cat1->{$y}{no_product_with}});
    }
    return 1;
  }
  sub product_with0 {  
    my($x, $p, $cat0, $cat1name) = (shift, shift, shift, shift);
    for my $c (split / /, $cat1name) {
      return 0 if ($p->{$c} and (not defined($cat0->{$x}{product_with}) or 
                        not grep $_ eq $c, @{$cat0->{$x}{product_with}}));
    }
    return 1;
  }
  sub product_with1 {   
    my($y, $p, $cat1, $cat0name) = (shift, shift, shift, shift);
    for my $c (split / /, $cat0name) {
      return 0 if ($p->{$c} and (not defined($cat1->{$y}{product_with}) or
                        not grep $_ eq $c, @{$cat1->{$y}{product_with}}));
    }
    return 1;
  }
  
  # Ick, so much code duplication.
  for my $x (keys %$cat0) {
    next unless use0($x,$cat0,$cat1name) and product_with0($x,\%prod01,$cat0,$cat1name);
    for my $y (keys %$cat1) {
      next unless use1($y,$cat1,$cat0name) and product_with1($y,\%prod10,$cat1,$cat0name);

      # Edges are as in the cartesian product, except that if this product isn't
      # allowed for some of the nodes we have to catch that. 
      $graph{"$x $y"}{edges} = [];
      for my $xx (@{$cat0->{$x}{edges}}) {
        my($xx_weight, $xx_name) = decolonize $xx;
        next unless use0($xx_name,$cat0,$cat1name);
        $xx_weight *= defined($cat1->{$y}{syncretism}{$_}) ? 
                              $cat1->{$y}{syncretism}{$_} : 
                     (defined($cat1->{$y}{syncretism}{""}) ? 
                              $cat1->{$y}{syncretism}{""} : 1) for split / /, $cat0name;
        push @{$graph{"$x $y"}{edges}}, product_with0($xx_name,\%prod01,$cat0,$cat1name) 
            ? "$xx_name $y:$xx_weight" 
            : "$xx_name _:$xx_weight";
      }
      for my $yy (@{$cat1->{$y}{edges}}) {
        my($yy_weight, $yy_name) = decolonize $yy;
        next unless use1($yy_name,$cat1,$cat0name);
        $yy_weight *= defined($cat0->{$x}{syncretism}{$_}) ? 
                              $cat0->{$x}{syncretism}{$_} : 
                     (defined($cat0->{$x}{syncretism}{""}) ? 
                              $cat0->{$x}{syncretism}{""} : 1) for split / /, $cat1name;
        push @{$graph{"$x $y"}{edges}}, product_with1($yy_name,\%prod10,$cat1,$cat0name) 
            ? "$x $yy_name:$yy_weight" 
            : "_ $yy_name:$yy_weight"; 
      }
      # Seed_weight multiplies.
      $graph{"$x $y"}{seed_weight} = ($cat0->{$x}{seed_weight} || 1) * 
                                     ($cat1->{$y}{seed_weight} || 1);
      # Seed_retention multiplies too.
      $graph{"$x $y"}{seed_retention} = ($cat0->{$x}{seed_retention} || 1) * 
                                        ($cat1->{$y}{seed_retention} || 1);

      # For product_with, this treatment is likely to be nonassociative.
      # I haven't tried to figure out whether it's sensible.
      # In any event, the whole implementation of product_with behaves ungracefully
      # in situations where we're multiplying G by H\times I, H and I being unrelated,
      # and G has product_by H.
      $graph{"$x $y"}{product_with} = 
          [defined($cat0->{$x}{product_with}) ? @{$cat0->{$x}{product_with}} : (), 
           defined($cat1->{$y}{product_with}) ? @{$cat1->{$y}{product_with}} : ()];
      # Similarly for no_product_with.
      $graph{"$x $y"}{no_product_with} = 
          [defined($cat0->{$x}{no_product_with}) ? @{$cat0->{$x}{no_product_with}} : (), 
           defined($cat1->{$y}{no_product_with}) ? @{$cat1->{$y}{no_product_with}} : ()];
      # Syncretism multiplies.
      $graph{"$x $y"}{syncretism} = defined($cat0->{$x}{syncretism}) ? 
                                         {%{$cat0->{$x}{syncretism}}} : {}; # deep copy
      if (defined $cat1->{$y}{syncretism}{""}) {
        for (keys %{$graph{"$x $y"}{syncretism}}) {
          $graph{"$x $y"}{syncretism}{$_} *= $cat1->{$y}{syncretism}{""} 
              if !defined($cat1->{$y}{syncretism}{$_});
        }
      }
      $graph{"$x $y"}{syncretism}{$_} = 
          defined($graph{"$x $y"}{syncretism}{$_}) ?
              $graph{"$x $y"}{syncretism}{$_} * $cat1->{$y}{syncretism}{$_} :
              $cat1->{$y}{syncretism}{$_}
          for keys %{$cat1->{$y}{syncretism}};
      # No retention or uncorrelated_retention or replaces or necessitates, 
      # those are done on the factors.
    } #for $y
  } #for $x
  
  for my $x (keys %$cat0) {
    next unless use0($x,$cat0,$cat1name) and not product_with0($x,\%prod01,$cat0,$cat1name);
    
    $graph{"$x _"}{edges} = [];
    for my $xx (@{$cat0->{$x}{edges}}) {
      my($xx_weight, $xx_name) = decolonize $xx;
      next unless use0($xx_name,$cat0,$cat1name);
      # we lie over too many categories for me to understand what syncretism would do
      if (product_with0($xx_name,\%prod01,$cat0,$cat1name)) {
        $xx_weight /= keys %$cat1;
        for (keys %$cat1) {
          push @{$graph{"$x _"}{edges}}, "$xx_name $_:$xx_weight" if use1($_,$cat1,$cat0name);
        }
      } else {
        push @{$graph{"$x _"}{edges}}, "$xx_name _:$xx_weight"; 
      }
    }
    $graph{"$x _"}{seed_weight} = $cat0->{$x}{seed_weight} || 1;
    $graph{"$x _"}{seed_retention} = $cat0->{$x}{seed_retention} || 1;
    $graph{"$x _"}{product_with} = 
        defined($cat0->{$x}{product_with}) ? [@{$cat0->{$x}{product_with}}] : [];    
    $graph{"$x _"}{no_product_with} = 
        defined($cat0->{$x}{no_product_with}) ? [@{$cat0->{$x}{no_product_with}}] : [];    
    $graph{"$x _"}{syncretism} = $cat0->{$x}{syncretism};
  }
 
  for my $y (keys %$cat1) {
    next unless use1($y,$cat1,$cat0name) and not product_with1($y,\%prod10,$cat1,$cat0name);
    
    $graph{"_ $y"}{edges} = [];
    for my $yy (@{$cat1->{$y}{edges}}) {
      my($yy_weight, $yy_name) = decolonize $yy;
      next unless use1($yy_name,$cat1,$cat0name);
      # nothing to syncretise
      if (product_with1($yy_name,\%prod10,$cat1,$cat0name)) {
        $yy_weight /= keys %$cat0;
        for (keys %$cat0) {
          push @{$graph{"_ $y"}{edges}}, "$_ $yy_name:$yy_weight" if use0($_,$cat0,$cat1name);
        }
      } else {
        push @{$graph{"_ $y"}{edges}}, "_ $yy_name:$yy_weight"; 
      }
    }
    $graph{"_ $y"}{seed_weight} = $cat1->{$y}{seed_weight} || 1;
    $graph{"_ $y"}{seed_retention} = $cat1->{$y}{seed_retention} || 1;
    $graph{"_ $y"}{product_with} = 
        defined($cat1->{$y}{product_with}) ? [@{$cat1->{$y}{product_with}}] : [];    
    $graph{"_ $y"}{no_product_with} = 
        defined($cat1->{$y}{no_product_with}) ? [@{$cat1->{$y}{no_product_with}}] : [];    
    $graph{"_ $y"}{syncretism} = $cat0->{$y}{syncretism};
  }

  return ("$cat0name $cat1name", \%graph);
}

sub show_graph {
  my $graph = shift;
  $, = ", ";
  $\ = "\n";
  print "--- GRAPH";
  for my $x (keys %$graph) {
    print $x;
    print @{$graph->{$x}{edges}} if defined $graph->{$x}{edges};
    my $attr;
    for $attr qw(seed_weight, retention, uncorrelated_retention) {
      if (defined $graph->{$x}{$attr}) {
        print $attr, $graph->{$x}{$attr};
      }
    }
    for $attr qw(product_with no_product_with replaces necessitates) {
      if (defined $graph->{$x}{$attr}) {
        print $attr, @{$graph->{$x}{$attr}};
      }
    }
    for $attr qw(syncretism) {
      if (defined $graph->{$x}{$attr}) {
        print $attr, %{$graph->{$x}{$attr}};
      }
    }
    print;
  }
}


sub joined_cartesian_product {
  my ($sep, $a, $b) = @_;
  my @l;
  for my $x (@$a) {
    push @l, (map $x . $sep . $_, @$b);
  }
  @l;
}

=pod

C<annotate_with_product_with> takes a graph and puts a top-level
property C<product_with> in, naming all the categories for which
it has C<product_with> in some node.

=cut

sub product_annotation {
  my $graph = shift;
  my %h;
  for my $x (keys %$graph) {
    $h{$_} = 1 for @{$graph->{$x}{product_with}};
  }
  keys %h;
}

=pod

If C<choose_contrasts> is given a list of category names, it returns a list
of forms in the product category.  E.g. 
  choose_contrasts qw(tense aspect mode)
would make a tense-aspect-mode system (as long as tense and aspect and mode are
names of category graphs).  Each form is a list of the situations it is used for.

C<choose_contrasts> takes the product of the argument graphs in the order
they are listed.  Listing them in a bad order may yield bad results
for reasons of the sloppy implementation of C<product_with>.

If given no arguments, C<choose_contrasts> will produce a list of lists of 
forms, choosing a random set of categories to mark in each.

C<choose_contrasts> will screw up if there are values of different categories
that share a name.  -- Actually, this may not be true anymore.  
But one should certainly avoid having a value of a category named '_'.

=cut

sub choose_contrasts {
  my %odds = ('' => 1, map(($_ => $categories->{$_}{odds}), keys %$categories)); # mustn't modify the global structure
  my %all_cats; # which categories have been used?
  my @sg; # list of lists of forms

  while(1) {
    my @cats;
    if (@_) {
      @cats = @_;
    } else {
      my $x = weighted_one_of map "$_:$odds{$_}", keys %odds;
      last unless $x;
      # Propagate along conflations for conflated things.
      # Order is significant here, because of the sketchy behaviour of product_with.
      my (@newcats, @tempcats);
      @cats = ();
      @newcats = $x;
      while (@newcats) {
        push @cats, @newcats; 
        @tempcats = @newcats;
        @newcats = ();
        for $x (@tempcats) {
          push @newcats, (grep {rand() < $categories->{$x}{conflation}{$_}} keys %{$categories->{$x}{conflation}});
        }
      }
      for (my $i=$#cats; $i>=0; --$i) {
        splice @cats, $i, 1 if grep $_ eq $cats[$i], @cats[0..$i-1];
      }
    }

    print @cats if $debug;

    $all_cats{$_} = 1 for @cats;
    my $cat = $cats[0];
    my $graph = $categories->{$cat}{graph};
    my $cat1;
    my (@left_prod_anns, @right_prod_anns);
    my (@left_graphs,  @left_heads,  @left_indices,  @left_members, 
       @right_graphs, @right_heads, @right_indices, @right_members);
    for (my $i = 1; $cat1 = $cats[$i]; $i++) {
      my $graph1 = $categories->{$cat1}{graph};

      @{$left_prod_anns[$i-1]} = product_annotation $graph;
      $left_graphs[$i-1] = $graph;
      %{$left_heads[$i-1]} = subgraphize $graph, 1;
      ($left_indices[$i-1], $left_members[$i-1]) = equivalence_classes $left_heads[$i-1];
      $graph = remake_graph $graph, $left_heads[$i-1];

      @{$right_prod_anns[$i-1]} = product_annotation $graph1;
      $right_graphs[$i-1] = $graph1;
      %{$right_heads[$i-1]} = subgraphize $graph1, 1;
      ($right_indices[$i-1], $right_members[$i-1]) = equivalence_classes $right_heads[$i-1];
      $graph1 = remake_graph $graph1, $right_heads[$i-1];

      ($cat, $graph) = product $cat, $graph, $cat1, $graph1;
      if ($debug) {
        print "--- HASHES";
        print %{$left_heads[$i-1]};
        print %{$right_heads[$i-1]};
        print;
        show_graph $left_graphs[$i-1]; 
      }
    }

    my @slot = @{subgraphize $graph};

    if ($debug) {
      print @$_ for @slot;
      print;
    }

    # An older version of this code without $record_pw had the following problem
    # in restoring product_with:
    # for example, say dual masculine gets retained in the left_graph
    # but trial masculine doesn't.  Then, after the recursive call, @$lefts 
    # will contain both, but 'trial masculine' simply isn't a node name
    # and so we can't tell at this stage that it has product_with associativity!
    # Hence $record_pw, which says which product_withs we shuold track.
    sub expand_product_names {
      my ($x, $cats, $left_prod_anns, $right_prod_anns,
           $left_graphs,  $left_indices,  $left_members, 
          $right_graphs, $right_indices, $right_members, 
          $i, $record_pw) = @_;

      if ($x !~ / /) { # base case
        for my $cat (@$cats) {
          return {} if (grep $_ eq $cat, @{$left_graphs->[$i+1]{$x}{no_product_with}});
        }
        my %prods = ($x => {map {
            my $cat = $_;
            $cat => ! (grep $_ eq $cat, @{$left_prod_anns->[$i+1]}
                  and !grep $_ eq $cat, @{$left_graphs->[$i+1]{$x}{product_with}}) 
          } @$record_pw});
        return %prods;
      }
      $x =~ /^(.*) ([^ ]*)$/;
      my ($my1, $my2) = ($1, $2);

      # For product_with, I'm just doing the direction where
      # the left node has them for the right category, since that's the only
      # direction for which the model really works well to begin with.
      
      my %prods;
      my $rights;
      my %lefts = map expand_product_names($_, $cats, $left_prod_anns, $right_prod_anns, 
                                            $left_graphs,  $left_indices,  $left_members, 
                                           $right_graphs, $right_indices, $right_members, 
                                           $i-1, [@$record_pw, $cats->[$i+1]]), 
          @{$left_members->[$i][ $left_indices->[$i]{$my1}]};
      @$rights = @{$right_members->[$i][$right_indices->[$i]{$my2}]};
      for my $cat (@$cats) {
        @$rights = grep {my $x = $_; 
            !grep $_ eq $cat, @{$left_graphs->[$i+1]{$x}{no_product_with}};
          } @$rights;
      }

      for my $l (keys %lefts) {
        if ($lefts{$l}{$cats->[$i+1]}) {
          for my $r (@$rights) {
            $prods{"$l $r"} = {%{$lefts{$l}}}; # some deep copy idiom
            for my $cat (keys %{$prods{"$l $r"}}) {
              $prods{"$l $r"}{$cat} = 0
                  if (grep $_ eq $cat, @{$right_prod_anns->[$i]}
                  and !grep $_ eq $cat, @{$right_graphs->[$i]{$r}{product_with}});
            }
          }
        } else {
          $prods{$l} = {%{$lefts{$l}}} 
              if $right_indices->[$i]{$my2} == 0; # conveniently subsuming the case  $my2 eq '_'
        }
      }

      %prods;
    }

    for my $s (@slot) {
      @$s = map {
          my %h = expand_product_names($_, \@cats, \@left_prod_anns, \@right_prod_anns, 
                                        \@left_graphs,  \@left_indices,  \@left_members,
                                       \@right_graphs, \@right_indices, \@right_members,
                                       $#left_heads, []);
          keys %h;
        } @$s;
    }

    push @sg, [@slot];

    last if @_;
    # adjust odds for repetition: if not many subgraphs came out, we're likelier
    # to give the contrast encoded in these subgraphs another chance
    for (@cats) {
      $odds{$_} = 1/(4*@slot-1) if $odds{$_} > 1/(4*@slot-1);
    }
  } #while(1)
  
  if ($debug) {
    for my $x (@sg) {
      print @$_ for @$x;
      print;
    }
  }

  my %drop;
  for my $cat (keys %all_cats) {
    my $extent = 1 + rand; # correlation for dropping these contrasts
    $drop{$_} = (
            # usual correlated retention
            ((defined $categories->{$cat}{graph}{$_}{retention}) and
             (rand() > $extent * $categories->{$cat}{graph}{$_}{retention})) or
            # uncorrelated retention
            ((defined $categories->{$cat}{graph}{$_}{uncorrelated_retention}) and
             (rand() > $categories->{$cat}{graph}{$_}{uncorrelated_retention}))
        ) for keys %{$categories->{$cat}{graph}};
  }
  
  for my $cat (keys %all_cats) {
    for my $x (keys %{$categories->{$cat}{graph}}) {
      unless ($drop{$x}) {
        delete $drop{$_} for @{$categories->{$cat}{graph}{$x}{necessitates}};
        $drop{$_} = 1 for @{$categories->{$cat}{graph}{$x}{replaces}};
      }
    }
  }
  
  for my $slot (@sg) {
    for my $form (@$slot) {
      SIT: for (my $i = $#$form; $i >= 0; $i--) {
        for (split / /, $form->[$i]) {
          splice(@$form, $i, 1), next SIT if $drop{$_};
        }
      }
    }
    @$slot = grep {@$_} @$slot;
  }

  @sg = grep {@$_ >= 2} @sg;

  return @sg;
}


$, = ", ";
$\ = "\n";

my $whole_system = 0;
my @catnames = ();
if (@ARGV == 1 and -d $ARGV[0]) {
  $whole_system = 1;
  my $dir = $ARGV[0];
  @ARGV = ();
  opendir(my $dh, $dir) || die "can't opendir $dir: $!";
  for (readdir($dh)) {
    push @ARGV, "$dir/$_" if (/\.dot$/ and -f "$dir/$_");
  }
  closedir $dh;
} 
for (@ARGV) {
  my $cat = ReadDotGraphs::read($_);
  $categories->{$cat->{name}} = $cat;
  push @catnames, $cat->{name};
}

my @sg = $whole_system ? choose_contrasts : choose_contrasts @catnames;
print "--- FINAL SYSTEM" if $debug;
for my $x (@sg) {
  print @$_ for @$x;
  print;
}




