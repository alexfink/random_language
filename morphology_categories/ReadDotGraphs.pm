# ReadDotGraphs module for morphology.pl
# Read dot graphs using Graph::Reader::Dot, and convert them to how
# morphology.pl prefers them.

use strict;
use Graph::Reader::Dot;
use Graph;
use YAML::Any;

package ReadDotGraphs;

BEGIN {
    use Exporter();
    our @ISA = qw(Exporter);
    our @EXPORT = qw( &read );
}

my $debug = 0;

sub read {
  $Graph::Reader::Dot::UseNodeAttr = $Graph::Reader::Dot::UseEdgeAttr = 1;
  my $reader = Graph::Reader::Dot->new();
  my $g = $reader->read_graph(shift);

  my $cat = {graph => {}};
  # name is a graph attribute
  while ((my $att, $_) = each %{$g->get_graph_attributes()}) {
    # as below
    $cat->{$att} = /^\s*[\{\[](.*)[\}\]]\s*$/ ? YAML::Any::Load($_) : $_;
  }
  # TODO graph attributes
  for my $v ($g->vertices) {
    $cat->{graph}{$v} = {edges => []};
    if (defined $g->get_vertex_attributes($v)) {
      while ((my $att, $_) = each %{$g->get_vertex_attributes($v)}) {
        # If the value is delimited in [] or {}, take it as an inline-type YAML object.
        $cat->{graph}{$v}{$att} = /^\s*[\{\[](.*)[\}\]]\s*$/ ? YAML::Any::Load($_) : $_;
      }
    }
  }

  for my $e ($g->edges) {
    my $modifier = '';
    my $twoway;
    if (defined $g->get_edge_attributes($e->[0], $e->[1])) {
      while ((my $att, $_) = each %{$g->get_edge_attributes($e->[0], $e->[1])}) {
        # The only edge attribute that the morphology graph code can handle at present is weight.
        $modifier .= ":$_" if ($att eq 'weight');
        $twoway = $_ if ($att eq 'twoway');
      }
    }
    push @{$cat->{graph}{$e->[0]}{edges}}, $e->[1] . $modifier; 
    push @{$cat->{graph}{$e->[1]}{edges}}, $e->[0] . $modifier if $twoway;
  }

  if ($debug) {
    $, = ", ";
    $\ = "\n";
    print $cat->{odds};
    for my $x (keys %{$cat->{graph}}) {
      print "***";
      print $x;
      print @{$cat->{graph}{$x}{edges}};
      print $cat->{graph}{$x}{seed_weight};
      # one thing of each type to see if it's working
      print @{$cat->{graph}{$x}{product_with}} if defined $cat->{graph}{$x}{product_with};
      print %{$cat->{graph}{$x}{syncretism}} if defined $cat->{graph}{$x}{syncretism};
    }
  }

  return $cat;
}

1;

=pod

The special edge attribute 'twoway' causes normally directed edges
to be interpreted as undirected.

=cut

