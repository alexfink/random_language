#!/usr/bin/perl
# Generate random phonologies, featurally and via ordered rules with persistence,
# with allophony and the potential for good morphophonology and the works.  (Getting there!)
# Alex Fink, January 2010 -- present.
# Thanks to Marcus Smith <http://smithma.bol.ucla.edu/> for unwitting inspiration,
# and Marcus and UPSID for being proximal sources for various numbers.
# (A much greater proportion of the numbers are wholly fabricated, though!)

# What next?  Should we privilege 
# (a) advanced inventory tracking, with the bigram transition matrix stuff; or
# (b) new phonology?  (long-distance rules; syllable tracking > moraic stuff)

use strict;
use YAML::Any;
use CGI;

use FeatureSystem;
use PhoneSet;
use PhonologicalRule;
use Phonology;
use PhonologySynchronicState;
use Transcription;
use PhonologyDescriber;

my $version = '0.3.1b';
my $credits = 'Gleb, a phonology generator, by Alex Fink' . 
              (' ' x (29 - length($version))) . # for a total length of 78
              "version $version";

my $verbose;
my $show_seed;
my $use_html;
my $CGI;
my $seed =  time ^ $$ ^ $$<<15; 



my $outfile;
my $annotate_output;
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

-I              Produce a segmental inventory, with frequencies of appearance
                in each syllable position.
-d              Produce English descriptions of the phonology's rules, etc.
-w N            Generate N random words.
-c              When generating random words, also compute canonical phonemic 
                representations, which don't require unnecessary rules.
-h              Use HTML.

-o <filename>   Phonology output file.  Defaults to no output.  The output is a 
                YAML-formatted, not human-friendly, collection of the data 
                needed to run the phonology generator.  
-O <filename>   As above, with a little extra annotation for 
                like translations of the internal phone notation.
                (Still not human-friendly.)
-i <filename>   Input the phonology from the named file, rather than generating
                a new one.

-r N            Use N as the random seed.
-v              Verbose.  Show progress and a few other things.
-D              Show some debugging output.
-p <string>     Do some conversions between phone formats.  Do nothing else.

USAGE
  exit 1;
}

sub parse_args {
  my $arg;
  while ($arg = shift) {
    if ($arg eq '-D') {
      $Phonology::debug++;
    }
    elsif ($arg eq '--noprune') {
      $Phonology::noprune++;
    }
    elsif ($arg eq '-r') {
      $seed = shift;
      die "-r expects an integer argument\n" if !defined $seed or ($seed !~ /^\-?[0-9]+$/);
    }
    elsif ($arg =~ /^-[oO]$/) {
      $outfile = shift;
      $annotate_output = 1 if $arg eq '-O';
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
      $verbose = $Phonology::verbose = 1;
    }
    elsif ($arg eq '--showseed') {
      $show_seed = 1;
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

die 'feature system not found' unless -f 'features.yml';
my $FS = FeatureSystem::load_file('features.yml');

my $phonetic_alphabet;
if ($use_html && -f 'IPA_HTML.yml') {
  $phonetic_alphabet = Transcription::load_file('IPA_HTML.yml', $FS);
} elsif (-f 'CXS.yml') {
  $phonetic_alphabet = Transcription::load_file('CXS.yml', $FS);
} else {
  die 'no suitable phonetic alphabet found';
}
$Phonology::debug_alphabet = Transcription::load_file('CXS.yml', $FS) if -f 'CXS.yml';
  
if (defined $phone_to_interpret) {
  $phone_to_interpret = $FS->parse($phone_to_interpret, undefined => 1) unless $phone_to_interpret =~ /^[.01u]*$/;
  print '[' . $phonetic_alphabet->name_phone($phone_to_interpret) . '] ' . $FS->feature_string($phone_to_interpret);
  $phone_to_interpret =~ /[01]/g;
  print '   ' . (pos($phone_to_interpret) - 1) if defined pos($phone_to_interpret);
  print "\n";
  exit 0;
}

print STDERR "seed $seed\n" if $verbose or $show_seed; 
srand $seed; 

my $pd;

if (defined $infile) {
  $pd = Phonology::load_file($infile, $FS);
} else {
  $pd = Phonology::generate($FS);
}

if (defined $outfile) {
  $pd->dump_file($outfile, $annotate_output);
}

my $pdes = PhonologyDescriber::new($phonetic_alphabet, YAML::Any::LoadFile('phon_descr.yml'), $use_html);

if ($show_inventory) {
  print $pdes->describe_inventory($pd, html => $use_html); 
}

if ($show_all) {
  $pdes->tabulate($pd, annotate_only => 1); # should this be given a name of its own?
  my ($template, $elaborations) = $pdes->describe_syllable_structure($pd, html => $use_html);
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
  my $rules = $pdes->describe_rules($pd);
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
print STDERR "generating sample words...\n" if $verbose;
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
          $phonetic_alphabet->spell($generated_word),
          "//</td>" 
        if defined $canonicalise;
    print "<td>/", 
          $phonetic_alphabet->spell($word), 
          "/</td><td>[",
          $phonetic_alphabet->spell($surface_word),
          "]</td></tr>\n";
  } else {
    print "//" . $phonetic_alphabet->spell($generated_word). "//\t" if defined $canonicalise;
    print "/" . $phonetic_alphabet->spell($word) . "/\t[" . $phonetic_alphabet->spell($surface_word) . "]\n";
    for my $phone (@$surface_word) {
      $_ = $phonetic_alphabet->name_phone($phone);
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




