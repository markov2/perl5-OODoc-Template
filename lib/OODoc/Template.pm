# This code is part of perl distribution OODoc-Template.  It is licensed under
# the same terms as Perl itself: https://spdx.org/licenses/Artistic-2.0.html

use strict;
use warnings;

package OODoc::Template;

use Log::Report  'oodoc-template';

use IO::File     ();
use File::Spec   ();
use Data::Dumper qw(Dumper);
use Scalar::Util qw(weaken);

my @default_markers = ('<!--{', '}-->', '<!--{/', '}-->');

=chapter NAME

OODoc::Template - Simple template system

=chapter SYNOPSIS

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 # list context keeps parse tree as well
 my ($output, $tree) = $t->process($template, \%values);

 # scalar context catches output in string
 my $output = $t->process($template, \%values);
 my $output = $t->process($tree, \%values);
 
 # void context output to selected file-handle
 select OUTPUT;
 $t->process($template, \%values);
 $t->process($tree, \%values);

=chapter DESCRIPTION

The C<OODoc::Template module> is a light-weight but powerful template
system, only providing display needs for applications, not behavior.
Let's start with a promise: this module will never grow into a new
programming language, as all the other template systems did over time.

There are at least a zillion comparible modules on CPAN, but this
one is mine ;-)

=section Short introduction

If you are used to template systems, then you should read this to
get a rapid overview on the features implemented by this module.

=over 4

=item
all values which are to be filled in are either passed-in when the
template is used, or defined within the template files themselves: there
are no dirty tricks played with name-spaces or such to collect the data.

=item
only the if(data available)-then-else construct is defined: programming
and text templates concepts should not be merged, simply because that
is awful.  Programming should be left to programming languages.

=item
the templates are compiled, so a second run within the same program
will be very fast.

=back

=chapter METHODS

=section Constructors

=c_method new OPTIONS
Create a new formatter object.  All OPTIONS are used as global set of
attributes, so used for each template processing started via this
object.  Probably, it is a good habit to set the required changes
of the predefined tags (like C<template> and C<search>) here.

=cut

sub new(@)
{   my ($class, %args) = @_;
    (bless {}, $class)->init(\%args);
}

sub init($)
{   my ($self, $args) = @_;

    $self->{cached}     = {};
    $self->{macros}     = {};

    my $s = $self; weaken $s;   # avoid circular ref
    $args->{template} ||= sub { $s->includeTemplate(@_) };
    $args->{macro}    ||= sub { $s->defineMacro(@_) };

    $args->{search}   ||= '.';
    $args->{markers}  ||= \@default_markers;
    $args->{define}   ||= sub { shift; (1, @_) };

    $self->pushValues($args);
    $self;
}

=section Processing

=method process STRING|SCALAR|ARRAY, HASH|PAIRS
The template is passed as unprocessed SCALAR to the STRING, or
an already parsed template (which is an ARRAY).  Values are
passed as a HASH or list of PAIRS.
=cut

sub process($)
{   my ($self, $templ) = (shift, shift);

    my $values = @_==1 ? shift : @_ ? {@_} : {};

    my $tree     # parse with real copy
      = ref $templ eq 'SCALAR' ? $self->parseTemplate($$templ)
      : ref $templ eq 'ARRAY'  ? $templ
      :                          $self->parseTemplate("$templ");

    defined $tree
        or return ();

    $self->pushValues($values)
        if keys %$values;

    my @output;
    foreach my $node (@$tree)
    {   unless(ref $node)
        {   push @output, $node;
            next;
        }
    
        my ($tag, $attr, $then, $else) = @$node;

        my %attrs;
        while(my($k, $v) = each %$attr)
        {   $attrs{$k} = ref $v ne 'ARRAY' ? $v
              : @$v==1 ? scalar $self->valueFor(@{$v->[0]})
              : join '',
                   map {ref $_ eq 'ARRAY' ? scalar $self->valueFor(@$_) : $_}
                      @$v;
        }

        (my $value, my $attrs, $then, $else)
           = $self->valueFor($tag, \%attrs, $then, $else);

        unless(defined $then || defined $else)
        {   defined $value
                or next;

            ref $value ne 'ARRAY' && ref $value ne 'HASH'
                or error __x"value for {tag} is {value}, must be single"
                     , tag => $tag, value => $value;

            push @output, $value;
            next;
        }

        my $take_else
           = !defined $value || (ref $value eq 'ARRAY' && @$value==0);

        my $container = $take_else ? $else : $then;

        defined $container
            or next;

        $self->pushValues($attrs) if keys %$attrs;

        if($take_else)
        {    my ($nest_out, $nest_tree) = $self->process($container);
             push @output, $nest_out;
             $node->[3] = $nest_tree;
        }
        elsif(ref $value eq 'HASH')
        {    my ($nest_out, $nest_tree) = $self->process($container, $value);
             push @output, $nest_out;
             $node->[2] = $nest_tree;
        }
        elsif(ref $value eq 'ARRAY')
        {    foreach my $data (@$value)
             {   my ($nest_out, $nest_tree) = $self->process($container, $data);
                 push @output, $nest_out;
                 $node->[2] = $nest_tree;
             }
        }
        else
        {    my ($nest_out, $nest_tree) = $self->process($container);
             push @output, $nest_out;
             $node->[2] = $nest_tree;
        }

        $self->popValues if keys %$attrs;
    }
    
    $self->popValues if keys %$values;

              wantarray ? (join('', @output), $tree)  # LIST context
    : defined wantarray ? join('', @output)           # SCALAR context
    :                     print @output;              # VOID context
}

=method processFile FILENAME, HASH|PAIRS
Process the content of the file with specified FILENAME.  The current
value of the C<search> path is used as path to find it.  The returns
behaves the same as M<process()>.

If the FILENAME is not found, then C<undef> is returned as output.
However, then this method is used in VOID context, there is no output:
then an error is raised in stead.

The result of parsing is cached, so there is no need for optimization:
call this method as often as you want without serious penalty.
=cut

sub processFile($;@)
{   my ($self, $filename) = (shift, shift);

    my $values = @_==1 ? shift : {@_};
    $values->{source} ||= $filename;

    my $cache  = $self->{cached};

    my ($output, $tree, $template);
    if(exists $cache->{$filename})
    {   $tree   = $cache->{$filename};
        $output = $self->process($tree, $values)
            if defined $tree;
    }
    elsif($template = $self->loadFile($filename))
    {   ($output, $tree) = $self->process($template, $values);
        $cache->{$filename} = $tree;
    }
    else
    {   $tree = $cache->{$filename} = undef;
    }

    defined $tree || defined wantarray
        or error __x"cannot find template file {fn}", fn => $filename;

              wantarray ? ($output, $tree)  # LIST context
    : defined wantarray ? $output           # SCALAR context
    :                     print $output;    # VOID context
}

=section Internal administration

=method defineMacro TAG, ATTRIBUTES, THEN, ELSE
=cut

sub defineMacro($$$$)
{   my ($self, $tag, $attrs, $then, $else) = @_;
    my $name = delete $attrs->{name}
        or error __x"macro requires a name";

    defined $else
        and error __x"macros cannot have an else part ({macro})",macro => $name;

    my %attrs = %$attrs;   # for closure
    $attrs{markers} = $self->valueFor('markers');

    $self->{macros}{$name} =
        sub { my ($tag, $at) = @_;
              $self->process($then, +{%attrs, %$at});
            };

    ();
    
}

=method valueFor TAG, [ATTRIBUTES, THEN, ELSE]
Lookup the value for TAG in the known data.  See section L</values>
about the way this is done.  The ATTRIBUTES (HASH of key-value pairs)
and THEN/ELSE content text references are used when the TAG relates to
a code reference which is to produce new values dynamicly.
=cut

sub valueFor($;$$$)
{   my ($self, $tag, $attrs, $then, $else) = @_;

#warn "Looking for $tag";
#warn Dumper $self->{values};
    for(my $set = $self->{values}; defined $set; $set = $set->{NEXT})
    {   my $v = $set->{$tag};

        if(defined $v)
        {   # HASH  defines container
            # ARRAY defines container loop
            # object or other things can be stored as well, but may get
            # stringified.
            return wantarray ? ($v, $attrs, $then, $else) : $v
                if ref $v ne 'CODE';

            return wantarray
                 ? $v->($tag, $attrs, $then, $else)
                 : ($v->($tag, $attrs, $then, $else))[0]
        }

        return wantarray ? (undef, $attrs, $then, $else) : undef
            if exists $set->{$tag};

        my $code = $set->{DYNAMIC};
        if(defined $code)
        {   my ($value, @other) = $code->($tag, $attrs, $then, $else);
            return wantarray ? ($value, @other) : $value
                if defined $value;
            # and continue the search otherwise
        }
    }

    wantarray ? (undef, $attrs, $then, $else) : undef;
}

=method allValuesFor TAG, [ATTRIBUTES, THEN, ELSE]
Collects all values related to TAG in all nestings of values.  The most
preferred is listed first.
=cut

sub allValuesFor($;$$$)
{   my ($self, $tag, $attrs, $then, $else) = @_;
    my @values;

    for(my $set = $self->{values}; defined $set; $set = $set->{NEXT})
    {   
        if(defined(my $v = $set->{$tag}))
        {   my $t = ref $v eq 'CODE' ? $v->($tag, $attrs, $then, $else) : $v;
            push @values, $t if defined $t;
        }

        if(defined(my $code = $set->{DYNAMIC}))
        {   my $t = $code->($tag, $attrs, $then, $else);
            push @values, $t if defined $t;
        }
    }

    @values;
}

=method pushValues HASH
Add new level of values to the known list.  The data in the HASH is
copied, and a reference to the copy returned.  The copy may be changed
afterwards.
=cut

sub pushValues($)
{   my ($self, $attrs) = @_;

    if(my $markers = $attrs->{markers})
    {   my @markers = ref $markers eq 'ARRAY' ? @$markers
          : map {s/\\\,//g; $_} split /(?!<\\)\,\s*/, $markers;

        push @markers, $markers[0] . '/'
            if @markers==2;

        push @markers, $markers[1]
            if @markers==3;

        $attrs->{markers}
          = [ map { ref $_ eq 'Regexp' ? $_ : qr/\Q$_/ } @markers ];
    }

    if(my $search = $attrs->{search})
    {   $attrs->{search} = [ split /\:/, $search ]
            if ref $search ne 'ARRAY';
    }

    $self->{values} = { %$attrs, NEXT => $self->{values} };
}

=method popValues
Remove one level of values.
=cut

sub popValues()
{   my $self = shift;
    $self->{values} = $self->{values}{NEXT};
}

=method includeTemplate TAG, ATTRIBUTES, THEN, ELSE
This is the implementation for the C<template> tag.
=cut

sub includeTemplate($$$)
{   my ($self, $tag, $attrs, $then, $else) = @_;

    defined $then || defined $else
        and error __x"template is not a container";

    if(my $fn = $attrs->{file})
    {   my $output = $self->processFile($fn, $attrs);
        $output    = $self->processFile($attrs->{alt}, $attrs)
            if !defined $output && $attrs->{alt};

        defined $output
            or error __x"cannot find template file {fn}", fn => $fn;

        return ($output);
    }

    if(my $name = $attrs->{macro})
    {    my $macro = $self->{macros}{$name}
            or error __x"cannot find macro {name}", name => $name;

        return $macro->($tag, $attrs, $then, $else);
    }

    error __x"file or macro attribute required for template in {source}"
      , source => $self->valueFor('source') || '??';
}

=method loadFile FILENAME
Returns a string containing the whole contents of the file, or C<undef>
if the file was not found.
=cut

sub loadFile($)
{   my ($self, $relfn) = @_;
    my $absfn;

    if(File::Spec->file_name_is_absolute($relfn))
    {   my $fn = File::Spec->canonpath($relfn);
        $absfn = $fn if -f $fn;
    }

    unless($absfn)
    {   my @srcs = map { @$_ } $self->allValuesFor('search');
        foreach my $dir (@srcs)
        {   $absfn = File::Spec->rel2abs($relfn, $dir);
            last if -f $absfn;
            $absfn = undef;
        }
    }

    defined $absfn
        or return undef;

    my $in = IO::File->new($absfn, 'r');
    unless(defined $in)
    {   my $source = $self->valueFor('source') || '??';
        fault __x"Cannot read from {fn} in {file}", fn => $absfn, file=>$source;
    }

    \(join '', $in->getlines);  # auto-close in
}

=section Parsing

=method parse STRING, (HASH|PAIRS)
This method is deprecated.  Please use M<process()>.
=cut

sub parse($@)
{   my ($self, $template) = (shift, shift);
    $self->process(\$template, @_);
}

=method parseTemplate STRING
Try to understand the STRING. Returned is a reference to a tree which
needs to be called with the correct values.
=cut

sub parseTemplate($)
{   my ($self, $template) = @_;

    defined $template
        or return undef;

    my $markers = $self->valueFor('markers');

    # Remove white-space escapes
    $template =~ s! \\ (?: \s* (?: \\ \s*)? \n)+
                    (?: \s* (?= $markers->[0] | $markers->[3] ))?
                  !!mgx;

    my @frags;

    # NOT_$tag supported for backwards compat
    while( $template =~ s!^(.*?)        # text before container
                           $markers->[0] \s*
                           (?: IF \s* )?
                           (NOT (?:_|\s+) )?
                           ([\w.-]+) \s*    # tag
                           (.*?) \s*    # attributes
                           $markers->[1]
                         !!xs
         )
    {   push @frags, $1;
        my ($not, $tag, $attr) = ($2, $3, $4);
        my ($then, $else);

        if($template =~ s! (.*?)           # contained
                           ( $markers->[2]
                             \s* \Q$tag\E \s*  # "our" tag
                             $markers->[3]
                           )
                         !!xs)
        {   $then       = $1;
            my $endline = $2;
        }

        if($not) { ($then, $else) = (undef, $then) }
        elsif(!defined $then) { }
        elsif($then =~ s! $markers->[0]
                          \s* ELSE (?:_|\s+)
                          \Q$tag\E \s*
                          $markers->[1]
                          (.*)
                        !!xs)
        {   # ELSE_$tag for backwards compat
            $else = $1;
        }

        push @frags, [$tag, $self->parseAttrs($attr), $then, $else];
    }

    push @frags, $template;
    \@frags;
}

=method parseAttrs STRING
Returns an ARRAY of PAIRS which will create the attributes for
the called code fragments.  The STRING syntax is described in the
DETAILS section of this manual page.
=cut

sub parseAttrs($)
{   my ($self, $string) = @_;

    my %attrs;
    while( $string =~
        s!^\s* (?: '([^']+)'        # attribute name (might be quoted)
               |   "([^"]+)"
               |   (\w+)
               )
           \s* (?: \= \>? \s*       # an optional value
                   ( \"[^"]*\"          # dquoted value
                   | \'[^']*\'          # squoted value
                   | \$\{ [^}]+ \}      # complex variable
                   | [^\s,]+            # unquoted value
                   )
                )?
                \s* \,?             # optionally separated by commas
         !!xs)
    {   my ($k, $v) = ($1||$2||$3, $4);
        unless(defined $v)
        {  $attrs{$k} = 1;
           next;
        }

        if($v =~ m/^\'(.*)\'$/)
        {   # Single quoted parameter, no interpolation
            $attrs{$k} = $1;
            next;
        }

        $v =~ s/^\"(.*)\"$/$1/;
        my @v = split /( \$\{[^\}]+\} | \$\w+ )/x, $v;

        if(@v==1 && $v[0] !~ m/^\$/)
        {   $attrs{$k} = $v[0];
            next;
        }

        my @steps;
        foreach (@v)
        {   if( m/^ (?: \$(\w+) | \$\{ (\w+) \s* \} ) $/x )
            {   push @steps, [ $+ ];
            }
            elsif( m/^ \$\{ (\w+) \s* ([^\}]+? \s* ) \} $/x )
            {   push @steps, [ $1, $self->parseAttrs($2) ];
            }
            else
            {   push @steps, $_ if length $_;
            }
        }

        $attrs{$k} = \@steps;
    }

    error __x"attribute error in '{tag}'", tag => $_[1]
        if length $string;

    \%attrs;
}

=chapter DETAILS

This module works as simple as possible: pass a string to M<process()>
with some values to be inserted in the string, and the result is printed
to STDOUT.

=section Getting started

=subsection context

There are three ways to produce output via the template system.  It
depends in which context you call M<process()>, where the output goes
to.

=over 4

=item . VOID context

the output is sent to the selected file-handle:

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 open OUTPUT, ">", $filename or die;
 select OUTPUT;

 $t->process($template, \%values);

or

 $t->process($template, a => 3);


=item . SCALAR context

the output is returned as string:

 my $output = $t->process($parsed, a => 13);

=item . LIST context

now both the output as the parsed template are returned.  You can reuse
the parsed templates, improving the performance enormously:

 my ($output, $parsed) = $t->process($template, a => 42);

=back
 
=subsection prepare for performance

When used in a website, you may want to produce the various templates
once, before the processes get forked.  Just select the output to
the null device, and then call all templates once.

   my %top;
   foreach my $lang ( qw/en nl de/ )
   {   my ($output, $parsed) = $t->process($template, lang => $lang);
       $top{$lang} = $parsed;
   }

   print $t->process($top{nl}, a => 42);

Some processing tricks will seriously hinder the caching of the parsed
templates.  If you use DYNAMIC, then you are on your own.  If you use
variables in the filenames for included templates, then you may miss
the cache.

=section Expanding variables

The C<$template> string contains HTML with special comment blocks.  These
special comment blocks are replaced by the specified C<values>.  The block
can appear in two shapes (which may provided different output):

   <!--{TAG ATTRIBUTES}-->
      some text
   <!--{/TAG}-->

or

   <!--{TAG ATTRIBUTES}-->

The first example shows a I<container>, the second a terminal tag.
The TAG is one of the specified values. ATTRIBUTES are used when the
TAG is not a constant value, but dynamically produced.

Containers are used to enclose a region where additional values
as set.  The TAG is related to an ARRAY of HASHes, which are effeciated
one after the other, as such creating a loop around this block

=section Conditionals

The standard conditional structure, which is used everywhere, is the
simple container.  When the container has values attached to is (always
a HASH or ARRAY-of-HASHES filled with key-value pairs), the content
is displayed.  So, a simple if-then looks like this:

 <!--{want_something ATTRIBUTES}-->
   ...
 <!--{/want_something}-->

The optional ATTRIBUTES are extra values set when processing the
container.  The pre-defined tag C<defined> can be used to only
set attributes: it's a no-op.

You may decide to be more explicit in the if-then, by using the
optional C<IF> keyword:

 <!--{IF want_something ATTRIBUTES}-->
   ...
 <!--{/want_something}-->

When the TAG starts with C<<NOT >> or C<<NOT_>>, it is used to negate
the boolean interpretation of the values returned by evaluating the tag:

 <!--{NOT want_something ATTRIBUTES}-->
   ...
 <!--{/want_something}-->

An if-then-else looks like this:

 <!--{want_something ATTRIBUTES}-->
   ...
 <!--{ELSE want_something}-->
   ...
 <!--{/want_something}-->

The C<want_something> tag must produce either a HASH or an ARRAY-of-HASHes
or C<undef>, because that is what containers do.  Because of parser
limitations, the 

=section Definition

=subsection tags

Tags are barewords (may only contain [0-9a-zA-Z_]), which are looked-up in
the C<< %values >>, which are passed with M<new()> and M<process()> to
produce a value.
 
=subsection attributes

Attibutes are values which are used when the text which is to be inserted
is produced dynamically.  Their syntax is like this:

 # attributes are optionally separated by comma's
 attrs:  attr , attrs
       | attr attrs

 # a hash initiation syntax may be used, but single
 # barewords as well
 attr:   bareword
       | bareword => " string " | bareword = " string "
       | bareword => ' char* '  | bareword = ' char* '
       | bareword => bareword   | bareword = bareword
       | bareword => variable   | bareword = variable

 string: ( char | variable ) *

 # pass value produced by other tag
 variable:
         '$' tag
       | '${' tag attrs '}'

A string may contain variables, which are stringified.  This means that
tags which produce hashes or arrays are not usuable to interpolate.

=examples

 <!--{section nr => 2, show_number, a => "monkey", chapter => $cnr}-->
 <!--{section nr=2 show_number a=monkey chapter=$cnr}-->

The attributes result (internally) in a hash (of ARGS) which contains
the keys C<nr>, C<show_number>, C<a>, and C<chapter> with respecively
values C<2>, true, C<monkey>, and the looked-up value for C<cnr>.

=subsection values

The values which are related to the tags are rather powerful.  When
a certain tag can not be found, the value is C<undef>.

=over 4

=item * undef
When the value is C<undef> (explicitly or because it was not found),
the container or terminator will be skipped.  The whole text simply
disappears.

=item * string
When the value is a C<string>, that string is inserted.  In case of
a container, the container's text is not used.

=item * HASH
In case the value is (reference to, of course) a HASH, the values
of that HASH are remembered.  They are used when parsing the contents
of the container, and overrule the values defined by encapsulating
blocks.  The container's text is parsed only once.

The HASH key of C<DYNAMIC> has a special purpose, which is described in
the next section.  The C<NEXT> key is reserved.

=item * ARRAY of HASHes
When the value is an ARRAY of HASHes, the container is parsed again
for each HASH.  In practice, this is a C<foreach> loop over the
array.

=item * CODE
As last option, you can provide a CODE reference.  This function is
called with the tag, the specified attributes (as HASH reference), the
container's positive content (then part), and the container's negative
content (else part).  The content text is passed as reference, to avoid
needless copying of large strings.

Of course, the C<tag> as first parameter is usually not really
interesting: in most cases you already know which tag relates to
the routine.

A list of four elements must be returned: the value to be used,
a HASH with attributes to be used when processing the container,
the then-part and the else-part.  In most cases, the latter three
parameters are the same as when the code is called, but it may as
well be removed.

=back

=subsection the DYNAMIC value

The procedure of a value lookup is quite straight forward: start with
the values defined by the innermost block (container) which defined a
HASH or ARRAY of HASHes, and work the way back through the enclosing
blocks until the initial values have been reached.

If the tag was not found as key, C<undef> is used.  If the key was found,
than the related value is treated as described in the previous section.

Working through the list of blocks, a miss on a value HASH will cause
a second lookup: for the key C<DYNAMIC>.  If a block's set of values
contains this key, the related CODE reference is called to produce
a value.  If the produced value is C<undef>, the search will continue
to outer blocks.  Other results will be treated as any other value.

The C<DYNAMIC> keys may be used like AUTOLOAD: to handle unexpected
keys.  For instance, used in the initial hash of values (passed to
the C<parse> method) it can be used to produce warnings on use of
undefined tags.

=section Pre-defined tags

Tags can as well represent procedures, which are executed to produce
data when filling in templates (via CODE references), or represent
constants.

Pre-defined values:

=over 4

=item . search STRING|ARRAY
A colon separated list of directories, packed in a STRING or an ARRAY
of directories, to be searched to find a named template.  All search paths
are used when a template is being searched for, from inside out defined by
the nestings.  By default, only the current directory is searched.

=item . markers ARRAY-of-2..4|STRING
The markers are the open and close patterns which enclose tags which
needs processing. Each element can be a STRING or a regular expression.
The first two defined the opening of a container, the last two the
closing.  The third value defaults to the first with a slash appended.
The fourth value defaults to the second.  You can also use a
comma-delimited STRING with three or four values.

The markers are initialized as C<< <--{ >>, C<< }--> >>,
C<< <--{/ >>, and C<< }--> >>.  (I should suffice to define only the
first two, because the other follow the default production rules).

=back

Pre-defined procedures:

=over 4

=item . define
With this no-op code reference, you set additional values in the
environment.

=item . macro
A C<name> attribute is required.  Macro blocks are not inserted on the
place where they are defined, but only on the spot where they are used
via a C<template> tag.  Only the variables available on the moment of
application are used, with an exception of the C<markers>, which is taken
from the definition environment.

=item . template
Insert some template.  The tag requires either a C<file> or a C<macro>
attribute. The filename must be absolute or relative to one of the
searched directories.  The macro is the name of a pre-declared macro
block.

Then the C<file> cannot be found (for instance, when the path name
contains a language component but that template has not yet be
translated), then the C<alt> (alternative) is attempted if available.
=back

=example change the markers locally
In this example, the content of the container uses other markup
symbols than the container itself.

  <!--{define markers="<{,}>" }-->\
    value of c: <{c}>\
  <!--{/define}-->

=example use of macro
A macro is used to define a piece of template, but apply it later.

 <!--{macro name="chapter"}-->
    <h2><!--{title}--></h2>
 <!--{/macro}-->

 <!--{template macro="chapter" title="hi there!"}-->

=example use of template file

 <!--{template file=$lang/header.txt alt=en/header.txt}->

=section White-space removal

The template tags are usually quite large with respect to the output
that they produce.  Therefore, you often wish to use more lines in
the template file, than will be present in the output.  However, you
have to help the output processor.

A backslash (followed by any number of invisible blanks) before a new-line
will have that new-line, and all following (visually) blank lines removed.
When the first line with (visual) content starts with a (start or end)
marker, then the blanks before that are removed as well.  In other cases,
the blanks are left intact.

=example of white-space removal

The template looks like this:

 The follow\
 ing error was\

  produced:
     <!--{error}-->, \
     <!--{errno}-->

The output is:

 The following error was produced:
    No such file or directory, 2

=cut

1;
