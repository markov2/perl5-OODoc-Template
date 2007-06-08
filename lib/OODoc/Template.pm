use strict;
use warnings;

package OODoc::Template;

use IO::File   ();

=chapter NAME

OODoc::Template - Simple template system

=chapter SYNOPSYS

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 select OUTPUT;
 $t->parse($template, \%values);

=chapter DESCRIPTION

The OODoc::Template is probably the simpelest (most feature-less)
template system you can image, but still capable of doing almost
everything.

=chapter METHODS

=c_method new OPTIONS
Create a new formatter object.  The C<template> and C<search> options can
be overruled just as any other value.

=option  template CODE
=default template <build-in>
The callback to insert some template.  This is treated and used as any
other value.  By default M<includeTemplate()> is called.

=option  search STRING|ARRAY
=default search '.'
A colon separated list of directories, packed in a STRING or an ARRAY
of directories, to be searched to find a named template.  All search paths
are used when a template is being searched for, from inside out defined by
the nestings.
=cut

sub new(@)
{   my ($class, %args) = @_;
    (bless {}, $class)->init(\%args);
}

sub init($)
{   my ($self, $args) = @_;

    my $templ  = $args->{template} || sub { $self->includeTemplate($_[1]) };

    $self->pushValues
     ( template => $templ
     , search   => '.'
     );
    
    $self;
}

=method parse TEMPLATE, (VALUES|PAIRS)
The TEMPLATE is a string, which is processed.  All tags are replaced
by values.  The VALUES is a hash which relates tags to values to be
inserted.  These parameters can also be passed as list of PAIRS.

See L</Values> about the ways to control the output.
=cut

sub parse($@)
{   my ($self, $template) = (shift, shift);

    my $values = @_==1 ? shift : {@_};
    $values->{source} ||= 'parse()';
    $self->pushValues($values);

    while( $template =~ s|^(.*?)        # text before container
                           \<\!\-\-\{   # tag open
                           \s* (NOT_)?
                               (\w+)    # tag
                           \s* (.*?)    # attributes
                           \s* \}\-\-\> # tag open end
                         ||sx
         )
    {   print $1;
        my ($not, $tag, $attributes) = ($2, $3, $4);

        if($template =~ s| (.*?)             # something
                           ( \<\!\-\-\{      # tag close
                             \s* \/$tag      # "our" tag
                             \s* \}\-\-\>    # tag close end
                           )
                         ||sx
           )
        {   # found container
            my ($container, $endtag) = ($1, $2);

            if( $container =~ m/\<\!\-\-\{\s*$tag\b/ )
            {   # oops: container is terminated for a brother (nesting
                # is not permitted. Try to correct my greedyness.
                $template = $container.$endtag.$template;
                $self->handle($tag, $attributes, undef);
            }
            else
            {   # container is mine!
                $self->handle($tag, $attributes, $container);
            }
        }
        else
        {   # not a container
            $self->handle($tag, $attributes, undef);
        }
    }

    print $template;                    # remains
    $self->popValues;
}

=method parseFile FILENAME, (VALUES|PAIRS)
Parse the content of the file with specified FILENAME.  The current value
of C<search> is used as path to find it.
=cut

sub parseFile($@)
{   my ($self, $filename) = (shift, shift);
    
    my $values = @_==1 ? shift : {@_};
    $values->{source} ||= 'parseFile()';

    $self->parse($self->loadTemplate($filename));
}

=section Internals

=method handle TAG, [ATTRIBUTES, [TEXT]]

The TAG was found to be filled-in.  In the block of the start tags,
some extra ATTRIBUTES (here still as string) may be specified. The
TEXT of the container (the encapsulated text) is the third argument.
The latter may be C<undef> if the tag is not used as block but as
terminator.  You probably will not call this method.

=cut

sub handle($;$$)
{   my ($self, $tag, $attributes, $container) = @_;
    defined $attributes or $attributes = '';
    defined $container  or $container  = '';

    my %attrs;
    while( $attributes =~
        s/^\s*(\w+)                     # attribute name
           \s* (?: \=\> \s*             # optional value
                   ( \"[^"]*\"          # dquoted value
                   | \'[^']*\'          # squoted value
                   | \$\{ [^}]* \}      # complex variable
                   | \$\w+              # simple variable
                   | \S+                # unquoted value
                   )
                )?
                \s* \,?                 # optionally separated by commas
          //xs)
    {  my ($k, $v) = ($1, $2);
       defined $v or $v = 1;

       if($v =~ m/^\'(.*)\'$/)
       {  # Single quoted parameter, no interpolation
          $v = $1;
       }
       elsif($v =~ m/^\"(.*)\"$/)
       {  # Double quoted parameter, with interpolation
          $v = $1;
          $v =~ s/\$\{(\w+)\s*(.*?)}|\$(\w+)/$self->handle($1, $2)/ge;
       }
       elsif($v =~ m/^\$\{(\w+)\s*(.*?)}$/)
       {  # complex variables
          $v = $self->handle($1, $2);
       }
       elsif($v =~ m/^\$(\w+)$/)
       {  # simple variables
          $v = $self->handle($1);
       }

       $attrs{$k} = $v;
    }

    my $value  = $self->valueFor($tag, \%attrs, \$container);
    return unless defined $value;       # ignore container

       if(!ref $value)           { print $value }
    elsif(ref $value eq 'HASH')  { $self->parse($container, $value) }
    elsif(ref $value eq 'ARRAY') { $self->parse($container, $_) for @$value }
    else { die "Huh? value for $tag is a ".ref($value)."\n" }
}

=method valueFor TAG, ATTRIBUTES, TEXTREF
Lookup the value for TAG in the known data.  See L</Values> about the
way this is done.  The ATTRIBUTES (hash of key-values) and TEXTREF
(text of contained block) are used when the TAG related to a code
reference which is to produce new values dynamicly.
=cut

sub valueFor($$$)
{   my ($self, $tag, $attrs, $textref) = @_;

    for(my $set = $self->{values}; defined $set; $set = $set->{NEXT})
    {   
        if(defined(my $v = $set->{$tag}))
        {   # HASH  defines container
            # ARRAY defines container loop
            # object or other things can be stored as well, but may get
            # stringified.
            return ref $v eq 'CODE' ? $v->($tag, $attrs, $textref) : $v;
        }

        if(defined(my $code = $set->{DYNAMIC}))
        {   my $value = $code->($tag, $attrs, $textref);
            return $value if defined $value;
            # and continue the search otherwise
        }
    }

    undef;
}

=method allValuesFor TAG
Collects all values related to TAG in all nestings of values.  The most
preferred is listed first.
=cut

sub allValuesFor($)
{   my ($self, $tag) = @_;
    my @values;

    for(my $set = $self->{values}; defined $set; $set = $set->{NEXT})
    {   
        if(defined(my $v = $set->{$tag}))
        {   my $t = ref $v eq 'CODE' ? $v->($tag, $attrs, $textref) : $v;
            push @values, $t if defined $t;
        }

        if(defined(my $code = $set->{DYNAMIC}))
        {   my $t = $code->($tag, $attrs, $textref);
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
    $self->{values} = { %$attrs, NEXT => $self->{values} };
}

=method popValues
Remove one level of values.
=cut

sub popValues()
{   my $self = shift;
    $self->{values} = $self->{values}{NEXT};
}

=method includeTemplate ATTRIBUTES
This is the default implementation for the "template" tag.  The ATTRIBUTES
are set as values, visible in the included file.  Useful attributes are
C<file> -to specified an input file- and C<search> to define directories
as template search path.
=cut

sub includeTemplate($)
{   my ($self, $attrs) = @_;
    my $values = $self->pushValues($attrs);

    my $fn = $self->valueFor('file');
    unless(defined $fn)
    {   my $source = $self->valueFor('source') || '??';
        die "ERROR: there is no filename found with template in $source\n";
    }

    $self->popValues;
}

=method loadTemplate FILENAME
Returns the string, which is the whole contents of the file, and some
info about the file as HASH.
=cut

sub loadTemplate($)
{   my ($self, $relfn) = @_;
    my $absfn;

    if(File::Spec->file_name_is_absolute($relfn))
    {   my $fn = File::Spec->canonpath($relfn);
        $absfn = $fn if -f $fn;
    }

    unless($absfn)
    {   my @srcs = map { ref $_ eq 'ARRAY' ? @$_ : split(':',$_) }
                      $self->allValuesFor('source');

        foreach my $dir (@srcs)
        {   my $fn = File::Spec->rel2abs($relfn, $dir);
            last if -f $fn;
        }
    }

    unless($absfn)
    {   my $source = $self->valueFor('source');
        die "ERROR: Cannot find template $relfn as mentioned in $source\n";
    }

    if(my $cached = $templ_cache{$absfn})
    {   my $mtime = $cached->[1]{mtime};
        return @$cached if -M $absfn==$mtime;
    }

    my $in = IO::File->new($absfn, 'r');
    unless(defined $in)
    {   my $source = $self->valueFor('source');
        die "ERROR: Cannot read from $absfn, named in $source\n";
    }

    join '', $in->getlines;  # auto-close in
}

=chapter DETAILS

This module work as simple as possible: pass a string to the C<parse>
method with some values to be inserted in the string, and the result
is printed to stdout.  See the C<select> statement of Perl to read how
to redirect the output to a different destination, for instance a file.

=section Getting started

The general set-up is like this:

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 open OUTPUT, ">", $filename or die;
 select OUTPUT;

 $t->parse($template, \%values);

=section Expanding

The C<$template> string contains HTML with special comment blocks.  These
special comment blocks are replaced by the specified C<values>.  The block
can appear in two shapes (which may provided different output):

 <!--{TAG ATTRIBUTES}-->
    some text
 <!--{/TAG}-->

or

 <!--{TAG ATTRIBUTES}-->

The first shows a I<container>, the second a terminal tag.  The TAG is
one of the specified values. ATTRIBUTES are used when the TAG is not a
constant value, but dynamically produced.

When the TAG starts with "NOT_", it is used to negate
the boolean interpretation of the values returned by evaluating the tag:

 <!--{NOT_want_something ATTRIBUTES}-->
   ...
 <!--{/want_something}-->

An if-then-else looks like this:

 <!--{want_something ATTRIBUTES}-->
   ...
 <!--{ELSE_want_something}-->
   ...
 <!--{/want_something}-->

=section Tags

Tags are barewords (may only contain [0-9a-zA-Z_]), which are looked-up in
the "%values", which are passed with M<parse()> to produce a value (see
section about Values).
 
=section Attributes

Attibutes are values which are used when the text which is to be inserted
is produced dynamically.  Their syntax is like this:

 # attributes are optionally separated by comma's
 attrs:  attr , attrs
       | attr attrs

 # a hash initiation syntax may be used, but single
 # barewords as well
 attr:   bareword
       | bareword => "string'
       | bareword => 'string'
       | bareword => bareword
       | bareword => variable

 # pass value produced by other tag
 variable:
         '$' tag
       | '${' tag attrs '}'


A string may contain variables, which are stringified.  This means that
tags which produce hashes or arrays are not usuable to interpolate.

=example

 <!--{section nr => 2, show_number, a => "monkey", chapter => $cnr}-->

The attributes result (internally) in a hash (of ARGS) which contains
the keys C<nr>, C<show_number>, C<a>, and C<chapter> with respecively
values C<2>, true, C<monkey>, and the looked-up value for C<cnr>.

=section Values

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
called with the tag, the specified attributes (as HASH reference),
and the container's text.  The value which is returned can be anything
of the above (only CODE references are not accepted).

=back

=section DYNAMIC value

The procedure of a value lookup is quite straight forward: start with
the values defined by the innermost block (container) which defined a
HASH or ARRAY of HASHes, and work the way back through the enclosing
blocks until the initial values have been reached.

If the tag was not found as key, C<undef> is used.  If the key was found,
than the related value is treated as described in the previous section.

Working through the list of blocks, a miss on a hash will cause a second
lookup: for the key C<DYNAMIC>.  If a block's set of values contains this
key, the related CODE reference is called to produce a value.  If the
produced value is C<undef>, the search will continue to outer blocks.
Other results will be treated as any other value.

The C<DYNAMIC> keys may be used like AUTOLOAD: to handle unexpected
keys.  For instance, used in the initial hash of values (passed to
the C<parse> method) it can be used to produce warnings on use of
undefined tags.

=cut

1;
