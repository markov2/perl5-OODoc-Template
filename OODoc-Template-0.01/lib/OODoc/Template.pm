package OODoc::Template;
our $VERSION = '0.01';

=head1 NAME

OODoc::Template - Simple template system

=head1 SYNOPSYS

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 select OUTPUT;
 $t->parse($template, \%values);

=head1 DESCRIPTION

The OODoc::Template is probably the simpelest (most feature-less)
template system you can image, but still capable of doing almost
everything.

=head1 METHODS

=over 4

=item new OPTIONS

Create a new formatter object.  There are no OPTIONS defined yet.

=cut

sub new(@)
{   my ($class, %args) = @_;
    (bless {}, $class)->init(\%args);
}

sub init($)
{   my ($self, $args) = @_;
    $self;
}

=item parse TEMPLATE, VALUES

The TEMPLATE is a string, which is processed.  All tags are replaced
by values.  The VALUES is a hash which relates tags to values to be
inserted.  See L</Values> about the ways to control the output.

=cut

sub parse($$)
{   my ($self, $template, $values) = @_;

    my $uplevel      = $self->{values}; # unshift values
    $values->{NEXT}  = $uplevel;
    $self->{values}  = $values;

    while( $template =~ s|^(.*?)        # text before container
                           \<\!\-\-\{   # tag open
                           \s* (\w+)    # tag
                           \s* (.*?)    # attributes
                           \s* \}\-\-\> # tag open end
                         ||sx
         )
    {   print $1;
        my ($tag, $attributes) = ($2, $3);

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
            {   # oops: container is terminated for a brother.  Try to
                # correct my greedyness.
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
    $self->{values} = $uplevel;         # shift values
}

=item handle TAG, ATTRIBUTES, TEXT

The TAG was found to be filled-in.  In the block of the start tags,
some extra ATTRIBUTES (here still as string) may be specified. The
TEXT of the container (the encapsulated text) is the third argument.
The latter may be C<undef> if the tag is not used as block but as
terminator.  You probably will not call this method.

=cut

sub handle($$$)
{   my ($self, $tag, $attributes, $container) = @_;

    my %attrs;
    while( $attributes =~
        s/^\s*(\w+)                     # attribute name
           \s* (?: \=\> \s*             # optional value
                   ( \"[^"]*\"          # dquoted value
                   | \'[^']*\'          # squoted value
                   | \S+                # unquoted value
                   )
                )?
                \s* \,?                 # optionally separated by commas
          //xs)
    {   $attrs{$1} = $2;
    }

    my $value  = $self->valueFor($tag, \%attrs, \$container);
    return unless defined $value;       # ignore container

       if(!ref $value)           { print $value }
    elsif(ref $value eq 'HASH')  { $self->parse($container, $value) }
    elsif(ref $value eq 'ARRAY') { $self->parse($container, $_) for @$value }
    else { die "Huh? value for $tag is a ".ref($value)."\n" }
}

=item valueFor TAG, ATTRIBUTES, TEXTREF

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

=back

=head1 DETAILS

This module work as simple as possible: pass a string to the C<parse>
method with some values to be inserted in the string, and the result
is printed to stdout.  See the C<select> statement of Perl to read how
to redirect the output to a different destination, for instance a file.

=head2 Getting started

The general set-up is like this:

 use OODoc::Template;
 my $t = OODoc::Template->new;

 my $template = ".....";  # usually read from file
 my %values   = ( a => 3 );

 open OUTPUT, ">", $filename or die;
 select OUTPUT;

 $t->parse($template, \%values);

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

 
=head2 Attributes

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

Example:

 <!--{section nr=> 2, show_number, a => "monkey"}-->

The attributes result (internally) in a hash (of ARGS) which contains
the keys C<nr>, C<show_number>, and C<a> with respecively values
C<2>, C<true>, and C<monkey>.

=head2 Values

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

=head2 DYNAMIC value

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

=head2 Examples

Examples can be found in the manual page of the more powerful
L<Text::MagicTemplate|Text::MagicTemplate> module.  It's tutorial
page explains how you can use the values to build switches, and
other complex behavior.

=cut

1;
