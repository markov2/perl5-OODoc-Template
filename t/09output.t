#!/usr/bin/perl

use warnings;
use strict;

use lib 'lib', '../lib', 't';
use Test::More tests => 19;

use OODoc::Template;

my $t = OODoc::Template->new;
ok(defined $t, 'create object');
isa_ok($t, 'OODoc::Template');

my $plain = "This has no tags.";

#
## list context
#

my @list = $t->process($plain);
cmp_ok(scalar @list, '==', 2, 'LIST context');

my ($output, $tree) = @list;
ok(defined $output, 'got string');
is($output, $plain);

ok(defined $tree, 'got parsed');
is(ref $tree, 'ARRAY');

# re-use compiled

my @list2 = $t->process($tree);
cmp_ok(scalar @list2, '==', 2);

my ($output2, $tree2) = @list2;
ok(defined $output2, 'got string again');
is($output2, $plain);

ok(defined $tree2, 'got parsed again');
is(ref $tree2, 'ARRAY');
cmp_ok($tree, '==', $tree2);  # same

#
## SCALAR context
#

my $scalar = $t->process($plain);
ok(defined $scalar, 'SCALAR context');
is($scalar, $plain);

my $scalar2 = $t->process($tree);
ok(defined $scalar2);
is($scalar2, $plain);

#
## VOID context
#

my $out = '';
open OUT, '>', \$out or die $!;
my $old = select OUT;

$t->process($plain);

select $old;
ok(defined $out, 'VOID context');
is($out, $plain);
