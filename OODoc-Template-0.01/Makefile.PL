use ExtUtils::MakeMaker;

require 5.006_001;

WriteMakefile
 ( NAME	        => 'OODoc::Template'
 , VERSION_FROM => 'lib/OODoc/Template.pm'
 , AUTHOR       => 'Mark Overmeer'
 , ABSTRACT     => 'Simple template system'
 , PREREQ_PM    =>
    { Test::More          => 0.47
    }
 );
