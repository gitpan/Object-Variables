use 5.006;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Object::Variables',
    VERSION_FROM      => 'lib/Object/Variables.pm',
    PREREQ_PM         => {
    	'Lexical::Util'		=>	'0.3',
    	'Tie::IxHash'		=>	'1.21',
    	'Filter::Simple'	=>	'0.78',
    	'Test::More'		=>	'0.47',
    },
    ABSTRACT_FROM     => 'lib/Object/Variables.pm',
    AUTHOR            => 'Kevin Michael Vail <kevin@vaildc.net>',
);
