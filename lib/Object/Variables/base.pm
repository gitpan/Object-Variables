##==============================================================================
## Object::Variables::base - Contains constructor for Object::Variables
##==============================================================================
## $Id: base.pm,v 1.1 2004/07/30 01:25:14 kevin Exp $
##==============================================================================
require 5.006;

package Object::Variables::base;
use strict;
use warnings;
our ($VERSION) = q$Revision: 1.1 $ =~ /Revision:\s+(\S+)/ or $VERSION = "0.0";
no strict 'refs';

=pod

=head1 NAME

Object::Variables::base - Contains constructor for Object::Variables

=head1 DESCRIPTION

This module just contains the L<new|Object::Variables/new> method for objects created using L<Object::Variables|Object::Variables>.

=head1 AUTHOR

Kevin Michael Vail <F<kvail>@F<cpan>.F<org>>

=cut

##==============================================================================
## These are used to set suitable initial values for each instance variable.
##==============================================================================
my %initializers = (
	'%' => sub { {} },
	'@' => sub { [] },
	'$' => sub { undef },
);

##==============================================================================
## new
##==============================================================================
sub new {
	my $class = shift;
	my $self = bless [], $class;

	push @$self, $initializers{$_}->() 
		foreach (split m//, ${"$class\::OBJECTVARTYPES"});
	return $self;
}

1;

##==============================================================================
## $Log: base.pm,v $
## Revision 1.1  2004/07/30 01:25:14  kevin
## Initial revision
##
##==============================================================================

