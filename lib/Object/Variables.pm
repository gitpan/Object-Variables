##==============================================================================
## Object::Variables - variables for objects
##==============================================================================
## Copyright 2004 Kevin Michael Vail
## This program is free software. It may be copied and/or redistributed under
## the same terms as Perl itself.
##==============================================================================
## $Id: Variables.pm,v 0.9 2004/07/30 01:24:45 kevin Exp $
##==============================================================================
require 5.006;

package Object::Variables;
use strict;
use warnings;
our ($VERSION) = q$Revision: 0.9 $ =~ /^Revision:\s+(\S+)/ or $VERSION = "0.0";
use Filter::Simple;
use Lexical::Util qw(frame_to_cvref lexical_alias);
use Tie::IxHash;
use Object::Variables::base;
use base qw(Exporter);
our @EXPORT_OK = qw(reference_to pedigree);
no strict 'refs';

##==============================================================================
## If this flag is set to a non-zero value, the filtered text will be sent
## to standard output during compilation.
##==============================================================================
our $DEBUG;

##==============================================================================
## This is indexed by class name; each class name has a "strictness" flag
## associated with it.
##==============================================================================
my %STRICTNESS;

##==============================================================================
## An array of bitmaps controlling which variables are defined in a particular
## method. The hash is used to detect and combine duplicates.
##==============================================================================
my (@BITMAPS, %BITMAPS);

=pod

=head1 NAME

Object::Variables - variables for objects

=head1 SYNOPSIS

The old way:

	package Example;

	sub new {
	    my $self = bless {}, shift;
	    $self->{foo} = 1;
	    $self->{bar} = [ qw(foo bar) ];
	    $self->{baz} = {};
	    return $self;
	}

	sub set_foo {
	    my ($self, $value) = @_;
	    $self->{foo} = $value;
	}

	sub foo {
	    my ($self) = @_;

	    $self->{foo};
	}

	sub method1 {
	    my ($self, $input) = @_;

	    $self->set_foo($input);
	    $self->{baz}{$self->{foo}} = 'huh?';
	    print $self->{bar}[$self->{foo}], "\n";
	}

The new way:

	package Example;
	use Object::Variables qw(set: $foo noset: @bar %baz);

	sub new {
	    my ($class) = @_;
	    my $self = $class->SUPER::new;
	    use object vars : $self;
	    $foo = 1;
	    @bar = qw(foo bar);
	    return $self;
	}

	sub method1 {
	    my ($self, $input) = @_;
	    use object vars;

	    $self->set_foo($input);
	    $baz{$foo} = 'huh?';
	    print $bar[$foo], "\n";
	}

=head1 DESCRIPTION

One area where Perl's object-oriented features is less than stellar is its
handling (or lack of handling) of instance variables. You can C<use
L<fields|fields>> to create variables that are checked at compile time, but you
still have to refer to each one as something like C<< $self->{scalar} >>, which
is especially inconvenient with arrays and hashes...you might have an expression
that has to be written as something like

	$self->{hash}{$self->{key}}[$self->{index}]

which is cumbersome, to say the least. C<Object::Variables> lets you write that
as

	$hash{$key}[$index]

which is certainly easier to write and to figure out later.

In addition, you can specify that automatic accessor methods are to be created
for some or all of the variables for the object.

This module is especially useful when combined with
L<Sub::Declaration|Sub::Declaration>.

=head1 HOW IT WORKS

=head2 Declaration

You declare the variables you want to use by listing them in the arguments
passed on the C<use> line. Include the leading "line noise" character so that
the type of the variable is known...it will be initialized to a suitable default
value when the object is constructed (B<undef> for scalars, C<[]> for arrays,
and C<{}> for hashes).

Variable names beginning with C<_> are private, and aren't made accessible to
subclasses.

You may also list any of the subroutines L<below|"EXPORTABLE SUBROUTINES"> that
you wish to have available.

In addition, the following directives can be included in the list.

=over 4

=item C<< access-ro: >>

All variables after this word have accessor methods generated for them. The
method name will be the same as the variable name minus the type character.

=item C<< access-rw: >>

Like C<access-ro:>, except that the generated method will set the variable's
value if called with arguments. In that case it will return the prior value.

=item C<< noaccess: >>

Turns off both C<access-ro:> and C<access-rw:>.

=item C<< set: >>

All variables after this word will have methods generated to set their values.
The method name will be C<set_> followed by the variable name.

=item C<< noset: >>

Turns off C<set:>.

=item C<< strict: >>

If this occurs anywhere within the argument list, each method must explicitly
name the variables it uses.

=item C<< self:$I<name> >>

Specifies that the name for the object reference will be $I<name>. The default
is C<$self> (i.e., C<self:$self>), but C++ fans might want to use C<< self:$this
>>. (You can leave off the C<$>.)

=back

=head2 Usage

You use the variables by including the following line at or near the beginning
of each method:

C<< S<< 	use object vars : I<$self> (I<varlist>); >> >>

where I<$self> is the variable holding the object reference, and I<varlist> is
the list of variable names. You may omit the C<I<$self>> part, in which case
the object reference is taken to be I<$self> (unless you've used the
L<self|"self:name"> directive). You may also omit the C<(I<varlist>)>
part, in which case all instance variables are made available (but this
generates a fatal error if the L<"strict:"> modifier was in the import list).

(For backward compatibility with earlier versions of this module, you may leave
off the word "use", but this is deprecated.)

This directive has effect both at compile time and at runtime.

=over 4

=item *

At compile time, it generates a B<my> declaration for each instance variable
that is used.

=item *

At run time, it links (aliases) these variables to the actual object field,
taking inheritance into account.

=back

That's it!

=head2 Implementation Details

C<Object::Variables> is implemented via a source filter in order to insert the
appropriate variable declarations into the source code. See
L<Filter::Simple|Filter::Simple> for more information about source filters.

=head1 METHODS

This method is inherited by all classes that C<use Object::Variables>.

=over 4

=item new

C<< I<$object> = I<$class>->new; >>

This is the object constructor. You must ensure that this is called, either by
not defining your own B<new> method, or by calling the superclass's method from
within yours. It creates the variables, including those for superclasses, and
sets up initial values.

The current implementation creates I<$object> as an array, but you should not
depend on this.

=back

=head1 EXPORTABLE SUBROUTINES

=over

=item reference_to

C<< I<$reference> = Object::Variables::reference_to(I<$object>, I<$varname>); >>

Returns a reference to the raw value for the specified variable. The raw value
is the scalar contained in the underlying object array or hash: the scalar
itself for scalar variables, or the appropriate reference for array or hash
variables.

=cut

##==============================================================================
## reference_to
##==============================================================================
sub reference_to ($$) {
	my ($self, $varname) = @_;
	my $class = ref $self;
	my $vars = \%{"$class\::OBJECTVARIABLES"};

	unless (exists $vars->{$varname}) {
		my ($package, $filename, $line) = caller;
		die <<"::";
$filename($line): \$varname: no such instance variable in $class
::
	}

	return \$self->[$vars->{$varname}];
}

=pod

=item pedigree

C<< I<@list> = pedigree(I<$class>); >>

Returns the list of ancestors to I<$class> in the reverse order that Perl
searches for methods, i.e., from the top down. The last entry in I<@list> is
I<$class>.

=cut

##==============================================================================
## _recursive_pedigree
##==============================================================================
sub _recursive_pedigree ($\%);
sub _recursive_pedigree ($\%) {
	my ($class, $pedigree) = @_;

	$pedigree->{$class} = 1;
	foreach (@{"$class\::ISA"}) {
		_recursive_pedigree $_, %$pedigree;
	}
}

##==============================================================================
## pedigree
##==============================================================================
sub pedigree ($) {
	my ($proto) = @_;
	my $class = ref $proto ? ref $proto : $proto;
	tie my %pedigree, 'Tie::IxHash';

	_recursive_pedigree $class, %pedigree;

	reverse keys %pedigree;
}

=pod

=back

=head1 DEBUGGING

If you are having a problem and need to see how C<Object::Variables> has
processed your code, you can set the variable C<$Object::Variables::DEBUG> to a
non-zero value. This causes the replaced program text to be sent to standard
output after it is filtered. You need to do this in a BEGIN block:

	BEGIN { $Object::Variables::DEBUG = 1 };

	use Object::Variables qw(...);

=head1 RESTRICTIONS

=over 4

=item *

Because of the way source filters work, the C<use object vars> directive will
get replaced wherever it occurs. To prevent this from happening where you don't
want it to happen (in a string or a comment, for example), just put a backslash
in front of it, which will be removed.  If for some reason you actually want the
string "\use object vars", use two backslashes.

=item *

If you split a module over more than one source file, only the first module that
gets compiled can contain variable declarations or directives; the others can
only import routines.  Why would you do this?  The only case I can think of is a
module that simply adds a method to another module.  In that case, this
restriction shouldn't be an issue...if you needed to add variables as well, you
would create a subclass.

=item *

If you have two variables with the same name in different types, and create
accessors for both of them, you'll only end up with an accessor for one of them.
Since people don't generally create accessors for anything but scalars, this
actually shouldn't arise much in practice.

=back

=head1 MODULES REQUIRED

L<Tie::IxHash|Tie::IxHash>,
L<Filter::Simple|Filter::Simple>,
L<Lexical::Util|Lexical::Util> (version 0.8 or later)

=head1 SEE ALSO

L<Perl6::Binding|Perl6::Binding>, L<Lexical::Alias|Lexical::Alias>,
L<fields|fields>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 Kevin Michael Vail

This program is free software.  It may be copied and/or redistributed under the
same terms as Perl itself.

=head1 AUTHOR

Kevin Michael Vail <F<kvail>@F<cpan>.F<org>>

=cut

use constant FL_ACCESS				=>	1;	## generate accessor
use constant FL_ACCESS_WRITE		=>	2;	## if accessor can set value, too
use constant FL_SET					=>	4;	## generate set_ method

my @create_accessors = (
	{							## [0] - used for read-only accessors
		'$' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					$_[0]->[$index];
				};
			}
		},
		'@' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					@{$_[0]->[$index]};
				};
			}
		},
		'%' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					%{$_[0]->[$index]};
				};
			}
		},
	},
	{							## [1] - used for read-write accessors
		'$' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					my $self = shift;
					my $value = $self->[$index];
					if (@_) {
						$self->[$index] = shift;
					}
					return $value;
				};
			}
		},
		'@' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					my $self = shift;
					my @value = @{$self->[$index]};
					if (@_) {
						@{$self->[$index]} = @_;
					}
					return @value;
				};
			}
		},
		'%' => sub {
			my ($class, $method, $index) = @_;
			$method = join '::', $class, $method;
			unless (defined &{$method}) {
				*{$method} = sub {
					my $self = shift;
					my %value = %{$self->[$index]};
					if (@_) {
						%{$self->[$index]} = @_;
					}
					return %value;
				};
			}
		},
	},
);

my %create_set_method = (
	'$' => sub {
		my ($class, $method, $index) = @_;
		$method = join '::', $class, "set_$method";
		unless (defined &{$method}) {
			*{$method} = sub {
				my $self = shift;
				$self->[$index] = shift;
			};
		}
	},
	'@' => sub {
		my ($class, $method, $index) = @_;
		$method = join '::', $class, "set_$method";
		unless (defined &{$method}) {
			*{$method} = sub {
				my $self = shift;
				@{$self->[$index]} = @_;
			};
		}
	},
	'%' => sub {
		my ($class, $method, $index) = @_;
		$method = join '::', $class, "set_$method";
		unless (defined &{$method}) {
			*{$method} = sub {
				my $self = shift;
				%{$self->[$index]} = @_;
			};
		}
	},
);

my ($package, $filename, $line, $selfname);

##==============================================================================
## import
##==============================================================================
sub import {
	my $class = shift;
	($package, $filename, $line) = caller;
	my $flags = '';
	my @routines;					## Routines to be exported
	my @messages;					## Accumulated error messages
	my @vartypes;					## List of object variable types
	my $no_new_vars;				## If true, can't add variables

	##--------------------------------------------------------------------------
	## If this is the first time we've been called for $package, create
	## %OBJECTVARIABLES and @OBJECTVARNAMES. We detect this by seeing if
	## $OBJECTVARTYPES exists or not.
	## $OBJECTVARIABLES{$varname} ==> the index of $varname within the list
	##	of object variables for the package
	## $OBJECTVARNAMES[$vindex] ==> the name of the variable at the specified
	##	index.
	## $OBJECTVARTYPES ==> contains a list of variable types (e.g., '$$@%')
	##	which control the variables created when the object is constructed.
	## If it's not the first time import has been called for $package, set
	## a flag to inhibit creating new variables, but allowing routines to be
	## imported.
	##--------------------------------------------------------------------------
	if (defined ${"$package\::OBJECTVARTYPES"}) {
		$no_new_vars = 1;
	} else {
		tie %{"$package\::OBJECTVARIABLES"}, 'Tie::IxHash';
		@{"$package\::OBJECTVARNAMES"} = ();
		${"$package\::OBJECTVARTYPES"} = '';
	}

	my $vars = \%{"$package\::OBJECTVARIABLES"};
	my $varnames = \@{"$package\::OBJECTVARNAMES"};

	##--------------------------------------------------------------------------
	## Get the list of fields defined in our superclasses, and their indices.
	## We keep a parallel count of the number of variables; if we find an index
	## that doesn't match, it probably means multiple inheritance has been
	## used, which is a no-no.
	## When we're through, %allvars will contain (in order) all of the variables
	## inherited from ancestor classes, with the class it's inherited from and
	## the index within the object array at which it is located.
	##--------------------------------------------------------------------------
	my %allvars;
	my $vindex = 0;
	ANCESTOR: foreach my $aclass (pedigree $package) {
		next if $aclass eq $package;
		next unless defined ${"$aclass\::OBJECTVARTYPES"};
		my $avars = \%{"$aclass\::OBJECTVARIABLES"};
		while (my ($varname, $aindex) = each %$avars) {
			if ($aindex != $vindex) {
				push @messages,
					"possible invalid multiple inheritance in $aclass";
				last ANCESTOR;
			}
			if ($aclass eq $package || $varname !~ /^._/) {
				$vars->{$varname} = $aindex;
				$varnames->[$aindex] = $varname;
				$allvars{$_} = '' if $package eq $aclass;
			}
			push @vartypes, unpack 'a1', $varname;
			++$vindex;
		}
	}
	die join("\n", "*** $filename($line):", @messages) . "\n" if @messages;

	##--------------------------------------------------------------------------
	## The above has given us the maximum index used so far.  Assign indices
	## and attributes to each of our own variables.
	##--------------------------------------------------------------------------
	foreach (@_) {
		/^[\$\@%]/ and do {
			if ($no_new_vars) {
				push @messages, "can't add variables to existing class";
			} elsif (/^([\$\@%])[[:alpha:]]\w*$/) {
				if (exists $allvars{$_}) {
					push @messages,
						"variable name '$_' duplicated within $package";
				} else {
					push @vartypes, $1;
					$varnames->[$vindex] = $_;
					$vars->{$_} = $vindex++;
					$allvars{$_} = $flags;
				}
			} else {
				push @messages, "invalid variable name '$_'";
			}
			next;
		};
		/^(.*):$/ and do {
			$1 eq 'strict' and do {
				$STRICTNESS{$package} = 1;
				next;
			};
			$1 =~ /^(no)?access(?:-(r[ow]))?$/ and do {
				my $access = defined $1 ? 0 : 1;
				vec($flags, FL_ACCESS, 1) = $access;
				if ($access) {
					if (defined $2) {
						my $writeable = $2 eq 'rw' ? 1 : 0;
						vec($flags, FL_ACCESS_WRITE, 1) = $writeable;
					} else {
						push @messages, "invalid directive '$_'";
					}
				}
				next;
			};
			$1 =~ /^(no)?set$/ and do {
				vec($flags, FL_SET, 1) = defined $1 ? 0 : 1;
				next;
			};
			push @messages, "invalid directive '$_'";
			next;
		};
		/^self:(.+)$/ and do {
			my $self = $1;
			if ($self && $self =~ /^\$?([[:alpha:]]\w*)$/) {
				$selfname = "\$$1";
			} else {
				push @messages, "invalid 'self' variable name '$self'";
			}
			next;
		};
		/^[[:alpha:]]\w+$/ and do {
			push @routines, $_;
			next;
		};
		##----------------------------------------------------------------------
		## Anything else is invalid!
		##----------------------------------------------------------------------
		push @messages, "invalid import item '$_'";
	}

	die join("\n", "*** $filename($line):", @messages) . "\n" if @messages;

	$selfname = '$self' unless defined $selfname;

	${"$package\::OBJECTVARTYPES"} = join '', @vartypes unless $no_new_vars;

	##--------------------------------------------------------------------------
	## Create any accessor methods that were requested.
	##--------------------------------------------------------------------------
	while (my ($varname, $flags) = each %allvars) {
		my ($type, $name) = unpack 'a1a*', $varname;
		if (vec($flags, FL_ACCESS, 1)) {
			$create_accessors[
				vec($flags, FL_ACCESS_WRITE, 1)
			]{$type}->($package, $name, $vars->{$varname});
		}
		if (vec($flags, FL_SET, 1)) {
			$create_set_method{$type}->($package, $name, $vars->{$varname});
		}
	}

	##--------------------------------------------------------------------------
	## If any routines were named, call the actual Exporter->export_to_level
	## method to export them.
	##--------------------------------------------------------------------------
	@routines ? $class->export_to_level(1, $class, @routines) : 1;
}

my $id = qr/[[:alpha:]_]\w*/;
my $arglist = qr/[\$\@%\w\d,\s]+/;

##==============================================================================
## _varnames $package, $bitmap;
## Returns a list of variable names corresponding to each 1 bit in $bitmap
##==============================================================================
sub _varnames ($$) {
	my ($package, $bitmap) = @_;
	my $varnames = \@{"$package\::OBJECTVARNAMES"};
	my @list = @{$varnames}[grep {vec $bitmap, $_, 1} (0 .. $#{$varnames})];

	return @list;
}

##==============================================================================
## translate_object_vars
##==============================================================================
sub translate_object_vars ($$) {
	my ($selfvar, $arglist) = @_;
	my $output = '';
	my $bitmap = '';
	my $varnames = \@{"$package\::OBJECTVARNAMES"};
	my $vars = \%{"$package\::OBJECTVARIABLES"};
	my $pos = pos;

	my $arg_newlines = defined $arglist ? $arglist =~ tr/\n/\n/ : 0;
	my $line_number = $line + (substr($_, 0, pos) =~ tr/\n/\n/);
	$selfvar = $selfname unless defined $selfvar;
	if (defined $arglist) {
		if ($arglist =~ /^\((.*)\)$/s) {
			foreach (
				map {
					s/^\s+//s;
					s/\s+$//s;
					$_;
				} split m/,/, $1
			) {
				/^$/ and next;
				/^[\$\@%][[:alpha:]_]\w*$/ and do {
					unless (exists $vars->{$_}) {
						die <<"::"
$filename($line_number): $_: no such variable in $package
::
					}
					vec($bitmap, $vars->{$_}, 1) = 1;
					next;
				};
				die <<"::";
$filename($line_number): '$_': invalid item in 'use object vars' list
::
			}
		}
	} elsif ($STRICTNESS{$package}) {
		die <<"::";
$filename($line_number): must list variables when 'strict:' is in effect
::
	} else {
		foreach my $bitnum (0 .. $#{$varnames}) {
			next unless defined $varnames->[$bitnum];
			vec($bitmap, $bitnum, 1) = 1;
		}
	}
	if ($bitmap) {
		my $bindex;
		if (exists $BITMAPS{$bitmap}) {
			$bindex = $BITMAPS{$bitmap};
		} else {
			$bindex = @BITMAPS;
			push @BITMAPS, $bitmap;
			$BITMAPS{$bitmap} = $bindex;
		}
		$output = 'my ('
				. join(', ', _varnames($package, $bitmap))
				. '); ';
		$output .= "Object::Variables::object_vars($selfvar, $bindex);";
		$output .= "\n" x $arg_newlines;
	}

	return $output;
}

##==============================================================================
## FILTER
##==============================================================================
FILTER {
	return unless defined $_ && $_ ne '';
	$_ = <<"::" . $_;
use base qw(Object::Variables::base);
::

	##--------------------------------------------------------------------------
	## Substitute the "object vars" directives not preceded by a backslash.
	##--------------------------------------------------------------------------
	s{
	    (?<!\\)                         ## Skip if preceded by a backslash
	    \b(?:use\s+)?object\s+vars\b\s* ## Look for "[use] object vars"
	    (?::\s*(\$\w+)\s*)?             ## "self" name into $1 if present
	    (\(\s*(?:$arglist)?\s*\))?      ## Capture arg list into $2 if present
	    \s*;                            ## Keep going to semicolon
	}{
		translate_object_vars $1, $2;
	}sexg;

	##--------------------------------------------------------------------------
	## Remove the backslash from any that start with one.
	##--------------------------------------------------------------------------
	s{\\(?=(?:use\s+)?object\s+vars\b)}{}gs;

	##--------------------------------------------------------------------------
	## If the $DEBUG flag is set, print the results.
	##--------------------------------------------------------------------------
	print $_, "\n" if $DEBUG;
};

##==============================================================================
## object_vars
##==============================================================================
sub object_vars ($$) {
	my ($self, $bindex) = @_;
	my ($package, $filename, $line) = caller;
	my $class = ref $self;
	my $bitmap = $BITMAPS[$bindex];
	my $vars = \%{"$class\::OBJECTVARIABLES"};
	my $varnames = \@{"$class\::OBJECTVARNAMES"};
	my $cvref = frame_to_cvref(1);

	foreach my $bitnum (0 .. $#{$varnames}) {
		next unless vec($bitmap, $bitnum, 1);
		my $varname = $varnames->[$bitnum];
		my $vindex = $vars->{$varname};
		for (unpack 'a1', $varname) {
			$_ eq '$' and do {
				my $msg = lexical_alias($cvref, $varname, \$self->[$vindex]);
				die "$filename($line): $msg\n" if $msg;
				last;
			};
			/^[\@%]$/ and do {
				my $msg = lexical_alias($cvref, $varname, $self->[$vindex]);
				die "$filename($line): $msg\n" if $msg;
				last;
			};
			die <<"::";
$filename($line): internal error: invalid var type '$varname'
::
		}
	}
}

1;

##==============================================================================
## $Log: Variables.pm,v $
## Revision 0.9  2004/07/30 01:24:45  kevin
## Pull the 'new' method into another class in another file.
## (This avoids an unintentional inheritance of our 'import' method.)
##
## Revision 0.8  2004/07/29 03:21:23  kevin
## Use lexical_alias rather than lexalias routine.
##
## Revision 0.7  2004/07/29 02:13:46  kevin
## 1. Change directive to 'use object vars'.
## 2. Move 'new' into separate class to prevent the 'import' routine from being
##    unexpectedly inherited.
##
## Revision 0.6  2004/07/27 22:10:24  kevin
## Fix problem in generating "set_" accessors.
##
## Revision 0.5  2004/07/27 01:53:14  kevin
## Don't wipe out $class::OBJECTVARTYPES on second import.
##
## Revision 0.4  2004/07/25 05:20:51  kevin
## Eliminate raw_value methods in favor of a single reference_to method,
## primarily for testing.
##
## Revision 0.3  2004/07/25 04:50:43  kevin
## Numerous cleanups and bug fixes. I think all known problems are now fixed.
##
## Revision 0.2  2004/07/23 02:20:26  kevin
## Bug fixes.
##
## Revision 0.1  2004/07/21 02:03:21  kevin
## Initial revision
##==============================================================================
