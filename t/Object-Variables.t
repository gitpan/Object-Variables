BEGIN {
    package X;
    use Test::More tests => 25;
    use strict;
    use warnings;
    use Object::Variables qw(
        $scalar @array %hash
        reference_to
    );

    ok(1);

    sub set_testvars {
        my X $self = shift;
        use object vars;

        $scalar = 'one';
        @array = ( qw/two three/ );
        %hash = ( 'four' => 4, 'five' => 5 );

        my $rscalar = reference_to $self, '$scalar';
        my $rarray = ${reference_to $self, '@array'};
        my $rhash = ${reference_to $self, '%hash'};
        ok( $$rscalar eq 'one' );
        ok( $rarray->[0] eq 'two' && $rarray->[1] eq 'three' );
        ok( $rhash->{'four'} == 4 && $rhash->{'five'} == 5 );
    }

    sub examine_testvars {
        my X $self = shift;
        use object vars;

        ok( $scalar eq 'one' );
        ok( $array[0] eq 'two' && $array[1] eq 'three' );
        ok( $hash{'four'} == 4 && $hash{'five'} == 5 );

        $scalar = 'won';
        
        my $rscalar = reference_to $self, '$scalar';
        ok( $$rscalar eq 'won' );
        ok( $rscalar eq \$scalar );

        $self->nested_testvars;
    }

    sub examine_two {
        (my X $this) = @_;
        use object vars : $this ($scalar, @array, %hash);

        ok( $scalar eq 'won' );
        ok( $array[0] eq 'two' && $array[1] eq 'three' );
        ok( $hash{'four'} == 4 && $hash{'five'} == 5 );

        ok( \$scalar == reference_to $this, '$scalar' );

        $this->nested_testvars;
    }

    sub nested_testvars {
        my $self = shift;
        use object vars : $self;

        ok( $scalar eq 'won' );
        ok( $array[0] eq 'two' && $array[1] eq 'three' );
        ok( $hash{'four'} == 4 && $hash{'five'} == 5 );
    }

    sub recursive_testvars {
        my $self = shift;
        use object vars;
        my ($level) = @_;

        ok( defined $scalar && $scalar eq 'won' );

        unless ($level) {
            $self->recursive_testvars($level + 1);
        }
    }

    sub comments {
##      use object vars : $self;
##      \use object vars : $self;

        my $text = '\field vars : my $self ($scalar);';
        ok(1);
    }

    sub fancy {
        my $self = shift;
        use object vars : $self (
            $scalar, %hash, @array,
        );

        ok( defined $scalar && $scalar eq 'won' );
        ok( $array[0] eq 'two' and $array[1] eq 'three' );
        ok( $hash{'four'} == 4 && $hash{'five'} == 5 );
    }
    
    sub nolist {
        my $self = shift;
        use object vars ();
    }
    
    sub nolist2 {
        my $self = shift;
        use object vars : $self ();
    }
}

package main;

my $t = X->new;

$t->set_testvars;
$t->examine_testvars;
$t->examine_two;
$t->recursive_testvars(0);
$t->fancy;
$t->comments;
