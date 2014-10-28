# Подробное условие - Зайдельман2_2.rtf
# Дан двудольный граф.
# Найти наибольшее паросочетание, используя алгоритм Форда-Фалкерсона.
# Вход: матрица смежности k*l
# Вывод: массив 

use strict;
use warnings;

my $fin  = "in.txt";
my $fout = "out.txt";
if (-e $fin){
    open (STDIN, '<', $fin);
    close STDOUT;
    open (STDOUT, '>', $fout) or die "Could not open the output file $fout: $!";
}

my $k = <STDIN>;
my $l = <STDIN>;
# тут первый индекс из X, второй из Y
my $adj;
my $flow;
my @sx = ();
my @yt = ();

for (my $_ = 1; $_ <= $k; ++$_){
    my $s = <STDIN>;
    # $adj -> [$_] -> [0] = 1;
    push @{$adj->[$_]}, split (' ', $s);
}




if (-e $fin){
    close (STDIN);
    close (STDOUT);
}

sub findPath{
    my ($from, $to, )
}