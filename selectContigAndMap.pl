# select the related contigs sequences
# map to reference genome, then check which gap is correct

use warnings;

( $scafFile, $contigFile, $refFile ) = @ARGV;

# select the related contig file
open( SCA, "$scafFile" ) or die $!;
open( OUTPUT, ">newContig.fasta" ) or die $!;
while( <SCA> )
{
    if( !/>/ )
    {
	split "\t";
	$contig{ $_[ 0 ] } = "yes";
    }
}
close SCA;

open( CON, "$contigFile" ) or die $!;
while( <CON> )
{
    if( />/ )
    {
	if( />(\S+)/ )
	{
	    $ifPrint = "no";
	    if( defined( $contig{ $1 } ) )
	    {
		$ifPrint = "yes";
	    }
	}
    }

    if( $ifPrint eq "yes" )
    {
	print OUTPUT $_;
    }
}
close CON;
close OUTPUT;

# map to reference genome
`nucmer -c 30 -p newContig $refFile newContig.fasta`;
`show-coords -c -l -r -T newContig.delta > newContig.coords`;

# remove all small mappings
`perl /home/gisv85/script/scaffold/removeSmallMapping.pl newContig.coords newContigWithoutSmall.coords 5 10`;

# remove overlap mappings
`perl /home/gisv85/script/scaffold/removeOverlap.pl newContigWithoutSmall.coords newContig-new.coords`;

# output correct order of contigs
#`perl ~/script/scaffold/genRefScaffold.pl newContig-new.coords newContig-ref.scaf`;
`perl /home/gisv85/script/gapFill/genRefScaffoldWithIndex.pl newContig-new.coords newContig-ref.scaf`;
