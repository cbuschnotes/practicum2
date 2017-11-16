# Simple perl to strip and clean header for CHR files.
# Largely replaced by R loader logic

use strict;
foreach my $n (@ARGV){
	my $ofn=$n;
	my $nfn=$ofn;
	$nfn=~s/ //g;
	open(IN,$ofn) || die;
	my @data=(<IN>);
	close(IN);
	 #h2=tolower(gsub('_+','_',gsub('[-]','_',gsub('\\s+','_',gsub('\\#','num',gsub('\\%','pct',h2))))))
	$data[0]=~s/\%/pct/g;
	$data[0]=~s/\#/num/g;
	$data[0]=~s/ +/_/g;
	$data[0]=~s/[-]/_/g;
	$data[0]=~s/_+/_/g;
	$data[0]=lc($data[0]);
	print $data[0];
	open(IN,">$nfn") || die;
	print IN @data;
	close(IN);
}

#end of file