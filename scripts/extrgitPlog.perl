use strict;
#  Author: Audris Mockus
#
#Extract each revision from cvs log output
#

use Time::Local;

my %paths = ();
my ($rev, $tree, $parent, $aname, $alogin, $atime, $cname, $clogin, $ctime, $comment) = ("","","","","","","","","","");
my ($getHeader, $getBody, $getPaths, $nl, $stopBody) = (0, 0, 0, 0, 1);
my $prj = "";

sub output {
	$comment =~ s/\r/ /g;
	$comment =~ s/\;/SEMICOLON/g;
	$comment =~ s/\n/__NEWLINE__/g;
        my $nf = 0;
	# Printing body with each delta makes for a huge delta file
	#$stopBody = 0;
	foreach my $f (keys %paths){
                $nf ++;
		print "$prj\;$rev\;$tree\;$parent\;$aname\;$cname\;$alogin\;$clogin\;$paths{$f}\;$atime\;$ctime\;$f\;$comment\n";
	}
	%paths = ();
	$nl = 0;
	($rev, $tree, $parent, $aname, $alogin, $atime, $cname, $clogin, $ctime, $comment) = ("","","","","","","","","","");
}


while(<STDIN>){
	chop ();	
	#catch end of last revision information
	$nl ++;
	#print "$_\n";
	if (/^STARTOFTHECOMMIT:(.*)$/){
		output () if $rev ne "";
		$prj = $1;
		($getHeader, $getPaths, $getBody, $nl) = (1, 0, 0, 0);
		next;
	}
	#process file header
	if ($getHeader){
		$_ =~ s/ \| /\|/g;
		($rev, $tree, $parent, $aname, $alogin, $atime, $cname, $clogin, $ctime, $comment) = split(/\;/, $_, -1);
		$getHeader = 0;
		$getBody = 1;
		next;
		#print STDERR "getHeader $rev, $login, $date, $line\n"; 
	}
        if ($getBody){
		my $x = $_; 
		if ($x =~ /^NOTES/){
			$getBody = 0;
			$getPaths = 1;
			next;
		}else{
			if ($x =~ /^Delete:/){
				$stopBody = 1;
				next;
			}else{
				$comment .= "NEWLINE$x" if $x !~ /^$/ && !$stopBody;
				next;
			}
		}
        }
	if ($getPaths && /^$/){
		next;
	}
	if ($getPaths && /^[0-9]/){
		/(\d+)\s+(\d+)\s+(.*)$/;
		my ($nadd, $ndel, $path) = ($1, $2, $3);
		$paths{$path}="$nadd:$ndel";
	}
}

output ();
