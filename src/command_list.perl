#! /usr/local/bin/perl
#
# $Id: command_list.perl,v 1.1.1.1 1993/04/07 19:39:49 harry Exp $
#
# This script reads "command_list" and generates the following:
#
# cmndlst.icl		Fortran include with parameter definitions.
# cmndlst_data.icl	Fortran include with command data statements.
# cmndlst.h		C include with number of commands.
#
# $Log: command_list.perl,v $
# Revision 1.1.1.1  1993/04/07 19:39:49  harry
# Initial CVS checkin of IVE.
#
# Revision 1.2  1992/11/10  16:32:25  harry
# CHange comments from ! to # in command_list.
#
# Revision 1.1  1992/11/10  16:19:54  harry
# Initial revision
#
#
$max_commands = 0;
open(INPUT, "../src/command_list");
open(PARAMETERS, ">../src/cmndlst.icl");
print PARAMETERS "C derived from command_list by command_list.perl\n";
print PARAMETERS "      INTEGER CMNDLEN, NVCMND\n";
print PARAMETERS "      PARAMETER (CMNDLEN=24)\n";
open(DATA, ">../src/cmndlst_data.icl");
print DATA       "C derived from command_list by command_list.perl\n";
open(CINC, ">../h/cmndlst.h");
print CINC       "/* derived from command_list by command_list.perl */\n";
while (<INPUT>) {
    if (/^\#/) {next;}
    s///;
    chop;
    print CINC       "#define P_$_ $max_commands\n";
    $max_commands++;
    print PARAMETERS "      INTEGER    P_$_\n";
    print PARAMETERS "      PARAMETER (P_$_=$max_commands)\n";
    print DATA       "      DATA valcmd(P_$_) /'$_'/\n";
}
print PARAMETERS "      PARAMETER (NVCMND=$max_commands)\n";
print PARAMETERS "      CHARACTER*(CMNDLEN) valcmd(NVCMND)\n";
print PARAMETERS "      COMMON /valcmd/valcmd\n";
print CINC       "#define NVCMND $max_commands\n";
close INPUT;
close PARAMETERS;
close DATA;
close CINC;
