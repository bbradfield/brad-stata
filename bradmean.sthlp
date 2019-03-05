{smcl}
{* *! version 1.5.3  05march2019}{...}
{vieweralsosee "[R] mean" "help mean"}{...}
{viewerjumpto "Syntax" "bradmean##syntax"}{...}
{viewerjumpto "Description" "bradmean##description"}{...}
{viewerjumpto "Options" "bradmean##options"}{...}
{viewerjumpto "Examples" "bradmean##examples"}{...}
{title:Title}

{phang}
{bf:bradmean} {hline 2} Computes multiple independent means in a single table

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradmean:}
[{varlist}]
{ifin}
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Weight}
{synopt:{opt svy:}}statistics will be survey weighted{p_end}
{synopt:{opth sub:pop(varname)}}subpopulation estimation by {it:varname}; {it:varname} must be 0/1{p_end}

{syntab:Over}
{synopt:{opth over(varlist)}}estimation over groups defined by {it:varlist}{p_end}
{synopt:{opth overopt(string)}}options for over variables{p_end}
{synopt:{opth test(string)}}options for significance testing{p_end}

{syntab:Output}
{synopt:{opth dis:play(string)}}general display options{p_end}
{synopt:{opth title(string)}}optional custom title or "{bf:none}" to display no title{p_end}
{synopt:{opth sort(string)}}sorting results within a series{p_end}
{synopt:{opth st:ats(string)}}select which statistics to be displayed{p_end}
{synopt:{opth format(string)}}formatting options for displayed statistics{p_end}
{synopt:{opth excel(string)}}Excel output options{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:weights} are allowed; see {help svyset}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradmean} computes multiple independent means of {varlist}. Estimations can be run by groups and can include significance testing.

{marker options}{...}
{title:Options}

{dlgtab:Weight}

{phang}
{opt svy} specifies that statistics will be survey weighted.

{phang}
{opth sub:pop(varname)} specifies that estimates be computed using subpopulation {varname}. {varname} must be 0/1.

{dlgtab:Over}

{phang}
{opth over(varlist)} specifies that estimates be computed for multiple groups, which are identified by the different values of the variable(s) {varlist}.

{phang}
{opth overopt(string)} has the following options:

{p2colset 8 18 19 8}{p2col:{opt nolab:els}} do not display over labels{p_end}
{p2colset 8 18 19 8}{p2col:{opt noleg:end}} do not display legend for over groups{p_end}
{p2colset 8 18 19 8}{p2col:{opt nomi:ss}} do not display groups with no non-missing values{p_end}
{p2colset 8 18 19 8}{p2col:{opt tot:al}} display overall statistics{p_end}
{p2colset 8 18 19 8}{p2col:{opt group}} display each group size below name (wide only){p_end}

{phang}
{opth test(string)} has the following options:

{p2colset 8 26 27 8}{p2col:{opt chi:2}} display Chi2 p-values for categorical variables. When data is {help svyset}, Chi2 is corrected to a Pearson F test{p_end}
{p2colset 8 26 27 8}{p2col:{opth ttest(string)}} display t-test p-values for {opt over:all} comparisons (only applies when there are 2 groups), {opt ind:ividual} comparisons, or {opt all} for both overall and individual{p_end}
{p2colset 8 26 27 8}{p2col:{opth ftest(string)}} display adjusted Wald F-test p-values for {opt over:all} comparisons, {opt ind:ividual} comparisons, or {opt all} for both overall and individual. {opth mtest(string)} allows adjustments for multiple comparisons using {opt b:onferroni}, {opt h:olm}, or {opt s:idak}{p_end}
{p2colset 8 26 27 8}{p2col:{opth star:s(numlist)}} creates up to 3 significance stars for overall p-values less than {it:{help numlist}} containing 0-3 values. Leaving {it:{help numlist}} empty defaults to p < 0.05 and p < 0.01{p_end}
{p2colset 8 26 27 8}{p2col:{opth script:s(numlist)}} creates up to 18 significance scripts for individual p-values less than  {it:{help numlist}} containing 0-1 values. Leaving {it:{help numlist}} empty defaults to p < 0.05{p_end}
{p2colset 8 26 27 8}{p2col:{opt stat}} display test statistics with p-values{p_end}
{p2colset 8 26 27 8}{p2col:{opt force}} display p-values even with stars or scripts enabled{p_end}
{p2colset 8 26 27 8}{p2col:{opt nofo:oter}} do not display footer explaining significance stars and scripts{p_end}

{dlgtab:Output}

{phang}
{opth display(string)} has the following options:

{p2colset 8 23 24 8}{p2col:{opt xi}} enable both xi value and xi variable labels{p_end}
{p2colset 8 23 24 8}{p2col:{opt xival:s}} enable xi value labels (default is {bf:ON}){p_end}
{p2colset 8 23 24 8}{p2col:{opt xivar:s}} enable xi variable labels (default is {bf:OFF}){p_end}
{p2colset 8 23 24 8}{p2col:{opt series}} enable both series value and series variable labels{p_end}
{p2colset 8 23 24 8}{p2col:{opt seriesval:s}} enable series value labels (default is {bf:ON}){p_end}
{p2colset 8 23 24 8}{p2col:{opt seriesvar:s}} enable series variable labels (default is {bf:OFF}){p_end}
{p2colset 8 23 24 8}{p2col:{opt wide}} print table in a wide format{p_end}
{p2colset 8 23 24 8}{p2col:{opth al:ign(string)}} choose {opt l:eft}, {opt c:enter}, or {opt r:ight} alignment of statistics{p_end}
{p2colset 8 23 24 8}{p2col:{opt nostat}} do not display statistic names (wide only & single statistic only){p_end}
{p2colset 8 23 24 8}{p2col:{opt noprint}} do not display table (can be used with Excel output){p_end}

{phang}
{opth title(string)} specifies an optional custom title or {bf:"none"} to display no title.

{phang}
{opth sort(string)} allows sorting within series by choosing direction ({bf:+} for ascending, {bf:-} for descending) and statistic (obs nyes mean se sd var min max).

{phang}
{opth st:ats(string)} allows users to choose from the following statistics:

{p2colset 8 14 15 8}{p2col:{opt obs}} observations{p_end}
{p2colset 8 14 15 8}{p2col:{opt nyes}} number of "yes" answers (only for binary variables){p_end}
{p2colset 8 14 15 8}{p2col:{opt mean}} mean{p_end}
{p2colset 8 14 15 8}{p2col:{opt se}} standard error{p_end}
{p2colset 8 14 15 8}{p2col:{opt sd}} standard deviation{p_end}
{p2colset 8 14 15 8}{p2col:{opt var}} variance{p_end}
{p2colset 8 14 15 8}{p2col:{opt ci}} confidence interval{p_end}
{p2colset 8 14 15 8}{p2col:{opt min}} minimum{p_end}
{p2colset 8 14 15 8}{p2col:{opt max}} maximum{p_end}
{p2colset 8 14 15 8}{p2col:{opt all}} all of the above{p_end}

{phang}
{opth format(string)} sets the formatting for statistics. Individual statistics can be formatted using {opth stat(string)} where {opt stat} can be obs, nyes, mean, se, sd, var, ci, min, max, count (obs/nyes), error (se/sd/var), or minmax (min/max). The following options are allowed:

{p2colset 8 27 28 8}{p2col:{opt round(#)}} round for both binary and continuous variables. Default is {bf:7}{p_end}
{p2colset 8 27 28 8}{p2col:{opt roundi(#)}} round for binary variables. Default is {bf:7}{p_end}
{p2colset 8 27 28 8}{p2col:{opt roundc(#)}} round for continuous variables. Default is {bf:7}{p_end}
{p2colset 8 27 28 8}{p2col:{opt pct}} format binary variables as a percentage{p_end}
{p2colset 8 27 28 8}{p2col:{opt per:cent}} format binary variables as a percentage{p_end}
{p2colset 8 27 28 8}{p2col:{opt nosym:bol}} do not display % after percentage{p_end}
{p2colset 8 27 28 8}{p2col:{opth not:ation(string)}} choose to surround statistic with {opt par:entheses} or {opt bra:ckets}{p_end}
{p2colset 8 27 28 8}{p2col:{opt star:s}} display significance stars on this statistic. Default is {bf:mean}{p_end}
{p2colset 8 27 28 8}{p2col:{opt script:s}} display significance scripts on this statistic. Default is {bf:ci}{p_end}
{p2colset 8 27 28 8}{p2col:{opt lvl(#)}} ({bf:ci} only) choose level for confidence interval{p_end}
{p2colset 8 27 28 8}{p2col:{opt lev:el(#)}} ({bf:ci} only) choose level for confidence interval{p_end}
{p2colset 8 27 28 8}{p2col:{opt prop:ortion}} ({bf:ci} only) logit transform the confidence interval (similar to {help proportion}){p_end}
{p2colset 8 27 28 8}{p2col:{opt comb:ined}} ({bf:ci} only) put lower CI and upper CI in 1 column{p_end}
{p2colset 8 27 28 8}{p2col:{opth sep:arator(string)}} ({bf:ci} only) use {bf:"-"} or {bf:","} to separate a combined CI{p_end}
{p2colset 8 27 28 8}{p2col:{opt nocomma}} ({bf:count} only) do not display thousands separators{p_end}

{phang}
{opth excel(string)} has the following options:

{p2colset 8 23 24 8}{p2col:{opth file(string)}} location of output file. Default is a file named {bf:bradmean_output.xlsx} in the current working directory{p_end}
{p2colset 8 23 24 8}{p2col:{opth sheet(string)}} name of sheet to be used. Default is the first file in the sheet or {bf:Sheet1} in a new workbook{p_end}
{p2colset 8 23 24 8}{p2col:{opt rep:lace}} replace the workbook{p_end}
{p2colset 8 23 24 8}{p2col:{opt sheetrep:lace}} replace the sheet{p_end}
{p2colset 8 23 24 8}{p2col:{opt mod:ify}} append table to the end of the sheet{p_end}
{p2colset 8 23 24 8}{p2col:{opth font(string)}} choose the font face from {bf:Arial}, {bf:Calibri}, {bf:Garamond}, {bf:Helvetica}, {bf:TNR} (Times New Roman), or {bf:Verdana}. Default is {bf:Calibri}{p_end}
{p2colset 8 23 24 8}{p2col:{opt size(#)}} choose the font size between 9 and 12. Default is {bf:11}{p_end}
{p2colset 8 23 24 8}{p2col:{opth color(string)}} choose the color styles from {bf:bradmean}, {bf:monochrome}, {bf:rti}, {bf:material_red}, {bf:material_purple}, {bf:material_indigo}, {bf:material_blue}, {bf:material_green}, and {bf:material_orange}{p_end}
