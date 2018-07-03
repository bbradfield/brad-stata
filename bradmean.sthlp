{smcl}
{* *! version 1.4.2  25jun2018}{...}
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
{syntab:General}
{synopt:{opt svy:}}statistics will be survey weighted{p_end}
{synopt:{opth sub:pop(varname)}}subpopulation estimation by {it:varname}; {it:varname} must be 0/1{p_end}
{synopt:{cmd:over(}{it:{help varlist}}{cmd:)}}estimation over groups defined by {it:varlist}{p_end}

{syntab:Format}
{synopt:{cmd:overopt(}{it:{help string}}{cmd:)}}display options for over variables{p_end}
{synopt:{opth pval:ues(string)}}select which type of p-values to be displayed{p_end}
{synopt:{cmd:ci(}{it:{help string}}{cmd:)}}display options for confidence intervals{p_end}
{synopt:{opth st:ats(string)}}select which stats to be displayed{p_end}
{synopt:{opth dis:play(string)}}general display options{p_end}

{syntab:Output}
{synopt:{opth ex:cel(string)}}excel output settings{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:weights} are allowed; see {help svyset}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradmean} computes multiple independent means of {varlist}. Estimations can be run by groups, which will include comparative p-values (using adjusted Wald test).

{marker options}{...}
{title:Options}

{dlgtab:General}

{phang}
{opt svy} specifies that statistics will be survey weighted.

{phang}
{opth sub:pop(varname)} specifies that estimates be computed using subpopulation {varname}. {varname} must be 0/1.

{phang}
{cmd:over(}{it:{help varlist}}{cmd:)} specifies that estimates be computed for multiple groups, which are identified by the different values of the variable(s) {varlist}.

{dlgtab:Format}

{phang}
{cmd:overopt(}{it:{help string}}{cmd:)} has the following options:

{space 8}{opt nolab:els}{space 3} do not display over labels
{space 8}{opt noleg:end}{space 3} do not display legend for over groups
{space 8}{opt nomiss}{space 5} do not display groups with no non-missing values
{space 8}{opt sep:arator}{space 2} display levels of separate over variables in separate rows
{space 8}{opt tot:al}{space 6} display overall statistics

{phang}
{opth pval:ues(string)} has the following options:

{space 8}{opt all}{space 14} display all p-values
{space 8}{opt ind:ividual}{space 7} display individual p-values
{space 8}{opt over:all}{space 10} display overall p-values
{space 8}{opt none}{space 13} display no p-values
{space 8}{opth mtest(string)}{space 4} choose from {opt b:onferroni}, {opt h:olm}, or {opt s:idak}
{space 8}{opth star:s(numlist)}{space 3} display up to 3 significance stars for p-values between 0 and 1;
{space 25} default is 0.05 and 0.01
{space 8}{opt script:s(#)}{space 7} display significance superscripts for 1 p-value between 0 and 1;
{space 25} default is 0.05
{space 8}{opt force}{space 12} force displayp-values even with significance stars or superscripts

{phang}
{cmd:ci(}{it:{help string}}{cmd:)} has the following options:

{space 8}{opt prop:ortion}{space 8} display logit-transformed CIs (like {help proportion})
{space 8}{opt log:it}{space 13} display logit-transformed CIs (like {help proportion})
{space 8}{opt lev:el(#)}{space 10} calculate CIs at a number between 0 and 100
{space 8}{opt round(#)}{space 10} display between 0 and 7 decimals in CIs
{space 8}{opt comb:ined}{space 10} display CIs as a combined column
{space 8}{opt par:entheses}{space 7} display combined CIs with parentheses
{space 8}{opt brac:kets}{space 10} display combined CIs with brackets
{space 8}{opth sep:arator(string)}{space 1} display combined CIs separated by - or ,

{phang}
{opth st:ats(string)} has the following options:

{space 8}{opt all}{space 4} all statistics below
{space 8}{opt obs}{space 4} observations (n)
{space 8}{opt nyes}{space 2} observations with {it:varname} != 0 & !missing({it:varname})
{space 8}{opt mean}{space 3} mean (b)
{space 8}{opt se}{space 5} standard error (se)
{space 8}{opt sd}{space 5} standard deviation (sd)
{space 8}{opt var}{space 4} variance (var)
{space 8}{opt ci}{space 5} confidence interval (lci-uci)
{space 8}{opt lci}{space 4} lower bound of confidence interval
{space 8}{opt uci}{space 4} upper bound of confidence interval
{space 8}{opt min}{space 4} minimum value of variable
{space 8}{opt max}{space 4} maximum value of variable

{pmore}
default for {bf:long} is {cmd:obs nyes mean sd ci}
{p_end}
{pmore}
default for {bf:wide} is {cmd:mean}
{p_end}


{phang}
{opth dis:play(string)} has the following options:

{space 8}{opt pct}{space 12} display binary variables as a percentage
{space 8}{opt per:cent}{space 8} display binary variables as a percentage
{space 8}{opth al:ign(string)}{space 2} set alignment as {opt l:eft}, {opt c:enter}, {opt r:ight}
{space 8}{opt round(#)}{space 7} display between 0 and 7 decimals in non-CI numbers
{space 8}{cmd:title(}{it:{help string}}{cmd:)}{space 2} display a title or use {opt none} to display no title
{space 8}{opt wide}{space 11} display in a wide format
{space 8}{opt nofo:oter}{space 7} do not display footer (only applies when stars or scripts active)
{space 8}{opt noprint}{space 8} do not print results table (only applies when excel output is active)
{space 8}{opt xival:ues}{space 7} display labels of xi values instead of numbers; default {opt on}
{space 8}{opt xivar:s}{space 9} display labels of xi variables instead of {help varname}; default {opt off}
{space 8}{opt xi}{space 13} sets both xivalues and xivars {opt on}
{space 8}{opt noxi}{space 11} sets both xivalues and xivars {opt off}
{space 8}{opt seriesval:ues}{space 3} display answers of individual variables in series (see below); default {opt off}
{space 8}{opt seriesvar:s}{space 5} display questions of variables in series (see below); default {opt off}
{space 8}{opt series}{space 9} sets both seriesvalues and seriesvars {opt on}
{space 8}{opt noseries}{space 7} sets both seriesvalues and seriesvars {opt off}

{pmore}
For series options to correctly work, variable labels must be in the following format:
{p_end}
{pmore}
{opt [answer] question}
{p_end}

{dlgtab:Output}

{phang}
{opth ex:cel(string)} creates excel output using the following:

{space 8}{opt f:ile(path)}{space 5} the path for the excel output;
{space 23} default path is the current working directory
{space 23} default filename is {opt bradmean_output.xlsx}
{space 8}{opth sheet(string)}{space 2} choose sheet for output;
{space 23} default is {opt Sheet 1}
{space 8}{opt rep:lace}{space 8} replace file
{space 8}{opt sheetrep:lace}{space 3} replace sheet
{space 8}{opt mod:ify}{space 9} modify (append) sheet

{pmore}
One of {opt rep:lace}, {opt sheetrep:place}, {opt mod:ify} must be chosen.
{p_end}

