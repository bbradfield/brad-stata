{smcl}
{* *! version 1.3.8  16feb2018}{...}
{vieweralsosee "[R] mean" "help mean"}{...}
{viewerjumpto "Syntax" "bradmean_old##syntax"}{...}
{viewerjumpto "Description" "bradmean_old##description"}{...}
{viewerjumpto "Options" "bradmean_old##options"}{...}
{viewerjumpto "Examples" "bradmean_old##examples"}{...}
{title:Title}

{phang}
{bf:bradmean_old} {hline 2} Computes multiple independent means in a single table


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradmean_old:}
[{varlist}]
{ifin}
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:General}
{synopt:{opt svy:}}statistics will be survey weighted{p_end}
{synopt:{cmd:subpop(}{it:{help varname}}{cmd:)}}subpopulation estimation by {it:varname}; {it:varname} must be 0/1{p_end}
{synopt:{cmd:over(}{it:{help varlist}}{cmd:)}}estimation over groups defined by {it:varlist}{p_end}
{synopt:{opt level(#):}}set confidence level; default is {bf:level(95)}{p_end}
{synopt:{cmd:disopt(}{it:{help (strings:string}}{cmd:)}}select which statistics will be displayed and in what order{p_end}
{synopt:{opt nomiss:}}categories of over where all variables are missing will not be shown{p_end}

{syntab:Format}
{synopt:{cmd:format(}{it:{help (strings:string}}{cmd:)}}allows pre-defined format options{p_end}
{synopt:{opt nolabs:}}turn off group labels{p_end}
{synopt:{opt nolegend:}}turn off group legend{p_end}
{synopt:{cmd:mtest(}{it:{help (strings:string}}{cmd:)}}adjust p-values for multiple comparisons{p_end}
{synopt:{opt pct:}}displays results as percentage (multiplied by 100){p_end}
{synopt:{cmd:pvals(}{it:{help (strings:string}}{cmd:)}}select which type of p-values to be displayed{p_end}
{synopt:{opt round(#):}}rounds to # decimal places; range 0-7{p_end}
{synopt:{opt series:}}treat series of variables as a series with unique group labels{p_end}
{synopt:{cmd:title(}{it:{help (strings:string}}{cmd:)}}title displayed above table{p_end}
{synopt:{opt total:}}displays total as a group when {bf:over} is specified{p_end}
{synopt:{opt wide:}}displays in wide format{p_end}
{synopt:{opt noxilabs:}}do not display labels for xi variables{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:weights} are allowed; see {help svyset}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradmean_old} computes multiple independent means of {varlist}. Estimations can be run by groups, which will include comparative p-values (using adjusted Wald test).


{marker options}{...}
{title:Options}

{dlgtab:General}

{phang}
{opt svy} specifies that statistics will be survey weighted.

{phang}
{cmd:subpop(}{it:{help varname}}{cmd:)} specifies that estimates be computed using subpopulation {varname}. {varname} must be 0/1.

{phang}
{cmd:over(}{it:{help varlist}}{cmd:)} specifies that estimates be computed for multiple groups, which are identified by the different values of the variable(s) {varlist}.

{phang}
{opt level(#)} see {helpb estimation options##level():[R] estimation options}.

{phang}
{cmd:disopt(}{it:{help (strings:string}}{cmd:)} specifies which statistics will be display and in what order. Available statistics:

{space 12}{opt _all} {space 4} all statistics below
{space 12}{opt all} {space 5} all statistics below
{space 12}{opt obs} {space 5} observations (n)
{space 12}{opt n_yes} {space 3} observations with {it:varname} != 0 & !missing({it:varname})
{space 12}{opt mean} {space 4} mean (b)
{space 12}{opt se} {space 6} standard error (se)
{space 12}{opt sd} {space 6} standard deviation (sd)
{space 12}{opt var} {space 5} variance (var)
{space 12}{opt ci} {space 6} confidence interval (lci-uci)

{pmore}
default for {bf:long} is {cmd:disopt(obs n_yes mean sd ci)}
{p_end}
{pmore}
default for {bf:wide} is {cmd:disopt(mean)}
{p_end}

{phang}
{opt nomiss} specifies that categories of over where all variables are missing will not be shown.

{dlgtab:Format}

{phang}
{cmd:format(}{it:{help (strings:string}}{cmd:)} interprets pre-defined format options. Available statistics:

{space 12}{opt tips} {space 6} disopt(obs n_yes mean), labs(on), pct(off), pvals(overall), round(7), total(on), wide(off)
{space 12}{opt simple_ci} {space 1} disopt(mean lci uci), labs(on), pct(off), pvals(overall), round(7), total(off), wide(off)

{phang}
{opt nolabs} turns off group labels.

{phang}
{opt nolegend} turns off group legend.

{phang}
{cmd:mtest(}{it:{help (strings:string}}{cmd:)} adjusts p-values for multiple comparisons using the following methods:

{space 12}{bf:{ul:n}oadjust} {space 2} no adjustment
{space 12}{bf:{ul:b}onferroni} {space 0} Bonferroni's method
{space 12}{bf:{ul:h}olm} {space 6} Holm's method
{space 12}{bf:{ul:s}idak} {space 5} Sidak's method

{phang}
{opt pct} specifies that statistics will be displayed as percentage (multiplied by 100).

{phang}
{cmd:pvals(}{it:{help (strings:string}}{cmd:)} specifies which p-values to display. Options are "all", "overall", "individual", "none". Default is "overall".

{phang}
{opt round(#)} rounds statistics to # decimal places (range 0-7).

{phang}
{opt series} treat series of variables as a series with unique group labels. Series are made up of variables in the form of with the same prefix and a differing identifier. Variable labels in the format "{bf:[answer] question}" are required for this option to work. If you are using a range ("F4_1-F4_5"), the identifier will be after the last "_". If you are using a wildcard, the identifier will be the last wildcard and must be enclosed by underscores ("F4_*" "*_Computer" "F7_*_Computer"). At the moment, series use "_" counting to operate properly so having a wildcard that fills with "_" before the last wildcard can cause issues.

{phang}
{cmd:title(}{it:{help (strings:string}}{cmd:)} display specified title above table. Defaults to display {varlist} and label (for single variables or series). If you do not want to display a title, use {bf:title(none)}.

{phang}
{opt total} displays total as a group when {bf:over} is specified.

{phang}
{opt wide} specifies that estimates will be output in a wide format (only in cases where {opt over} is specified).

{phang}
{opt noxilabs} turns off labels for xi variables. Otherwise, bradmean_old will use the value label to create labels for the indicator variables.

{marker results}{...}
{title:Stored results}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:r(results)}}matrix of stored estimates{p_end}

