{smcl}
{* *! version 1.2.3  06jan2017}{...}
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
{syntab:Main}
{synopt:{opt svy:}}statistics will be survey weighted{p_end}
{synopt:{cmd:subpop(}{it:{help varname}}{cmd:)}}subpopulation estimation by {it:varname}{p_end}
{synopt:{cmd:over(}{it:{help varlist}}{cmd:)}}estimation over groups defined by {it:varlist}{p_end}
{synopt:{opt level(#):}}set confidence level; default is {bf:level(95)}{p_end}
{synopt:{opt round(#):}}rounds to # decimal places; range 0-7{p_end}
{synopt:{opt pct:}}displays results as percentage (multiplied by 100){p_end}
{synopt:{opt nopvals:}}does not calculate p-values; default is {bf:off}{p_end}
{synopt:{cmd:disopt(}{it:{help (strings:string}}{cmd:)}}select which statistics will be displayed and in what order{p_end}
{synopt:{opt wide:}}displays in wide format{p_end}
{synopt:{cmd:title(}{it:{help (strings:string}}{cmd:)}}title displayed above table{p_end}
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

{dlgtab:Main}

{phang}
{opt svy} specifies that statistics will be survey weighted.

{phang}
{cmd:subpop(}{it:{help varname}}{cmd:)} specifies that estimates be computed using subpopulation {varname}. If {varname} is categorical, bradmean splits it into indicator variables for each level.

{phang}
{cmd:over(}{it:{help varlist}}{cmd:)} specifies that estimates be computed for multiple groups, which are identified by the different values of the variable(s) {varlist}.

{phang}
{opt level(#)} see {helpb estimation options##level():[R] estimation options}.

{phang}
{opt round(#)} rounds statistics to # decimal places (range 0-7).

{phang}
{opt pct} specifies that statistics will be displayed as percentage (multiplied by 100).

{phang}
{opt nopvals} does not calculate p-values; default is {bf:off}.

{phang}
{cmd:disopt(}{it:{help (strings:string}}{cmd:)} specifies which statistics will be display and in what order. Available statistics:

{space 12}{opt obs} {space 5} observations (n)
{space 12}{opt mean} {space 4} mean (b)
{space 12}{opt se} {space 6} standard error (se)
{space 12}{opt sd} {space 6} standard deviation (sd)
{space 12}{opt ci} {space 6} confidence interval (lci-uci)

{pmore}
default for {bf:long} is {cmd:disopt(obs mean sd ci)}
{p_end}
{pmore}
default for {bf:wide} is {cmd:disopt(mean)}
{p_end}

{phang}
{opt wide} specifies that estimates will be output in a wide format (only in cases where {opt over} is specified).

{phang}
{cmd:title(}{it:{help (strings:string}}{cmd:)} display specified title above table.

{marker examples}{...}
{title:Examples}

{phang}{cmd:. bradmean mpg foreign}{p_end}

{phang}{cmd:. bradmean mpg foreign, disopt(mean sd)}{p_end}

{phang}{cmd:. bradmean mpg foreign, svy}{p_end}

{phang}{cmd:. bradmean mpg, svy over(foreign)}{p_end}

{phang}{cmd:. bradmean mpg, svy subpop(make) over(foreign)}{p_end}

{phang}{cmd:. bradmean mpg, svy subpop(make0 over(foreign) wide}{p_end}

{marker results}{...}
{title:Stored results}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:r(results)}}matrix of stored estimates{p_end}

