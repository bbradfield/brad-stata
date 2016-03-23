{smcl}
{* *! version 2.1.1  14mar2016}{...}
{vieweralsosee "[R] mean" "help mean"}{...}
{viewerjumpto "Syntax" "examplehelpfile##syntax"}{...}
{viewerjumpto "Description" "examplehelpfile##description"}{...}
{viewerjumpto "Options" "examplehelpfile##options"}{...}
{viewerjumpto "Examples" "examplehelpfile##examples"}{...}
{title:Title}

{phang}
{bf:bradmean} {hline 2} Computes multiple independent means in a single table


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradmean:}
[{varlist}]
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{opt svy:}}statistics will be survey weighted{p_end}
{synopt :{cmd:over(}{it:{help varlist}}{cmd:)}}group over
subpopulations defined by {it:varlist}{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:weights} are allowed; see {help svyset}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradmean} computes multiple independent means of {varlist}. Variables can be grouped over subpopulations,
which will also include an overall adjusted Wald test.


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opt svy} specifies that statistics will be survey weighted.

{phang}
{opt over} specifies that estimates be computed for multiple subpopulations, which are identified by the different values of the variables in varlist.


{marker examples}{...}
{title:Examples}

{phang}{cmd:. bradmean mpg weight}{p_end}

{phang}{cmd:. bradmean mpg weight, svy}

{phang}{cmd:. bradmean mpg, svy over(weight)}{p_end}
