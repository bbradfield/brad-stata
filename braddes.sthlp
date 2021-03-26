{smcl}
{* *! version 0.2.0  25mar2021}{...}
{vieweralsosee "[R] describe" "help describe"}{...}
{viewerjumpto "Syntax" "braddes##syntax"}{...}
{viewerjumpto "Description" "braddes##description"}{...}
{viewerjumpto "Options" "braddes##options"}{...}
{title:Title}

{phang}
{bf:braddes} {hline 2} Describe data in a single-line per variable format

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:braddes:}
[{varlist}]
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Data}
{synopt:{opt data}}display header with dataset-level information{p_end}
{synopt:{opt field:s(string)}}choose which variable information to display{p_end}

{syntab:Display}
{synopt:{opt sort:(string)}}sort by a displayed field{p_end}
{synopt:{opt sepby:(string)}}separate sections of the table by a displayed field{p_end}
{synoptline}

{marker description}{...}
{title:Description}

{pstd}
{cmd:braddes} describes data in a single-line per variable format with additional fields from describe, including counts of missing and non-missing observations.

{marker options}{...}
{title:Options}

{dlgtab:Data}

{phang}
{opt data} displays a header with dataset-level information. This includes label, file path, date saved, size, number of observations, and number of variables.

{phang}
{opt field:s(string)} allows users to choose which variable information to display. The default is variable name, variable label, and value label. The user may choose from the following fields:

{p2colset 8 18 19 8}{p2col:{opt varname}} variable name{p_end}
{p2colset 8 18 19 8}{p2col:{opt varlab}} variable label{p_end}
{p2colset 8 18 19 8}{p2col:{opt vallab}} value label{p_end}
{p2colset 8 18 19 8}{p2col:{opt type}} variable type{p_end}
{p2colset 8 18 19 8}{p2col:{opt format}} variable format{p_end}
{p2colset 8 18 19 8}{p2col:{opt letter}} first letter of variable name{p_end}
{p2colset 8 18 19 8}{p2col:{opt term}} term of variable through the last underscore (e.g. C25_1 has a term of C25){p_end}
{p2colset 8 18 19 8}{p2col:{opt question}} variable question when label is formatted as "[answer] question"{p_end}
{p2colset 8 18 19 8}{p2col:{opt answer}} variable answer when label is formatted as "[answer] question"{p_end}
{p2colset 8 18 19 8}{p2col:{opt numeric}} whether variable is numeric or string{p_end}
{p2colset 8 18 19 8}{p2col:{opt minum}} number of observations with missing data{p_end}
{p2colset 8 18 19 8}{p2col:{opt mipct}} percentage of observations with missing data{p_end}
{p2colset 8 18 19 8}{p2col:{opt nvnum}} number of observations with . or ""{p_end}
{p2colset 8 18 19 8}{p2col:{opt nvpct}} percentage of observations with . or ""{p_end}
{p2colset 8 18 19 8}{p2col:{opt ansnum}} number of observations with non-missing values{p_end}
{p2colset 8 18 19 8}{p2col:{opt anspct}} percentage of observations with non-missing values, where the denominator is all observations minus observations with extended missing values (.a through .z){p_end}

{dlgtab:Display}

{phang}
{opt sort(string)} allows sorting by choosing direction ({bf:+} for ascending, {bf:-} for descending) and field.{p_end}

{phang}
{opt sepby(string)} displays a horizontal line separating variables by the selected field.{p_end}
