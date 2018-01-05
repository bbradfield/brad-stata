{smcl}
{* *! version 1.0.1  05jan2018}{...}
{vieweralsosee "[R] codebook" "help codebook"}{...}
{title:Title}

{phang}
{bf:bradbook} {hline 2} Creates a cleaner codebook for export


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradbook:}
[{varlist}]
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt :{cmd:export(}{it:filename}{cmd:)}}{it:filename} for PDF file to be saved{p_end}
{synopt:{opt replace:}}replaces file it already exists{p_end}

{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opt export} exports a {it:filename}.PDF to the current working directory.

{phang}
{opt replace} replaces {it:filename}.PDF if it already exists

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradbook} creates a cleaner codebook to export. {cmd:bradbook} can be used for the entire dataset or just {varlist}.

{marker examples}{...}
{title:Examples}

{phang}{cmd:. bradbook}{p_end}

{phang}{cmd:. bradbook mpg foreign}

{phang}{cmd:. bradbook, export(filename)}

{phang}{cmd:. bradbook, export(filename) replace}{p_end}
