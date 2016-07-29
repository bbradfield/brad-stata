{smcl}
{* *! version 1.5.0  29jul2016}{...}
{vieweralsosee "[R] codebook" "help codebook"}{...}
{viewerjumpto "Syntax" "examplehelpfile##syntax"}{...}
{viewerjumpto "Description" "examplehelpfile##description"}{...}
{viewerjumpto "Options" "examplehelpfile##options"}{...}
{viewerjumpto "Examples" "examplehelpfile##examples"}{...}
{title:Title}

{phang}
{bf:bradbook} {hline 2} Creates a cleaner codebook for export to Word


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradbook:}
[{varlist}]

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradbook} creates a cleaner codebook to export to Word. {cmd:bradbook} can be used for the entire dataset or just {varlist}.

{marker examples}{...}
{title:Examples}

{phang}{cmd:. bradbook}{p_end}

{phang}{cmd:. bradbook mpg foreign}{p_end}
