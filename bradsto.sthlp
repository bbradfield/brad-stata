{smcl}
{* *! version 1.0.0  03feb2022}{...}
{vieweralsosee "[R] mean" "help mean"}{...}
{viewerjumpto "Syntax" "bradsto_dev##syntax"}{...}
{viewerjumpto "Description" "bradsto_dev##description"}{...}
{viewerjumpto "Options" "bradsto_dev##options"}{...}
{viewerjumpto "Examples" "bradsto_dev##examples"}{...}
{title:Title}

{phang}
{bf:bradsto_dev} {hline 2} Stores estimates for use in bradout

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradsto_dev:}
[{cmd:,} {it:options}] [:
{it:{help estimation_command}}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{synopt:{opth mlabel(string)}}specify a label for the stored estimates; default is {bf:est#}{p_end}
{synopt:{opth prefix(string)}}specify a prefix for the stored estimates; default is {bf:est}{p_end}
{synopt:{opth mgroup(string)}}specify a group label for the stored estimates{p_end}
{synopt:{opt clear}}clear stored estimates{p_end}
{synoptline}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradsto_dev} stores estimates for later use by {bf:bradout}.

{marker options}{...}
{title:Options}

{phang}
{opth mlabel(string)} specifies a label for the stored estimates. The default name is {bf:est#}, where # is the position of the saved model.

{phang}
{opth prefix(string)} specifies a prefix for the stored estimates. The default prefix is {bf:est}.

{phang}
{opth mgroup(string)} specifies a group label for the stored estimates.

{phang}
{opt clear} clears all stored estimates.

