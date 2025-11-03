{smcl}
{* *! version 1.0.1  03nov2025}{...}
{vieweralsosee "[D] mean" "help contract"}{...}
{viewerjumpto "Syntax" "biggertab##syntax"}{...}
{viewerjumpto "Description" "biggertab##description"}{...}
{viewerjumpto "Options" "biggertab##options"}{...}
{title:Title}

{phang}
{bf:biggertab} {hline 2} Displays cross-tabs of variables and allows more than 3 variables

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:biggertab:}
{varlist}
{ifin}
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab :Options}
{synopt :{opth st:ats(string)}}select which statistics to be displayed{p_end}
{synopt :{opth sepby(varlist2)}}draw a separator line whenever {it:varlist2} values change{p_end}
{synopt :{opth title(string)}}specifies an optional custom title{p_end}
{synopt :{opt div:ider}}draw divider lines between columns{p_end}
{synopt :{opt nol:abel}}display numeric codes rather than label values{p_end}
{synopt :{opt z:ero}}include combinations with frequency zero{p_end}
{synopt :{opt nomiss}}drop observations with missing values{p_end}
{synoptline}
{p2colreset}{...}

{marker description}{...}
{title:Description}

{pstd}
{cmd:biggertab} displays cross-tabs of variables with frequencies and percentages. Biggertab can display more 
than 3 variables, but with many variables users may need to use the option {opt nol:abel} to avoid line 
overflow.

{marker options}{...}
{title:Options}

{dlgtab:Options}

{phang}
{opth st:ats(string)} allows users to choose from the following statistics:

{p2colset 8 16 15 8}{p2col:{opt freq}} frequency{p_end}
{p2colset 8 16 15 8}{p2col:{opt pct}} percent{p_end}
{p2colset 8 16 15 8}{p2col:{opt cumfreq}} cumulative frequency{p_end}
{p2colset 8 16 15 8}{p2col:{opt cumpct}} cumulative percent{p_end}
{p2colset 8 16 15 8}{p2col:{opt rowfreq}} row frequency (within the sepby {varlist}){p_end}
{p2colset 8 16 15 8}{p2col:{opt rowpct}} row percent (within the sepby {varlist}){p_end}

{phang}
{opth sepby(varlist2)} draws a separator line whenever {it:varlist2} values change. The default
behavior is to draw a separator every 9,999 lines.

{phang}
{opth title(string)} specifies an optional custom title. The default behavior is to display 
no title.

{phang}
{opt div:ider} draws divider lines between each column.

{phang}
{opt nol:abel} specifies that the numeric codes be displayed rather than label
values.

{phang}
{opt zero} specifies that combinations with frequency zero be included.

{phang}
{opt nomiss} specifies that observations with missing values on any
variable in {varlist} be dropped. If {opt nomiss} is not specified, all
observations possible are used.

