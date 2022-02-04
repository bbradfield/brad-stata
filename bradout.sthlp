{smcl}
{* *! version 1.0.0  03feb2022}{...}
{vieweralsosee "[R] mean" "help mean"}{...}
{viewerjumpto "Syntax" "bradout##syntax"}{...}
{viewerjumpto "Description" "bradout##description"}{...}
{viewerjumpto "Options" "bradout##options"}{...}
{viewerjumpto "Examples" "bradout##examples"}{...}
{title:Title}

{phang}
{bf:bradout} {hline 2} Outputs multiple estimation results in a clean table

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:bradout:}
[string]
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Statistics}
{synopt:{opth stat:s(string)}}select which covariate statistics will be displayed{p_end}
{synopt:{opth star:s(numlist)}}creates up to 3 significance stars for p-values{p_end}
{synopt:{opth mstat:s(string)}}select which model statistics will be displayed{p_end}
{synopt:{opth minfo(string)}}select which model information will be displayed{p_end}

{syntab:Covariates}
{synopt:{opth order(string)}}choose which covariates are displayed first{p_end}
{synopt:{opth drop(string)}}choose which covariates to drop, while keeping all others{p_end}
{synopt:{opth keep(string)}}choose which covariates to keep, while dropping all others{p_end}
{synopt:{opt nobase:s}}do not display base levels of covariates CHECK THIS{p_end}
{synopt:{opt noomit:s}}do not display omitted covariates{p_end}
{synopt:{opt noref:s}}do not display variable header for categorical variables CHECK THIS{p_end}

{syntab:Labelling}
{synopt:{opth collab:els(string)}}specify column labels{p_end}
{synopt:{opth mlab:els(string)}}specify model labels{p_end}
{synopt:{opt novarlab:els}}do not display variable labels{p_end}
{synopt:{opth refcat:s(string)}}specify manual reference categories{p_end}
{synopt:{opth catfill(string)}}specify fill string for category cells{p_end}
{synopt:{opth reffill(string)}}specify fill string for reference cells{p_end}
{synopt:{opth omitfill(string)}}specify fill string for omitted cells{p_end}

{syntab:Display}
{synopt:{opth title(string)}}optional custom title or "{bf:none}" to display no title{p_end}
{synopt:{opt hsep:arator}}display horizontal separator lines{p_end}
{synopt:{opt vsep:arator}}display vertical separator lines{p_end}
{synopt:{opt wide}}display in a wide format{p_end}
{synopt:{opt bind}}bind models such that all their columns are displayed in one table{p_end}

{syntab:Output}
{synopt:{opth excel(string)}}Excel output options{p_end}

{syntab:System}
{synopt:{opt clear}}clear stored estimates after command{p_end}

{synoptline}

{marker description}{...}
{title:Description}

{pstd}
{cmd:bradout} outputs multiple estimation results in a clean table that can be exported to Excel. It also has many customization options to structure the table how you'd like.

{marker options}{...}
{title:Options}

{dlgtab:Statistics}

{phang}
{opth stat:s(string)} allows users to choose from the following statistics:

{p2colset 8 15 15 8}{p2col:{opt b}} b (Coefficient/OR/etc){p_end}
{p2colset 8 15 15 8}{p2col:{opt se}} standard error{p_end}
{p2colset 8 15 15 8}{p2col:{opt tz}} t/z{p_end}
{p2colset 8 15 15 8}{p2col:{opt pvalue}} p-value{p_end}
{p2colset 8 15 15 8}{p2col:{opt ci}} confidence interval{p_end}

{phang}
{opth star:s(numlist)} creates up to 3 significance stars for p-values less than {it:{help numlist}} containing 0-3 values. Leaving {it:{help numlist}} empty defaults to p < 0.05 and p < 0.01{p_end}

{phang}
{opth mstat:s(string)} allows users to choose from the following model statistics:

{p2colset 8 17 15 8}{p2col:{opt n}} number of observations{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_yes}} number of yes's{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_pop}} size of population{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_strata}} number of strata{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_clust}} number of clusters{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_psu}} number of primary sampling units{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_cds}} number of completely determined successes{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_cdf}} number of completely determined failures{p_end}
{p2colset 8 17 15 8}{p2col:{opt n_cd}} number of completely determined observations{p_end}
{p2colset 8 17 15 8}{p2col:{opt df_m}} model degrees of freedom{p_end}
{p2colset 8 17 15 8}{p2col:{opt mss}} model sum of squares{p_end}
{p2colset 8 17 15 8}{p2col:{opt df_r}} residual degrees of freedom{p_end}
{p2colset 8 17 15 8}{p2col:{opt rss}} residual sum of squares{p_end}
{p2colset 8 17 15 8}{p2col:{opt rmse}} root mean squared error{p_end}
{p2colset 8 17 15 8}{p2col:{opt r2}} R-squared{p_end}
{p2colset 8 17 15 8}{p2col:{opt r2_a}} adjusted R-squared{p_end}
{p2colset 8 17 15 8}{p2col:{opt r2_p}} pseudo R-squared{p_end}
{p2colset 8 17 15 8}{p2col:{opt ll}} log likelihood{p_end}
{p2colset 8 17 15 8}{p2col:{opt ll0}} log likelihood, constant-only model{p_end}
{p2colset 8 17 15 8}{p2col:{opt aic}} Akaike’s information criterion{p_end}
{p2colset 8 17 15 8}{p2col:{opt bic}} Bayesian information criterion{p_end}
{p2colset 8 17 15 8}{p2col:{opt f}} F statistic{p_end}
{p2colset 8 17 15 8}{p2col:{opt chi2}} chi-squared{p_end}
{p2colset 8 17 15 8}{p2col:{opt p}} p-value for model test{p_end}
{p2colset 8 17 15 8}{p2col:{opt k}} number of parameters{p_end}
{p2colset 8 17 15 8}{p2col:{opt ic}} number of iterations{p_end}
{p2colset 8 17 15 8}{p2col:{opt rank}} rank of e(V){p_end}

{phang}
{opth minfo(string)} allows users to choose from the following model information:

{p2colset 8 15 15 8}{p2col:{opt mgroup}} model group{p_end}
{p2colset 8 15 15 8}{p2col:{opt depvar}} dependent variable{p_end}
{p2colset 8 15 15 8}{p2col:{opt cmd}} command{p_end}
{p2colset 8 15 15 8}{p2col:{opt weight}} weight{p_end}

{dlgtab:Covariates}

{phang}
{opth order(string)} specifies which covariates are displayed first.

{phang}
{opth drop(string)} specifies which covariates to drop, while keeping all others.

{phang}
{opth keep(string)} specifies which covariates to keep, while dropping all others.

{phang}
{opt nobase:s} hides display of base levels of covariates.

{phang}
{opt noomit:s} hides display of omitted covariates.

{phang}
{opt noref:s} hides display of variable headers for categorical variables.

{dlgtab:Labelling}

{phang}
{opth collab:els(string)} specifies the labels of columns. If there are fewer specified column labels than total columns, the labels will repeat.

{phang}
{opth mlab:els(string)} specifies the labels of models.

{phang}
{opt novarlabels} hides display of variable labels.

{phang}
{opth refcat:s(string)} specifies manual reference categories.

{phang}
{opth catfill(string)} specifies the fill string for category cells.

{phang}
{opth reffill(string)} specifies the fill string for reference cells.

{phang}
{opth omitfill(string)} specifies the fill string for omitted cells.

{dlgtab:Display}

{phang}
{opth title(string)} specifies an optional custom title or {bf:"none"} to display no title.

{phang}
{opt hsep:arator} displays horizontal separator lines.

{phang}
{opt vsep:arator} displays vertical separator lines.

{phang}
{opt wide} displays results in a wide format.

{phang}
{opt bind} binds models such that all their columns are displayed in one table.

{dlgtab:Output}

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

{dlgtab:System}

{phang}
{opt clear} clears stored results.

