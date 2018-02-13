**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.3.7                                                **
**   Date:         02/13/2018                                           **
**                                                                      **
**======================================================================**
**======================================================================**

version 14.0
mata:

void selectMatrix(string scalar matrixname,
                  real vector rows,
                  real vector cols,
                  real scalar round)
{
  processmatrix = st_matrix(matrixname)
  processmatrix = processmatrix[rows, cols]
  processmatrix = round(processmatrix, round)

  st_matrix("r(results)", processmatrix)
}

end

#delimit;

**======================================================================**
**   Routine: bradmean                                                  **
**======================================================================**;

program define bradmean, rclass sortpreserve;
syntax varlist(numeric) [if] [in],
  [
    SVY                     /* gen: turn on                   */
    SUBpop(varname numeric) /* gen: subpop                    */
    OVER(varlist)           /* gen: over()                    */
    LEVEL(cilevel)          /* gen: ci level                  */
    DISopt(string)          /* gen: display stats & order     */
    noMISS                  /* gen: display over with missing */
    FORMAT(string)          /* fmt: presets                   */
    noLABS                  /* fmt: no labels                 */
    noLEGEND                /* fmt: no legend                 */
    MTEST(string)           /* fmt: multiple comparisons type */
    PCT                     /* fmt: format as pct             */
    PVALS(string)           /* fmt: pvals                     */
    ROUND(integer 7)        /* fmt: number of decimals        */
    SERIES                  /* fmt: check for series          */
    TITLE(string)           /* fmt: title                     */
    TOTAL                   /* fmt: total                     */
    WIDE                    /* fmt: wide                      */
    noXILabs                /* fmt: no xi labels              */
  ];

*--------------------------------------------------------------*
*   01. Setting `matsize' & `input_list'                       *
*--------------------------------------------------------------*;

  local original_matsize = c(matsize);
  if(c(matsize) < 800)
  {;
    set matsize 800;
  };

  local input_list = cond(strpos(`"`0'"', ",") == 0, `"`0'"', substr(`"`0'"', 1, strpos(`"`0'"', ",") - 1));
  local input_list = trim(itrim("`input_list'"));

*--------------------------------------------------------------*
*   02. Setting Survey Options                                 *
*--------------------------------------------------------------*;

  /* svy - Error Code */

    if("`svy'" != "" | "`subpop'" != "")
    {;
      cap qui svydescribe;

      if(_rc != 0)
      {;
        di as err "data not set up for svy, use svyset" as text "";
        error 119;
      };
    };

  /* svy - svy */

    local svy = cond("`svy'" == "", "", "svy: ");

  /* svy - subpop */

    local svy = cond("`subpop'" == "", "`svy'", "svy, subpop(`subpop'): ");

*--------------------------------------------------------------*
*   03. Marking Sample                                         *
*--------------------------------------------------------------*;

  tempname touse;

  mark `touse' `if' `in';

  if("`svy'" != "")
  {;
    svymarkout `touse';
  };

  if("`miss'" != "")
  {;
    tempvar no_miss;
    qui egen `no_miss' = rownonmiss(`varlist');
    local no_miss_if = "if `touse' & `no_miss' != 0";
  };

*--------------------------------------------------------------*
*   04. Generating `group_var'                                 *
*--------------------------------------------------------------*;

  local over_count = 1;

  if("`over'" != "")
  {;
    /* Creating `group_string' & `group_numeric' */

      tempvar group_string;
      qui egen strL `group_string' = concat(`over') `no_miss_if', decode p(", ");

      tempvar group_numeric;
      qui egen `group_numeric' = group(`over') `no_miss_if';

    /* Creating `over_count' */

      qui levelsof `group_numeric', local(over_lvls);
      local over_count : word count `over_lvls';

    /* Adding `group_numeric' to `group_string' */

      local num_format = "%0" + string(length("`over_count'")) + ".0f";
      qui replace `group_string' = string(`group_numeric', "`num_format'") + " " + `group_string';
      qui replace `group_string' = "" if `group_numeric' == .;

    /* Converting `group_string' to `group_numeric' */

      tempname group_var;
      encode `group_string', generate(`group_var');

    /* Setting `opt_over' */

      local opt_over = "over(`group_var')";
      markout `touse' `group_var';

    /* Cleaning */

      cap drop `group_numeric';
      cap drop `group_string';
    };

*--------------------------------------------------------------*
*   05. Translating Format                                     *
*--------------------------------------------------------------*;

  local format = trim(strlower("`format'"));

  /* Tips */

    if("`format'" == "tips")
    {;
      local disopt = "obs n_yes mean";
      local labs   = "";
      local pct    = 1;
      local pvals  = cond(`over_count' == 1, "none", "overall");
      local round  = 7;
      local total  = cond(`over_count' == 1, "", "total");
      local wide   = "";
    };

  /* Simple_CI */

    if("`format'" == "simple_ci")
    {;
      local disopt = "mean lci uci";
      local pct    = 1;
      local pvals  = cond(`over_count' == 1, "none", "overall");
      local round  = 7;
      local wide   = "";
    };

*--------------------------------------------------------------*
*   06. Setting Format Options                                 *
*--------------------------------------------------------------*;

  local dis_len = 14;

  /* fmt - pct */

    local pct = cond("`pct'" == "", 1, 100);

  /* fmt - pvals */

    local pvals = trim(strlower("`pvals'"));
    local pvals = cond(strpos("`pvals'", "all") == 1, "all",
                  cond(strpos("`pvals'", "ind") == 1, "individual",
                  cond(strpos("`pvals'", "no")  == 1, "none", "`pvals'")));
    local pvals = cond(!inlist("`pvals'", "all", "overall", "individual", "none"), "overall", "`pvals'");
    local pvals = cond(`over_count' == 1, "none", "`pvals'");

    local pvals = cond(`over_count' == 2 & "`pvals'" != "none", "overall", "`pvals'");

  /* fmt - mtest */

    local mtest = trim(lower("`mtest'"));
    local mtest = cond(strpos("`mtest'","b") == 1, "bonferroni",
                  cond(strpos("`mtest'","h") == 1, "holm",
                  cond(strpos("`mtest'","s") == 1, "sidak", "noadjust")));

  /* fmt - round */

    local round = cond(inrange(`round', 0, 7), 1/(10^`round'), 1/(10^7));

  /* fmt - title */

    local xiprefix : char _dta[__xi__Vars__Prefix__];
    local xiprefix : word 1 of `xiprefix';
    local xiprefix = cond("`xiprefix'" == "", "_I", "`xiprefix'");

    ** Default Title **;
    if(trim("`title'") == "")
    {;
      ** 1 Variable **;
      if(`: word count `varlist'' == 1)
      {;
        local title = "`varlist' - `: variable label `varlist''";
      };
      ** Multiple Variables **;
      else
      {;
        ** 1 Series **;
        if(`: word count `input_list'' == 1 & strpos("`input_list'", "`xiprefix'") != 1 & "`series'" != "")
        {;
          foreach var of varlist `varlist'
          {;
            local temp_title = "`: variable label `var''";
            local temp_title = trim(substr("`temp_title'", strpos("`temp_title'", "]") + 1, .));
            local title = cond(length("`temp_title'") > length("`title'"), "`temp_title'", "`title'");
          };
          local title = "`input_list' - `title'";
        };
        ** 1 XI Variable **;
        if(`: word count `input_list'' == 1 & strpos("`input_list'", "`xiprefix'") == 1)
        {;
          local temp_title = word(subinstr("`: variable label `: word 1 of `varlist'''", "==", " ", .), 1);
          local title = "`temp_title' - `: variable label `temp_title''";
        };
      };

      ** If still missing, input_list **;
      if(trim("`title'") == "")
      {;
        local title = subinstr("`input_list'", " ", ", ", .);
      };
    };

    ** None Title **;
    if(trim(lower("`title'")) == "none")
    {;
      local title = "";
    };

  /* fmt - total */

    local total = cond(`over_count' == 1, "", "`total'");

  /* fmt - wide */

    local wide = cond(`over_count' == 1, "", "`wide'");

*--------------------------------------------------------------*
*   07. Setting `disopt'                                       *
*--------------------------------------------------------------*;

  /* Subbing in Shorthand Words */

    local disopt = trim(itrim(strlower("`disopt'")));
    local disopt = subinword("`disopt'", "ci", "lci uci", .);
    local disopt = subinword("`disopt'", "all", "obs n_yes mean se sd var lci uci", .);
    local disopt = subinword("`disopt'", "_all", "obs n_yes mean se sd var lci uci", .);

  /* Removing Invalid Words */

    foreach word of local disopt
    {;
      if(!inlist("`word'", "obs", "n_yes", "mean", "se", "sd", "var", "lci", "uci"))
      {;
        local disopt = subinword("`disopt'", "`word'", "", .);
      };
    };

  /* Setting Defaults */

    if("`disopt'" == "")
    {;
      local disopt = cond("`wide'" == "", "obs n_yes mean sd lci uci", "mean");
    };

  /* Subbing in Position for 1st Instance */

    local i = 1;
    foreach word in obs n_yes mean se sd var lci uci
    {;
      local disopt = subinword("`disopt'", "`word'", "`i'", 1);
      local disopt = subinword("`disopt'", "`word'", "", .);
      local i = `i' + 1;
    };

  /* Cleaning & Getting Stat Count */

    local disopt = trim(itrim("`disopt'"));

    local stat_count : word count `disopt';

    local disopt = subinstr("`disopt'", " ", ",", .);

*--------------------------------------------------------------*
*   08. Preparing General Row & Column Specs                   *
*--------------------------------------------------------------*;

  /* Stat Names */

    local statnames = subinstr("`disopt'", ",", " ", .);
    local statnames = subinword(`"`statnames'"', "1", `""Obs""', .);
    local statnames = subinword(`"`statnames'"', "2", `""n_Yes""', .);
    local statnames = subinword(`"`statnames'"', "3", `""Mean""', .);
    local statnames = subinword(`"`statnames'"', "4", `""Std Err""', .);
    local statnames = subinword(`"`statnames'"', "5", `""Std Dev""', .);
    local statnames = subinword(`"`statnames'"', "6", `""Var""', .);
    local statnames = subinword(`"`statnames'"', "7", `""Lower CI""', .);
    local statnames = subinword(`"`statnames'"', "8", `""Upper CI""', .);

  /* Column Formats */

    local colformats = subinstr("`disopt'", ",", " & ", .);
    local colformats = subinword("`colformats'", "1", "%9.0fc", .);
    local colformats = subinword("`colformats'", "2", "%9.0fc", .);
    local colformats = subinword("`colformats'", "3", "%9.0g", .);
    local colformats = subinword("`colformats'", "4", "%9.0g", .);
    local colformats = subinword("`colformats'", "5", "%9.0g", .);
    local colformats = subinword("`colformats'", "6", "%9.0g", .);
    local colformats = subinword("`colformats'", "7", "%9.0g", .);
    local colformats = subinword("`colformats'", "8", "%9.0g", .);

  /* P Names - Overall */

    if(inlist("`pvals'", "all", "overall"))
    {;
      local pnames = `" `pnames' "Overall" "';
    };

  /* P Names (Individual) & Over Names - Long */

    if("`wide'" == "" & "`over'" != "")
    {;
      forvalues i = 1/`over_count'
      {;
        if(inlist("`pvals'", "all", "individual"))
        {;
          local pnames = `" `pnames' "vs `i'" "';
        };

        local temp_name = trim(substr("`: label (`group_var') `i''", length("`over_count'")+1, .));
        local overnames = cond(length("`temp_name'") >= 32 | "`labs'" != "", `" `overnames' "_over_`i'" "', `" `overnames' "`temp_name'" "');
        local dis_len = cond(length("`temp_name'") > `dis_len' & length("`temp_name'") < 32, length("`temp_name'") + 1, `dis_len');
      };

      if("`total'" != "")
      {;
        local overnames = `" `overnames' "Total" "';
      };
    };

  /* P Names (Individual) & Over Names - Wide */

    if("`wide'" != "" & "`over'" != "")
    {;
      forvalues i = 1/`over_count'
      {;
        if(inlist("`pvals'", "all", "individual") & `i' < `over_count')
        {;
          forvalues j = `=1+`i''/`over_count'
          {;
            local pnames = `" `pnames' "`i'v`j'" "';
          };
        };

        local temp_name = trim(substr("`: label (`group_var') `i''", length("`over_count'")+1, .));
        local overnames = cond(length("`temp_name'") >= 10 | "`labs'" != "", `" `overnames' "' + (`" "_over_`i':" "' * `stat_count'), `" `overnames' "' + (`" "`temp_name':" "' * `stat_count'));
      };

      if("`total'" != "")
      {;
        local overnames = `" `overnames' "' + (`" "Total" "' * `stat_count');
      };
    };

*--------------------------------------------------------------*
*   09. Creating Results                                       *
*--------------------------------------------------------------*;

  tempname var_original;
  tempname var_process;
  tempname var_final;
  tempname total_final;

  local xivars : char _dta[__xi__Vars__To__Drop__];

  local group_count  = cond("`total'" == "", `over_count', `=`over_count'+1');
  local xi_count     = 0;
  local series_count = 0;
  local var_pos      = 1;

  foreach var of varlist `varlist'
  {;
    /* Setting Display Length */

      local dis_len = cond(length("`var'") >= `dis_len', length("`var'") + 1, `dis_len');

    /* Calculating - Over */

      calculateResults `var', sample(`touse') svy(`svy') subpop(`subpop') `opt_over' level(`level') pct(`pct') pvals(`pvals') mtest(`mtest');
      matrix `var_original' = r(results);

    /* Calculating - Total */

      if("`total'" != "")
      {;
        calculateResults `var', sample(`touse') svy(`svy') subpop(`subpop') `opt_over' `total' level(`level') pct(`pct') pvals(`pvals') mtest(`mtest');
        matrix `var_original' = `var_original' \ r(results);
      };

    /* Finalizing Variable Matrix - Long */

      if("`wide'" == "")
      {;
        ** Stats **;
        mata: selectMatrix("`var_original'", (1::`group_count'), (`disopt'), `round');
        matrix `var_final' = r(results);

        ** PVals - Overall **;
        if(inlist("`pvals'", "all", "overall"))
        {;
          mata: selectMatrix("`var_original'", (1::`group_count'), (9), 0.0001);
          matrix `var_final' = `var_final', r(results);
        };

        ** PVals - Individual **;
        if(inlist("`pvals'", "all", "individual"))
        {;
          mata: selectMatrix("`var_original'", (1::`group_count'), (10::`=9+`over_count''), 0.0001);
          matrix `var_final' = `var_final', r(results);
        };
      };

    /* Finalizing Variable Matrix - Wide */

      if("`wide'" != "")
      {;
        ** Stats **;
        mata: selectMatrix("`var_original'", (1::`group_count'), (`disopt'), `round');
        matrix `var_final' = vec(r(results)')';

        ** PVals - Overall **;
        if(inlist("`pvals'", "all", "overall"))
        {;
          mata: selectMatrix("`var_original'", (1), (9), 0.0001);
          matrix `var_final' = `var_final', r(results);
        };

        ** PVals - Individual **;
        if(inlist("`pvals'", "all", "individual"))
        {;
          mata: selectMatrix("`var_original'", (2::`over_count'), (10::`=8+`over_count''), 0.0001);
          matrix `var_process' = r(results);

          mata: st_matrix("`var_process'",vech(st_matrix("`var_process'"))');
          matrix `var_final' = `var_final', `var_process';
        };
      };

    /* Adding to Final Matrix */

      matrix `total_final' = nullmat(`total_final') \ `var_final';

    /* Format - Expanding */

      unab expanded : `: word `var_pos' of `input_list'';

      local is_xi = strpos(" `xivars' ", " `var' ") != 0;
      local is_series = strpos(" `input_list' ", " `var' ") == 0;

    /* Format - xi */

      if(`is_xi' == 1 & "`xilabs'" == "" & ("`wide'" != "" | "`over'" == ""))
      {;
        local var_name = word(subinstr("`: variable label `var''", "==", " ", .), 1);
        local var_num  = word(subinstr("`: variable label `var''", "==", " ", .), 2);
        local var_lab  = "`: label (`var_name') `var_num''";

        local roweqs     = `" `roweqs' "`var_name':" "';
        local rownames   = cond(length("`var_lab'") >= 32, `" `rownames' "`var_num'" "', `" `rownames' "`var_lab'" "');
        local rowformats = cond("`var'" == word("`expanded'", -1), "`rowformats'-", "`rowformats'&");

        local dis_len = cond(length("`var_lab'") > `dis_len' & length("`var_lab'") < 32, length("`var_lab'") + 1, `dis_len');

        local xi_count = `xi_count' + 1;

        if("`var'" == word("`expanded'", -1))
        {;
          local var_pos = `var_pos' + 1;
        };
        continue;
      };

    /* Format - series */

      if(`is_series' == 1 & `is_xi' == 0 & "`series'" != "" & ("`wide'" != "" | "`over'" == ""))
      {;
        local var_name = substr("`var'", 1, strrpos("`var'", "_") - 1);
        local var_num  = substr("`var'", strrpos("`var'", "_") + 1, .);
        local var_lab  = "`: variable label `var''";
        local var_lab  = substr("`var_lab'", strpos("`var_lab'", "[") + 1, strpos("`var_lab'", "]") - strpos("`var_lab'", "[") - 1);

        local roweqs     = `" `roweqs' "`var_name':" "';
        local rownames   = cond(length("`var_lab'") >= 32, `" `rownames' "`var_num'" "', `" `rownames' "`var_lab'" "');
        local rowformats = cond("`var'" == word("`expanded'", -1), "`rowformats'-", "`rowformats'&");

        local dis_len = cond(length("`var_lab'") > `dis_len' & length("`var_lab'") < 32, length("`var_lab'") + 1, `dis_len');

        local series_count = `series_count' + 1;

        if("`var'" == word("`expanded'", -1))
        {;
          local var_pos = `var_pos' + 1;
        };
        continue;
      };

    /* Format - other */

      /* Long */
      if("`wide'" == "")
      {;
        local roweqs     = `" `roweqs' "' + (`" "`var':" "' * `group_count');
        local rownames   = `" `rownames' "'  + (`" "`var'" "' * `group_count');
        local rowformats = "`rowformats'" + ("&" * `=`group_count'-1') + "-";
      };

      /* Wide */
      if("`wide'" != "")
      {;
        local roweqs     = `" `roweqs' "`var':" "';
        local rownames   = `" `rownames' "`var'" "';
        local rowformats = "`rowformats'-";
      };

      if("`var'" == word("`expanded'", -1))
      {;
        local var_pos = `var_pos' + 1;
      };
  };

*--------------------------------------------------------------*
*   10. Applying Row & Column Specs                            *
*--------------------------------------------------------------*;

  /* Long */

    if("`wide'" == "")
    {;
      /* colnames */

        local colnames = trim(itrim(`" `statnames' `pnames' "'));

      /* coleqs */

        local coleqs = (`" "_: " "' * `stat_count') + (`" "P_Vals: " "' * `: list sizeof pnames');

      /* colformats */

        local colformats = "`colformats'" + cond("`pvals'" == "none", " &", " |" + (" %5.4f &" * `: list sizeof pnames'));

      /* rownames */

        if("`over'" != "")
        {;
          local rownames = `"`overnames'"' * `: list sizeof varlist';
        };
    };

  /* Wide */

    if("`wide'" != "")
    {;
      /* colnames */

        local colnames = trim(itrim((`" `statnames' "' * `group_count') + `" `pnames' "'));

      /* coleqs */

        local coleqs = `" `overnames' "' + (`" "P_Vals: " "' * `: list sizeof pnames');

      /* colformats */

        local colformats = ("`colformats' | " * `=`group_count'-1') + "`colformats'" + cond("`pvals'" == "none", " &", " |" + (" %5.4f &" * `: list sizeof pnames'));
    };

  /* Finalizing */

    if(`xi_count' == 0 & `series_count' == 0 & ("`over'" == "" | "`wide'" != ""))
    {;
      local roweqs = `" "_:" "' * (`: word count of `varlist'' - 1);
      local rowformats = "&" * (`: word count of `varlist'' - 1);
    };

    local colnames   = trim(itrim(`"`colnames'"'));
    local coleqs     = trim(itrim(`"`coleqs'"'));
    local colformats = trim(itrim(`"& %-`dis_len's | `colformats'"'));
    local rownames   = trim(itrim(`"`rownames'"'));
    local roweqs     = trim(itrim(`"`roweqs'"'));
    local rowformats = "&-" + substr("`rowformats'", 1, length("`rowformats'")-1) + "&";

  /* Applying */

    *dis `" colnames   - `colnames'"';
    *dis `" coleq      - `coleqs'"';
    *dis `" colformats - `colformats'"';
    *dis `" rownames   - `rownames'"';
    *dis `" roweq      - `roweqs'"';
    *dis `" rowformats - `rowformats'"';
    *matlist `total_final';

    matrix colnames `total_final' = `colnames';
    matrix coleq    `total_final' = `coleqs';
    matrix rownames `total_final' = `rownames';
    matrix roweq    `total_final' = `roweqs';

*--------------------------------------------------------------*
*   11. Displaying Results                                     *
*--------------------------------------------------------------*;

  /* Title */

    dis;

    if("`title'" != "")
    {;
      dis "{title:`title'}";
    };

  /* Group Label */

    local num_format = 6 + length("`over_count'");

    if("`over'" != "" & "`legend'" == "")
    {;
      if("`title'" != "")
      {;
        dis;
      };

      forvalues i = 1/`over_count'
      {;
        local temp_name = trim(substr("`: label (`group_var') `i''", length("`over_count'")+1, .));
        dis %-`num_format's "_over_`i'" " | " subinstr("`over'",",",", ",.) " = " `""`temp_name'""';
      };
    };

  /* Listing Matrix */

    matlist `total_final', cspec(`colformats')
                           rspec(`rowformats')
                           showcoleq(combined);

*--------------------------------------------------------------*
*   12. Returning Results & Resetting Changes                  *
*--------------------------------------------------------------*;

  return matrix results = `total_final';
  cap qui set matsize `original_matsize';

end;

**======================================================================**
**   Subroutine: calculateResults                                       **
**======================================================================**;

program define calculateResults, rclass;
syntax varname(numeric),
  SAMPLE(varname numeric)
  [
    SVY(string)
    SUBPOP(string)
    OVER(varname numeric)
    TOTAL
    LEVEL(cilevel)
    PCT(integer 1)
    PVALS(string)
    MTEST(string)
  ];

*--------------------------------------------------------------*
*   01. Initializing Options                                   *
*--------------------------------------------------------------*;

  /* Matrices */

    tempname results_mean;
    tempname results_obs;
    tempname results_sd;
    tempname results_variance;
    tempname results_total;

  /* Over */

    local over_count = 1;
    if("`over'" != "")
    {;
      qui levelsof `over', local(over_lvls);

      local over_count : word count `over_lvls';

      local opt_over = "over(`over', nolabel)";
    };

    local col_count = 9 + `over_count';

  /* Total */

    if("`total'" != "")
    {;
      local over_count = 1;
      local opt_over = "";
    };

  /* Subpop */

    local in_subpop = cond("`subpop'" == "", "", "& `subpop' == 1");

*--------------------------------------------------------------*
*   02. Getting Mean & SD Estimates                            *
*--------------------------------------------------------------*;

  qui `svy' mean `varlist' if `sample', level(`level') `opt_over';

  /* `n_over' */

    local n_over = e(N_over);

  /* `results_mean' */

    matrix `results_mean' = r(table);

  /* `results_obs' */

    matrix `results_obs' = e(_N);

  /* `results_overlabs' */

    local results_overlabs = "";

    if("`opt_over'" != "")
    {;
      local results_overlabs = e(over_labels);
    };

  /* `results_sd' & `results_variance' */

    qui estat sd;

    matrix `results_sd' = r(sd);
    matrix `results_variance' = r(variance);

*--------------------------------------------------------------*
*   03. Creating Initial Matrix                                *
*--------------------------------------------------------------*;

  matrix `results_total' = J(`over_count', `col_count', .);

  /* Initial Matrix */

    tempname results_temp;
    matrix `results_temp' = `results_obs'[1,1...]                \
                            J(1, `n_over', .)                    \
                            (`results_mean'[1,1...]    ) * `pct' \
                            (`results_mean'[2,1...]    ) * `pct' \
                            (`results_sd'[1,1...]      ) * `pct' \
                            (`results_variance'[1,1...]) * `pct' \
                            (`results_mean'[5,1...]    ) * `pct' \
                            (`results_mean'[6,1...]    ) * `pct' ;

  /* P-Values */

    if(`over_count' > 1)
    {;
      /* Overall */

        if(inlist("`pvals'", "all", "overall"))
        {;
          local test_cmd = "[`varlist']" + subinstr("`e(over_namelist)'"," ","=[`varlist']",.);
          cap qui test `test_cmd';
          matrix `results_temp' = `results_temp' \ J(1, `n_over', r(p));
        };
        else
        {;
          matrix `results_temp' = `results_temp' \ J(1, `n_over', .);
        };

      /* Individual */

        if(inlist("`pvals'", "all", "individual"))
        {;
          tempname results_pvals;

          local pos = 1;
          forvalues i = 1/`over_count'
          {;
            if("`: word `pos' of `results_overlabs''" == "`i'")
            {;
              local test_cmd = "[`varlist']`i'=[`varlist']" + subinstr("`e(over_namelist)'", " ", "=[`varlist']", .);

              cap qui test `test_cmd', mtest(`mtest');
              if(_rc != 0)
              {;
                matrix `results_temp' = `results_temp' \ J(1, `n_over', .);
                local pos = `pos' + 1;
                continue;
              };

              matrix `results_pvals' = r(mtest);
              if("`mtest'" == "noadjust")
              {;
                matrix `results_pvals' = `results_pvals'[1...,3]';
              };
              else
              {;
                matrix `results_pvals' = `results_pvals'[1...,4]';
              };

              matrix `results_temp'  = `results_temp' \ `results_pvals';

              local pos = `pos' + 1;
            };
            else
            {;
              matrix `results_temp' = `results_temp' \ J(1, `n_over', .);
            };
          };
        };
    };

*--------------------------------------------------------------*
*   04. Reshaping Matrix                                       *
*--------------------------------------------------------------*;

  if(`over_count' > 1)
  {;
    local pos = 1;
    forvalues i = 1/`over_count'
    {;
      if("`: word `pos' of `results_overlabs''" == "`i'")
      {;
        qui count if `sample' & `varlist' & !missing(`varlist') & `over' == `i' `in_subpop';
        matrix `results_temp'[2,`pos'] = r(N);

        matrix `results_total'[`i',1] = `results_temp'[1...,`pos']';
        local pos = `pos' + 1;
      };
    };
  };
  else
  {;
    qui count if `sample' & `varlist' & !missing(`varlist') `in_subpop';
    matrix `results_temp'[2,1] = r(N);
    matrix `results_total'[1,1] = `results_temp'[1...,1]';
  };

*--------------------------------------------------------------*
*   05. Returning Matrix                                       *
*--------------------------------------------------------------*;

  return matrix results = `results_total';

end;
