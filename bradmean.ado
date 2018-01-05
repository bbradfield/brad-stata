**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.3.4                                                **
**   Date:         01/05/2018                                           **
**                                                                      **
**======================================================================**
**======================================================================**

version 14.0
mata:

void selectMatrix(string scalar matrixname,
                  real vector rows,
                  real vector cols)
{
  processmatrix = st_matrix(matrixname)
  processmatrix = processmatrix[rows, cols]

  st_matrix("r(results)", processmatrix);
}

end;

#delimit;

**======================================================================**
**   Routine: bradmean                                                  **
**======================================================================**;

program define bradmean, rclass sortpreserve;
syntax varlist(numeric) [if] [in],
  [
    SVY                     /* gen: turn on               */
    SUBpop(varname numeric) /* gen: subpop                */
    OVER(varlist)           /* gen: over()                */
    LEVEL(cilevel)          /* gen: ci level              */
    DISopt(string)          /* gen: display stats & order */
    FORMAT(string)          /* fmt: presets               */
    noLABS                  /* fmt: no labels             */
    PCT                     /* fmt: format as pct         */
    PVALS(string)           /* fmt: pvals                 */
    ROUND(integer 7)        /* fmt: number of decimals    */
    TITLE(string)           /* fmt: title                 */
    TOTAL                   /* fmt: total                 */
    WIDE                    /* fmt: wide                  */
  ];

*--------------------------------------------------------------*
*   01. Setting `matsize'                                      *
*--------------------------------------------------------------*;

  local original_matsize = c(matsize);
  if(c(matsize) < 800)
  {;
    set matsize 800;
  };

*--------------------------------------------------------------*
*   02. Generating `group_var'                                 *
*--------------------------------------------------------------*;

  local over_count = 1;

  if("`over'" != "")
  {;
    /* Creating `group_string' & `group_numeric' */

      tempvar group_string;
      qui egen strL `group_string' = concat(`over'), decode p(", ");

      tempvar group_numeric;
      qui egen `group_numeric' = group(`over');

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

    /* Cleaning */

      cap drop `group_numeric';
      cap drop `group_string';
    };

*--------------------------------------------------------------*
*   03. Translating Format                                     *
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
*   04. Setting Options                                        *
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

  /* fmt - pct */

    local pct = cond("`pct'" == "", 1, 100);

  /* fmt - pvals */

    local pvals = trim(strlower("`pvals'"));
    local pvals = cond(!inlist("`pvals'", "all", "overall", "individual", "none"), "overall", "`pvals'");
    local pvals = cond(`over_count' == 1, "none", "`pvals'");

    local pvals = cond(`over_count' == 2 & "`pvals'" != "none", "overall", "`pvals'");

  /* fmt - round */

    local round = cond(inrange(`round', 0, 7), 1/(10^`round'), 1/(10^7));

  /* fmt - total */

    local total = cond(`over_count' == 1, "", "`total'");

  /* fmt - wide */

    local wide = cond(`over_count' == 1, "", "`wide'");

*--------------------------------------------------------------*
*   05. Setting `disopt'                                       *
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
*   06. Marking Sample                                         *
*--------------------------------------------------------------*;

  tempname touse;

  mark `touse' `if' `in';

  if("`opt_over'" != "")
  {;
    markout `touse' `group_var';
  };

  if("`svy'" != "")
  {;
    svymarkout `touse';
  };

  if("`subpop'" != "")
  {;
    tempvar in_subpop;
    qui generate `in_subpop' = `subpop';
    qui replace  `in_subpop' = . if `subpop' != 1;
    markout `touse' `in_subpop';
  };

*--------------------------------------------------------------*
*   07. Creating Results                                       *
*--------------------------------------------------------------*;

  tempname var_original;
  tempname var_process;
  tempname var_final;
  tempname total_final;

  local group_count = cond("`total'" == "", `over_count', `=`over_count'+1');

  local dis_len = 14;

  foreach var of varlist `varlist'
  {;
    /* Setting Display Length */

      local dis_len = cond(length("`var'") >= `dis_len', length("`var'") + 1, `dis_len');

    /* Calculating - Over */

      calculateResults `var', sample(`touse') svy(`svy') `opt_over' level(`level') round(`round') pct(`pct') pvals(`pvals');
      matrix `var_original' = r(results);

    /* Calculating - Total */

      if("`total'" != "")
      {;
        calculateResults `var', sample(`touse') svy(`svy') `opt_over' `total' level(`level') round(`round') pct(`pct') pvals(`pvals');
        matrix `var_original' = `var_original' \ r(results);
      };

    /* Finalizing Variable Matrix - Long */

      if("`wide'" == "")
      {;
        ** Stats **;
        mata: selectMatrix("`var_original'", (1::`group_count'), (`disopt'));
        matrix `var_final' = r(results);

        ** PVals - Overall **;
        if(inlist("`pvals'", "all", "overall"))
        {;
          mata: selectMatrix("`var_original'", (1::`group_count'), (9));
          matrix `var_final' = `var_final', r(results);
        };

        ** PVals - Individual **;
        if(inlist("`pvals'", "all", "individual"))
        {;
          mata: selectMatrix("`var_original'", (1::`group_count'), (10::`=9+`over_count''));
          matrix `var_final' = `var_final', r(results);
        };
      };

    /* Finalizing Variable Matrix - Wide */

      if("`wide'" != "")
      {;
        ** Stats **;
        mata: selectMatrix("`var_original'", (1::`group_count'), (`disopt'));
        matrix `var_final' = vec(r(results)')';

        ** PVals - Overall **;
        if(inlist("`pvals'", "all", "overall"))
        {;
          mata: selectMatrix("`var_original'", (1), (9));
          matrix `var_final' = `var_final', r(results);
        };

        ** PVals - Individual **;
        if(inlist("`pvals'", "all", "individual"))
        {;
          mata: selectMatrix("`var_original'", (2::`over_count'), (10::`=8+`over_count''));
          matrix `var_process' = r(results);

          mata: st_matrix("`var_process'",vech(st_matrix("`var_process'"))');
          matrix `var_final' = `var_final', `var_process';
        };
      };

    /* Adding to Final Matrix */

      matrix `total_final' = nullmat(`total_final') \ `var_final';
  };

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

  /* P Names */

    if(inlist("`pvals'", "all", "overall"))
    {;
      local pnames = `" `pnames' "Overall" "';
    };

    if(inlist("`pvals'", "all", "individual"))
    {;
      forvalues i = 1/`over_count'
      {;
        ** Long **;
        if("`wide'" == "")
        {;
          local pnames = `" `pnames' "vs `i'" "';
        };

        ** Wide **;
        if("`wide'" != "" & `i' < `over_count')
        {;
          forvalues j = `=1+`i''/`over_count'
          {;
            local pnames = `" `pnames' "`i'v`j'" "';
          };
        };

      };
    };

    local pnames = trim(itrim(`"`pnames'"'));

  /* Over Names */

    local lab_len = 14;

    if("`opt_over'" != "")
    {;
      forvalues i = 1/`over_count'
      {;
        local temp_name = trim(substr("`: label (`group_var') `i''", length("`over_count'")+1, .));
        local overnames_long  = `" `overnames_long' "`temp_name'" "';
        local overnames_short = `" `overnames_short' "_over_`i'" "';

        local lab_len = cond(length("`temp_name'") >= `lab_len', length("`temp_name'") + 1, `lab_len');
      };

      if("`total'" != "")
      {;
        local overnames_long  = `" `overnames_long' "Total" "';
        local overnames_short = `" `overnames_short' "Total" "';
      };

      local overnames_long  = trim(itrim(`"`overnames_long'"'));
      local overnames_short = trim(itrim(`"`overnames_short'"'));
    };

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

*--------------------------------------------------------------*
*   09. Applying Row & Column Specs                            *
*--------------------------------------------------------------*;

  /* Long */

    if("`wide'" == "")
    {;
      ** dis_len **;
      local dis_len = cond(`lab_len' > `dis_len', cond(`lab_len' > 30, `dis_len', `lab_len'), `dis_len');

      ** colnames **;
      local colnames = trim(itrim(`" `statnames' `pnames' "'));

      ** coleqs **;
      local coleqs = ("_: " * `stat_count') + ("P_Vals: " * `: list sizeof pnames');

      ** colformats **;
      local colformats = "`colformats'" + cond("`pvals'" == "none", " &", " |" + (" %5.4f &" * `: list sizeof pnames'));

      ** rownames **;
      local rownames = cond("`over'" == "", "`varlist'", cond(`lab_len' > 30, `"`overnames_short'"', `"`overnames_long'"') * `: list sizeof varlist');

      ** roweqs **;
      foreach var of varlist `varlist'
      {;
        local roweqs = "`roweqs' " + cond("`over'" == "", "_: ", ("`var': " * `group_count'));
      };

      ** rowformats **;
      local rowformats = (("&" * `=`group_count'-1') + cond("`over'" == "", "&", "-")) * `: list sizeof varlist';

      ** Finalizing **;
      local colformats = "& %-`dis_len's | `colformats'";
      local rowformats = "&-`rowformats'";
    };

  /* Wide */

    if("`wide'" != "")
    {;
      ** colnames **;
      local colnames = trim(itrim((`" `statnames' "' * `group_count') + `" `pnames' "'));

      ** coleqs **;
      local overnames_temp = cond(`lab_len' > 9, `"`overnames_short'"', `"`overnames_long'"');
      foreach word of local overnames_temp
      {;
        local temp_name = `" "`word':" "' * `stat_count';
        local coleqs = trim(itrim(`" `coleqs' `temp_name' "'));
      };
      local temp_name = `" "P_Vals:" "' * `: list sizeof pnames';
      local coleqs = trim(itrim(`" `coleqs' `temp_name' "'));

      ** colformats **;
      local colformats = ("`colformats' | " * `=`group_count'-1') + "`colformats'" + cond("`pvals'" == "none", " &", " |" + (" %5.4f &" * `: list sizeof pnames'));

      ** rownames **;
      local rownames = `"`varlist'"';

      ** roweqs **;
      local roweqs = trim(itrim("_: " * `: list sizeof varlist'));

      ** rowformats **;
      local rowformats = "&" * `: list sizeof varlist';

      ** Finalizing **;
      local colformats = "& %-`dis_len's | `colformats'";
      local rowformats = "&-`rowformats'";
    };

  /* Applying */

    matrix colnames `total_final' = `colnames';
    matrix coleq    `total_final' = `coleqs';
    matrix rownames `total_final' = `rownames';
    matrix roweq    `total_final' = `roweqs';

*--------------------------------------------------------------*
*   10. Displaying Results                                     *
*--------------------------------------------------------------*;

  /* Title */

    if("`title'" != "")
    {;
      dis;
      dis "{title:`title'}";
    };

  /* Group Label */

    local num_format = 6 + length("`over_count'");

    if("`over'" != "")
    {;
      dis;
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
*   11. Returning Results & Resetting Changes                  *
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
    OVER(varname numeric)
    TOTAL
    LEVEL(cilevel)
    ROUND(real 0.0000001)
    PCT(integer 1)
    PVALS(string)
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

*--------------------------------------------------------------*
*   02. Getting Mean & SD Estimates                            *
*--------------------------------------------------------------*;

  qui `svy' mean `varlist' if `sample', level(`level') `opt_over';

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
*   03. Creating Matrix to Return                              *
*--------------------------------------------------------------*;

  matrix `results_total' = J(`over_count', `col_count', .);

  local pos = 1;
  forvalues i = 1/`over_count'
  {;
    if("`opt_over'" != "" & "`: word `pos' of `results_overlabs''" != "`i'")
    {;
      continue;
    };

    /* Getting n_yes */

      if(`over_count' == 1)
      {;
        qui count if `sample' & `varlist' & !missing(`varlist');
        local n_yes = r(N);
      };
      else
      {;
        qui count if `sample' & `varlist' & !missing(`varlist') & `over' == `i';
        local n_yes = r(N);
      };

    /* Inputting Mean Results */

      matrix `results_total'[`i',1] = `results_obs'[1,`pos'];
      matrix `results_total'[`i',2] = `n_yes';
      matrix `results_total'[`i',3] = round(`results_mean'[1,`pos']     * `pct', `round');
      matrix `results_total'[`i',4] = round(`results_mean'[2,`pos']     * `pct', `round');
      matrix `results_total'[`i',5] = round(`results_sd'[1,`pos']       * `pct', `round');
      matrix `results_total'[`i',6] = round(`results_variance'[1,`pos'] * `pct', `round');
      matrix `results_total'[`i',7] = round(`results_mean'[5,`pos']     * `pct', `round');
      matrix `results_total'[`i',8] = round(`results_mean'[6,`pos']     * `pct', `round');

    /* PVals - Overall */

      if(`over_count' > 1 & inlist("`pvals'", "all", "overall"))
      {;
        local test_cmd = "[`varlist']" + subinstr("`e(over_namelist)'"," ","=[`varlist']",.);
        cap qui test `test_cmd';
        matrix `results_total'[`i',9] = round(r(p), 0.0001);
      };

    /* PVals - Individual */

      if(`over_count' > 1 & inlist("`pvals'", "all", "individual"))
      {;
        forvalues j = 1/`over_count'
        {;
          cap qui test [`varlist']`i'=[`varlist']`j';
          matrix `results_total'[`i',`=9+`j''] = round(r(p), 0.0001);
        };
      };

    /* Incrementing */

      local pos = `pos' + 1;
  };

*--------------------------------------------------------------*
*   04. Returning Matrix                                       *
*--------------------------------------------------------------*;

  return matrix results = `results_total';

end;
