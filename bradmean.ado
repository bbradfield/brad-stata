**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.3.3                                                **
**   Date:         11/16/2017                                           **
**                                                                      **
**======================================================================**
**======================================================================**

/* Mata */

version 14.0
mata:

  /* select_long */

    void select_long(real scalar varcount,
                     real scalar subcount,
                     real scalar overcount,
                     real vector colorder)
    {
      matrix current, results

      for(i=1; i <= varcount; i++)
      {
        for(j=1; j <= subcount; j++)
        {
          /* Getting Original Matrix */
          originalname = "output_" + strofreal(i) + "_" + strofreal(j)
          originalname = st_local(originalname)
          original = st_matrix(originalname)

          /* Adding Stats to Current Matrix */
          current = original[colorder, .]
          current = transposeonly(current)

          /* Adding P-Vals to Current Matrix */
          pvals = st_local("pvals")
          if(overcount > 1 & pvals == "")
          {
            endrow = 9 + overcount
            current = current,original[(10::endrow),(2::overcount)]
          }

          /* Adding Current Matrix to Results Matrix */
          if(i == 1 & j == 1) results = current
          else results = results\current
        }
      }

      st_matrix("r(results)",results)
    }

  /* select_wide */

    void select_wide(real scalar varcount,
                     real scalar subcount,
                     real scalar overcount,
                     real vector colorder)
    {
      matrix current, results

      for(i=1; i <= varcount; i++)
      {
        for(j=1; j <= subcount; j++)
        {
          /* Getting Original Matrix */
          originalname = "output_" + strofreal(i) + "_" + strofreal(j)
          originalname = st_local(originalname)
          original = st_matrix(originalname)

          /* Adding Stats to Current Matrix */
          current = vec(original[colorder, .])

          /* Adding P-Values to Current Matrix */
          pvals = st_local("pvals")
          if(pvals == "")
          {
            for(k=1; k < overcount; k++)
            {
              startrow = 10 + k
              endrow = 9 + overcount
              current = current\original[(startrow::endrow),k]
            }
          }

          /* Adding Current Matrix to Results Matrix */
          current = transposeonly(current)
          if(i == 1 & j == 1) results = current
          else results = results\current
        }
      }

      st_matrix("r(results)",results)
    }

end

/* Stata */

program define bradmean, rclass
version 14.0
#delimit;
set linesize 255;

syntax varlist(numeric) [if] [in], [SVY /* svy */
                                    SUBpop(varname numeric) /* subpop() */
                                    OVER(varlist) /* over() */
                                    LEVEL(cilevel) /* ci level */
                                    ROUND(integer 7) /* number of decimals */
                                    PCT /* format as % */
                                    noPVALS /* don't include pvals */
                                    noLABS /* don't include labels */
                                    DISopt(string) /* display stats & order */
                                    WIDE /* wide format */
                                    TITLE(string) /* displays title above table */
                                   ];

*--------------------------------------------------------------*
*   01. Checking Error Codes                                   *
*--------------------------------------------------------------*;

  /* `svy' - data not set up for svy, use svyset */

    if("`svy'" != "")
    {;
      cap qui svydescribe;

      if(_rc != 0)
      {;
        di as err "data not set up for svy, use svyset" as text "";
        error 119;
      };
    };

*--------------------------------------------------------------*
*   02. Preparing Varlist                                      *
*--------------------------------------------------------------*;

  local var_count : list sizeof varlist;
  tokenize `varlist';

*--------------------------------------------------------------*
*   03. Setting Option Macros                                  *
*--------------------------------------------------------------*;

  /* Setting `svy' */

    local opt_svy_1 = "";

    ** `svy' off, but has subpop() **;
    if("`svy'" == "" & "`subpop'" != "")
    {;
      cap qui svyset;
      local orig_svyset = r(settings);

      tempvar unweighted;
      qui generate `unweighted' = 1;
      qui svyset [pw=`unweighted'];

      local svy = "svy";
    };

    ** `svy' on **;
    if("`svy'" != "")
    {;
      local opt_svy_1 = "svy: ";
      qui svyset;
      local svy_var = r(wvar);
    };

  /* Setting `over' */

    local over_vars = cond("`over'" == "", "", subinstr("`over'"," ",",",.));

  /* Setting `round' */

    local opt_round = cond(inrange(`round',0,7), `round', 7);

    local opt_round = 1/(10^`opt_round');

  /* Setting `pct' */

    local opt_pct = cond("`pct'" == "", 1, 100);

  /* Setting `wide' */

    if("`over'" == "")
    {;
      local wide = "";
    };

  /* Marking sample `touse' */

    local temp_if = "`if'";
    local temp_if = cond("`temp_if'" == "",
                         cond("`subpop'" == "","","if !missing(`subpop')"),
                         cond("`subpop'" == "","`temp_if'","`temp_if' & !missing(`subpop')"));
    local temp_if = cond("`temp_if'" == "",
                         cond("`over'" == "","","if !missing(`over_vars')"),
                         cond("`over'" == "","`temp_if'","`temp_if' & !missing(`over_vars')"));
    local temp_if = cond("`temp_if'" == "",
                         cond("`svy'" == "","","if !missing(`svy_var')"),
                         cond("`svy'" == "","`temp_if'","`temp_if' & !missing(`svy_var')"));

    tempvar touse;
    qui mark `touse' `temp_if' `in';

*--------------------------------------------------------------*
*   04a. Finding Unique Values of `subpop'                     *
*--------------------------------------------------------------*;

  local sub_count = 1;

  if("`subpop'" != "")
  {;
    qui levelsof `subpop' if `touse', local(subpop_lvls);
    local sub_count : word count `subpop_lvls';

    /* If only 1 level, remove the subpop option */

      if(`sub_count' == 1)
      {;
        di as err "subpop only has 1 level, not using option" as text "";
        local subpop = "";
      };

    /* If only 0 level, remove the subpop option & set to 1 */

      if(`sub_count' == 0)
      {;
        di as err "subpop missing for all weighted observations, not using option" as text "";
        local subpop = "";
        local sub_count = 1;
      };

    /* If already dichotomous, do not make multiple variables */

      if("`subpop_lvls'" == "0 1")
      {;
        local sub_count = 1;
        local opt_svy_1 = "svy, subpop(`subpop'): ";
      };

    /* If categorical, make dichotomous indicator variables */

      if(`sub_count' > 1)
      {;
        forvalues i = 1/`sub_count'
        {;
          local curr_level : word `i' of `subpop_lvls';

          tempvar subpop_`i';
          qui generate `subpop_`i'' = `subpop' == `curr_level' if !missing(`subpop');

          local opt_svy_`i' = "svy, subpop(`subpop_`i''): ";
        };
      };
  };

*--------------------------------------------------------------*
*   04b. Finding Unique Values of `over'                       *
*--------------------------------------------------------------*;

  local over_count = 1;
  local lab_len = 0;

  if("`over'" != "")
  {;
    /* Creating ordered `group_order' */

      qui tempvar group_var;
      qui egen strL `group_var' = concat(`over') if `touse', decode p(", ");

      qui tempvar group_order;
      qui egen `group_order' = group(`over') if `touse';

    /* Creating `over_count' */

      qui levelsof `group_order', local(over_lvls);
      qui local over_count : word count `over_lvls';

    /* If `over_count' == 0, set to 1 */

      if(`over_count' == 0)
      {;
        local over_count = 1;
      };

    /* Labelling `group_order' */

      qui tempvar sort_orig;
      qui generate `sort_orig' = _n;

      qui tempvar sort_temp;
      qui generate `sort_temp' = .;

      qui tempname group_lab;

      local lab_len = 0;
      forvalues i = 1/`over_count'
      {;
        qui replace `sort_temp' = cond(`group_order' == `i',1,.);
        sort `sort_temp';

        local temp_lab = trim(itrim(`group_var'[1]));
        if(length("`temp_lab'") > `lab_len' & "`labs'" == "")
        {;
          local lab_len = length("`temp_lab'");
        };
        label define `group_lab' `i' "`temp_lab'", modify;
      };

      sort `sort_orig';
      label values `group_order' `group_lab';

    /* Setting opt_over */

      local opt_over = "over(`group_order', nolabel)";
  };

*--------------------------------------------------------------*
*   05. Setting `disopt'                                       *
*--------------------------------------------------------------*;

  /* Checking `over_count' */

    if(`over_count' == 1)
    {;
      local over = "";
      local opt_over = "";
      local wide = "";
    };

  /* Cleaning Entries */

    if("`disopt'" != "")
    {;
      local disopt = strlower("`disopt'");
      local disopt = subinword("`disopt'","ci","lci uci",.);
      local disopt = subinword("`disopt'","_all","obs mean se sd var lci uci",.);

      local tempdisopt = "";

      forvalues i = 1/`: word count `disopt''
      {;
        if(inlist(word("`disopt'",`i'),"obs","mean","se","sd","var","lci","uci"))
        {;
          local tempdisopt = "`tempdisopt' " + word("`disopt'",`i');
        };
      };

      local disopt = trim("`tempdisopt'");
    };

  /* Setting Defaults */

    if("`disopt'" == "" & "`wide'" == "")
    {;
      local disopt = "obs mean sd lci uci";
    };
    if("`disopt'" == "" & "`wide'" != "")
    {;
      local disopt = "mean";
    };

  /* Setting `stat_count'*/

    local stat_count : word count `disopt';

*--------------------------------------------------------------*
*   06. Creating Matrix to be Passed to Mata                   *
*--------------------------------------------------------------*;

  tempname returns;
  tempname grpobs;
  tempname sd;
  tempname variance;
  tempname pvalues;

  forvalues i = 1/`var_count'
  {;
    forvalues j = 1/`sub_count'
    {;
      /* Initializing Matrix */

        tempname output_`i'_`j';
        matrix `output_`i'_`j'' = J(9,`over_count',.);
        matrix rownames `output_`i'_`j'' = b se sd var lci uci totobs subobs grpobs;

      /* Mean `returns' */

        *cap qui `opt_svy_`j'' mean ``i'' `if' `in', level(`level') `opt_over';
        cap qui `opt_svy_`j'' mean ``i'' if `touse', level(`level') `opt_over';

        matrix `returns'    = r(table);
        local  totobs       = e(N);
        local  subobs       = e(N_sub);
        matrix `grpobs'     = e(_N);
        local  n_over       = e(N_over);
        local  n_over_names = e(over_namelist);

        cap qui estat sd;

        matrix `sd' = r(sd);
        matrix `variance' = r(variance);

      /* Creating P-Values */

        if(`over_count' > 1 & "`pvals'" != "nopvals")
        {;
          matrix `pvalues' = J(`over_count',`over_count',.);
          local temp_names;

          forvalues x = 1/`over_count'
          {;
            local temp_names = "`temp_names' p_v`x'";
            forvalues y = 1/`over_count'
            {;
              if(`x' != `y')
              {;
                cap qui test [``i'']`x' == [``i'']`y';
                local pval = r(p);
                matrix `pvalues'[`x',`y'] = round(`pval',0.0001);
              };
            };
          };

          matrix rownames `pvalues' = `temp_names';
          matrix `output_`i'_`j'' = `output_`i'_`j'' \ `pvalues';
        };

      /* Placing `returns' in Matrix */

        if("`over'" == "")
        {;
          matrix `output_`i'_`j''[1,1] = round(`returns'[1,1] *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[2,1] = round(`returns'[2,1] *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[3,1] = round(`sd'[1,1]      *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[4,1] = round(`variance'[1,1]*`opt_pct',`opt_round');
          matrix `output_`i'_`j''[5,1] = round(`returns'[5,1] *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[6,1] = round(`returns'[6,1] *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[7,1] = `totobs';
          matrix `output_`i'_`j''[8,1] = `subobs';
        };
        else
        {;
          local x = 1;
          foreach col in `n_over_names'
          {;
            matrix `output_`i'_`j''[1,`col'] = round(`returns'[1,`x'] *`opt_pct',`opt_round');
            matrix `output_`i'_`j''[2,`col'] = round(`returns'[2,`x'] *`opt_pct',`opt_round');
            matrix `output_`i'_`j''[3,`col'] = round(`sd'[1,`x']      *`opt_pct',`opt_round');
            matrix `output_`i'_`j''[4,`col'] = round(`variance'[1,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[5,`col'] = round(`returns'[5,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[6,`col'] = round(`returns'[6,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[7,`col'] = `totobs';
            matrix `output_`i'_`j''[8,`col'] = `subobs';
            matrix `output_`i'_`j''[9,`col'] = `grpobs'[1,`x'];

            local ++x;
          };
        };

    };
  };

*--------------------------------------------------------------*
*   07. Getting Information to Format Table                    *
*--------------------------------------------------------------*;

  /* Length */

    local length = 10;

    /* `sub', `over' */

      ** Long **;
      if("`wide'" == "")
      {;
        if(`lab_len' > 30)
        {;
          local labs = "nolabs";
        };

        local temp_len = cond(`sub_count' == 1, 0, length("_sub_`sub_count'"));
        local temp_len = `temp_len' + cond(`over_count' == 1, 0, cond("`labs'" == "", length("_over_`over_count'"), `lab_len'));

        local length = cond(`temp_len' > `length', `temp_len', `length');
      };

      ** Wide **;
      if("`wide'" != "")
      {;
        if(`lab_len' > `=`stat_count'*10')
        {;
          local labs = "nolabs";
        };

        local temp_len = cond(`sub_count' == 1, 0, length("_sub_`sub_count'"));

        local length = cond(`temp_len' > `length', `temp_len', `length');
      };

    /* `varlist' */

      forvalues i = 1/`var_count'
      {;
        if(length("``i''") >= `length')
        {;
          local length = length("``i''") + 1;
        };
      };

  /* By `disopt': `col_order', `col_names', `col_format' */

    local col_order  = "";
    local col_names  = "";
    local col_format = "";

    foreach i in `disopt'
    {;
      /* obs */

        if("`i'" == "obs")
        {;
          local col_names = `" `col_names' "Obs" "';
          if("`over'" != "")
          {;
            local col_order = "`col_order',9";
            local col_format = "`col_format' & %9.0fc";
            continue;
          };
          if("`subpop'" != "")
          {;
            local col_order = "`col_order',8";
            local col_format = "`col_format' & %9.0fc";
            continue;
          };
          local col_order = "`col_order',7";
          local col_format = "`col_format' & %9.0fc";
        };

      /* mean */

        if("`i'" == "mean")
        {;
          local col_order = "`col_order',1";
          local col_names = `" `col_names' "Mean" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* se */

        if("`i'" == "se")
        {;
          local col_order = "`col_order',2";
          local col_names = `" `col_names' "Std Err" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* sd */

        if("`i'" == "sd")
        {;
          local col_order = "`col_order',3";
          local col_names = `" `col_names' "Std Dev" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* var */

        if("`i'" == "var")
        {;
          local col_order = "`col_order',4";
          local col_names = `" `col_names' "Variance" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* lci */

        if("`i'" == "lci")
        {;
          local col_order = "`col_order',5";
          local col_names = `" `col_names' "Lower CI" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* uci */

        if("`i'" == "uci")
        {;
          local col_order = "`col_order',6";
          local col_names = `" `col_names' "Upper CI" "';
          local col_format = "`col_format' & %9.0g";
        };
    };

    local col_order  = "(" + substr("`col_order'",2,.) + ")";
    local col_names  = trim(`"`col_names'"');
    local col_format = trim("`col_format'");

  /* `row_format' */

    local temp;

    ** Long **;
    if("`wide'" == "")
    {;
      local temp = (`=`over_count'-1' * "&")
                 + cond(`over_count' > 1, "-", "&");

      local temp = (`=`sub_count'-1' * "`temp'")
                 + substr("`temp'", 1, length("`temp'")-1)
                 + cond(`sub_count' > 1 | `over_count' > 1, "-", "&");

      local temp = `var_count' * "`temp'";
    };

    ** Wide **;
    if("`wide'" != "")
    {;
      local temp = (`=`sub_count'-1' * "&")
                 + cond(`sub_count' > 1, "-", "&");

      local temp = `var_count' * "`temp'";
    };

    local row_format = "&-" + substr("`temp'", 1, length("`temp'")-1) + "&";

  /* `col_format' */

    local temp;

    if("`wide'" != "")
    {;
      local temp = subinword("`col_format'", "&", "|", 1);
      local col_format = `over_count' * "`temp' ";
    };

  /* `col_eqs', `row_names', `row_eqs' */

    local col_eqs   = "";
    local row_names = "";
    local row_eqs   = "";

    forvalues i = 1/`var_count'
    {;
      forvalues j = 1/`sub_count'
      {;
        forvalues k = 1/`over_count'
        {;
          /* `col_eqs' */

            ** Long **;
            if("`wide'" == "" & `"`col_eqs'"' == "")
            {;
              local col_eqs = trim(`stat_count' * "_: ");
            };

            ** Wide **;
            if("`wide'" != "" & `i' == 1 & `j' == 1)
            {;
              local temp_lab = cond("`labs'" == "",
                               `stat_count' * `" "`: label (`group_order') `k''" "',
                               `stat_count' * "_over_`k' ");

              local col_eqs = `" `col_eqs' `temp_lab' "';
            };

          /* `row_names' */

            ** Long **;
            if("`wide'" == "")
            {;
              if(`sub_count' == 1 & `over_count' == 1)
              {;
                local row_names = "`row_names' ``i''";
              };

              if(`sub_count' == 1 & `over_count' > 1)
              {;
                local temp_lab = cond(`over_count' == 1, "", cond("`labs'" == "", `"`: label (`group_order') `k''"', "_over_`k'"));
                local row_names = `" `row_names' "`temp_lab'" "';
              };

              if(`sub_count' > 1)
              {;
                local temp_lab = "_sub_`j'" + cond(`over_count' == 1, "", "_over_`k'");
                local row_names = `"`row_names' `temp_lab'"';
              };
            };

            ** Wide **;
            if("`wide'" != "" & `k' == 1)
            {;
              local row_names = "`row_names' "
                              + cond(`sub_count' > 1, "_sub_`j'", "``i''");
            };

          /* `row_eqs' */

            ** Long **;
            if("`wide'" == "")
            {;
              local row_eqs = "`row_eqs' "
                            + cond(`=`sub_count'*`over_count'' > 1, "``i'':", "");
            };

            ** Wide **;
            if("`wide'" != "" & `k' == 1)
            {;
              local row_eqs = "`row_eqs' "
                            + cond(`sub_count' > 1, "``i'':", "");
            };

        };
      };
    };

  /* `p_names', `p_eqs', `p_format' */

    local p_names   = "";
    local p_eqs     = "";
    local p_format  = "";

    ** Long **;
    if("`wide'" == "" & "`pvals'" != "nopvals")
    {;
      forvalues i = 2/`over_count'
      {;
        local p_names = `" `p_names' "vs `i'" "';
        local p_eqs = "`p_eqs' P_Vals:";
        local p_format = "`p_format' & %5.4f";
      };
    };

    ** Wide **;
    if("`wide'" != "" & "`pvals'" != "nopvals")
    {;
      forvalues i = 1/`over_count'
      {;
        local j = `i' + 1;
        while `j' <= `over_count'
        {;
          local p_names = `" `p_names' "`i'-`j'" "';
          local p_eqs = "`p_eqs' P_Vals:";
          local p_format = "`p_format' & %5.4f";
          local ++j;
        };
      };
    };

  /* Trimming Information */

    local col_order  = trim(itrim(`"`col_order'"'));
    local col_names  = trim(itrim(`"`col_names'"'));
    local col_eqs    = trim(itrim(`"`col_eqs'"'));
    local col_format = trim(itrim(`"`col_format'"'));

    local p_names    = trim(itrim(`"`p_names'"'));
    local p_eqs      = trim(itrim(`"`p_eqs'"'));
    local p_format   = trim(itrim(`"`p_format'"'));

    local row_names  = trim(itrim(`"`row_names'"'));
    local row_eqs    = trim(itrim(`"`row_eqs'"'));
    local row_format = trim(itrim(`"`row_format'"'));

  /* Checking Display Length (first row) */

    foreach i in `row_names'
    {;
      if (length("`i'") >= `length')
      {;
        local length = length("`i'") + 2;
      };
    };

  /* Finalizing Information */

    ** Long **;
    if("`wide'" == "")
    {;
      local col_names  = trim(`" `col_names' `p_names' "');
      local col_eqs    = trim(`" `col_eqs' `p_eqs' "');
      local col_format = "& %-`length's " +
                         subinstr("`col_format'", "&", "|", 1) +
                         " " +
                         subinstr("`p_format'", "&", "|", 1) +
                         " &";
      local col_format = trim(itrim("`col_format'"));
    };

    ** Wide **;
    if("`wide'" != "")
    {;
      local col_names  = trim(itrim((`"`col_names'"' * `over_count')));
      local col_names  = trim(`" `col_names' `p_names' "');
      local col_eqs    = trim(`" `col_eqs' `p_eqs' "');
      local col_format = "& %-`length's `col_format' " +
                         subinstr("`p_format'", "&", "|", 1) +
                         " &";
      local col_format = trim(itrim("`col_format'"));
    };

*--------------------------------------------------------------*
*   08. Creating Results Matrix                                *
*--------------------------------------------------------------*;

  tempname results;

  /* Long */

    if("`wide'" == "")
    {;
      mata: select_long(`var_count', `sub_count', `over_count', `col_order');
      matrix `results' = r(results);

      matrix colnames `results' = `col_names';
      matrix rownames `results' = `row_names';
      matrix coleq    `results' = `col_eqs';
      matrix roweq    `results' = `row_eqs';
    };

  /* Wide */

    if("`wide'" != "")
    {;
      mata: select_wide(`var_count', `sub_count', `over_count', `col_order');
      matrix `results' = r(results);

      matrix colnames `results' = `col_names';
      matrix rownames `results' = `row_names';
      matrix coleq    `results' = `col_eqs';
      matrix roweq    `results' = `row_eqs';
    };

*--------------------------------------------------------------*
*   09. Displaying Results                                     *
*--------------------------------------------------------------*;

  /* Title */

    if("`title'" != "")
    {;
      di;
      di "{title:`title'}";
    };

  /* Group Labels */

    /* Length */

      local length = 1;

      if(length("_sub_`sub_count'") > `length')
      {;
        local length = length("_sub_`sub_count'");
      };

      if(length("_over_`over_count'") > `length')
      {;
        local length = length("_over_`over_count'");
      };

    /* Subpop */

      if(`sub_count' > 1)
      {;
        di;
        local length = length("_sub_`sub_count'");

        forvalues i = 1/`sub_count'
        {;
          di %-`length's "_sub_`i'" " | `subpop' = " `""`: label (`subpop') `: word `i' of `subpop_lvls'''""';
        };
      };

    /* Over */

      if(`over_count' > 1)
      {;
        di;
        local length = length("_over_`over_count'");

        forvalues i = 1/`over_count'
        {;
          di %-`length's "_over_`i'" " | " subinstr("`over_vars'",",",", ",.) " = " `""`: label (`group_order') `i''""';
        };

      };

  /* Listing Matrix */

    matlist `results', cspec(`col_format')
                       rspec(`row_format')
                       showcoleq(combined);

  /* Returning Results */

    return matrix results = `results';

  /* Resetting `svyset' */

    cap qui svyset `orig_svyset';

end;

