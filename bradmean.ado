**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.3.0                                                **
**   Date:         10/30/2017                                           **
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
            endrow = 8 + overcount
            current = current,original[(9::endrow),(2::overcount)]
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
              startrow = 9 + k
              endrow = 8 + overcount
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
                                    DISopt(string) /* display stats & order */
                                    WIDE /* wide format */
                                    TITLE(string) /* displays title above table */
                                   ];

*--------------------------------------------------------------*
*   01. Preparing Varlist                                      *
*--------------------------------------------------------------*;

  local var_count : list sizeof varlist;
  tokenize `varlist';

  ** Note: Gets the length of the longest varname for display purposes **;
  local length = 13;
  forvalues i = 1/`var_count'
  {;
    if(length("``i''") >= `length')
    {;
      local length = length("``i''") + 1;
    };
  };

*--------------------------------------------------------------*
*   02. Checking Error Codes                                   *
*--------------------------------------------------------------*;

  /* Setting `svy' if `subpop' has values */

    if("`subpop'" != "")
    {;
      local svy = "svy";
    };

  /* `svy' - data not set up for svy, use svyset */

    if("`svy'" != "" | "`subpop'" != "")
    {;
      cap qui svydescribe;

      if(_rc != 0)
      {;
        di as err "data not set up for svy, use svyset" as text "";
        error 119;
      };
    };

*--------------------------------------------------------------*
*   03. Setting Option Macros                                  *
*--------------------------------------------------------------*;

  /* Setting `wide' */

    if("`over'" == "")
    {;
      local wide = "";
    };

  /* Setting `pct' */

    local opt_pct = 1;

    if("`pct'" != "")
    {;
      local opt_pct = 100;
    };

  /* Setting `svy' */

    local opt_svy_1 = "";

    if("`svy'" != "")
    {;
      local opt_svy_1 = "svy: ";
      qui svyset;
      local svy_var = r(wvar);
    };

  /* Setting `round' */

    local opt_round = `round';

    if(!inrange(`round',0,7))
    {;
      local opt_round = 7;
    };

    local opt_round = 1/(10^`opt_round');

*--------------------------------------------------------------*
*   04a. Finding Unique Values of `subpop'                     *
*--------------------------------------------------------------*;

  local sub_count = 1;

  if("`subpop'" != "")
  {;
    qui levelsof `subpop' if !missing(`svy_var'), local(subpop_lvls);

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

  if("`over'" != "")
  {;
    local over_vars = subinstr("`over'"," ",",",.);

    /* Creating ordered `group_order' */

      qui tempvar  group_order;
      qui tempvar  group_var;
      qui egen    `group_var' = concat(`over'), p(", ");
      qui replace `group_var' = "" if missing(`over_vars');
      if("`svy'" != "")
      {;
        qui replace `group_var' = "" if missing(`svy_var');
      };
      qui encode `group_var', generate(`group_order');
      qui drop `group_var';

    /* Creating `over_count' */

      qui levelsof `group_order', local(over_lvls);
      qui local over_count : word count `over_lvls';

    /* Setting opt_over */

      local opt_over = "over(`group_order', nolabel)";

    /* If `over_count' == 0, set to 1 */

      if(`over_count' == 0)
      {;
        local over_count = 1;
      };

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

      local tempdisopt = "";

      forvalues i = 1/`: word count `disopt''
      {;
        if(inlist(word("`disopt'",`i'),"obs","mean","se","sd","lci","uci"))
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
  tempname pvalues;

  forvalues i = 1/`var_count'
  {;
    forvalues j = 1/`sub_count'
    {;
      /* Initializing Matrix */

        tempname output_`i'_`j';
        matrix `output_`i'_`j'' = J(8,`over_count',.);
        matrix rownames `output_`i'_`j'' = b se sd lci uci totobs subobs grpobs;

      /* Mean `returns' */

        cap qui `opt_svy_`j'' mean ``i'' `if' `in', level(`level') `opt_over';

        matrix `returns'    = r(table);
        local  totobs       = e(N);
        local  subobs       = e(N_sub);
        matrix `grpobs'     = e(_N);
        local  n_over       = e(N_over);
        local  n_over_names = e(over_namelist);

        cap qui estat sd;

        matrix `sd' = r(sd);

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
          matrix `output_`i'_`j''[1,1] = round(`returns'[1,1]*`opt_pct',`opt_round');
          matrix `output_`i'_`j''[2,1] = round(`returns'[2,1]*`opt_pct',`opt_round');
          matrix `output_`i'_`j''[3,1] = round(`sd'[1,1]     *`opt_pct',`opt_round');
          matrix `output_`i'_`j''[4,1] = round(`returns'[5,1]*`opt_pct',`opt_round');
          matrix `output_`i'_`j''[5,1] = round(`returns'[6,1]*`opt_pct',`opt_round');
          matrix `output_`i'_`j''[6,1] = `totobs';
          matrix `output_`i'_`j''[7,1] = `subobs';
        };
        else
        {;
          local x = 1;
          foreach col in `n_over_names'
          {;
            matrix `output_`i'_`j''[1,`col'] = round(`returns'[1,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[2,`col'] = round(`returns'[2,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[3,`col'] = round(`sd'[1,`x']     *`opt_pct',`opt_round');
            matrix `output_`i'_`j''[4,`col'] = round(`returns'[5,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[5,`col'] = round(`returns'[6,`x']*`opt_pct',`opt_round');
            matrix `output_`i'_`j''[6,`col'] = `totobs';
            matrix `output_`i'_`j''[7,`col'] = `subobs';
            matrix `output_`i'_`j''[8,`col'] = `grpobs'[1,`x'];

            local ++x;
          };
        };

    };
  };

*--------------------------------------------------------------*
*   07. Getting Information to Format Table                    *
*--------------------------------------------------------------*;

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
            local col_order = "`col_order',8";
            local col_format = "`col_format' & %9.0fc";
            continue;
          };
          if("`subpop'" != "")
          {;
            local col_order = "`col_order',7";
            local col_format = "`col_format' & %9.0fc";
            continue;
          };
          local col_order = "`col_order',6";
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

      /* lci */

        if("`i'" == "lci")
        {;
          local col_order = "`col_order',4";
          local col_names = `" `col_names' "Lower CI" "';
          local col_format = "`col_format' & %9.0g";
        };

      /* uci */

        if("`i'" == "uci")
        {;
          local col_order = "`col_order',5";
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
            if("`wide'" == "" & "`col_eqs'" == "")
            {;
              local col_eqs = trim(`stat_count' * "_: ");
            };

            ** Wide **;
            if("`wide'" != "" & `i' == 1 & `j' == 1)
            {;
              local col_eqs = "`col_eqs' " + (`stat_count' * "_over_`k' ");
            };

          /* `row_names' */

            ** Long **;
            if("`wide'" == "")
            {;
              local row_names = "`row_names' "
                              + cond(`=`sub_count'*`over_count'' == 1, "``i''", "")
                              + cond(`sub_count' > 1, "_sub_`j'", "")
                              + cond(`over_count' > 1, "_over_`k'","");
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
          di %-`length's "_sub_`i'" " | `subpop' = " `""`: label (`subpop') `i''""';
        };
      };

    /* Over */

      if(`over_count' > 1)
      {;
        di;
        local length = length("_over_`over_count'");

        forvalues i = 1/`over_count'
        {;
          local output_string = "_over_`i' |";
          local current_label = "`: label (`group_order') `i''";

          forvalues j = 1/`: word count `over''
          {;
            local current_over = "`: word `j' of `over''";
            local current_value = real(subinstr("`: word `j' of `current_label''",",","",.));
            local current_value : label (`current_over') `current_value';
            local output_string = `" `output_string' `current_over' = "`current_value'", "';
          };

          local output_string = trim(`"`output_string'"');
          di substr(`"`output_string'"',1,length(`"`output_string'"')-1);
        };

      };

  /* Listing Matrix */

    matlist `results', cspec(`col_format')
                       rspec(`row_format')
                       showcoleq(combined);

  /* Returning Results */

    return matrix results = `results';

end;

