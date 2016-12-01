version 13.1
#delimit;
set linesize 255;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.1.6                                                **
**   Date:         12/01/2016                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

capture program drop bradmean;
program define bradmean, rclass;
syntax varlist(numeric) [if] [in], [SVY LEVEL(cilevel) OVER(varlist) WIDE];

  /* Creating Varlist Macros */

    local varlistlength : list sizeof varlist;
    tokenize `varlist';

    ** Note: Truncates the length of variable names for display purposes **;
    local length = 13;
    forvalues i = 1/`varlistlength'
    {;
      if(length("``i''")>=`length')
      {;
        local length = length("``i''") + 1;
      };
    };

  /* Setting 'Over' Options */

    local opt_over = "";
    if("`over'"!="")
    {;
      local over_cnt = wordcount("`over'");
      tokenize `over';

      ** Note: Creates numeric observations to be grouped **;
      local group_var = "";
      forvalues i = 1/`over_cnt'
      {;
        tempvar num_`i';
        cap encode ``i'', generate(`num_`i'');
        if(_rc!=0)
        {;
          clonevar `num_`i'' = ``i'';
        };
        local group_var = "`group_var'" + " `num_`i''";
      };

      ** Note: Creates the `over_var' to be used for over() **;
      tempvar over_var;
      egen `over_var' = group(`group_var'), label;

      ** Note: If there are more than 1 over variables, this formats value **;
      **       labels for the `over_var' that are easy to read             **;
      if(`over_cnt'>1)
      {;
        qui summ `over_var';
        local group_max = r(max);

        forvalues i = 1/`group_max'
        {;
          local new_lab = "";
          forvalues j = 1/`over_cnt'
          {;
            qui summ `num_`j'' if `over_var' == `i', meanonly;
            local val = r(min);
            local lab_`j' : label (`num_`j'') `val';

            if(`j'==1)
            {;
              local new_lab = "`lab_`j''";
            };
            else
            {;
              local new_lab = "`new_lab'" + ", " + "`lab_`j''";
            };
          };
          label define `over_var' `i' "`new_lab'", modify;
        };
      };

      local opt_over = " over(`over_var')";
    };

  /* Setting 'Survey' Options */

    local opt_svy = "";
    if("`svy'"!="")
    {;
      local opt_svy = "svy: ";
    };

  /* Finding Unique Values of 'Over' */

    if("`over'"!="")
    {;
      qui tempname freq code;
      qui tab `over_var' `if' `in', matcell(`freq') matrow(`code');
      local subpop_count = rowsof(`freq');

      forvalues i = 1/`subpop_count'
      {;
        local ci = `code'[`i',1];
        local subpop_label_`i' : label (`over_var') `ci';
      };
    };
    else
    {;
      local subpop_count = 1;
    };

    if(`subpop_count' == 1)
    {;
      local over = "";
    };

  /* Creating Values to be Returned */

    forvalues i = 1/`varlistlength'
    {;
      tokenize `varlist';

      /* Mean Results */

        qui `opt_svy' mean ``i'' `if' `in', level(`level') `opt_over';

        matrix results = r(table);
        matrix subpop = e(_N);

        local pop_`i' = e(N);
        local n_over = e(N_over);
        local n_over_names = e(over_namelist);
        local n_over_labels = e(over_labels);

      /* Testing */

        ** Note: If there are more than 1 subpops, an adjusted Wald test is run **;
        if(`n_over'>1 & `n_over'!=.)
        {;
          local test_var = "";

          ** Note: Creates the testing expression by running through the subpop count **;
          forvalues j = 1/`n_over'
          {;
            local over_name : word `j' of `n_over_names';
            if(`j'!=1)
            {;
              local test_var = "`test_var'" + " == ";
            };
            local test_var = "`test_var'" + "[``i'']`over_name'";
          };

          qui test `test_var';
          local pval_`i' = r(p);
        };

      /* Creating Results Matrix */

        ** Note: Set matrix to 6 rows for - b, se, ll, ul, pval, obs **;
        matrix output_`i' = J(7,`subpop_count',.);

        /* No Over */

          if("`over'"=="")
          {;
            matrix output_`i'[1,1] = results[1,1];
            matrix output_`i'[2,1] = results[2,1];
            matrix output_`i'[3,1] = results[5,1];
            matrix output_`i'[4,1] = results[6,1];
            matrix output_`i'[6,1] = subpop[1,1];
          };

        /* Over */

          if ("`over'"!="")
          {;

            ** Note: Places results where the n_over_label matches the subpop_label **;
            ** Note: Needed to account for a subpop missing from any variable       **;
            tokenize `"`n_over_labels'"';
            forvalues j = 1/`n_over'
            {;
              forvalues k = 1/`subpop_count'
              {;
                if("``j''"=="`subpop_label_`k''")
                {;
                  matrix output_`i'[1,`k'] = results[1,`j'];
                  matrix output_`i'[2,`k'] = results[2,`j'];
                  matrix output_`i'[3,`k'] = results[5,`j'];
                  matrix output_`i'[4,`k'] = results[6,`j'];
                  matrix output_`i'[5,`k'] = `pval_`i'';
                  matrix output_`i'[6,`k'] = subpop[1,`j'];
                };
              };
            };

          };
    };
    tokenize `varlist';

  /* Creating Sub-Population Labels */

    if("`over'"!="")
    {;
      local n_length = length("_subpop_`subpop_count'")+1;
      local header = "";
      di;
      di "SubPopulations - `over'";
      di;

      forvalues i = 1/`subpop_count'
      {;
        di "_subpop_`i'" _col(`n_length') " | `over' = " `"""' "`subpop_label_`i''" `"""';

        local temp_len = 10 - length("_subpop_`i'");
        local temp_lab = "| ";
        local temp_lab = "`temp_lab'" + " "*`temp_len';
        local temp_lab = "`temp_lab' _subpop_`i' ";
        local header = "`header'`temp_lab'";
      };

      if(`n_length'>`length' & "`wide'"=="")
      {;
        local length = `n_length';
      };
    };

  /* Output - No Over */

    if("`over'"=="")
    {;
      di;
      di _dup(`length') "-" "----------------------------------------------------------------------";
      di _dup(`length') " " "|        Mean |   Std. Err. | `level'% LowerCI | `level'% UpperCI |        Obs.";
      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------";

      forvalues i = 1/`varlistlength'
      {;
        local name = "``i''";
        di in gr %`=`length'-1's "`name'" " | " in ye %11.6f  output_`i'[1,1] " | " in ye %11.6f output_`i'[2,1] " | "
                                                in ye %11.6f  output_`i'[3,1] " | " in ye %11.6f output_`i'[4,1] " | "
                                                in ye %11.0fc output_`i'[6,1];
      };

      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------";
    };

  /* Output - Over, Long */

    if("`over'"!="" & "`wide'"=="")
    {;
      di;
      di _dup(`length') "-" "------------------------------------------------------------------------------------";
      di _dup(`length') " " "|        Mean |   Std. Err. | `level'% LowerCI | `level'% UpperCI |   P Value   | Subpop Obs.";
      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------+-------------";

      forvalues i = 1/`varlistlength'
      {;
        local name = "``i''";

        di in gr %-`=`length'-1's "`name'" " |             |             |             |             |             |";

        forvalues j = 1/`subpop_count'
        {;
          local poplabel = substr("_subpop_`j'",1,`=`length'-1');
          di in gr %`=`length'-1's "`poplabel'" " | " in ye %11.6f output_`i'[1,`j'] " | " in ye %11.6f  output_`i'[2,`j'] " | "
                                                      in ye %11.6f output_`i'[3,`j'] " | " in ye %11.6f  output_`i'[4,`j'] " | "
                                                      in ye %11.4f output_`i'[5,`j'] " | " in ye %11.0fc output_`i'[6,`j'];
        };

        di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------+-------------";
      };
    };

  /* Output - Over, Wide */

    if("`over'"!="" & "`wide'"!="")
    {;
      di;
      di _dup(`length') "-" _dup(`=`subpop_count'+2') "--------------";
      di _dup(`length') " " "`header'" "|   P Value   |        Obs.";
      di _dup(`length') "-" _dup(`=`subpop_count'+2') "+-------------";

      forvalues i = 1/`varlistlength'
      {;
        local name = "``i''";
        local dis_string = "";

        ** Note: Concatenates a string to display all the means and finally the pvalue **;
        forvalues j = 1/`subpop_count'
        {;
          local tmp_string : di %11.6f output_`i'[1,`j'];
          local dis_string = "`dis_string' | `tmp_string'";

          if(`j'==`subpop_count')
          {;
            local tmp_string : di %11.4f `pval_`i'';
            local dis_string = "`dis_string' | `tmp_string'";

            local tmp_string : di %11.0fc `pop_`i'';
            local dis_string = "`dis_string' | `tmp_string'";
          };
        };

        di in gr %-`=`length'-1's "`name'" "`dis_string'";
      };

      di _dup(`length') "-" _dup(`=`subpop_count'+2') "+-------------";
    };

end;
