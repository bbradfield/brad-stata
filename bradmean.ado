version 13.1
#delimit;
set linesize 255;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      2.7.2                                                **
**   Date:         03/25/2016                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

capture program drop bradmean;
program define bradmean, rclass;
syntax varlist(numeric), [SVY OVER(varname numeric)];

  /* Creating Varlist Macros */

    local varlistlength : list sizeof varlist;
    tokenize `varlist';

    local length = 13;
    forvalues i = 1/`varlistlength'
    {;
      if(length("``i''")>13)
      {;
        local length = length("``i''") + 1;
      };
    };

  /* Setting Options */

    local opt_over = "";
    if("`over'"!="")
    {;
      local opt_over = ", over(`over')";
    };

    local opt_svy = "";
    if("`svy'"!="")
    {;
      local opt_svy = "svy: ";
    };

  /* Finding Unique Values of Over */

    if("`over'"!="")
    {;
      qui tempname freq code;
      qui tab `over', matcell(`freq') matrow(`code');
      local subpop_count = rowsof(`freq');

      forvalues i = 1/`subpop_count'
      {;
        local ci = `code'[`i',1];
        local subpop_label_`i' : label (`over') `ci';
      };
    };

  /* Creating Values to be Returned */

    forvalues i = 1/`varlistlength'
    {;
      /* Observations */
      qui count if !missing(``i'');
      local obs_`i' = r(N);

      /* Mean Results */
      qui `opt_svy' mean ``i'' `opt_over';

      matrix results_`i' = r(table);
      matrix subpop_`i' = e(_N);

      local n_over_`i' = e(N_over);
      local n_over_names_`i' = e(over_namelist);
      local n_over_labels_`i' = e(over_labels);

      /* Testing */
      local pval_`i' = .;
      if(`n_over_`i''>1 & `n_over_`i''!=.)
      {;
        local test_var = "";
        forvalues j = 1/`n_over_`i''
        {;
          local over_name : word `j' of `n_over_names_`i'';
          if(`j'!=1)
          {;
            local test_var = "`test_var'" + " == ";
          };
          local test_var = "`test_var'" + "[``i'']`over_name'";
        };
        qui test `test_var';
        local pval_`i' = r(p);
      };
    };

  /* Outputting Sub-Population Labels */

    if("`over'"!="")
    {;
      local n_length = length("_subpop_`subpop_count'")+1;
      di;
      di "SubPopulations - `over'";
      di;
      forvalues i = 1/`subpop_count'
      {;
        di "_subpop_`i'" _col(`n_length') " | `over' = " `"""' "`subpop_label_`i''" `"""';
      };

      if(`n_length'>`length')
      {;
        local length = `n_length';
      };
    };

  /* Outputting Header */

    di;
    if("`over'"=="")
    {;
      di _dup(`length') "-" "----------------------------------------------------------------------";
      di _dup(`length') " " "|        Mean |   Std. Err. | 95% LowerCI | 95% UpperCI |        Obs.";
      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------";
    };
    else
    {;
      di _dup(`length') "-" "------------------------------------------------------------------------------------";
      di _dup(`length') " " "|        Mean |   Std. Err. | 95% LowerCI | 95% UpperCI |   P Value   |        Obs.";
      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------+-------------";
    };

  /* Outputting Results */

    forvalues i = 1/`varlistlength'
    {;
      local name = "``i''";

      if("`over'"=="")
      {;
        di in gr %`=`length'-1's "`name'" " | " in ye %11.6f results_`i'[1,1] " | " in ye %11.6f results_`i'[2,1] " | "
                                                in ye %11.6f results_`i'[5,1] " | " in ye %11.6f results_`i'[6,1] " | "
                                                in ye %11.0fc `obs_`i'';
      };
      else
      {;
        di in gr %-`=`length'-1's "`name'" " |             |             |             |             |             |";

        local pop = 1;
        forvalues j = 1/`subpop_count'
        {;
          local poplabel = substr("_subpop_`j'",1,`=`length'-1');
          local templabel : word `pop' of `n_over_labels_`i'';
          if("`templabel'"=="`subpop_label_`j''")
          {;
            di in gr %`=`length'-1's "`poplabel'" " | " in ye %11.6f results_`i'[1,`pop'] " | " in ye %11.6f results_`i'[2,`pop'] " | "
                                                        in ye %11.6f results_`i'[5,`pop'] " | " in ye %11.6f results_`i'[6,`pop'] " | "
                                                        in ye %11.4f `pval_`i''           " | " in ye %11.0fc subpop_`i'[1,`pop'];
            local pop = `pop' + 1;
          };
          else
          {;
            local fakeval = .;
            local fakepop = 0;
            di in gr %`=`length'-1's "`poplabel'" " | " in ye %11.6f `fakeval' " | " in ye %11.6f `fakeval'  " | "
                                                        in ye %11.6f `fakeval' " | " in ye %11.6f `fakeval'  " | "
                                                        in ye %11.4f `fakeval' " | " in ye %11.0fc `fakepop';
          };
        };

        di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------+-------------";
      };
    };

  /* Outputting Footer */

    if("`over'"=="")
    {;
      di _dup(`length') "-" "+-------------+-------------+-------------+-------------+-------------";
    };



end;

