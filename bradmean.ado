version 13.1
#delimit;
set linesize 255;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Running multiple means in a single function          **
**   Programmers:  Brian Bradfield                                      **
**   Version:      2.1.1                                                **
**   Date:         03/14/2016                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

capture program drop bradmean;
program define bradmean, rclass;
syntax varlist(numeric), [SVY OVER(varlist)];

  /* Creating VarList Macros */

    local varlistlength : list sizeof varlist;
    tokenize `varlist';

    local length = 13;

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

  /* Creating Values to be Returned */

    forvalues i = 1/`varlistlength'
    {;
      qui count if !missing(``i'');
      local obs_`i' = r(N);

      qui `opt_svy' mean ``i'' `opt_over';

      matrix results_`i' = r(table);
      matrix subpop_`i' = e(_N);

      local n_over = e(N_over);
      local n_over_names = e(over_namelist);
      local n_over_labels = e(over_labels);
      local n_length = length("_subpop_`n_over'")+1;

      if("`over'"!="")
      {;
        local test_var = "";
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
    };

  /* Outputting Sub-Population Labels */

    if("`over'"!="")
    {;
      local overlist = subinstr("`over'"," ",",",.);
      di;
      di "SubPopulations - `overlist'";
      forvalues j = 1/`n_over'
      {;
        local poplabel : word `j' of `n_over_labels';
        di "_subpop_`j'" _col(`n_length') " | " `""`poplabel'""';
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
      if(length("``i''")>12)
      {;
        local name = substr("``i''",1,12);
      };

      if("`over'"=="")
      {;
        di in gr %-12s "`name'" " | " in ye %11.6g results_`i'[1,1] " | " in ye %11.6g results_`i'[2,1] " | "
                                      in ye %11.6g results_`i'[5,1] " | " in ye %11.6g results_`i'[6,1] " | "
                                      in ye %11.0fc `obs_`i'';
      };
      else
      {;
        forvalues j = 1/`n_over'
        {;
          local poplabel = substr("_subpop_`j'",1,12);
          if(`j'==1)
          {;
            di in gr %-12s "`name'" " |             |             |             |             |             |";
          };
          di in gr %12s "`poplabel'" " | " in ye %11.6g results_`i'[1,`j'] " | " in ye %11.6g results_`i'[2,`j'] " | "
                                           in ye %11.6g results_`i'[5,`j'] " | " in ye %11.6g results_`i'[6,`j'] " | "
                                           in ye %11.4f `pval_`i''         " | " in ye %11.0fc subpop_`i'[1,`j'];
        };
      };

      if("`over'"!="")
      {;
        di "-------------+-------------+-------------+-------------+-------------+-------------+-------------";
      };
    };

  /* Outputting Footer */

    if("`over'"=="")
    {;
      di "-------------+-------------+-------------+-------------+-------------+-------------";
    };

end;

