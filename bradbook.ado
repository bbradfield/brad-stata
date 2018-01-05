
**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradbook.ado                                         **
**   Purpose:      Outputting a better formatted codebook               **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.0.1                                                **
**   Date:         01/05/2018                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

version 13.0
#delimit;

**======================================================================**
**   Routine: bradbook                                                  **
**======================================================================**;

  program define bradbook, rclass sortpreserve;
  syntax [varlist],
    [
      EXPort(string)
      REPLACE
    ];

  *------------------------------------------------------------*
  *   01. Defining Header Variables                            *
  *------------------------------------------------------------*;

    ** Date **;
    local date = trim(substr("`c(filedate)'", 1, strrpos("`c(filedate)'"," ")));

    ** VarTotal **;
    qui des;
    local vartotal = r(k) - 1;

    ** Size **;
    qui memory;
    if (`r(data_data_u)' >= (1000 * 1000))
    {;
      local size = `r(data_data_u)' / (1000 * 1000);
      local size : dis %-9.2f `size';
      local size = trim("`size'") + " GB";
    };
    else
    {;
      local size = `r(data_data_u)' / 1000;
      local size : dis %-9.0f `size';
      local size = trim("`size'") + " MB";
    };

  *------------------------------------------------------------*
  *   02. Creating Export File                                 *
  *------------------------------------------------------------*;

    local export_valid = 0;

    if("`export'" != "")
    {;
      cap qui log using "`export'.smcl", replace;
      if(_rc != 0)
      {;
        dis as err "ERROR! File cannot be created!" as text "";
      };
      else
      {;
        local export_valid = 1;
      };
    };

  *------------------------------------------------------------*
  *   03. Displaying Overall Header                            *
  *------------------------------------------------------------*;

      if(`: list sizeof varlist' == `vartotal')
      {;
        dis;
        dis "**" ("=" * 82) "**";
        dis "**    Title: " "`: data label'"                _col(85) "**";
        dis "**    Date:  " "`date'"                        _col(85) "**";
        dis "**    Obs:   " %-9.0fc _N                      _col(85) "**";
        dis "**    Vars:  " %-9.0fc `: list sizeof varlist' _col(85) "**";
        dis "**    Size:  " "`size'"                        _col(85) "**";
        dis "**" ("=" * 82) "**";
      };

  *------------------------------------------------------------*
  *   04. Displaying Individual Variables                      *
  *------------------------------------------------------------*;

    foreach var of varlist `varlist'
    {;
      local varlab = substr("`: variable label `var''", 1, 81);

      dis;
      dis "+" ("-" * 84) "+";
      dis "|  `var'"    _col(86) "|";
      dis "|  `varlab'" _col(86) "|";
      dis "+" ("-" * 84) "+";
      dis "|" _col(86) "|";

      if(strpos("`: type `var''", "str") == 1)
      {;
        varString `var';
      };
      else
      {;
        if("`: value label `var''" == "")
        {;
          varNumeric `var';
        };
        else
        {;
          varNumericLabel `var';
        };
      };

    };

  *------------------------------------------------------------*
  *   05. Closing Export File                                  *
  *------------------------------------------------------------*;

    if(`export_valid' == 1)
    {;
      qui log close;

      if("`replace'" == "")
      {;
        cap qui translate "`export'.smcl" "`export'.pdf";
        if(_rc!=0)
        {;
          di as err "ERROR! File already exists!" as text "";
        };
      };
      else
      {;
        cap qui translate "`export'.smcl" "`export'.pdf", replace;
      };

      qui erase "`export'.smcl";
    };

  end;

**======================================================================**
**   Subroutine: varString                                              **
**======================================================================**;

  program define varString, rclass;
  syntax varname;

  *------------------------------------------------------------*
  *   01. Getting Information                                  *
  *------------------------------------------------------------*;

    /* Unique */

      tempvar tag;
      qui bysort `varlist': generate `tag' = 1 if _n == 1;
      qui count if `tag' == 1 & !missing(`varlist');
      local unique = r(N);

      local unique : dis %9.0fc `unique';
      local unique = trim("`unique'");

    /* Missing */

      qui count if missing(`varlist');
      local missing = r(N);
      local misspct = (`missing'/_N) * 100;

      local missing : dis %9.0fc `missing';
      local missing = trim("`missing'");

      local misspct : dis %9.1f `misspct';
      local misspct = trim("`misspct'");

  *------------------------------------------------------------*
  *   02. Displaying Information                               *
  *------------------------------------------------------------*;

    dis "|     type:  string (`: type `varlist'')" _col(86) "|";
    dis "|   unique:  `unique'"                    _col(86) "|";
    dis "|  missing:  `missing' (`misspct'%)"      _col(86) "|";
    dis "|"                                        _col(86) "|";
    dis "+" ("-" * 84) "+";

  end;

**======================================================================**
**   Subroutine: varNumeric                                             **
**======================================================================**;

  program define varNumeric, rclass;
  syntax varname;

  *------------------------------------------------------------*
  *   01. Getting Information                                  *
  *------------------------------------------------------------*;

    /* Unique */

      tempvar tag;
      qui bysort `varlist': generate `tag' = 1 if _n == 1;
      qui count if `tag' == 1 & !missing(`varlist');
      local unique = r(N);

      local unique : dis %9.0fc `unique';
      local unique = trim("`unique'");

    /* Missing */

      qui count if missing(`varlist');
      local missing = r(N);
      local misspct = (`missing'/_N) * 100;

      local missing : dis %9.0fc `missing';
      local missing = trim("`missing'");

      local misspct : dis %9.1f `misspct';
      local misspct = trim("`misspct'");

    /* Summary Statistics */

      qui summ `var';

      local min : dis %9.2f r(min);
      local min = trim("`min'");

      local max : dis %9.2f r(max);
      local max = trim("`max'");

      local mean : dis %9.2f r(mean);
      local mean = trim("`mean'");

      local sdev : dis %9.2f r(sd);
      local sdev = trim("`sdev'");

  *------------------------------------------------------------*
  *   02. Displaying Information                               *
  *------------------------------------------------------------*;

    dis "|     type:  `: type `varlist''"     _col(86) "|";
    dis "|   unique:  `unique'"               _col(86) "|";
    dis "|  missing:  `missing' (`misspct'%)" _col(86) "|";
    dis "|"                                   _col(86) "|";
    dis "|    range:  [`min',`max']"          _col(86) "|";
    dis "|"                                   _col(86) "|";
    dis "|     mean:  `mean'"                 _col(86) "|";
    dis "|     sdev:  `sdev'"                 _col(86) "|";
    dis "|"                                   _col(86) "|";
    dis "+" ("-" * 84) "+";

  end;

**======================================================================**
**   Subroutine: varNumericLabel                                        **
**======================================================================**;

  program define varNumericLabel, rclass;
  syntax varname;

  *------------------------------------------------------------*
  *   01. Getting Information                                  *
  *------------------------------------------------------------*;

    /* Unique */

      tempvar tag;
      qui bysort `varlist': generate `tag' = 1 if _n == 1;
      qui count if `tag' == 1 & !missing(`varlist');
      local unique = r(N);

      local unique : dis %9.0fc `unique';
      local unique = trim("`unique'");

    /* Missing */

      qui count if missing(`varlist');
      local missing = r(N);
      local misspct = (`missing'/_N) * 100;

      local missing : dis %9.0fc `missing';
      local missing = trim("`missing'");

      local misspct : dis %9.1f `misspct';
      local misspct = trim("`misspct'");

  *------------------------------------------------------------*
  *   02. Displaying Information                               *
  *------------------------------------------------------------*;

    dis "|     type:  `: type `varlist''"     _col(86) "|";
    dis "|   unique:  `unique'"               _col(86) "|";
    dis "|  missing:  `missing' (`misspct'%)" _col(86) "|";
    dis "|"                                   _col(86) "|";

  *------------------------------------------------------------*
  *   03. Displaying Table                                     *
  *------------------------------------------------------------*;

    tempname freqs;
    qui levelsof `varlist', local(lvls) matcell(`freqs') matrow(`freqs');

    if("`lvls'" != "")
    {;
      /* Getting Label Length */

        local lab_len = 15;
        foreach i of local lvls
        {;
          local lab_len = max(`lab_len', length("`i' - `: label (`varlist') `i''"));
        };
        local lab_len = cond(`lab_len' > 62, 62, `lab_len');

        local col1 = `lab_len' + 4;
        local col2 = `lab_len' + 1;

      /* Displaying Table Header */

        dis "|  " in gr _col(`col1')     "   code  |   freq"  _col(86) in ye "|";
        dis "|  " in gr _dup(`col2') "-"  "--------+--------" _col(86) in ye "|";

      /* Displaying Lines for Labels */

        foreach i of local lvls
        {;
          qui count if `varlist' == `i';
          local freq = r(N);
          local vallab = substr("`i' - `: label (`varlist') `i''", 1, `lab_len');
          local colpos = `lab_len' + 11 - length("`vallab'");

          dis "|  " in gr _col(`colpos') "`vallab'  |" %7.0f `freq' _col(86) in ye "|";
        };

      /* Displaying Missing */

        dis "|  " in gr _dup(`col2') "-"  "--------+--------" _col(86) in ye "|";

        qui count if missing(`varlist');
        local freq = r(N);
        local vallab = "<missing>";
        local colpos = `lab_len' + 11 - length("`vallab'");

        dis "|  " in gr _col(`colpos') "`vallab'  |" %7.0f `freq' _col(86) in ye "|";

      /* Displaying Total */

        dis "|  " in gr _dup(`col2') "-"  "--------+--------" _col(86) in ye "|";

        local freq = _N;
        local vallab = "Total";
        local colpos = `lab_len' + 11 - length("`vallab'");

        dis "|  " in gr _col(`colpos') "`vallab'  |" %7.0f `freq' _col(86) in ye "|";
        dis "|"                                             _col(86) "|";
    };

  *------------------------------------------------------------*
  *   04. Displaying Footer                                    *
  *------------------------------------------------------------*;

    dis "+" ("-" * 84) "+";

  end;