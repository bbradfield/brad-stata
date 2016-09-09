#delimit;
set linesize 255;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradbook.ado                                         **
**   Purpose:      Outputting a better formatted codebook               **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.4.2                                                **
**   Date:         07/29/2016                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

capture program drop bradbook;
program define bradbook, rclass;
syntax [varlist];

  *------------------------------------------------------------*
  *   01. Counting Number of Variables                         *
  *------------------------------------------------------------*;

    local varlistlength : list sizeof varlist;
    tokenize `varlist';

  *------------------------------------------------------------*
  *   02. Defining Header Variables                            *
  *------------------------------------------------------------*;

    **  Title - Title of dataset  **;

      local title : data label;

    **  Date - Date of dataset  **;

      local date = trim(substr("`c(filedate)'",1,strrpos("`c(filedate)'"," ")));

    **  ObsCount - Number of observations  **;

      local obscount : dis %-9.0fc _N;

    **  VarTotal - Numeric count of variables  **;
    **  VarCount - Display count of variables  **;

      qui des _all;
      local vartotal = r(k);
      local varcount : dis %-9.0fc r(k);

    **  Size - Size of dataset  **;

      qui memory;
      if (`r(data_data_u)'>=1000000)
      {;
        local size = `r(data_data_u)'/1000000;
        local size : dis %-9.2f `size';
        local size = trim("`size'") + " GB";
      };
      else
      {;
        local size = `r(data_data_u)'/1000;
        local size : dis %-9.0f `size';
        local size = trim("`size'") + " MB";
      };

  *------------------------------------------------------------*
  *   03. Creating Original Sort Order                         *
  *------------------------------------------------------------*;

    tempvar sortorder;
    qui generate `sortorder' = _n;

  *------------------------------------------------------------*
  *   04. Displaying Header                                    *
  *------------------------------------------------------------*;

    if(`varlistlength'==`vartotal')
    {;
      dis;
      dis "**==================================================================================**";
      dis "**    Title: `title'"    _col(85) "**";
      dis "**    Date:  `date'"     _col(85) "**";
      dis "**    Obs:   `obscount'" _col(85) "**";
      dis "**    Vars:  `varcount'" _col(85) "**";
      dis "**    Size:  `size'"     _col(85) "**";
      dis "**==================================================================================**";
    };

  *------------------------------------------------------------*
  *   05. Displaying Individual Variables                      *
  *------------------------------------------------------------*;

    foreach var of varlist `varlist'
    {;
      *--------------------------------------------------------*
      *   A. Defining Individual Variables                     *
      *--------------------------------------------------------*;

        **  VarType - Variable type  **;

          local vartype : type `var';

        **  ValLab - Value label name  **;

          local vallab : value label `var';

        **  Fmt - Variable format  **;

          local fmt : format `var';

        **  Uniq - Number of unique values  **;

          tempvar tag;
          qui bysort `var': generate `tag' = 1 if _n == 1;
          qui count if `tag' == 1 & !missing(`var');
          local uniq = r(N);

        **  Miss    - Number of observations with missing values   **;
        **  MissPct - Percent of observations with missing values  **;

          qui count if missing(`var');
          local miss = r(N);
          local misspct = (`miss'/_N)*100;

        **  Min  - Minimum value (only for numeric examples without label)  **;
        **  Mean - Mean value (only for numeric examples without label)     **;
        **  Max  - Maximum value (only for numeric examples without label)  **;

          if(inlist("`vartype'","byte","int","long","double"))
          {;
            qui summ `var', detail;
            local min = r(min);
            local max = r(max);
            local mean = r(mean);
            local sdev = r(sd);
          };

      *--------------------------------------------------------*
      *   B. Setting Display Formats                           *
      *--------------------------------------------------------*;

        **  All_Miss - No unique values of variable  **;

          local all_miss = `uniq' == 0;

        **  Uniq - Number of unique values  **;

          local uniq : dis %-9.0fc `uniq';
          local uniq = trim("`uniq'");

        **  Miss - Number of observations with missing values  **;

          local miss : dis %-9.0fc `miss';
          local miss = trim("`miss'");

        **  MissPct - Percent of observations with missing values  **;

          local misspct : dis %-9.1f `misspct';
          local misspct = trim("`misspct'") + "%";

        **  Min  - Minimum value (only for numeric examples without label)  **;
        **  Mean - Mean value (only for numeric examples without label)     **;
        **  Max  - Maximum value (only for numeric examples without label)  **;

          if(inlist("`vartype'","byte","int","long","double"))
          {;
            if(strpos("`fmt'","%t")==0)
            {;
              local min : dis %-9.2f `min';
              local min = trim("`min'");

              local max : dis %-9.2f `max';
              local max = trim("`max'");

              local mean : dis %-9.2f `mean';
              local mean = trim("`mean'");

              local sdev : dis %-9.2f `sdev';
              local sdev = trim("`sdev'");
            };
            else
            {;
              local min : dis %td `min';
              local min = trim("`min'");

              local max : dis %td `max';
              local max = trim("`max'");

              local mean : dis %td `mean';
              local mean = trim("`mean'");

              local sdev : dis %-9.2f `sdev';
              local sdev = trim("`sdev'");
            };
          };

      *--------------------------------------------------------*
      *   C. Displaying Variable Header                        *
      *--------------------------------------------------------*;

        dis;
        dis "+------------------------------------------------------------------------------------+";
        dis "|  " "`var'"                      _col(86) "|";
        dis "|  " `"`: variable label `var''"' _col(86) "|";
        dis "+------------------------------------------------------------------------------------+";
        dis "|" _col(86) "|";

      *--------------------------------------------------------*
      *   D1. Displaying Info for String                       *
      *--------------------------------------------------------*;

        if(strpos("`vartype'","str")==1)
        {;
          dis "|     type: string (`vartype')" _col(86) "|";
          dis "|   unique: `uniq'"             _col(86) "|";
          dis "|  missing: `miss' (`misspct')" _col(86) "|";
          dis "|"                              _col(86) "|";
        };

      *--------------------------------------------------------*
      *   D2. Displaying Info for Numeric without Label        *
      *--------------------------------------------------------*;

        if(strpos("`vartype'","str")!=1 & "`vallab'" == "")
        {;
          dis "|     type:  `vartype'"          _col(86) "|";
          dis "|   unique:  `uniq'"             _col(86) "|";
          dis "|  missing:  `miss' (`misspct')" _col(86) "|";
          dis "|"                               _col(86) "|";
          dis "|    range:  [`min',`max']"      _col(86) "|";
          dis "|"                               _col(86) "|";
          dis "|     mean:  `mean'"             _col(86) "|";
          dis "|     sdev:  `sdev'"             _col(86) "|";
          dis "|"                               _col(86) "|";
        };

      *--------------------------------------------------------*
      *   D3. Displaying Info for Numeric with Label           *
      *--------------------------------------------------------*;

        if(strpos("`vartype'","str")!=1 & "`vallab'" != "")
        {;
          dis "|     type:  `vartype'"          _col(86) "|";
          dis "|   unique:  `uniq'"             _col(86) "|";
          dis "|  missing:  `miss' (`misspct')" _col(86) "|";
          dis "|"                               _col(86) "|";

          /* Running Tabl */

          if(`all_miss'==0)
          {;
            **  Freq - Frequency of value  **;
            **  Code - Value               **;

              tempname freq code;
              qui tab `var', matcell(`freq') matrow(`code');

            **  Len - Max length of value label  **;
            **  CI  - Code of value `i'          **;
            **  LI  - Value label of value `i'   **;

              local len = 15;
              local i = 1;

              while `i' <= rowsof(`freq')
              {;
                local ci = `code'[`i',1];
                local li : label (`var') `ci';
                local len = max(`len',length(`"`li'"'));
                local i = `i' + 1;
              };

              if(`len'>62)
              {;
                local len = 62;
              };

            **  Display header for Tabl  **;

              local col1 = `len'+4;
              local col2 = `len'+1;

              dis "|  " in gr _col(`col1')     "   code  |   freq"  _col(86) "|";
              dis "|  " in gr _dup(`col2') "-"  "--------+--------" _col(86) "|";

            **  Displaying unique line for each unique value of variable  **;

              local i = 1;
              while `i' <= rowsof(`freq')
              {;
                local ci = `code'[`i',1];
                local li : label (`var') `ci';
                local li = substr("`li'",1,62);
                dis "|  " in gr %`len's `"`li'"' _col(`col1') %6.0f `ci' "   |" in ye %7.0f `freq'[`i',1] _col(86) "|";
                local i = `i' + 1;
              };

            **  Displaying missing value line for variable  **;

              qui count if (`var'==.);
              if r(N) > 0
              {;
                dis "|  " in gr _dup(`col2') "-" "--------+--------" _col(86) "|";
                dis "|  " in gr %`len's "<missing value>" _col(`col1') "     .   |" in ye %7.0f r(N) _col(86) "|";
              };

            **  Displaying total observations line for variable  **;
            **  Displaying footer for Tabl                       **;

              dis "|  " in gr _dup(`col2') "-" "--------+--------" _col(86) "|";
              dis "|  " in gr _col(`col1') " Total   |" in ye %7.0f = `=_N' _col(86) "|";
              dis "|" _col(86) "|";
          };
        };

      *--------------------------------------------------------*
      *   E. Displaying Footer                                 *
      *--------------------------------------------------------*;

        dis "+------------------------------------------------------------------------------------+";
    };

end;
