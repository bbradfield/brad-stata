version 15.1
#delimit;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      biggertab.ado                                        **
**   Purpose:      Displays cross-tabs of variables and allows more     **
**                 than 3 variables                                     **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.0.0                                                **
**   Date:         07/25/2025                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

  program define biggertab, nclass;
  syntax varlist [if] [in],
    [
      SEPBY(varlist)
      TITLE(string)
      DIVider
      NOLabel
      ZERO
      NOMISS
    ];

    quietly
    {;

    /* Preserving Data */

      preserve;

    /* Generating Frequency & Percent */

      contract `varlist' `if' `in', freq(Freq) percent(Pct) `zero' `nomiss';

    /* Displaying Title */

      if("`title'" != "")
      {;
        noi dis _newline(1) "{title:`title'}";
      };

    /* Displaying Table */

      if("`sepby'" == "")
      {;
        noi list `varlist' Freq Pct, sep(9999) noobs abb(32) `divider' `nolabel';
      };
      else
      {;
        noi list `varlist' Freq Pct, sepby(`sepby') noobs abb(32) `divider' `nolabel';
      };

    /* Restoring Data */

      restore;

    };

  end;

