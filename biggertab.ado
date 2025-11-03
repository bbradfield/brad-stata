version 15.1
#delimit;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      biggertab.ado                                        **
**   Purpose:      Displays cross-tabs of variables and allows more     **
**                 than 3 variables                                     **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.0.1                                                **
**   Date:         11/03/2025                                           **
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
      STATs(string)
    ];

    quietly
    {;

    /* Preserving Data */

      preserve;

    /* Getting Statistics */

      local stats = strtrim(strlower("`stats'"));

      if("`stats'" == "none")
      {;
        local stats = "";
      };
      else
      {;
        local full_stats freq pct cumfreq cumpct rowfreq rowpct;
        local stats      `stats';

        local stats : list full_stats & stats;

        local stats = cond("`stats'" == "", "freq pct", "`stats'");
        local stats = subinstr(subinstr(subinstr(subinstr("`stats'", "freq", "Freq", .), "pct", "Pct", .), "cum", "Cum", .), "row", "Row", .);
      };

    /* Generating Frequency & Percent */

      contract `varlist' `if' `in', freq(Freq) percent(Pct) cfreq(CumFreq) cpercent(CumPct) `zero' `nomiss';

    /* Generating Row Percent */

      if("`sepby'" != "")
      {;
        if(strpos("`stats'", "row") != 0)
        {;
          bysort `sepby' (CumFreq): generate RowFreq = CumFreq - CumFreq[1] + Freq[1];

          by `sepby': generate RowPct = (Freq / RowFreq[_N]) * 100;

          format RowPct %8.2f;
        };

        local sepby = "sepby(`sepby')";
      };
      else
      {;
        local sepby = "sep(9999)";
      };

    /* Displaying Title */

      if("`title'" != "")
      {;
        noi dis _newline(1) "{title:`title'}";
      };

    /* Displaying Table */

      noi list `varlist' `stats', `sepby' noobs abb(32) `divider' `nolabel';

    /* Restoring Data */

      restore;

    };

  end;
