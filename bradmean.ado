version 14.0
#delimit;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Mata implementation of Bradmean                      **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.4.2                                                **
**   Date:         06/25/2018                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

**======================================================================**
**   Stata Functions                                                    **
**======================================================================**;

  program define bradmean, rclass sortpreserve byable(recall);
  syntax varlist [if] [in],
    [
      SVY
      SUBpop(varname numeric)
      OVER(varlist)
      OVEROPT(string)
      PVALues(string)
      CI(string)
      STats(string)
      DISplay(string)
      EXcel(string)
    ];

  *----------------------------------------------------------*
  *   01. Setting Parameters & Options (1)                   *
  *----------------------------------------------------------*;

    mata: bd = newBraddev();

  *----------------------------------------------------------*
  *   02. Marking Sample                                     *
  *----------------------------------------------------------*;

    tempname touse;

    mark `touse' `if' `in';

    mata: st_numscalar("r(survey)", bd.opt.survey);
    if(r(survey) == 1) {; svymarkout `touse'; };

    mata: st_numscalar("r(miss)", bd.opt.ovo_miss);
    if(r(miss) == 0)
    {;
      tempvar all_missing;
      qui egen `all_missing' = rowtotal(`varlist'), m;
      markout `touse' `all_missing';
    };

    if(_by())
    {;
      quietly replace `touse' = 0 if `_byindex' != _byindex();
    };

  *----------------------------------------------------------*
  *   03. Creating Over Variable                             *
  *----------------------------------------------------------*;

    tempname freqs;

    if("`over'" != "")
    {;
      /* Creating `group_string' & `group_numeric' */

        tempvar group_string;
        qui egen strL `group_string' = concat(`over') if `touse', decode maxlength(31990) p(", ");

        tempvar group_numeric;
        qui egen `group_numeric' = group(`over') if `touse';

      /* Creating `over_count' */

        qui levelsof `group_numeric', local(over_lvls);
        local over_count : word count `over_lvls';

      /* Adding `group_numeric' to `group_string' */

        local num_format = "%0" + string(length("`over_count'")) + ".0f";
        qui replace `group_string' = string(`group_numeric', "`num_format'") + " " + `group_string';
        qui replace `group_string' = "" if `group_numeric' == .;

      /* Converting `group_string' to `group_var' */

        tempname group_var;
        encode `group_string', generate(`group_var');

      /* Getting Details */

        mata: bd.oi = getOverInfo(bd);

      /* Cleaning */

        cap drop `group_numeric';
        cap drop `group_string';

        local group_numeric;
        local group_string;
        local num_format;
        local over_count;
    };

  *----------------------------------------------------------*
  *   04. Setting Parameters & Options (2)                   *
  *----------------------------------------------------------*;

    mata: bd.opt = cleanOptions(bd);
    mata: bd.st = getStats(bd);

  *----------------------------------------------------------*
  *   05. Getting Results                                    *
  *----------------------------------------------------------*;

    mata: bd.res = generateResults(bd);

  *----------------------------------------------------------*
  *   06. Printing Results                                   *
  *----------------------------------------------------------*;

    mata: bd.st = formatStats(bd);
    mata: printer(bd);

  *----------------------------------------------------------*
  *   07. Excel Output                                       *
  *----------------------------------------------------------*;

    mata: createExcel(bd);

  *----------------------------------------------------------*
  *   08. Cleaning Up                                        *
  *----------------------------------------------------------*;

    mata: mata clear;

end;

**======================================================================**
**   Mata Structures (Initialize)                                       **
**======================================================================**;

  #delimit cr

  mata:

  /* struct - braddev */

    struct braddev
    {
      /* Variable Information */
      struct varInfo scalar vi

      /* Over Information */
      struct overInfo scalar oi

      /* Options */
      struct options scalar opt

      /* Stats */
      struct stats scalar st

      /* Results */
      struct results matrix res

      /* Excel */
      struct excel scalar xl

      /* Code List */
      string matrix code_list
    }

  /* struct - varInfo */

    struct varInfo
    {
      /* Counts */
      real scalar var_count
      real scalar series_count

      /* Names */
      string matrix name_original
      string matrix name_variable

      /* Series */
      string matrix series_name
      string matrix series_label
      real   matrix series_position
      string matrix series_status

      /* Display */
      string matrix display_name
      string matrix display_series
    }

  /* struct - overInfo */

    struct overInfo
    {
      /* List */
      string matrix list

      /* Count */
      real scalar count

      /* Names */
      string matrix name_short
      string matrix name_long
      string matrix name_select

      /* Freqs */
      real matrix freqs
    }

  /* struct - options */

    struct options
    {
      /* Survey */
      real scalar survey

      /* Subpop */
      string scalar subpop

      /* Over */
      real scalar ovo_labels
      real scalar ovo_legend
      real scalar ovo_miss
      real scalar ovo_sep
      real scalar ovo_total

      /* P-Values */
      real   scalar    pval_type
      real   scalar    pval_show
      real   scalar    pval_force
      string scalar    pval_mtest
      real   rowvector pval_stars
      real   scalar    pval_scripts

      /* Confidence Intervals */
      real   scalar ci_proportion
      real   scalar ci_combined
      string scalar ci_notation
      string scalar ci_separator
      real   scalar ci_round
      real   scalar ci_level

      /* Display */
      real   scalar dis_xivals
      real   scalar dis_xivars
      real   scalar dis_seriesvals
      real   scalar dis_seriesvars
      real   scalar dis_percent
      real   scalar dis_round
      real   scalar dis_wide
      real   scalar dis_footer
      string scalar dis_align
      string scalar dis_title
      real   scalar dis_print
    }

  /* struct - stats */

    struct stats
    {
      /* Names */
      string matrix name_short
      string matrix name_long
      string matrix name_abbrev

      /* Column Size */
      string matrix col_format
      real   matrix col_length

      /* Count */
      real scalar count
    }

  /* struct - results */

    struct results
    {
      /* Variable Information */
      string scalar name_original
      string scalar name_variable

      /* Series Information */
      string scalar series_name
      string scalar series_label
      real   scalar series_position
      string scalar series_status

      /* Display Information */
      string matrix display_name
      string matrix display_series

      /* Over Information */
      string matrix over_labels

      /* Binary */
      real scalar binary

      /* Values */
      real matrix vals_obs
      real matrix vals_nyes
      real matrix vals_mean
      real matrix vals_se
      real matrix vals_t
      real matrix vals_ci
      real matrix vals_df
      real matrix vals_sd
      real matrix vals_var
      real matrix vals_povr
      real matrix vals_pind
      real matrix vals_minmax

      /* Postscripts */
      string matrix ps_stars
      string matrix ps_scripts
    }

  /* struct - excel */

    struct excel
    {
      /* Command Information */
      real scalar output
      real scalar bookreplace
      real scalar sheetreplace

      /* File Information */
      string scalar file_path
      string scalar sheet

      /* Style */
      string scalar style
    }

  end

**======================================================================**
**   Mata Structure - braddev                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* newBraddev() */

    struct braddev scalar newBraddev()
    {
      struct braddev scalar bd

      /* Options */

        bd.opt = getOptions()

      /* Variable Information */

        bd.vi = getVarInfo(bd)

      /* Over Information */

        bd.oi = overInfo()
        bd.oi.count = 0

      /* Excel */

        bd.xl = parseExcel()

      /* Code List */

        bd.code_list = uchar(7468), uchar(7470), uchar(7472), uchar(7473), uchar(7475), uchar(7476)
        bd.code_list = bd.code_list, uchar(7477), uchar(7478), uchar(7479), uchar(7480), uchar(7481), uchar(7482)
        bd.code_list = bd.code_list, uchar(7484), uchar(7486), uchar(7487), uchar(7488), uchar(7489), uchar(7490)

      return(bd)
    }

  end

**======================================================================**
**   Mata Structure - varInfo                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* getVarInfo() */

    struct varInfo scalar getVarInfo(struct braddev scalar input_bd)
    {
      struct varInfo scalar vi

      /* Initalizing Term List */

        term_list = st_local("0")
        if(strpos(term_list, ",") != 0) term_list = substr(term_list, 1, strpos(term_list, ",") - 1)
        term_list = subinstr(term_list, st_local("if"), "", .)
        term_list = subinstr(term_list, st_local("in"), "", .)
        term_list = tokens(term_list)

      /* Initalizing Variable List */

        var_list = tokens(st_local("varlist"))

      /* Removing Strings */

        stata("qui ds " + st_local("varlist") + ", has(type string)")
        stata("local stringlist = r(varlist)")

        if(st_local("stringlist") != ".")
        {
          string_list = tokens(st_local("stringlist"))
          var_list = select(var_list, !inlist(var_list, string_list))
        }

        if(cols(var_list) == 0)
        {
          printf("{error:0 numeric variables in varlist}\n")
          exit(109)
        }

      /* Getting Initial Variable Information */

        stata("local xivars : char _dta[__xi__Vars__To__Drop__]")
        xivars = st_local("xivars") == "" ? "" : tokens(st_local("xivars"))

        var_labels    = J(1, cols(var_list), "")
        var_types     = J(1, cols(var_list), "")
        var_answers   = J(1, cols(var_list), "")
        var_questions = J(1, cols(var_list), "")
        var_series    = J(1, cols(var_list), "")
        var_positions = J(1, cols(var_list), .)

        for(i=1; i<=cols(var_list); i++)
        {
          /* Individual */

            var_labels[1,i]  = st_varlabel(var_list[1,i])
            var_types[1,i]   = "individual"
            var_series[1,i]  = var_list[1,i]

          /* XI Variable */

            if(inlist(var_list[1,i], xivars))
            {
              curr_label = tokens(subinstr(var_labels[1,i], "==", " "))

              var_types[1,i]     = "xi"
              var_answers[1,i]   = var_labels[1,i]
              var_questions[1,i] = st_varlabel(curr_label[1,1])
              var_series[1,i]    = curr_label[1,1]
              if(input_bd.opt.dis_xivals == 1) var_positions[1,i] = 0

              if(st_varvaluelabel(curr_label[1,1]) != "")
              {
                answer = st_vlmap(st_varvaluelabel(curr_label[1,1]), strtoreal(curr_label[1,2]))
                if(answer != "") var_answers[1,i] = answer
              }

              continue
            }

          /* Series Variable */

            if(strpos(var_labels[1,i], "]") > strpos(var_labels[1,i], "[") & strpos(var_labels[1,i], "[") != 0)
            {
              curr_label = var_labels[1,i]

              var_types[1,i]     = "series"
              var_answers[1,i]   = substr(curr_label, strpos(curr_label, "[") + 1, strpos(curr_label, "]") - strpos(curr_label, "[") - 1)
              var_questions[1,i] = substr(curr_label, strpos(curr_label, "]") + 1)
              if(input_bd.opt.dis_seriesvals) var_positions[1,i] = 0

              continue
            }
        }

      /* Cut-off Questions */

        if(max(udstrlen(var_labels)) :== 80)
        {
          cutoff = selectindex(udstrlen(var_labels) :== 80)
          for(i=1; i<=cols(cutoff); i++)
          {
            matches = selectindex(strpos(var_questions, var_questions[1,cutoff[1,i]]) :& (udstrlen(var_labels) :!= 80))
            if(cols(matches) > 0)
            {
              var_questions[1,cutoff[1,i]] = var_questions[1,matches[1,1]]
            }
          }
        }

      /* Constructing Series */

        series_list = select(var_questions, var_types :== "series")
        series_list = uniqrows(series_list')'

        for(i=1; i<=cols(series_list); i++)
        {
          series_vars  = select(var_list, var_questions :== series_list[1,i])
          series_index = selectindex(var_questions :== series_list[1,i])

          if(cols(series_index) == 1)
          {
            var_series[1,series_index[1,1]] = var_list[1,series_index[1,1]]
            continue
          }

          series_name  = commonString(series_vars)
          for(j=1; j<=cols(series_index); j++)
          {
            var_series[1,series_index[1,j]] = series_name
          }
        }

      /* Setting Series Position */

        if(input_bd.opt.dis_xivals)
        {
          series_vars = uniqrows(select(var_series, var_types :== "xi")')'

          for(i=1; i<=cols(series_vars); i++)
          {
            series_index = vectorIndex(selectindex(var_series :== series_vars[1,i]))

            for(j=1; j<=rows(series_index); j++)
            {
              var_positions[1,series_index[j,1]] = 1
              var_positions[1,series_index[j,2]] = -1
            }
          }
        }

        if(input_bd.opt.dis_seriesvals)
        {
          series_vars = uniqrows(select(var_series, var_types :== "series")')'

          for(i=1; i<=cols(series_vars); i++)
          {
            series_index = vectorIndex(selectindex(var_series :== series_vars[1,i]))

            for(j=1; j<=rows(series_index); j++)
            {
              var_positions[1,series_index[j,2]] = -1
              var_positions[1,series_index[j,1]] = 1
            }
          }
        }

      /* Setting Variable Names & Series Labels */

        display_name   = J(1, cols(var_list), "")
        display_series = J(1, cols(var_list), "")

        for(i=1; i<=cols(var_list); i++)
        {
          if(var_types[1,i] == "individual")
          {
            display_name[1,i]   = var_list[1,i]
            display_series[1,i] = var_list[1,i]
            continue
          }

          if(var_types[1,i] == "xi")
          {
            if(input_bd.opt.dis_xivals) display_name[1,i] = var_answers[1,i]
            else                        display_name[1,i] = var_list[1,i]

            if(input_bd.opt.dis_xivars) display_series[1,i] = var_questions[1,i]
            else                        display_series[1,i] = var_series[1,i]

            if(display_series[1,i] == "") display_series[1,i] = var_series[1,i]

            continue
          }

          if(var_types[1,i] == "series")
          {
            if(input_bd.opt.dis_seriesvals) display_name[1,i] = var_answers[1,i]
            else                            display_name[1,i] = var_list[1,i]

            if(input_bd.opt.dis_seriesvars) display_series[1,i] = var_questions[1,i]
            else                            display_series[1,i] = var_series[1,i]

            if(display_series[1,i] == "") display_series[1,i] = var_series[1,i]

            continue
          }
        }

        series_count = rows(uniqrows(display_series'))

      /* Cleaning Up */

        var_labels    = usubinstr(var_labels,    "%", "%%", .)
        var_answers   = usubinstr(var_answers,   "%", "%%", .)
        var_questions = usubinstr(var_questions, "%", "%%", .)
        var_series    = usubinstr(var_series,    "%", "%%", .)

      /* Placing Values */

        st_local("varlist", invtokens(var_list))

        /* Counts */
        vi.var_count    = cols(var_list)
        vi.series_count = series_count

        /* Names */
        vi.name_original = var_list
        vi.name_variable = var_answers

        /* Series */
        vi.series_name = var_series
        vi.series_label = var_questions
        vi.series_position = var_positions
        vi.series_status = var_types

        /* Display */
        vi.display_name   = display_name
        vi.display_series = display_series

      return(vi)
    }

  end

**======================================================================**
**   Mata Structure - overInfo                                          **
**======================================================================**;

  #delimit cr

  mata:

  /* getOverInfo() */

    struct overInfo scalar getOverInfo(struct braddev scalar input_bd)
    {
      struct overInfo scalar oi

      /* List */

        oi.list = tokens(st_local("over"))

      /* Count */

        oi.count = strtoreal(st_local("over_count"))

      /* Names */

        st_vlload(st_varvaluelabel(st_local("group_var")), values, text)
        oi.name_short  = ("_over_" :+ strofreal(values))
        oi.name_long   = subinstr(strtrim(substr(text, strpos(text, " "), strlen(text))), ".", "")
        oi.name_select = input_bd.opt.ovo_labels ? oi.name_long : oi.name_short

      /* Freqs */

        stata("qui tab " + st_local("group_var") + ", matcell(" + st_local("freqs") + ")")
        oi.freqs = st_matrix(st_local("freqs"))

      return(oi)
    }

  end

**======================================================================**
**   Mata Structure - options                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* getOptions() */

    struct options scalar getOptions()
    {
      struct options scalar opt

      /* Defaults */

        /* Survey */
        opt.survey = 0

        /* Subpop */
        opt.subpop = ""

        /* Over */
        opt.ovo_labels = 1
        opt.ovo_legend = 1
        opt.ovo_miss   = 1
        opt.ovo_sep    = 0
        opt.ovo_total  = 0

        /* P-Values */
        opt.pval_type  = 1
        opt.pval_force = 0
        opt.pval_mtest = "noadjust"

        /* Confidence Intervals */
        opt.ci_proportion = 0
        opt.ci_combined   = 0
        opt.ci_notation   = "[]"
        opt.ci_separator  = ","
        opt.ci_round      = 7
        opt.ci_level      = strtoreal(st_global("S_level"))

        /* Display */
        opt.dis_xivals     = 1
        opt.dis_xivars     = 0
        opt.dis_seriesvals = 0
        opt.dis_seriesvars = 0
        opt.dis_percent    = 0
        opt.dis_round      = 7
        opt.dis_wide       = 0
        opt.dis_footer     = 1
        opt.dis_align      = "lalign"
        opt.dis_title      = ""
        opt.dis_print      = 1

      /* Parsing */

        opt = parseOptions(opt)

      return(opt)
    }

  /* parseOptions() */

    struct options scalar parseOptions(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      opt = parseSurvey(opt)
      opt = parseSubpop(opt)
      opt = parseOver(opt)
      opt = parsePvalue(opt)
      opt = parseCI(opt)
      opt = parseDisplay(opt)

      return(opt)
    }

  /* parseSurvey() */

    struct options scalar parseSurvey(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      opt.survey = st_local("svy") != ""

      if(opt.survey == 1)
      {
        stata("cap noi _svy_newrule")
        stata("local _ec = _rc")
        if(st_local("_ec") != "0") exit(119)
      }

      return(opt)
    }

  /* parseSubpop() */

    struct options scalar parseSubpop(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      input_string = st_local("subpop")

      if(input_string != "")
      {
        stata("qui levelsof " + input_string + ", local(lvls)")
        levels = st_local("lvls")

        if(levels == "0 1" | levels == "1")
        {
          opt.subpop = input_string
          opt.survey = 1
          stata("cap noi _svy_newrule")
          stata("local _ec = _rc")
          if(st_local("_ec") != "0") exit(119)
        }
        else
        {
          printf("{error:Subpop requires a 0/1 variable}\n")
          exit(119)
        }

        st_local("lvls", "")
      }

      return(opt)
    }

  /* parseOver() */

    struct options scalar parseOver(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      input_string = st_local("overopt")

      t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
      tokenset(t, input_string)

      while((token = tokenget(t)) != "")
      {
        if(strpos(token, "nolab")  == 1) { opt.ovo_labels = 0; continue; }
        if(strpos(token, "noleg")  == 1) { opt.ovo_legend = 0; continue; }
        if(strpos(token, "nomiss") == 1) { opt.ovo_miss   = 0; continue; }
        if(strpos(token, "sep")    == 1) { opt.ovo_sep    = 1; continue; }
        if(strpos(token, "tot")    == 1) { opt.ovo_total  = 1; continue; }
      }

      return(opt)
    }

  /* parsePvalue() */

    struct options scalar parsePvalue(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      input_string = st_local("pvalues")

      t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
      tokenset(t, input_string)

      while((token = tokenget(t)) != "")
      {
        if(strpos(token, "all")   == 1) { opt.pval_type  = 2; continue; }
        if(strpos(token, "ind")   == 1) { opt.pval_type  = 2; continue; }
        if(strpos(token, "over")  == 1) { opt.pval_type  = 1; continue; }
        if(strpos(token, "none")  == 1) { opt.pval_type  = 0; continue; }
        if(strpos(token, "force") == 1) { opt.pval_force = 1; continue; }

        /* mtest */

          if(token == "mtest")
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)'
              if(strpos(token, "b") == 1) { opt.pval_mtest = "bonferroni"; continue; }
              if(strpos(token, "h") == 1) { opt.pval_mtest = "holm";       continue; }
              if(strpos(token, "s") == 1) { opt.pval_mtest = "sidak";      continue; }
            }
            continue
          }

        /* stars */

          if(strpos(token, "star") == 1)
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              values = strtoreal(tokens(token))
              values = sort(values', 1)'
              values = select(values, ((values :> 0) :& (values :< 1)))
              if(cols(values) > 3)  opt.pval_stars = values[., (1..3)]
              if(cols(values) != 0) opt.pval_stars = values
            }
            else
            {
              opt.pval_stars = (0.01, 0.05)
            }
            continue
          }

        /* script */

          if(strpos(token, "script") == 1)
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              values = strtoreal(tokens(token))
              values = sort(values', 1)'
              values = select(values, ((values :> 0) :& (values :< 1)))
              if(cols(values) != 0) opt.pval_scripts = values[1,1]
            }
            else
            {
              opt.pval_scripts = 0.05
            }
            continue
          }
      }

      return(opt)
    }

  /* parseCI() */

    struct options scalar parseCI(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      input_string = st_local("ci")

      t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
      tokenset(t, input_string)

      while((token = tokenget(t)) != "")
      {
        if(strpos(token, "prop") == 1) { opt.ci_proportion = 1;    continue; }
        if(strpos(token, "log") == 1)  { opt.ci_proportion = 1;    continue; }
        if(strpos(token, "comb") == 1) { opt.ci_combined   = 1;    continue; }
        if(strpos(token, "par")  == 1) { opt.ci_notation   = "()"; continue; }
        if(strpos(token, "brac") == 1) { opt.ci_notation   = "[]"; continue; }

        /* level */

          if(strpos(token, "lev") == 1)
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              values = strtoreal(tokens(token))
              values = trunc(values[1,1])
              if(values > 0 & values < 100) opt.ci_level = values
            }
            continue
          }

        /* round */

          if(token == "round")
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              values = strtoreal(tokens(token))
              values = trunc(values[1,1])
              if(values >= 0 & values <= 7) opt.ci_round = values
            }
            continue
          }

        /* separator */

          if(strpos(token, "sep") == 1)
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              token = strtrim(token)
              if(token == "-" | token == ",") opt.ci_separator = token
            }
            continue
          }
      }

      return(opt)
    }

  /* parseDisplay() */

    struct options scalar parseDisplay(struct options scalar input)
    {
      struct options scalar opt
      opt = input

      input_string = st_local("display")

      t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
      tokenset(t, input_string)

      while((token = tokenget(t)) != "")
      {
        if(strpos(token, "pct")    == 1)  { opt.dis_percent = 1; continue; }
        if(strpos(token, "per")    == 1)  { opt.dis_percent = 1; continue; }
        if(strpos(token, "wide")   == 1)  { opt.dis_wide    = 1; continue; }
        if(strpos(token, "nofo")   == 1)  { opt.dis_footer  = 0; continue; }
        if(strpos(token, "noprint") == 1) { opt.dis_print   = 0; continue; }

        /* xi & series */

          if(token == "xi")   { opt.dis_xivals = 1; opt.dis_xivars = 1; continue; }
          if(token == "noxi") { opt.dis_xivals = 0; opt.dis_xivars = 0; continue; }

          if(strpos(token, "xival")   == 1) {opt.dis_xivals = 1; continue; }
          if(strpos(token, "xivar")   == 1) {opt.dis_xivars = 1; continue; }
          if(strpos(token, "noxival") == 1) {opt.dis_xivals = 0; continue; }
          if(strpos(token, "noxivar") == 1) {opt.dis_xivars = 0; continue; }

          if(token == "series")   { opt.dis_seriesvals = 1; opt.dis_seriesvars = 1; continue; }
          if(token == "noseries") { opt.dis_seriesvals = 0; opt.dis_seriesvars = 0; continue; }

          if(strpos(token, "seriesval")   == 1) {opt.dis_seriesvals = 1; continue; }
          if(strpos(token, "seriesvar")   == 1) {opt.dis_seriesvars = 1; continue; }
          if(strpos(token, "noseriesval") == 1) {opt.dis_seriesvals = 0; continue; }
          if(strpos(token, "noseriesvar") == 1) {opt.dis_seriesvars = 0; continue; }

        /* align */

          if(strpos(token, "al") == 1)
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              if(strpos(token, "l") == 1) { opt.dis_align = "lalign";   continue; }
              if(strpos(token, "c") == 1) { opt.dis_align = "center"; continue; }
              if(strpos(token, "r") == 1) { opt.dis_align = "ralign";  continue; }
            }
            continue
          }

        /* round */

          if(token == "round")
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = substr(token, 2, strlen(token) - 2)
              values = strtoreal(tokens(token))
              values = trunc(values[1,1])
              if(values >= 0 & values <= 7) opt.dis_round = values
            }
            continue
          }

        /* title */

          if(token == "title")
          {
            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              token = tokenget(t)
              token = strtrim(substr(token, 2, strlen(token) - 2))

              if(substr(token, 1, 1) == `"""' & substr(token, -1, 1) == `"""')
              {
                token = strtrim(substr(token, 2, strlen(token) - 2))
              }

              opt.dis_title = token
            }
            continue
          }
      }

      return(opt)
    }

  /* cleanOptions() */

    struct options scalar cleanOptions(struct braddev scalar input_bd)
    {
      struct options scalar opt
      opt = input_bd.opt

      /* Wide */

        if(input_bd.oi.count <= 1)
        {
          opt.dis_wide = 0
        }

      /* Over */

        if(input_bd.oi.count == 0)
        {
          opt.ovo_total = 0
        }

      /* XI & Series */

        if(input_bd.opt.dis_xivars == 1)
        {
          input_bd.opt.dis_xivals = 1
        }

        if(input_bd.opt.dis_seriesvars == 1)
        {
          input_bd.opt.dis_seriesvals = 1
        }

      /* P-Values */

        /* Script + Stars -> Type */

          if(opt.pval_scripts != .)
          {
            opt.pval_type = 2
          }

          if(cols(opt.pval_stars) != 0 & opt.pval_type < 1)
          {
            opt.pval_type = 1
          }

        /* Script (Length) */

          if(opt.pval_scripts != . & input_bd.oi.count > 18)
          {
            printf("{error:Too many groups for superscripts}\n")
            opt.pval_scripts = .
          }

        /* Over Count */

          if(input_bd.oi.count == 2 & opt.pval_type == 2)
          {
            opt.pval_type = 1
          }

          if(input_bd.oi.count < 2)
          {
            opt.pval_type = 0
          }

        /* Show */

          opt.pval_show = opt.pval_type

          if(opt.pval_show == 2 & opt.pval_scripts != . & opt.pval_force == 0)
          {
            opt.pval_show = 1
          }

          if(opt.pval_show == 1 & cols(opt.pval_stars) != 0 & opt.pval_force == 0)
          {
            opt.pval_show = 0
          }

        /* Scripts + Stars Fix */

          if(opt.pval_type < 2) opt.pval_scripts = .
          if(opt.pval_type < 1) opt.pval_stars = J(0,0,.)

      /* CI */

        if(opt.pval_scripts != .)
        {
          opt.ci_combined = 1
        }

        if(opt.ci_round > opt.dis_round)
        {
          opt.ci_round = opt.dis_round
        }

      /* Footer */

        if(input_bd.oi.count == 0)
        {
          opt.dis_footer = 0
        }

        if(cols(opt.pval_stars) == 0 & opt.pval_scripts == .)
        {
          opt.dis_footer = 0
        }

      /* Print */

        if(input_bd.xl.output == 0)
        {
          opt.dis_print = 1
        }

      /* Title */

        opt.dis_title = getTitle(input_bd)

      return(opt)
    }

  end

**======================================================================**
**   Mata Structure - stats                                             **
**======================================================================**;

  #delimit cr

  mata:

  /* getStats() */

    struct stats scalar getStats(struct braddev scalar input_bd)
    {
      struct stats scalar st

      /* Input */

        st_local("stats_list", "obs nyes mean se sd var ci min max")

        st_local("stats", strtrim(stritrim(strlower(st_local("stats")))))
        st_local("stats", subinword(st_local("stats"), "all", "obs nyes mean se sd var ci min max"))

        stata("local stats : list uniq stats")
        stata("local stats : list stats & stats_list")

        st_local("stats_list", "")

      /* Defaults */

        if(st_local("stats") == "")
        {
          if(input_bd.opt.dis_wide)
          {
            st_local("stats", "mean")
          }
          else
          {
            st_local("stats", "obs nyes mean sd ci")
          }
        }

      /* Processing */

        name_short = ("obs", "nyes", "mean", "se", "ci", "lci", "uci", "sd", "var", "min", "max")
        name_long  = ("Obs", "n(Yes)", "Mean", "Std Err", "Confidence Interval", "Lower CI", "Upper CI", "Std Dev", "Variance", "Min", "Max")

        if(input_bd.opt.ci_combined == 0)
        {
          st_local("stats", subinstr(st_local("stats"), "ci", "lci uci", .))
        }

        t = tokens(st_local("stats"))

        st.name_short = J(1, cols(t), "")
        st.name_long  = J(1, cols(t), "")

        for(i=1; i<=cols(t); i++)
        {
          st.name_short[1,i] = t[1,i]
          st.name_long[1,i]  = name_long[1,selectindex(t[1,i] :== name_short)]
        }

        st.count = cols(st.name_short)

      return(st)
    }

  /* formatStats() */

    struct stats scalar formatStats(struct braddev scalar input_bd)
    {
      struct stats scalar st
      st = input_bd.st

      st.col_format  = J(1, st.count, "")
      st.col_length  = J(1, st.count, .)
      st.name_abbrev = J(1, st.count, "")

      for(i=1; i<=st.count; i++)
      {
        if(inlist(st.name_short[1,i], ("obs", "nyes")))
        {
          st = formatObs(input_bd, st, st.name_short[1,i], i)
          continue
        }

        if(inlist(st.name_short[1,i], ("mean")))
        {
          st = formatMean(input_bd, st, st.name_short[1,i], i)
          continue
        }

        if(inlist(st.name_short[1,i], ("se", "sd", "var")))
        {
          st = formatError(input_bd, st, st.name_short[1,i], i)
          continue
        }

        if(inlist(st.name_short[1,i], ("lci", "uci", "ci")))
        {
          st = formatCI(input_bd, st, st.name_short[1,i], i)
          continue
        }

        if(inlist(st.name_short[1,i], ("min", "max")))
        {
          st = formatMinMax(input_bd, st, st.name_short[1,i], i)
          continue
        }
      }

      return(st)
    }

  /* formatObs() */

    struct stats scalar formatObs(struct braddev scalar input_bd,
                                  struct stats   scalar input_st,
                                  string         scalar stat,
                                  real           scalar pos)
    {
      struct stats scalar st
      st = input_st

      /* Getting Values */

        for(i=1; i<=cols(input_bd.res); i++)
        {
          if(stat == "obs")  val = input_bd.res[1,i].vals_obs
          if(stat == "nyes") val = input_bd.res[1,i].vals_nyes

          if(i == 1) values = val
          else       values = values \ val
        }

      /* Format & Length */

        length = max(udstrlen(strtrim(strofreal(values, "%9.0fc"))))
        length = max((length, 5))

        st.col_format[1,pos] = ("%" + strofreal(length) + ".0fc")
        st.col_length[1,pos] = length

      /* Name */

        if(stat == "obs")
        {
          st.name_abbrev[1,pos] = "Obs"
        }

        if(stat == "nyes")
        {
          name = "n(Yes)"
          if(length < udstrlen(name)) name = "n(Y)"
          st.name_abbrev[1,pos] = name
        }

      return(st)
    }

  /* formatMean() */

    struct stats scalar formatMean(struct braddev scalar input_bd,
                                   struct stats   scalar input_st,
                                   string         scalar stat,
                                   real           scalar pos)
    {
      struct stats scalar st
      st = input_st

      /* Getting Values */

        for(i=1; i<=cols(input_bd.res); i++)
        {
          val = input_bd.res[1,i].vals_mean
          ps  = input_bd.res[1,i].ps_stars
          if(inlist(st.name_short, "ci") == 0) ps = ps :+ input_bd.res[1,i].ps_scripts

          if(i==1)
          {
            values      = val
            postscripts = ps
          }
          else
          {
            values      = values \ val
            postscripts = postscripts \ ps
          }
        }

      /* Percent */

        if(input_bd.opt.dis_percent)
        {
          values = values * 100
          postscripts = cols(postscripts) == 0 ? "%" : "%" :+ postscripts
          if(input_bd.opt.dis_round > 5) input_bd.opt.dis_round = 5
        }

      /* Format */

        prefix = max(udstrlen(strtrim(strofreal(values, "%9.0f")))) + (input_bd.opt.dis_round > 0)
        suffix = input_bd.opt.dis_round

        st.col_format[1,pos] = ("%" + strofreal(prefix + suffix) + "." + strofreal(suffix) + "f")

      /* Length */

        length = prefix + suffix
        if(cols(postscripts) > 0) length = length + max(udstrlen(postscripts))
        length = max((length, 5))

        st.col_length[1,pos] = length

      /* Name */

        st.name_abbrev[1,pos] = "Mean"

      return(st)
    }

  /* formatError() */

    struct stats scalar formatError(struct braddev scalar input_bd,
                                    struct stats   scalar input_st,
                                    string         scalar stat,
                                    real           scalar pos)
    {
      struct stats scalar st
      st = input_st

      /* Getting Values */

        for(i=1; i<=cols(input_bd.res); i++)
        {
          if(stat == "se")  val = input_bd.res[1,i].vals_se
          if(stat == "sd")  val = input_bd.res[1,i].vals_sd
          if(stat == "var") val = input_bd.res[1,i].vals_var

          if(i==1) values = val
          else     values = values \ val
        }

      /* Percent */

        if(input_bd.opt.dis_percent)
        {
          values = values * 100
          if(input_bd.opt.dis_round > 5) input_bd.opt.dis_round = 5
        }

      /* Format */

        prefix = max(udstrlen(strtrim(strofreal(values, "%9.0f")))) + (input_bd.opt.dis_round > 0)
        suffix = input_bd.opt.dis_round

        st.col_format[1,pos] = ("%" + strofreal(prefix + suffix) + "." + strofreal(suffix) + "f")

      /* Length */

        length = prefix + suffix + input_bd.opt.dis_percent
        length = max((length, 5))

        st.col_length[1,pos] = length

      /* Name */

        if(stat == "se")
        {
          name = "Std Err"
          if(length < udstrlen(name)) name = "SE"
        }

        if(stat == "sd")
        {
          name = "Std Dev"
          if(length < udstrlen(name)) name = "SD"
        }

        if(stat == "var")
        {
          name = "Variance"
          if(length < udstrlen(name)) name = "Var"
        }

        st.name_abbrev[1,pos] = name

      return(st)
    }

  /* formatCI() */

    struct stats scalar formatCI(struct braddev scalar input_bd,
                                 struct stats   scalar input_st,
                                 string         scalar stat,
                                 real           scalar pos)
    {
      struct stats scalar st
      st = input_st

      /* Getting Values */

        for(i=1; i<=cols(input_bd.res); i++)
        {
          val = input_bd.res[1,i].vals_ci
          ps  = input_bd.res[1,i].ps_scripts
          if(i==1)
          {
            values      = val
            postscripts = ps
          }
          else
          {
            values      = values \ val
            postscripts = postscripts \ ps
          }
        }

      /* Percent */

        if(input_bd.opt.dis_percent)
        {
          values = values * 100
          if(input_bd.opt.ci_round > 5) input_bd.opt.ci_round = 5
        }

      /* Format */

        prefix = max(udstrlen(strtrim(strofreal(values, "%9.0f")))) + (input_bd.opt.dis_round > 0)
        suffix = input_bd.opt.dis_round

        st.col_format[1,pos] = ("%" + strofreal(prefix + suffix) + "." + strofreal(suffix) + "f")

      /* Length */

        if(input_bd.opt.ci_combined == 0)
        {
          length = prefix + suffix + input_bd.opt.dis_percent
          length = max((length, 4))
        }
        else
        {
          length = ((prefix + suffix + input_bd.opt.dis_percent) * 2) + 3
          if(cols(postscripts) > 0) length = length + max(udstrlen(postscripts))
          length = max((length, 8))
        }

        st.col_length[1,pos] = length

      /* Name */

        if(stat == "lci")
        {
          name = "Lower CI"
          if(length < udstrlen(name)) name = "LCI"
        }

        if(stat == "uci")
        {
          name = "Upper CI"
          if(length < udstrlen(name)) name = "UCI"
        }

        if(stat == "ci")
        {
          name = "Confidence Interval"
          if(length < udstrlen(name)) name = "CI"
        }

        st.name_abbrev[1,pos] = name

      return(st)
    }

  /* formatMinMax() */

    struct stats scalar formatMinMax(struct braddev scalar input_bd,
                                     struct stats   scalar input_st,
                                     string         scalar stat,
                                     real           scalar pos)
    {
      struct stats scalar st
      st = input_st

      /* Getting Values */

        for(i=1; i<=cols(input_bd.res); i++)
        {
          if(i==1) values = input_bd.res[1,i].vals_minmax
          else     values = values \ input_bd.res[1,i].vals_minmax
        }

      /* Percent */

        if(input_bd.opt.dis_percent)
        {
          values = values * 100
          if(input_bd.opt.dis_round > 5) input_bd.opt.dis_round = 5
        }

      /* Format */

        prefix1 = max(udstrlen(strtrim(strofreal(values, "%9.0f")))) + (input_bd.opt.dis_round > 0)
        suffix1 = input_bd.opt.dis_round

        prefix2 = max(udstrlen(strtrim(strofreal(values, "%9.0f"))))
        suffix2 = 0

        fmt1 = "%" + strofreal(prefix1 + suffix1) + "." + strofreal(suffix1) + "f"
        fmt2 = "%" + strofreal(prefix2 + suffix2) + "." + strofreal(suffix2) + "f"

        if(strtoreal(strofreal(values, fmt1)) == strtoreal(strofreal(values, fmt2)))
        {
          prefix = prefix2
          suffix = suffix2
        }
        else
        {
          prefix = prefix1
          suffix = suffix1
        }

        st.col_format[1,pos] = ("%" + strofreal(prefix + suffix) + "." + strofreal(suffix) + "f")

      /* Length */

        length = prefix + suffix + input_bd.opt.dis_percent
        length = max((length, 5))

        st.col_length[1,pos] = length

      /* Names */

        if(stat == "min")
        {
          st.name_abbrev[1,pos] = "Min"
        }

        if(stat == "max")
        {
          st.name_abbrev[1,pos] = "Max"
        }

      return(st)
    }

  end

**======================================================================**
**   Mata Structure - results                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* generateResults() */

    struct results matrix generateResults(struct braddev scalar input_bd)
    {
      struct results scalar res

      res_mat = J(1, input_bd.vi.var_count, res)

      for(i=1; i<=input_bd.vi.var_count; i++)
      {
        res_mat[1,i] = calculateResults(input_bd, i)
      }

      return(res_mat)
    }

  /* calculateResults() */

    struct results scalar calculateResults(struct braddev scalar input_bd,
                                           real           scalar pos)
    {
      struct results scalar res
      res = results()

      /* Information */

        /* Variable Information */

          res.name_original = input_bd.vi.name_original[1,pos]
          res.name_variable = input_bd.vi.name_variable[1,pos]

        /* Series Information */

          res.series_name     = input_bd.vi.series_name[1,pos]
          res.series_label    = input_bd.vi.series_label[1,pos]
          res.series_position = input_bd.vi.series_position[1,pos]
          res.series_status   = input_bd.vi.series_status[1,pos]

        /* Display Information */

          res.display_name   = input_bd.vi.display_name[1,pos]
          res.display_series = input_bd.vi.display_series[1,pos]

        /* Binary */

          stata("qui levelsof " + res.name_original + ", local(logit_lvls)")
          res.binary = inlist(st_local("logit_lvls"), ("0 1", "0", "1"))
          st_local("logit_lvls", "")

      /* Checking if All Missing */

        stata("qui count if " + st_local("touse") + " & !missing(" + res.name_original + ")")

        if(st_numscalar("r(N)") == 0)
        {
          printf("{error:" + res.name_original + " has no observations in sample.}\n")

          group_count = (input_bd.oi.count == 0) ? 1 : (input_bd.oi.count + input_bd.opt.ovo_total)

          res.vals_obs    = J(group_count, 1, 0)
          res.vals_mean   = J(group_count, 1, .)
          res.vals_se     = J(group_count, 1, .)
          res.vals_t      = J(group_count, 1, .)
          res.vals_ci     = J(group_count, 2, .)
          res.vals_df     = J(group_count, 1, .)
          res.vals_sd     = J(group_count, 1, .)
          res.vals_var    = J(group_count, 1, .)
          res.vals_nyes   = J(group_count, 1, .)
          res.vals_minmax = J(group_count, 2, .)

          if(input_bd.oi.count > 0)
          {
            res.vals_povr = J(group_count, 1, .)
            res.vals_pind = J(group_count, input_bd.oi.count, .)

            if(cols(input_bd.opt.pval_stars) > 0)
            {
              res.ps_stars = J(group_count, 1, "")
            }

            if(input_bd.opt.pval_scripts != .)
            {
              res.ps_scripts = J(group_count, 1, "")
            }
          }

          return(res)
        }

      /* Defining Commands */

        /* Mean */

          cmd_mean = ""
          if(input_bd.opt.survey)
          {;
            cmd_mean = "svy" + (input_bd.opt.subpop != "" ? ", subpop(" + input_bd.opt.subpop + ")" : "") + ":"
          };
          cmd_mean = "qui " + cmd_mean + "mean " + res.name_original + " if " + st_local("touse") + ", level(" + strofreal(input_bd.opt.ci_level) + ")"

        /* Count */

          cmd_count = " if " + st_local("touse") + " & " + res.name_original + " & !missing(" + res.name_original + ")"
          if(input_bd.opt.subpop != "")
          {
            cmd_count = cmd_count + " & " + input_bd.opt.subpop + " == 1"
          }

      /* Values - No Over */

        if(input_bd.oi.count == 0)
        {;
          /* Mean */

            stata(cmd_mean)
            res.vals_obs  = (input_bd.opt.subpop == "") ? st_matrix("e(_N)")' : st_matrix("e(_N_subp)")'
            res.vals_mean = st_matrix("r(table)")[1,1]
            res.vals_se   = st_matrix("r(table)")[2,1]
            res.vals_t    = st_matrix("r(table)")[3,1]
            res.vals_ci   = st_matrix("r(table)")[(5..6),1]'
            res.vals_df   = st_matrix("r(table)")[7,1]

          /* SD */

            stata("qui estat sd")
            res.vals_sd  = st_matrix("r(sd)")
            res.vals_var = st_matrix("r(variance)")

          /* Count */

            stata("qui count " + cmd_count)
            res.vals_nyes = st_numscalar("r(N)")

          /* Min/Max */

            stata("qui summarize " + res.name_original + " if " + st_local("touse"))
            res.vals_minmax = J(1, 2, .)
            res.vals_minmax[1,1] = st_numscalar("r(min)")
            res.vals_minmax[1,2] = st_numscalar("r(max)")

          /* Logit Transform */

            if(input_bd.opt.ci_proportion == 1 & res.binary == 1)
            {
              res.vals_ci = logitTransform(res, input_bd.opt.ci_level)
            }

          return(res)
        };

      /* Values - Over (Individuals) */

        group_count = (input_bd.oi.count == 0) ? 1 : (input_bd.oi.count + input_bd.opt.ovo_total)

        if(input_bd.oi.count > 0)
        {
          /* Initializing */

            res.vals_obs    = J(group_count, 1, .)
            res.vals_mean   = J(group_count, 1, .)
            res.vals_se     = J(group_count, 1, .)
            res.vals_t      = J(group_count, 1, .)
            res.vals_ci     = J(group_count, 2, .)
            res.vals_df     = J(group_count, 1, .)
            res.vals_sd     = J(group_count, 1, .)
            res.vals_var    = J(group_count, 1, .)
            res.vals_nyes   = J(group_count, 1, .)
            res.vals_minmax = J(group_count, 2, .)
            res.vals_povr   = J(group_count, 1, .)
            res.vals_pind   = J(group_count, input_bd.oi.count, .)

          /* Mean */

            stata(cmd_mean + " over(" + st_local("group_var") + ", nolabel)")
            res.over_labels = tokens(st_global("e(over_namelist)"))'
            for(i=1; i<=rows(res.over_labels); i++)
            {
              over_pos = strtoreal(res.over_labels[i,1])
              res.vals_obs[over_pos,1]  = (input_bd.opt.subpop == "") ? st_matrix("e(_N)")[1,i] : st_matrix("e(_N_subp)")[1,i]
              res.vals_mean[over_pos,1] = st_matrix("r(table)")[1,i]
              res.vals_se[over_pos,1]   = st_matrix("r(table)")[2,i]
              res.vals_t[over_pos,1]    = st_matrix("r(table)")[3,i]
              res.vals_ci[over_pos,1]   = st_matrix("r(table)")[5,i]
              res.vals_ci[over_pos,2]   = st_matrix("r(table)")[6,i]
              res.vals_df[over_pos,1]   = st_matrix("r(table)")[7,i]
            }

          /* SD */

            stata("qui estat sd")
            for(i=1; i<=rows(res.over_labels); i++)
            {
              over_pos = strtoreal(res.over_labels[i,1])
              res.vals_sd[over_pos,1]  = st_matrix("r(sd)")[1,i]
              res.vals_var[over_pos,1] = st_matrix("r(variance)")[1,i]
            }

          /* Count & Min/Max */

            for(i=1; i<=rows(res.over_labels); i++)
            {
              over_pos = strtoreal(res.over_labels[i,1])

              /* Count */

                stata("qui count " + cmd_count + " & " + st_local("group_var") + " == " + res.over_labels[i,1])
                res.vals_nyes[over_pos,1] = st_numscalar("r(N)")

              /* Min/Max */

                stata("qui summarize " + res.name_original + " if " + st_local("touse") + " & " + st_local("group_var") + " == " + res.over_labels[i,1])
                res.vals_minmax[over_pos,1] = st_numscalar("r(min)")
                res.vals_minmax[over_pos,2] = st_numscalar("r(max)")
            }

          /* P-Values */

            test_labels = (("[" + res.name_original + "]") :+ res.over_labels)'

            /* Overall */

              stata("qui test " + invtokens(test_labels, " == ") + ", mtest(" + input_bd.opt.pval_mtest + ")")

              for(i=1; i<=rows(res.over_labels); i++)
              {
                over_pos = strtoreal(res.over_labels[i,1])
                res.vals_povr[over_pos,1] = st_numscalar("r(p)")
              }

            /* Individual */

              for(i=1; i<=cols(test_labels); i++)
              {
                stata("qui test " + test_labels[1,i] + " == " + invtokens(test_labels, " == ") + ", mtest(" + input_bd.opt.pval_mtest + ")")

                over_col = strtoreal(res.over_labels[i,1])
                for(j=1; j<=rows(res.over_labels); j++)
                {
                  over_row = strtoreal(res.over_labels[j,1])
                  res.vals_pind[over_row,over_col] = (input_bd.opt.pval_mtest == "noadjust") ? st_matrix("r(mtest)")[j,3] : st_matrix("r(mtest)")[j,4]
                }
              }
        }

      /* Values - Over (Total) */

        if(input_bd.oi.count > 0 & input_bd.opt.ovo_total == 1)
        {
          /* Mean */

            stata(cmd_mean)
            res.vals_obs[group_count,1]  = (input_bd.opt.subpop == "") ? st_matrix("e(_N)")[1,1] : st_matrix("e(_N_subp)")[1,1]
            res.vals_mean[group_count,1] = st_matrix("r(table)")[1,1]
            res.vals_se[group_count,1]   = st_matrix("r(table)")[2,1]
            res.vals_t[group_count,1]    = st_matrix("r(table)")[3,1]
            res.vals_ci[group_count,1]   = st_matrix("r(table)")[5,1]
            res.vals_ci[group_count,2]   = st_matrix("r(table)")[6,1]
            res.vals_df[group_count,1]   = st_matrix("r(table)")[7,1]

          /* SD */

            stata("qui estat sd")
            res.vals_sd[group_count,1]  = st_matrix("r(sd)")[1,1]
            res.vals_var[group_count,1] = st_matrix("r(variance)")[1,1]

          /* Count */

            stata("qui count " + cmd_count)
            res.vals_nyes[group_count,1] = st_numscalar("r(N)")

          /* Min/Max */

            stata("qui summarize " + res.name_original + " if " + st_local("touse"))
            res.vals_minmax[group_count,1] = st_numscalar("r(min)")
            res.vals_minmax[group_count,2] = st_numscalar("r(max)")
        }

      /* Stars & Scripts */

        if(cols(input_bd.opt.pval_stars) > 0)
        {
          res.ps_stars = J(group_count, 1, "")
          for(i=1; i<=cols(input_bd.opt.pval_stars); i++)
          {
            res.ps_stars = res.ps_stars :+ ((res.vals_povr :< input_bd.opt.pval_stars[1,i]) :* uchar(735))
          }
        }

        if(input_bd.opt.pval_scripts != .)
        {
          code_list = input_bd.code_list[1,(1..input_bd.oi.count)]
          pind = (res.vals_pind :< input_bd.opt.pval_scripts) :* code_list

          res.ps_scripts = J(group_count, 1, "")
          for(i=1; i<=cols(pind); i++)
          {
            res.ps_scripts = res.ps_scripts :+ pind[.,i]
          }
        }

      /* Logit Transform */

        if(input_bd.opt.ci_proportion == 1 & res.binary == 1)
        {
          res.vals_ci = logitTransform(res, input_bd.opt.ci_level)
        }

      return(res)
    }

  /* logitTransform() */

    real matrix logitTransform(struct results scalar input_res,
                               real           scalar ci_level)
    {
      df       = input_res.vals_df
      tterm    = (100 - ci_level) / 200
      logit_b  = logit(input_res.vals_mean)
      logit_se = input_res.vals_se :/ (input_res.vals_mean :* (1 :- input_res.vals_mean))

      lci = invlogit(logit_b :- invttail(df, tterm) :* logit_se)
      uci = invlogit(logit_b :+ invttail(df, tterm) :* logit_se)

      return((lci, uci))
    }

  end

**======================================================================**
**   Mata Structure - printer                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* printer() */

    void printer(struct braddev scalar input_bd)
    {
      if(input_bd.opt.dis_print == 0) return(J(0,0,.))

      /* Title */

        if(input_bd.opt.dis_title != "")
        {
          printf("\n{title:" + usubinstr(getTitle(input_bd), "%", "%%", .) + "}\n")
        }

      /* Legend */

        printLegend(input_bd)

      /* Table */

        printf("\n")

        if(input_bd.oi.count == 0)
        {
          printTableLongNoOver(input_bd)
        }

        if(input_bd.oi.count > 0 & input_bd.opt.dis_wide == 0)
        {
          printTableLongOver(input_bd)
        }

        if(input_bd.oi.count > 0 & input_bd.opt.dis_wide == 1)
        {
          printTableWide(input_bd)
        }

      /* Footer */

        if(input_bd.opt.dis_footer)
        {
          printFooter(input_bd)
        }

      printf("\n")
    }

  /* printLegend() */

    void printLegend(struct braddev scalar input_bd)
    {
      if(input_bd.oi.count == 0 | input_bd.opt.ovo_legend == 0) return(J(0,0,.))

      printf("\n")

      legend = "{lalign " :+ strofreal(max(udstrlen(input_bd.oi.name_short))) :+ ":" :+ input_bd.oi.name_short :+ "}"
      legend = legend :+ " {c |} " :+ invtokens(input_bd.oi.list, ", ") :+ `" = ""' :+ input_bd.oi.name_long :+ `"""'

      display(legend)
    }

  /* printTableLongNoOver() */

    void printTableLongNoOver(struct braddev scalar input_bd)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Name Width */

        name_width = max((14, udstrlen(input_bd.vi.display_name), udstrlen(input_bd.vi.display_series)))
        name_width = min((32, name_width))

      /* Linesize */

        linesize = c("linesize")
        linesize = max((140, linesize))
        linesize = min((240, linesize))

        linesize = linesize - 1

      /* Table Columns */

        table_cols = tableColumns(name_width + 2, linesize, (input_bd.st.col_length :+ 2))

      /* Header */

        table = J(2, 1 + input_bd.st.count, "")

        table[1,.] = ("{space " + strofreal(name_width + 2) + "}{c |}"), (" {" :+ al :+ " " :+ strofreal(input_bd.st.col_length) :+ ":" :+ input_bd.st.name_abbrev :+ "} ")
        table[2,.] = ("{hline " + strofreal(name_width + 2) + "}{c +}"), ("{hline " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}")

        separator = table[2,.]

      /* Variables & Statistics */

        for(i=1; i<=input_bd.vi.var_count; i++)
        {
          if(i > 1)
          {
            if(input_bd.vi.series_position[1,i] == 1)
            {
              table = table \ separator
            }

            if(input_bd.vi.series_position[1,i] == . & input_bd.vi.series_position[1,(i-1)] != .)
            {
              table = table \ separator
            }
          }

          table = table \ printResultsLongNoOver(input_bd, input_bd.res[1,i], name_width)
        }

      /* Printing */

        for(i=1; i<=rows(table_cols); i++)
        {
          if(rows(table_cols) > 1)
          {
            printf("\n(" + strofreal(i) + "/" + strofreal(rows(table_cols)) + ")\n")
          }

          print_table = table[.,1]
          for(j=table_cols[i,1]; j<=table_cols[i,2]; j++)
          {
            print_table = print_table :+ table[.,j]
          }
          display(print_table)
        }
    }

  /* printResultsLongNoOver() */

    string matrix printResultsLongNoOver(struct braddev scalar input_bd,
                                         struct results scalar input_res,
                                         real           scalar name_width)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Initializing Table */

        table = J(1, 1 + input_bd.st.count, "")

      /* Name */

        if(input_res.series_position == .)
        {
          table[1,1] = "{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.display_name, name_width) + "} {res}{c |}"
        }
        else
        {
          table[1,1] = "{txt} {ralign " + strofreal(name_width) + ":" + abbrev(input_res.display_name, name_width) + "} {res}{c |}"
        }

      /* Stats */

        for(i=1; i<=input_bd.st.count; i++)
        {
          /* Getting Stat */

            if(input_bd.st.name_short[1,i] == "obs")  values = input_res.vals_obs
            if(input_bd.st.name_short[1,i] == "nyes") values = input_res.vals_nyes
            if(input_bd.st.name_short[1,i] == "mean") values = input_res.vals_mean
            if(input_bd.st.name_short[1,i] == "se")   values = input_res.vals_se
            if(input_bd.st.name_short[1,i] == "lci")  values = input_res.vals_ci[.,1]
            if(input_bd.st.name_short[1,i] == "uci")  values = input_res.vals_ci[.,2]
            if(input_bd.st.name_short[1,i] == "ci")   values = input_res.vals_ci
            if(input_bd.st.name_short[1,i] == "sd")   values = input_res.vals_sd
            if(input_bd.st.name_short[1,i] == "var")  values = input_res.vals_var
            if(input_bd.st.name_short[1,i] == "min")  values = input_res.vals_minmax[.,1]
            if(input_bd.st.name_short[1,i] == "max")  values = input_res.vals_minmax[.,2]

          /* Percent (1) */

            if(input_bd.opt.dis_percent & input_res.binary & !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :* 100
            }

          /* Format */

            values = strofreal(values, input_bd.st.col_format[1,i])

          /* Percent (2) */

            if(input_bd.opt.dis_percent & input_res.binary * !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :+ ((values :!= ".") :* "%")
            }

          /* Specific Additions */

            if(input_bd.st.name_short[1,i] == "ci")
            {
              values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
              values = values :+ ((values :== "") :* ".")
            }

          /* Setting Field Size */

            values = " {" :+ al :+ " " :+ strofreal(input_bd.st.col_length[1,i]) :+ ":" :+ values :+ "} "

          /* Adding to Table */

            table[1,(i+1)] = values
        }

      /* Series */

        if(input_res.series_position == 1)
        {
          line = ("{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.display_series, name_width) + "} {res}{c |}"), ("{space " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}")
          table = line \ table
        }

      return(table)
    }

  /* printTableLongOver() */

    void printTableLongOver(struct braddev scalar input_bd)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Name Width */

        name_width = max((14, udstrlen(input_bd.oi.name_select')))

        if(input_bd.vi.series_count == 1)
        {
          name_width = max((name_width, udstrlen(input_bd.vi.display_name)))
        }

        name_width = min((32, name_width))

      /* Linesize */

        linesize = c("linesize")
        linesize = max((140, linesize))
        linesize = min((240, linesize))

        linesize = linesize - 1 - (input_bd.opt.pval_show > 0)

      /* Table Columns */

        p_count = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * input_bd.oi.count)

        table_cols = tableColumns(name_width + 2, linesize, ((input_bd.st.col_length :+ 2), J(1, p_count, 8)))

      /* Header */

        table = J(2 + (p_count > 0), 1 + input_bd.st.count + p_count, "")

        /* Groupings */

          line = ("{space " + strofreal(name_width + 2) + "}{c |}")
          line = line, ("{space " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}")

          p_col = 2 + input_bd.st.count

          for(i=1; i<=rows(table_cols); i++)
          {
            for(j=table_cols[i,1]; j<=table_cols[i,2]; j++)
            {
              if(j < p_col) continue

              if(j == p_col | (table_cols[i,1] > p_col & j == table_cols[i,1]))
              {
                length = ((table_cols[i,2] - j + 1) * 8) - 2
                line = line, (" {" + al + " " + strofreal(length) + ":" + abbrev("P-Values", length) + "} ")
                continue
              }

              line = line, ""
            }
          }

          if(p_count > 0) table[1,.] = line

        /* Stat */

          line = ("{space " + strofreal(name_width + 2) + "}{c |}")
          line = line, (" {" :+ al :+ " " :+ strofreal(input_bd.st.col_length) :+ ":" :+ input_bd.st.name_abbrev :+ "} ")

          if(input_bd.opt.pval_show > 0) line = line, (" {" :+ al :+ " 6:" :+ abbrev("Overall", 6) :+ "} ")
          if(input_bd.opt.pval_show == 2) line = line, (" {" :+ al :+ " 6:" :+ abbrev(("vs " :+ strofreal(range(1, input_bd.oi.count, 1)')), 6) :+ "} ")

          if(p_count > 0) table[2,.] = line
          else            table[1,.] = line

        /* Separator */

          separator = ("{hline " + strofreal(name_width + 2) + "}{c +}")
          separator = separator, ("{hline " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}")
          separator = separator, J(1, p_count, "{hline 8}")

          if(p_count > 0) table[3,.] = separator
          else            table[2,.] = separator

      /* Variables & Statistics */

        for(i=1; i<=input_bd.vi.var_count; i++)
        {
          table = table \ printResultsLongOver(input_bd, input_bd.res[1,i], name_width)

          if(i == cols(input_bd.res)) table = table \ subinstr(separator, "+", "BT")
          else                        table = table \ separator
        }

      /* Printing */

        v_separator = table[.,1]
        v_separator = substr(v_separator, strrpos(v_separator, "{"))

        p_col = 2 + input_bd.st.count

        for(i=1; i<=rows(table_cols); i++)
        {
          if(rows(table_cols) > 1)
          {
            printf("\n(" + strofreal(i) + "/" + strofreal(rows(table_cols)) + ")\n")
          }

          print_table = table[.,1]
          for(j=table_cols[i,1]; j<=table_cols[i,2]; j++)
          {
            if(j == p_col) print_table = print_table :+ v_separator
            print_table = print_table :+ table[.,j]
          }

          display(print_table)
        }

    }

  /* printResultsLongOver() */

    string matrix printResultsLongOver(struct braddev scalar input_bd,
                                       struct results scalar input_res,
                                       real           scalar name_width)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Initializing Table */

        group_count = input_bd.oi.count + input_bd.opt.ovo_total
        p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * input_bd.oi.count)

        table = J(group_count, 1 + input_bd.st.count + p_count, "")

      /* Over Names */

        over_names = input_bd.oi.name_select
        if(input_bd.opt.ovo_total) over_names = over_names \ "Total"

        table[.,1] = "{txt} {ralign " :+ strofreal(name_width) :+ ":" :+ abbrev(over_names, name_width) :+ "} {res}{c |}"

      /* Stats */

        for(i=1; i<=input_bd.st.count; i++)
        {
          /* Getting Stat */

            if(input_bd.st.name_short[1,i] == "obs")  values = input_res.vals_obs
            if(input_bd.st.name_short[1,i] == "nyes") values = input_res.vals_nyes
            if(input_bd.st.name_short[1,i] == "mean") values = input_res.vals_mean
            if(input_bd.st.name_short[1,i] == "se")   values = input_res.vals_se
            if(input_bd.st.name_short[1,i] == "lci")  values = input_res.vals_ci[.,1]
            if(input_bd.st.name_short[1,i] == "uci")  values = input_res.vals_ci[.,2]
            if(input_bd.st.name_short[1,i] == "ci")   values = input_res.vals_ci
            if(input_bd.st.name_short[1,i] == "sd")   values = input_res.vals_sd
            if(input_bd.st.name_short[1,i] == "var")  values = input_res.vals_var
            if(input_bd.st.name_short[1,i] == "min")  values = input_res.vals_minmax[.,1]
            if(input_bd.st.name_short[1,i] == "max")  values = input_res.vals_minmax[.,2]

          /* Percent (1) */

            if(input_bd.opt.dis_percent & input_res.binary & !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :* 100
            }

          /* Format */

            values = strofreal(values, input_bd.st.col_format[1,i])

          /* Percent (2) */

            if(input_bd.opt.dis_percent & input_res.binary * !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :+ ((values :!= ".") :* "%")
            }

          /* Specific Additions */

            if(input_bd.st.name_short[1,i] == "ci")
            {
              values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
              values = values :+ ((values :== "") :* ".")
            }

            if(input_bd.st.name_short[1,i] == "mean" & cols(input_res.ps_stars) > 0)
            {
              values = values :+ input_res.ps_stars
            }

            if(input_bd.st.name_short[1,i] == "ci" & cols(input_res.ps_scripts) > 0)
            {
              values = values :+ input_res.ps_scripts
            }

          /* Setting Field Size */

            values = " {" :+ al :+ " " :+ strofreal(input_bd.st.col_length[1,i]) :+ ":" :+ values :+ "} "

          /* Adding to Table */

            table[.,(i+1)] = values
        }

      /* P-Values */

        col = 2 + input_bd.st.count

        if(input_bd.opt.pval_show > 0)
        {
          table[.,col] = " {" :+ al :+ " 6:" :+ strofreal(input_res.vals_povr, "%6.4f") :+ "} "
          col = col + 1
        }

        if(input_bd.opt.pval_show == 2)
        {
          for(i=1; i<=input_bd.oi.count; i++)
          {
            table[.,col] = " {" :+ al :+ " 6:" :+ strofreal(input_res.vals_pind[.,i], "%6.4f") :+ "} "
            col = col + 1
          }
        }

      /* Variable Name */

        if(input_bd.vi.series_count == 1)
        {
          line = "{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.display_name, name_width) + "} {res}{c |}"
        }
        else
        {
          line = "{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.name_original, name_width) + "} {res}{c |}"
        }

        line = line, ("{space " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}")

        if(input_bd.opt.pval_show > 0) line = line, J(1, p_count, "{space 8}")

        table = line \ table

      return(table)
    }

  /* printTableWide() */

    void printTableWide(struct braddev scalar input_bd)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Name Width */

        name_width = max((14, udstrlen(input_bd.vi.display_name), udstrlen(input_bd.vi.display_series)))
        name_width = min((32, name_width))

      /* Linesize */

        group_count = input_bd.oi.count + input_bd.opt.ovo_total
        p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * (factorial(input_bd.oi.count)/(factorial(2) * factorial(input_bd.oi.count-2))))

        linesize = c("linesize")
        linesize = max((140, linesize))
        linesize = min((240, linesize))

        linesize = linesize - 1 - group_count - (input_bd.opt.pval_show > 0)

      /* Table Columns */

        table_cols = J(1, group_count, input_bd.st.col_length), J(1, p_count, 6)
        table_cols = tableColumns(name_width + 2, linesize, (table_cols :+ 2))

      /* Header */

        /* Over - Separate */

          if(input_bd.opt.ovo_sep == 1 & cols(input_bd.oi.list) > 1 & input_bd.opt.ovo_labels == 1)
          {
            table = J(cols(input_bd.oi.list), 1 + (group_count * input_bd.st.count) + p_count, "")
            breaks = J(cols(input_bd.oi.list), 1 + (group_count * input_bd.st.count) + p_count, .)

            for(i=1; i<=cols(input_bd.oi.list); i++)
            {
              line = "{space " + strofreal(name_width + 2) + "}"
              break_line = .

              stata("qui levelsof " + input_bd.oi.list[1,i] + " if !missing(" + st_local("group_var") + "), local(over_lvls)")
              over_lvls = strtoreal(tokens(st_local("over_lvls")))

              /* Over Variable */

                for(j=1; j<=cols(over_lvls); j++)
                {
                  stata("qui levelsof " + st_local("group_var") + " if " + input_bd.oi.list[1,i] + " == " + strofreal(over_lvls[1,j]) + ", local(curr_lvls)")
                  curr_lvls = strtoreal(tokens(st_local("curr_lvls")))

                  vec_index = vectorIndex(curr_lvls)
                  vec_index = vec_index, J(rows(vec_index), 1, over_lvls[1,j])

                  if(j == 1) index_lvls = vec_index
                  else       index_lvls = index_lvls \ vec_index
                }

                index_lvls = sort(index_lvls, 1)
                values = st_vlmap(st_varvaluelabel(input_bd.oi.list[1,i]), index_lvls[.,3])

                for(j=1; j<=rows(index_lvls); j++)
                {
                  line = line, J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, values[j,1])

                  break_col = 2 + ((index_lvls[j,1] - 1) * input_bd.st.count)
                  break_line = break_line, J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, break_col)
                }

              /* Total */

                if(input_bd.opt.ovo_total)
                {
                  if(i == cols(input_bd.oi.list)) line = line, J(1, input_bd.st.count, "Total")
                  else                            line = line, J(1, input_bd.st.count, "")

                  break_col = 2 + (input_bd.oi.count * input_bd.st.count)
                  break_line = break_line, J(1, input_bd.st.count, break_col)
                }

              /* P-Values */

                if(p_count > 0)
                {
                  if(i == cols(input_bd.oi.list)) line = line, J(1, p_count, "P-Values")
                  else                            line = line, J(1, p_count, "")

                  break_col = 2 + ((input_bd.oi.count + input_bd.opt.ovo_total) * input_bd.st.count)
                  break_line = break_line, J(1, p_count, break_col)
                }

              table[i,.] = line
              breaks[i,.] = break_line
            }
          }

        /* Over - Single */

          if(input_bd.opt.ovo_sep == 0 | cols(input_bd.oi.list) == 1 | input_bd.opt.ovo_labels == 0)
          {
            table = J(1, 1 + (group_count * input_bd.st.count) + p_count, "")
            breaks = J(1, 1 + (group_count * input_bd.st.count) + p_count, .)

            line = "{space " + strofreal(name_width + 2) + "}"
            break_line = .

            /* Over Variable */

              for(i=1; i<=input_bd.oi.count; i++)
              {
                line = line, J(1, input_bd.st.count, input_bd.oi.name_select[i,1])

                break_col = 2 + ((i - 1) * input_bd.st.count)
                break_line = break_line, J(1, input_bd.st.count, break_col)
              }

            /* Total */

              if(input_bd.opt.ovo_total)
              {
                line = line, J(1, input_bd.st.count, "Total")

                break_col = 2 + (input_bd.oi.count * input_bd.st.count)
                break_line = break_line, J(1, input_bd.st.count, break_col)
              }

            /* P-Values */

              if(p_count > 0)
              {
                line = line, J(1, p_count, "P-Values")

                break_col = 2 + ((input_bd.oi.count + input_bd.opt.ovo_total) * input_bd.st.count)
                break_line = break_line, J(1, p_count, break_col)
              }

            table[1,.] = line
            breaks[1,.] = break_line
          }

        /* Frequencies & Stats - 1 Statistic */

          if(input_bd.st.count == 1)
          {
            line = "{space " + strofreal(name_width + 2) + "}{c |}"

            line = line, J(1, input_bd.oi.count + input_bd.opt.ovo_total, (" {" :+ al :+ " " :+ strofreal(input_bd.st.col_length) :+ ":" :+ input_bd.st.name_abbrev :+ "} "))

            if(input_bd.opt.pval_show > 0)
            {
              line = line, (" {" :+ al :+ " 6:" :+ abbrev("Overall", 6) :+ "} ")
            }

            if(input_bd.opt.pval_show == 2)
            {
              for(i=1; i<=(input_bd.oi.count-1); i++)
              {
                line = line, (" {" :+ al :+ " 6:" :+ abbrev((strofreal(i) :+ "v" :+ strofreal(range(i+1, input_bd.oi.count, 1)))', 6) :+ "} ")
              }
            }

            table = table \ line
          }

        /* Frequencies & Stats - 2+ Statistics */

          if(input_bd.st.count > 1)
          {
            /* Frequencies */

              line = "{space " + strofreal(name_width + 2) + "}"

              for(i=1; i<=input_bd.oi.count; i++)
              {
                line = line, J(1, input_bd.st.count, ("(n = " + strofreal(input_bd.oi.freqs[i,1], "%9.0gc") + ")"))
              }

              if(input_bd.opt.ovo_total == 1)
              {
                line = line, J(1, input_bd.st.count, ("(n = " + strofreal(sum(input_bd.oi.freqs), "%9.0gc") + ")"))
              }

              if(input_bd.opt.pval_show > 0)
              {
                line = line, J(1, p_count, "")
              }

              table = table \ line

              breaks = breaks \ breaks[rows(breaks),.]

            /* Stats */

              line = "{space " + strofreal(name_width + 2) + "}{c |}"
              line = line, J(1, group_count, " {" :+ al :+ " " :+ strofreal(input_bd.st.col_length) :+ ":" :+ abbrev(input_bd.st.name_abbrev, input_bd.st.col_length) :+ "} ")

              if(input_bd.opt.pval_show > 0)
              {
                line = line, (" {" :+ al :+ " 6:" :+ abbrev("Overall", 6) :+ "} ")
              }

              if(input_bd.opt.pval_show == 2)
              {
                for(i=1; i<=(input_bd.oi.count-1); i++)
                {
                  line = line, (" {" :+ al :+ " 6:" :+ abbrev((strofreal(i) :+ "v" :+ strofreal(range(i+1, input_bd.oi.count, 1)))', 6) :+ "} ")
                }
              }

              table = table \ line
          }

        /* Separator */

          separator = "{hline " + strofreal(name_width + 2) + "}{c +}"
          separator = separator, J(1, group_count, ("{hline " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}"))
          separator = separator, J(1, p_count, ("{hline 8}"))

          table = table \ separator

        /* Header Row */

          header_row = rows(table) - 2
          stats_row  = header_row + 1

      /* Variables & Statistics */

        for(i=1; i<=input_bd.vi.var_count; i++)
        {
          if(i > 1)
          {
            if(input_bd.vi.series_position[1,i] == 1)
            {
              table = table \ separator
            }

            if(input_bd.vi.series_position[1,i] == . & input_bd.vi.series_position[1,(i-1)] != .)
            {
              table = table \ separator
            }
          }

          table = table \ printResultsWide(input_bd, input_bd.res[1,i], name_width)
        }

      /* Printing */

        v_separator = table[(stats_row..rows(table)),1]
        v_separator = substr(v_separator, strrpos(v_separator, "{"))

        v_pos = range(1, group_count, 1)'
        v_pos = 2 :+ (v_pos :* input_bd.st.count)

        col_size = name_width, J(1, group_count, input_bd.st.col_length), J(1, p_count, 6)
        col_size = col_size :+ 2

        for(i=1; i<=rows(table_cols); i++)
        {
          if(rows(table_cols) > 1)
          {
            printf("\n(" + strofreal(i) + "/" + strofreal(rows(table_cols)) + ")\n")
          }

          print_table = table[.,1]

          /* Header - Over */

            for(j=1; j<=header_row; j++)
            {
              for(k=table_cols[i,1]; k<=table_cols[i,2]; k++)
              {
                if(k != table_cols[i,1] & max(k :== breaks[j,.]) == 0)
                {
                  continue
                }

                /* Getting vectorIndex's of value */
                index = selectindex(k :== breaks[j,.])
                index = select(index, (index :>= table_cols[i,1]) :& (index :<= table_cols[i,2]))
                index = vectorIndex(index)

                /* If not starting a range, continue */
                if(!inlist(k, index[.,1]))
                {
                  continue
                }

                index = index[selectindex(k :== index[.,1]),.]
                width = sum(col_size[1,(index[1,1]..index[1,2])]) - 2

                /* If not P-Values, look for v_sep */
                if(table[j,k] != "P-Values")
                {
                  width = width + floor((index[1,2] - index[1,1]) / input_bd.st.count)
                }

                print_table[j,1] = print_table[j,1] :+ "{c |} {center " :+ strofreal(width) :+ ":" :+ abbrev(table[j,k], width) :+ "} "
              }
            }

          /* Other */

            for(j=table_cols[i,1]; j<=table_cols[i,2]; j++)
            {
              if(inlist(j, v_pos) & j != table_cols[i,1])
              {
                print_table[(stats_row..rows(print_table)),1] = print_table[(stats_row..rows(print_table)),1] :+ v_separator
              }

              print_table[(stats_row..rows(print_table)),1] = print_table[(stats_row..rows(print_table)),1] :+ table[(stats_row..rows(table)),j]
            }

          display(print_table)
        }
    }

  /* printResultsWide() */

    string matrix printResultsWide(struct braddev scalar input_bd,
                                   struct results scalar input_res,
                                   real           scalar name_width)
    {
      /* Alignment */

        al = input_bd.opt.dis_align

      /* Initializing Table */

        group_count = input_bd.oi.count + input_bd.opt.ovo_total
        p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * (factorial(input_bd.oi.count)/(factorial(2) * factorial(input_bd.oi.count-2))))

        table = J(group_count, input_bd.st.count, "")

      /* Stats */

        for(i=1; i<=input_bd.st.count; i++)
        {
          /* Getting Stat */

            if(input_bd.st.name_short[1,i] == "obs")  values = input_res.vals_obs
            if(input_bd.st.name_short[1,i] == "nyes") values = input_res.vals_nyes
            if(input_bd.st.name_short[1,i] == "mean") values = input_res.vals_mean
            if(input_bd.st.name_short[1,i] == "se")   values = input_res.vals_se
            if(input_bd.st.name_short[1,i] == "lci")  values = input_res.vals_ci[.,1]
            if(input_bd.st.name_short[1,i] == "uci")  values = input_res.vals_ci[.,2]
            if(input_bd.st.name_short[1,i] == "ci")   values = input_res.vals_ci
            if(input_bd.st.name_short[1,i] == "sd")   values = input_res.vals_sd
            if(input_bd.st.name_short[1,i] == "var")  values = input_res.vals_var
            if(input_bd.st.name_short[1,i] == "min")  values = input_res.vals_minmax[.,1]
            if(input_bd.st.name_short[1,i] == "max")  values = input_res.vals_minmax[.,2]

          /* Percent (1) */

            if(input_bd.opt.dis_percent & input_res.binary & !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :* 100
            }

          /* Format */

            values = strofreal(values, input_bd.st.col_format[1,i])

          /* Percent (2) */

            if(input_bd.opt.dis_percent & input_res.binary * !inlist(input_bd.st.name_short[1,i], ("obs", "nyes")))
            {
              values = values :+ ((values :!= ".") :* "%")
            }

          /* Specific Additions */

            if(input_bd.st.name_short[1,i] == "ci")
            {
              values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
              values = values :+ ((values :== "") :* ".")
            }

            if(input_bd.st.name_short[1,i] == "mean" & cols(input_res.ps_stars) > 0)
            {
              values = values :+ input_res.ps_stars
            }

            if(input_bd.st.name_short[1,i] == "ci" & cols(input_res.ps_scripts) > 0)
            {
              values = values :+ input_res.ps_scripts
            }

          /* Setting Field Size */

            values = " {" :+ al :+ " " :+ strofreal(input_bd.st.col_length[1,i]) :+ ":" :+ values :+ "} "

          /* Adding to Table */

            table[.,i] = values
        }

        table = rowshape(table, 1)

      /* P-Values */

        if(input_bd.opt.pval_show > 0)
        {
          if(nonmissing(input_res.vals_povr) == 0)
          {
            table = table, (" {" :+ al :+ " 6:" :+ strofreal(., "%6.4f") :+ "} ")
          }
          else
          {
            values = select(input_res.vals_povr, (input_res.vals_povr :!= .))[1,1]
            table = table, (" {" :+ al :+ " 6:" :+ strofreal(values, "%6.4f") :+ "} ")
          }
        }

        if(input_bd.opt.pval_show == 2)
        {
          values = input_res.vals_pind

          values = vech(values[(2..rows(values)-input_bd.opt.ovo_total),(1..(cols(values)-1))])'

          table = table, (" {" :+ al :+ " 6:" :+ strofreal(values, "%6.4f") :+ "} ")
        }

      /* Name */

        if(input_res.series_position == .)
        {
          table = ("{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.display_name, name_width) + "} {res}{c |}"), table
        }
        else
        {
          table = ("{txt} {ralign " + strofreal(name_width) + ":" + abbrev(input_res.display_name, name_width) + "} {res}{c |}"), table
        }

      /* Series */

        if(input_res.series_position == 1)
        {
          line = "{res} {lalign " + strofreal(name_width) + ":" + abbrev(input_res.display_series, name_width) + "} {res}{c |}"
          line = line, J(1, group_count, ("{space " :+ strofreal(input_bd.st.col_length :+ 2) :+ "}"))
          line = line, J(1, p_count, "{space 8}")

          table = line \ table
        }

      return(table)
    }

  /* printFooter() */

    void printFooter(struct braddev scalar input_bd)
    {
      if(inlist("mean", input_bd.st.name_short) & cols(input_bd.opt.pval_stars) > 0)
      {
        for(i=1; i<=cols(input_bd.opt.pval_stars); i++)
        {
          line = i * uchar(735) + ((cols(input_bd.opt.pval_stars) - i + 1) * " ")
          line = line + "p(overall) < 0" + strofreal(revorder(input_bd.opt.pval_stars)[1,i]) + "\n"
          printf(line)
        }
      }

      if(inlist("ci", input_bd.st.name_short) & input_bd.opt.pval_scripts != .)
      {
        table = input_bd.code_list[1,(1..input_bd.oi.count)]'
        table = table :+ `" sig. diff. from ""' :+ input_bd.oi.name_select :+ `"" (p < 0"' :+ strofreal(input_bd.opt.pval_scripts) :+ ")"
        display(table)
      }
    }

  end

**======================================================================**
**   Mata Structure - excel                                             **
**======================================================================**;

  #delimit cr

  mata:

  /* parseExcel () */

    struct excel scalar parseExcel()
    {
      struct excel scalar xl
      xl = excel()

      /* Setting Output On/Off */

        xl.output = st_local("excel") != ""
        if(xl.output == 0) return(xl)

      /* Defaults */

        xl.file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")
        xl.sheet = "Sheet1"

      /* Reading Options */

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, st_local("excel"))

        while((token = tokenget(t)) != "")
        {
          if(strpos(token, "rep")      == 1) { xl.bookreplace  = 1; continue; }
          if(strpos(token, "mod")      == 1) { xl.bookreplace  = 0; continue; }
          if(strpos(token, "sheetrep") == 1) { xl.sheetreplace = 1; continue; }

          /* File */

            if(strpos(token, "f") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")

                if(pathsuffix(token) == "")
                {
                  file_path = pathjoin(token, "bradmean_output.xlsx")
                }
                else
                {
                  if(inlist(pathsuffix(token), (".xlsx", ".xls"))) file_path = token
                  else file_path = pathrmsuffix(token) + ".xlsx"
                }

                pathsplit(file_path, path1, path2)
                if(direxists(path1) == 0)
                {
                  printf("{error:Directory does not exist, defaulting to " + `"""' + c("pwd") + `"""' + "}\n")
                  file_path = pathjoin(c("pwd"), path2)
                }

                xl.file_path = file_path
              }
            }

          /* Sheet */

            if(strpos(token, "sheet") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                xl.sheet = token
              }
            }

          /* Style */

            if(strpos(token, "style") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                if(inlist(token, ("bradfield"))) xl.style = token
              }
            }
        }

      /* Cleaning Up */

        if(xl.output == 1 & xl.bookreplace == . & xl.sheetreplace == .)
        {
          printf(`"{error:Must indicate "replace", "modify", or "sheetreplace"}\n"')
          xl.output = 0
        }

        if(xl.sheetreplace == 1)
        {
          xl.bookreplace = 0
        }

      return(xl)
    }

  /* createExcel() */

    void createExcel(struct braddev scalar input_bd)
    {
      if(input_bd.xl.output == 0) return(J(0,0,.))

      /* Initializing Object */

        class xl scalar B
        B = xl()

      /* Loading Book & Setting Worksheet */

        if(fileexists(input_bd.xl.file_path))
        {
          if(input_bd.xl.bookreplace) B.clear_book(input_bd.xl.file_path)
          B.load_book(input_bd.xl.file_path)

          if(input_bd.xl.sheet != "")
          {
            if(max(B.get_sheets() :== input_bd.xl.sheet) == 0) B.add_sheet(input_bd.xl.sheet)
            else B.set_sheet(input_bd.xl.sheet)

            if(input_bd.xl.bookreplace & input_bd.xl.sheet != "Sheet1") B.delete_sheet("Sheet1")
          }
          else
          {
            B.set_sheet(B.get_sheets()[1,1])
          }

          if(input_bd.xl.sheetreplace == 1) B.clear_sheet(B.query("sheetname"))
        }
        else
        {
          if(input_bd.xl.sheet == "") input_bd.xl.sheet = "Sheet1"
          B.create_book(input_bd.xl.file_path, input_bd.xl.sheet)
        }

      /* Getting Initial Position */

        row = 1
        col = 1

        while(row >= 1)
        {
          mat = B.get_string((row, row + 10), (col, 6))
          mat = rowmax(udstrlen(mat) :> 0)

          if(max(mat) == 0)
          {
            if(row != 1) row = row + 1
            break
          }

          maxindex(mat, 1, index, counts)
          row = row + index[rows(index),1]
        }

      /* Title */

        if(input_bd.opt.dis_title != "")
        {
          B.put_string(row, col, input_bd.opt.dis_title)
          B.set_font_bold(row, col, "on")

          if(input_bd.opt.ovo_labels == 0 & input_bd.opt.ovo_legend == 1 & input_bd.oi.count > 0)
          {
            B.set_font_underline(row, col, "on")
          }

          row = row + 1
        }

      /* Legend */

        if(input_bd.opt.ovo_labels == 0 & input_bd.opt.ovo_legend == 1 & input_bd.oi.count > 0)
        {
          legend = input_bd.oi.name_short :+ " | " :+ invtokens(input_bd.oi.list, ", ") :+ `" = ""' :+ input_bd.oi.name_long :+ `"""'
          B.put_string(row, col, legend)
          row = row + input_bd.oi.count
        }

      /* Table */

        B.set_missing("-")

        if(input_bd.oi.count == 0)
        {
          excelTableLongNoOver(input_bd, B, row)
        }

        if(input_bd.oi.count > 0 & input_bd.opt.dis_wide == 0)
        {
          excelTableLongOver(input_bd, B, row)
        }

        if(input_bd.oi.count > 0 & input_bd.opt.dis_wide == 1)
        {
          excelTableWide(input_bd, B, row)
        }
    }

  /* excelTableLongNoOver() */

    void excelTableLongNoOver(struct braddev scalar input_bd,
                              class  xl      scalar B,
                              real           scalar input_row)
    {
      sheet = B.query("sheetname")

      /* Positions */

        start_row = input_row
        end_row   = .
        row       = start_row

        start_col = 1
        end_col   = 1 + input_bd.st.count
        col       = start_col

      /* Styles (1) */

        font_bold = B.add_fontid()
        B.fontid_set_font_bold(font_bold, "on")

        /* Header - Stats */

          fmt_header_stats = B.add_fmtid()

          B.fmtid_set_fontid(fmt_header_stats, font_bold)

          B.fmtid_set_horizontal_align(fmt_header_stats, "center")

          B.fmtid_set_top_border(fmt_header_stats, "medium")
          B.fmtid_set_bottom_border(fmt_header_stats, "medium")

          B.fmtid_set_fill_pattern(fmt_header_stats, "solid", "228 223 236")

        /* Variable Names - Main */

          fmt_varname_main = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_main, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_main, "left")

          B.fmtid_set_left_border(fmt_varname_main, "medium")
          B.fmtid_set_right_border(fmt_varname_main, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_main, "solid", "238 236 225")

        /* Variable Names - Sub */

          fmt_varname_sub = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_sub, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_sub, "right")

          B.fmtid_set_text_indent(fmt_varname_sub, 1)

          B.fmtid_set_left_border(fmt_varname_sub, "medium")
          B.fmtid_set_right_border(fmt_varname_sub, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_sub, "solid", "238 236 225")

        /* Statistics */

          stats = input_bd.st.name_short

          formats = J(1,1,"")

          for(i=1; i<=cols(stats); i++)
          {
            if(stats[1,i] == "mean" & cols(input_bd.opt.pval_stars) > 0)
            {
              formats = formats, "fmt_stat_str"
              continue
            }

            if(inlist(stats[1,i], ("lci", "uci")) & input_bd.opt.dis_round != input_bd.opt.ci_round)
            {
              formats = formats, "fmt_stat_ci"
              continue
            }

            if(stats[1,i] == "obs")  { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "nyes") { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "mean") { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "se")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "lci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "uci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "ci")   { formats = formats, "fmt_stat_str"; continue; }
            if(stats[1,i] == "sd")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "var")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "min")  { formats = formats, "fmt_stat_minmax"; continue; }
            if(stats[1,i] == "max")  { formats = formats, "fmt_stat_minmax"; continue; }
          }

          formats = formats[1,(2..cols(formats))]

      /* Header */

        col = 2

        B.put_string(row, col, input_bd.st.name_long)

        B.set_fmtid(row, (col, end_col), fmt_header_stats)

        row = row + 1

      /* Variable Names & Formats */

        /* Values */

          table        = J(1,1,"")
          style        = J(1,2,"")
          format_table = formats

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            /* Series */

              if(input_bd.vi.series_position[1,i] == 1)
              {
                series = input_bd.vi.display_series[1,i]

                style = style \ ("main", "blank")
                table = table \ series

                if(input_bd.res[1,i].binary == 1 & input_bd.opt.dis_percent == 1)
                {
                  format_table = format_table \ subinstr(subinstr(formats, "fmt_stat_num", "fmt_stat_pct"), "fmt_stat_ci", "fmt_stat_ci_pct")
                }
                else
                {
                  format_table = format_table \ formats
                }
              }

            /* Variable Name */

              table = table \ input_bd.vi.display_name[1,i]

              if(input_bd.vi.series_position[1,i] == .) style = style \ ("main", "")
              else                                      style = style \ ("sub", "")

              if(input_bd.res[1,i].binary == 1 & input_bd.opt.dis_percent == 1)
              {
                format_table = format_table \ subinstr(subinstr(formats, "fmt_stat_num", "fmt_stat_pct"), "fmt_stat_ci", "fmt_stat_ci_pct")
              }
              else
              {
                format_table = format_table \ formats
              }

            /* Separator */

              if(input_bd.vi.series_position[1,i] == -1)
              {
                style[rows(style), 2] = "sep"
              }

              if(i != input_bd.vi.var_count)
              {
                if(input_bd.vi.series_position[1,i] == . & input_bd.vi.series_position[1,i+1] != .)
                {
                  style[rows(style), 2] = "sep"
                }
              }
          }

          table        = table[(2..rows(table)), .]
          style        = style[(2..rows(style)), .]
          format_table = format_table[(2..rows(format_table)), .]

          end_row = row + rows(table) - 1

          B.put_string(row, start_col, table)

        /* Style */

          if(sum(style[.,1] :== "main") > 0)
          {
            select_rows = vectorIndex(selectindex(style[.,1] :== "main")') :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_fmtid((select_rows[i,1], select_rows[i,2]), start_col, fmt_varname_main)
            }
          }

          if(sum(style[.,1] :== "sub") > 0)
          {
            select_rows = vectorIndex(selectindex(style[.,1] :== "sub")') :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_fmtid((select_rows[i,1], select_rows[i,2]), start_col, fmt_varname_sub)
            }
          }

          width = max((10 \ udstrlen(table))) + 2
          B.set_column_width(start_col, start_col, width)

      /* Statistics */

        /* Placing Values */

          excelResultsLongNoOver(input_bd, B, row, end_row)

        /* Formatting */

          /* fmt_stat_obs */

            if(sum(format_table :== "fmt_stat_obs") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_obs"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_obs"))) :+ 1

              /* Creating Format */

                fmt_stat_obs = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_obs, "center")
                B.fmtid_set_number_format(fmt_stat_obs, "#,##0")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_obs)
                  }
                }
            }

          /* fmt_stat_num */

            if(sum(format_table :== "fmt_stat_num") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_num"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_num"))) :+ 1

              /* Creating Format */

                fmt_stat_num = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_num, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0")
                B.fmtid_set_number_format(fmt_stat_num, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_num)
                  }
                }
            }

          /* fmt_stat_pct */

            if(sum(format_table :== "fmt_stat_pct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_pct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_pct"))) :+ 1

              /* Creating Format */

                fmt_stat_pct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_pct, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_pct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_pct)
                  }
                }
            }

          /* fmt_stat_ci */

            if(sum(format_table :== "fmt_stat_ci") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_ci"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_ci"))) :+ 1

              /* Creating Format */

                fmt_stat_ci = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_ci, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0")
                B.fmtid_set_number_format(fmt_stat_ci, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_ci)
                  }
                }
            }

          /* fmt_stat_cipct */

            if(sum(format_table :== "fmt_stat_cipct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_cipct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_cipct"))) :+ 1

              /* Creating Format */

                fmt_stat_cipct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_cipct, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_cipct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_cipct)
                  }
                }
            }

          /* fmt_stat_minmax */

            if(sum(formats :== "fmt_stat_minmax") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_minmax"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_minmax"))) :+ 1

              /* Creating Format */

                fmt_stat_minmax = B.add_fmtid()

                B.fmtid_set_horizontal_align(fmt_stat_minmax, "center")

                if(sum(stats :== "min") > 0) fmt = selectindex(input_bd.st.name_short :== "min")[1,1]
                else                         fmt = selectindex(input_bd.st.name_short :== "max")[1,1]

                fmt = strtoreal(substr(input_bd.st.col_format[1,fmt], -2, 1))
                fmt = "0" + ((fmt > 0) * ".") + (fmt * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_minmax, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_minmax)
                  }
                }
            }

          /* fmt_stat_str */

            if(sum(formats :== "fmt_stat_str") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_str"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_str"))) :+ 1

              /* Creating Format */

                fmt_stat_str = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_str, "center")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_minmax)
                  }
                }
            }

      /* Styles (2) */

        /* Blank Cells */

          if(sum(style[.,2] :== "blank") > 0)
          {
            select_rows = selectindex(style[.,2] :== "blank") :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.put_string(select_rows[i,1], start_col + 1, J(1, end_col - start_col, ""))
            }
          }

        /* Separator */

          if(sum(style[.,2] :== "sep") > 0)
          {
            select_rows = selectindex(style[.,2] :== "sep") :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_bottom_border((select_rows[i,1], select_rows[i,1]), (start_col, end_col), "thin")
            }
          }

        /* Overall Borders */

          B.set_border(start_row, start_col, "medium")
          B.set_bottom_border((end_row, end_row), (start_col, end_col), "medium")
          B.set_right_border((start_row, end_row), (end_col, end_col), "medium")
    }

  /* excelResultsLongNoOver() */

    void excelResultsLongNoOver(struct braddev scalar input_bd,
                                class  xl      scalar B,
                                real           scalar start_row,
                                real           scalar end_row)
    {
      /* Getting Type */

        stat_types = (input_bd.st.name_short :!= "ci") :* "numeric"

        string_pos = selectindex(stat_types :== "")

        for(i=1; i<=cols(string_pos); i++)
        {
          stat_types[1,string_pos[1,i]] = "string"
        }

      /* Creating Numeric Table */

        rows = input_bd.vi.var_count + cols(selectindex(input_bd.vi.series_position :== 1))
        cols = input_bd.st.count

        table = J(rows, cols, .)

        for(i=1; i<=input_bd.st.count; i++)
        {
          if(stat_types[1,i] == "string") continue

          row = 1
          for(j=1; j<=cols(input_bd.res); j++)
          {
            if(input_bd.res[1,j].series_position == 1) row = row + 1

            /* Getting Stat */

              if(input_bd.st.name_short[1,i] == "obs")  values = input_bd.res[1,j].vals_obs
              if(input_bd.st.name_short[1,i] == "nyes") values = input_bd.res[1,j].vals_nyes
              if(input_bd.st.name_short[1,i] == "mean") values = input_bd.res[1,j].vals_mean
              if(input_bd.st.name_short[1,i] == "se")   values = input_bd.res[1,j].vals_se
              if(input_bd.st.name_short[1,i] == "lci")  values = input_bd.res[1,j].vals_ci[.,1]
              if(input_bd.st.name_short[1,i] == "uci")  values = input_bd.res[1,j].vals_ci[.,2]
              if(input_bd.st.name_short[1,i] == "sd")   values = input_bd.res[1,j].vals_sd
              if(input_bd.st.name_short[1,i] == "var")  values = input_bd.res[1,j].vals_var
              if(input_bd.st.name_short[1,i] == "min")  values = input_bd.res[1,j].vals_minmax[.,1]
              if(input_bd.st.name_short[1,i] == "max")  values = input_bd.res[1,j].vals_minmax[.,2]

            /* Adding to Table */

              table[row,i] = values

            row = row + 1
          }
        }

        B.put_number(start_row, 2, table)

      /* Creating String Columns */

        if(cols(string_pos) > 0)
        {
          table = J(rows, 1, "")

          row = 1
          col = string_pos[1,1]

          for(j=1; j<=cols(input_bd.res); j++)
          {
            if(input_bd.res[1,j].series_position == 1) row = row + 1

            values = input_bd.res[1,j].vals_ci

            if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
            {
              values = values :* 100
            }

            values = strofreal(values, input_bd.st.col_format[1,col])

            if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
            {
              values = values :+ ((values :!= ".") :* "%")
            }

            values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
            values = values :+ ((values :== "") :* "-")

            table[row,1] = values

            row = row + 1
          }

          B.put_string(start_row, col + 1, table)
        }
    }

  /* excelTableLongOver() */

    void excelTableLongOver(struct braddev scalar input_bd,
                            class  xl      scalar B,
                            real           scalar input_row)
    {
      sheet = B.query("sheetname")

      group_count = input_bd.oi.count + input_bd.opt.ovo_total
      p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * input_bd.oi.count)

      /* Positions */

        start_row = input_row
        end_row   = input_row + (input_bd.opt.pval_show > 0) + ((1 + input_bd.oi.count + input_bd.opt.ovo_total) * input_bd.vi.var_count)
        row       = start_row

        header_row = start_row + (input_bd.opt.pval_show > 0)

        start_col = 1
        end_col   = 1 + input_bd.st.count + p_count
        col       = start_col

        p_col = 2 + input_bd.st.count

      /* Styles (1) */

        font_bold = B.add_fontid()
        B.fontid_set_font_bold(font_bold, "on")

        /* First Square */

          fmt_first_square = B.add_fmtid()

          B.fmtid_set_left_border(fmt_first_square, "medium")
          B.fmtid_set_right_border(fmt_first_square, "medium")

          B.fmtid_set_fill_pattern(fmt_first_square, "solid", "white")

        /* Header - Stats */

          fmt_header_stats = B.add_fmtid()

          B.fmtid_set_fontid(fmt_header_stats, font_bold)

          B.fmtid_set_horizontal_align(fmt_header_stats, "center")

          B.fmtid_set_fill_pattern(fmt_header_stats, "solid", "228 223 236")

        /* Header - P-Values */

          if(input_bd.opt.pval_show > 0)
          {
            fmt_header_pvals = B.add_fmtid()

            B.fmtid_set_fontid(fmt_header_pvals, font_bold)

            B.fmtid_set_horizontal_align(fmt_header_pvals, "center")

            B.fmtid_set_fill_pattern(fmt_header_pvals, "solid", "218 238 243")
          }

        /* Variable Names - Main */

          fmt_varname_main = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_main, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_main, "left")

          B.fmtid_set_left_border(fmt_varname_main, "medium")
          B.fmtid_set_right_border(fmt_varname_main, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_main, "solid", "238 236 225")

        /* Variable Names - Sub */

          fmt_varname_sub = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_sub, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_sub, "right")

          B.fmtid_set_text_indent(fmt_varname_sub, 1)

          B.fmtid_set_left_border(fmt_varname_sub, "medium")
          B.fmtid_set_right_border(fmt_varname_sub, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_sub, "solid", "238 236 225")

        /* Statistics */

          stats = input_bd.st.name_short

          formats = J(1,1,"")

          for(i=1; i<=cols(stats); i++)
          {
            if(stats[1,i] == "mean" & cols(input_bd.opt.pval_stars) > 0)
            {
              formats = formats, "fmt_stat_str"
              continue
            }

            if(inlist(stats[1,i], ("lci", "uci")) & input_bd.opt.dis_round != input_bd.opt.ci_round)
            {
              formats = formats, "fmt_stat_ci"
              continue
            }

            if(stats[1,i] == "obs")  { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "nyes") { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "mean") { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "se")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "lci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "uci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "ci")   { formats = formats, "fmt_stat_str"; continue; }
            if(stats[1,i] == "sd")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "var")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "min")  { formats = formats, "fmt_stat_minmax"; continue; }
            if(stats[1,i] == "max")  { formats = formats, "fmt_stat_minmax"; continue; }
          }

          formats = formats[1,(2..cols(formats))]

          formats = formats, J(1, p_count, "fmt_stat_pval")

      /* Header */

        /* Groupings */

          row = start_row

          if(input_bd.opt.pval_show > 0)
          {
            B.put_string(start_row, p_col, "P-Values")
            B.set_sheet_merge(sheet, (row, row), (p_col, end_col))

            row = row + 1
          }

        /* Stats */

          col = 2

          table = input_bd.st.name_long
          if(input_bd.opt.pval_show > 0)  table = table, "Overall"
          if(input_bd.opt.pval_show == 2) table = table, ("vs " :+ input_bd.oi.name_select')

          B.put_string(row, col, table)

        /* Style */

          B.set_fmtid((start_row, header_row), start_col, fmt_first_square)

          B.set_fmtid((start_row, header_row), (col, p_col - 1), fmt_header_stats)

          if(input_bd.opt.pval_show > 0)
          {
            B.set_fmtid((start_row, header_row), (p_col, end_col), fmt_header_pvals)
          }

          if(input_bd.opt.pval_show == 2)
          {
            width = max((8 \ (udstrlen("vs " :+ input_bd.oi.name_select)))) + 1
            B.set_column_width(p_col + 1, end_col, width)
          }

      /* Variable Names & Formats */

        /* Values */

          if(input_bd.vi.series_count == 1) table = input_bd.vi.display_name
          else                              table = input_bd.vi.name_original

          table = table \ J(1, input_bd.vi.var_count, input_bd.oi.name_select)
          table = table \ J(input_bd.opt.ovo_total, input_bd.vi.var_count, "Total")

          table = vec(table)

          B.put_string(header_row + 1, start_col, table)

        /* Formats */

          format_table = formats

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            if(input_bd.res[1,i].binary == 1 & input_bd.opt.dis_percent == 1)
            {
              format_table = format_table \ J(group_count + 1, 1, subinstr(subinstr(formats, "fmt_stat_num", "fmt_stat_pct"), "fmt_stat_ci", "fmt_stat_ci_pct"))
            }
            else
            {
              format_table = format_table \ J(group_count + 1, 1, formats)
            }
          }

          format_table = format_table[(2..rows(format_table)), .]

        /* Style */

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            row = header_row + 1 + ((i-1) * (input_bd.oi.count + input_bd.opt.ovo_total + 1))
            B.set_fmtid(row, start_col, fmt_varname_main)
          }

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            row_1 = header_row + 2 + ((i-1) * (input_bd.oi.count + input_bd.opt.ovo_total + 1))
            row_2 = row_1 + (input_bd.oi.count + input_bd.opt.ovo_total - 1)
            B.set_fmtid((row_1, row_2), start_col, fmt_varname_sub)
          }

          width = max((10 \ udstrlen(table))) + 2
          B.set_column_width(start_col, start_col, width)

      /* Statistics */

        /* Placing Values */

          row = header_row + 1

          excelResultsLongOver(input_bd, B, row, end_row)

        /* Formatting */

          /* fmt_stat_obs */

            if(sum(format_table :== "fmt_stat_obs") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_obs"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_obs"))) :+ 1

              /* Creating Format */

                fmt_stat_obs = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_obs, "center")
                B.fmtid_set_number_format(fmt_stat_obs, "#,##0")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_obs)
                  }
                }
            }

          /* fmt_stat_num */

            if(sum(format_table :== "fmt_stat_num") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_num"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_num"))) :+ 1

              /* Creating Format */

                fmt_stat_num = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_num, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0")
                B.fmtid_set_number_format(fmt_stat_num, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_num)
                  }
                }
            }

          /* fmt_stat_pct */

            if(sum(format_table :== "fmt_stat_pct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_pct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_pct"))) :+ 1

              /* Creating Format */

                fmt_stat_pct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_pct, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_pct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_pct)
                  }
                }
            }

          /* fmt_stat_ci */

            if(sum(format_table :== "fmt_stat_ci") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_ci"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_ci"))) :+ 1

              /* Creating Format */

                fmt_stat_ci = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_ci, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0")
                B.fmtid_set_number_format(fmt_stat_ci, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_ci)
                  }
                }
            }

          /* fmt_stat_cipct */

            if(sum(format_table :== "fmt_stat_cipct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_cipct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_cipct"))) :+ 1

              /* Creating Format */

                fmt_stat_cipct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_cipct, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_cipct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_cipct)
                  }
                }
            }

          /* fmt_stat_minmax */

            if(sum(formats :== "fmt_stat_minmax") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_minmax"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_minmax"))) :+ 1

              /* Creating Format */

                fmt_stat_minmax = B.add_fmtid()

                B.fmtid_set_horizontal_align(fmt_stat_minmax, "center")

                if(sum(stats :== "min") > 0) fmt = selectindex(input_bd.st.name_short :== "min")[1,1]
                else                         fmt = selectindex(input_bd.st.name_short :== "max")[1,1]

                fmt = strtoreal(substr(input_bd.st.col_format[1,fmt], -2, 1))
                fmt = "0" + ((fmt > 0) * ".") + (fmt * "0")
                B.fmtid_set_number_format(fmt_stat_minmax, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_minmax)
                  }
                }
            }

          /* fmt_stat_str */

            if(sum(formats :== "fmt_stat_str") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_str"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_str"))) :+ 1

              /* Creating Format */

                fmt_stat_str = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_str, "center")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_str)
                  }
                }
            }

          /* fmt_stat_pval */

            if(sum(format_table :== "fmt_stat_pval") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_pval"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_pval"))) :+ 1

              /* Creating Format */

                fmt_stat_pval = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_pval, "center")
                B.fmtid_set_number_format(fmt_stat_pval, "0.0000")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_pval)
                  }
                }
            }

      /* Footer */

        table = J(0,0,"")

        if(inlist("mean", input_bd.st.name_short) & cols(input_bd.opt.pval_stars) > 0)
        {
          for(i=1; i<=cols(input_bd.opt.pval_stars); i++)
          {
            line = i * uchar(735) + ((cols(input_bd.opt.pval_stars) - i + 1) * " ")
            line = line + "p(overall) < 0" + strofreal(revorder(input_bd.opt.pval_stars)[1,i])

            if(cols(table) == 0) table = line
            else                 table = table \ line
          }
        }

        if(inlist("ci", input_bd.st.name_short) & input_bd.opt.pval_scripts != .)
        {
          line = input_bd.code_list[1,(1..input_bd.oi.count)]'
          line = line :+ `" sig. diff. from ""' :+ input_bd.oi.name_select :+ `"" (p < 0"' :+ strofreal(input_bd.opt.pval_scripts) :+ ")"

          if(cols(table) == 0) table = line
          else                 table = table \ line
        }

        if(cols(table) > 0)
        {
          B.put_string(end_row + 1, start_col, table)
        }

      /* Styles (2) */

        /* Blank Cells */

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            row = header_row + 1 + ((i-1) * (input_bd.oi.count + input_bd.opt.ovo_total + 1))
            B.put_string(row, 2, J(1, end_col - start_col, ""))
          }

        /* Separator */

          row = header_row
          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            row = row + 1 + input_bd.oi.count + input_bd.opt.ovo_total
            B.set_bottom_border((row, row), (start_col, end_col), "thin")
          }

        /* Borders */

          B.set_top_border((start_row, start_row), (start_col, end_col), "medium")
          B.set_bottom_border((header_row, header_row), (start_col, end_col), "medium")
          B.set_bottom_border((end_row, end_row), (start_col, end_col), "medium")

          B.set_right_border((start_row, end_row), (end_col, end_col), "medium")

          if(input_bd.opt.pval_show > 0)
          {
            B.set_left_border((start_row, end_row), (p_col, p_col), "medium")
          }
    }

  /* excelResultsLongOver() */

    void excelResultsLongOver(struct braddev scalar input_bd,
                              class  xl      scalar B,
                              real           scalar start_row,
                              real           scalar end_row)
    {
      /* Getting Type */

        stat_types = (input_bd.st.name_short :!= "ci") :* "numeric"

        if(cols(input_bd.opt.pval_stars) > 0 & inlist("mean", input_bd.st.name_short))
        {
          stat_types[1,selectindex(input_bd.st.name_short :== "mean")[1,1]] = ""
        }

        string_pos = selectindex(stat_types :== "")

        for(i=1; i<=cols(string_pos); i++)
        {
          stat_types[1,string_pos[1,i]] = "string"
        }

      /* Creating Numeric Table */

        group_count = input_bd.oi.count + input_bd.opt.ovo_total
        p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * input_bd.oi.count)

        rows = (group_count + 1) * input_bd.vi.var_count
        cols = input_bd.st.count + p_count

        for(i=1; i<=cols(input_bd.res); i++)
        {
          table = J(group_count, cols, .)

          /* Stats */

            for(j=1; j<=input_bd.st.count; j++)
            {
              if(stat_types[1,j] == "string") continue

              /* Getting Stat */

                if(input_bd.st.name_short[1,j] == "obs")  values = input_bd.res[1,i].vals_obs
                if(input_bd.st.name_short[1,j] == "nyes") values = input_bd.res[1,i].vals_nyes
                if(input_bd.st.name_short[1,j] == "mean") values = input_bd.res[1,i].vals_mean
                if(input_bd.st.name_short[1,j] == "se")   values = input_bd.res[1,i].vals_se
                if(input_bd.st.name_short[1,j] == "lci")  values = input_bd.res[1,i].vals_ci[.,1]
                if(input_bd.st.name_short[1,j] == "uci")  values = input_bd.res[1,i].vals_ci[.,2]
                if(input_bd.st.name_short[1,j] == "sd")   values = input_bd.res[1,i].vals_sd
                if(input_bd.st.name_short[1,j] == "var")  values = input_bd.res[1,i].vals_var
                if(input_bd.st.name_short[1,j] == "min")  values = input_bd.res[1,i].vals_minmax[.,1]
                if(input_bd.st.name_short[1,j] == "max")  values = input_bd.res[1,i].vals_minmax[.,2]

              /* Adding to Table */

                table[.,j] = values
            }

          /* P-Values */

            if(input_bd.opt.pval_show > 0)
            {
              col = input_bd.st.count + 1

              table[.,col] = input_bd.res[1,i].vals_povr
            }

            if(input_bd.opt.pval_show == 2)
            {
              for(j=1; j<=input_bd.oi.count; j++)
              {
                table[.,(col+j)] = input_bd.res[1,i].vals_pind[.,j]
              }
            }

          /* Adding to Overall Table */

            if(i == 1) full_table = J(1, cols, .) \ table
            else       full_table = full_table \ (J(1, cols, .) \ table)
        }

        B.put_number(start_row, 2, full_table)

      /* Creating String Table */

        if(cols(string_pos) > 0)
        {
          for(i=1; i<=cols(string_pos); i++)
          {
            for(j=1; j<=cols(input_bd.res); j++)
            {
              /* Getting Stat */

                if(input_bd.st.name_short[1,string_pos[1,i]] == "mean") values = input_bd.res[1,j].vals_mean
                if(input_bd.st.name_short[1,string_pos[1,i]] == "ci")   values = input_bd.res[1,j].vals_ci

              /* Specific (Mean) */

                if(input_bd.st.name_short[1,string_pos[1,i]] == "mean" & cols(input_bd.opt.pval_stars) > 0)
                {
                  if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
                  {
                    values = values :* 100
                  }

                  values = strofreal(values, input_bd.st.col_format[1,string_pos[1,i]])

                  if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
                  {
                    values = values :+ ((values :!= ".") :* "%")
                  }

                  values = ((values :!= ".") :* values) :+ ((values :== ".") :* "-")

                  values = values :+ input_bd.res[1,j].ps_stars
                }

              /* Specific (CI) */

                if(input_bd.st.name_short[1,string_pos[1,i]] == "ci")
                {
                  if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
                  {
                    values = values :* 100
                  }

                  values = strofreal(values, input_bd.st.col_format[1,string_pos[1,i]])

                  if(input_bd.res[1,j].binary & input_bd.opt.dis_percent)
                  {
                    values = values :+ ((values :!= ".") :* "%")
                  }

                  values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
                  values = values :+ ((values :== "") :* "-")

                  if(input_bd.opt.pval_scripts != .) values = values :+ input_bd.res[1,j].ps_scripts
                }

              /* Adding to Overall Table */

                if(j == 1) full_table = "" \ values
                else       full_table = full_table \ ("" \ values)
            }

            B.put_string(start_row, 1 + string_pos[1,i], full_table)
          }
        }
    }

  /* excelTableWide() */

    void excelTableWide(struct braddev scalar input_bd,
                        class  xl      scalar B,
                        real           scalar input_row)
    {
      sheet = B.query("sheetname")

      p_count = (input_bd.opt.pval_show > 0) + (input_bd.opt.pval_show == 2) * (factorial(input_bd.oi.count)/(factorial(2) * factorial(input_bd.oi.count-2)))
      group_count = input_bd.oi.count + input_bd.opt.ovo_total

      /* Positions */

        start_row = input_row
        end_row   = .
        row       = start_row

        start_col = 1
        end_col   = 1 + (input_bd.st.count * group_count) + p_count
        col       = start_col

        p_col = 2 + (input_bd.st.count * group_count)

      /* Styles (1) */

        font_bold = B.add_fontid()
        B.fontid_set_font_bold(font_bold, "on")

        /* First Square */

          fmt_first_square = B.add_fmtid()

          B.fmtid_set_left_border(fmt_first_square, "medium")
          B.fmtid_set_right_border(fmt_first_square, "medium")

          B.fmtid_set_fill_pattern(fmt_first_square, "solid", "white")

        /* Header - Stats */

          fmt_header_stats = B.add_fmtid()

          B.fmtid_set_fontid(fmt_header_stats, font_bold)

          B.fmtid_set_horizontal_align(fmt_header_stats, "center")

          B.fmtid_set_fill_pattern(fmt_header_stats, "solid", "228 223 236")

        /* Header - P-Values */

          if(input_bd.opt.pval_show > 0)
          {
            fmt_header_pvals = B.add_fmtid()

            B.fmtid_set_fontid(fmt_header_pvals, font_bold)

            B.fmtid_set_horizontal_align(fmt_header_pvals, "center")

            B.fmtid_set_text_wrap(fmt_header_pvals, "on")

            B.fmtid_set_fill_pattern(fmt_header_pvals, "solid", "218 238 243")
          }

        /* Variable Names - Main */

          fmt_varname_main = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_main, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_main, "left")

          B.fmtid_set_left_border(fmt_varname_main, "medium")
          B.fmtid_set_right_border(fmt_varname_main, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_main, "solid", "238 236 225")

        /* Variable Names - Sub */

          fmt_varname_sub = B.add_fmtid()

          B.fmtid_set_fontid(fmt_varname_sub, font_bold)

          B.fmtid_set_horizontal_align(fmt_varname_sub, "right")

          B.fmtid_set_text_indent(fmt_varname_sub, 1)

          B.fmtid_set_left_border(fmt_varname_sub, "medium")
          B.fmtid_set_right_border(fmt_varname_sub, "medium")

          B.fmtid_set_fill_pattern(fmt_varname_sub, "solid", "238 236 225")

        /* Statistics */

          stats = input_bd.st.name_short

          formats = J(1,1,"")

          for(i=1; i<=cols(stats); i++)
          {
            if(stats[1,i] == "mean" & cols(input_bd.opt.pval_stars) > 0)
            {
              formats = formats, "fmt_stat_str"
              continue
            }

            if(inlist(stats[1,i], ("lci", "uci")) & input_bd.opt.dis_round != input_bd.opt.ci_round)
            {
              formats = formats, "fmt_stat_ci"
              continue
            }

            if(stats[1,i] == "obs")  { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "nyes") { formats = formats, "fmt_stat_obs"; continue; }
            if(stats[1,i] == "mean") { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "se")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "lci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "uci")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "ci")   { formats = formats, "fmt_stat_str"; continue; }
            if(stats[1,i] == "sd")   { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "var")  { formats = formats, "fmt_stat_num"; continue; }
            if(stats[1,i] == "min")  { formats = formats, "fmt_stat_minmax"; continue; }
            if(stats[1,i] == "max")  { formats = formats, "fmt_stat_minmax"; continue; }
          }

          formats = formats[1,(2..cols(formats))]

          formats = J(1, group_count, formats)

          formats = formats, J(1, p_count, "fmt_stat_pval")

      /* Header */

        /* Separate */

          if(input_bd.opt.ovo_sep == 1 & cols(input_bd.oi.list) > 1 & input_bd.opt.ovo_labels == 1)
          {
            width = 8

            for(i=1; i<=cols(input_bd.oi.list); i++)
            {
              stata("qui levelsof " + input_bd.oi.list[1,i] + " if !missing(" + st_local("group_var") + "), local(over_lvls)")
              over_lvls = strtoreal(tokens(st_local("over_lvls")))

              /* Over Variable */

                for(j=1; j<=cols(over_lvls); j++)
                {
                  stata("qui levelsof " + st_local("group_var") + " if " + input_bd.oi.list[1,i] + " == " + strofreal(over_lvls[1,j]) + ", local(curr_lvls)")
                  curr_lvls = strtoreal(tokens(st_local("curr_lvls")))

                  vec_index = vectorIndex(curr_lvls)
                  vec_index = vec_index, J(rows(vec_index), 1, over_lvls[1,j])

                  if(j == 1) index_lvls = vec_index
                  else       index_lvls = index_lvls \ vec_index
                }

                index_lvls = sort(index_lvls, 1)
                values = st_vlmap(st_varvaluelabel(input_bd.oi.list[1,i]), index_lvls[.,3])

                curr_width = udstrlen(values) :/ (((index_lvls[.,2] - index_lvls[.,1]) :+ 1) :* input_bd.st.count)
                width = max((width \ curr_width))

                for(j=1; j<=rows(index_lvls); j++)
                {
                  if(j == 1)
                  {
                    line = J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, values[j,1])
                    break_line = J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, index_lvls[j,1])
                  }
                  else
                  {
                    line = line, J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, values[j,1])
                    break_line = break_line, J(1, (index_lvls[j,2] - index_lvls[j,1] + 1) * input_bd.st.count, index_lvls[j,1])
                  }
                }

              /* Total */

                if(i == cols(input_bd.oi.list)) line = line, J(1, input_bd.opt.ovo_total * input_bd.st.count, "Total")
                else                            line = line, J(1, input_bd.opt.ovo_total * input_bd.st.count, "")

                if(input_bd.opt.ovo_total == 1)
                {
                  width = max((width \ (udstrlen("Total") / input_bd.st.count)))
                  break_line = break_line, J(1, input_bd.st.count, input_bd.oi.count + 1)
                }

              /* P-Values */

                if(i == cols(input_bd.oi.list)) line = line, J(1, p_count, "P-Values")
                else                            line = line, J(1, p_count, "")

                if(p_count > 0)
                {
                  break_line = break_line, J(1, p_count, input_bd.oi.count + input_bd.opt.ovo_total + 1)
                }

              if(i == 1)
              {
                table = line
                breaks = break_line
              }
              else
              {
                table = table \ line
                breaks = breaks \ break_line
              }
            }

            header_row = start_row + cols(input_bd.oi.list)
          }

        /* Single */

          if(input_bd.opt.ovo_sep == 0 | cols(input_bd.oi.list) == 1 | input_bd.opt.ovo_labels == 0)
          {
            width = 8

            /* Over Variable */

              for(i=1; i<=input_bd.oi.count; i++)
              {
                if(i == 1)
                {
                  table = J(1, input_bd.st.count, input_bd.oi.name_select[i,1])
                  breaks = J(1, input_bd.st.count, i)
                }
                else
                {
                  table = table, J(1, input_bd.st.count, input_bd.oi.name_select[i,1])
                  breaks = breaks, J(1, input_bd.st.count, i)
                }
              }

              curr_width = udstrlen(input_bd.oi.name_select) :/ input_bd.st.count
              width = max((width \ curr_width))

            /* Total */

              table = table, J(1, input_bd.opt.ovo_total * input_bd.st.count, "Total")

              if(input_bd.opt.ovo_total == 1)
              {
                width = max((width \ (udstrlen("Total") / input_bd.st.count)))
                breaks = breaks, J(1, input_bd.st.count, input_bd.oi.count + 1)
              }

            /* P-Values */

              table = table, J(1, p_count, "P-Values")

              if(p_count > 0)
              {
                breaks = breaks, J(1, p_count, input_bd.oi.count + input_bd.opt.ovo_total + 1)
              }

            header_row = start_row + 1
          }

        /* Frequencies & Stats - 1 Statistic */

          if(input_bd.st.count == 1)
          {
            line = "(n = " :+ strofreal(input_bd.oi.freqs', "%12.0gc") :+ ")"

            if(input_bd.opt.ovo_total == 1)
            {
              line = line, ("(n = " :+ strofreal(sum(input_bd.oi.freqs), "%12.0gc") :+ ")")
            }

            if(input_bd.opt.pval_show > 0)
            {
              line = line, "Overall"
            }

            if(input_bd.opt.pval_show == 2)
            {
              for(i=1; i<=input_bd.oi.count; i++)
              {
                for(j=i+1; j<=input_bd.oi.count; j++)
                {
                  line = line, (input_bd.oi.name_select[i,1] :+ char(10) :+ " vs " :+ char(10) :+ input_bd.oi.name_select[j,1])
                }
              }
            }

            table = table \ line
          }

        /* Frequencies & Stats - 2+ Statistics */

          if(input_bd.st.count > 1)
          {
            /* Frequencies */

              for(i=1; i<=input_bd.oi.count; i++)
              {
                if(i==1) line = J(1, input_bd.st.count, ("(n = " :+ strofreal(input_bd.oi.freqs[i,1], "%12.0gc") :+ ")"))
                else     line = line, J(1, input_bd.st.count, ("(n = " :+ strofreal(input_bd.oi.freqs[i,1], "%12.0gc") :+ ")"))
              }

              if(input_bd.opt.ovo_total == 1)
              {
                line = line, J(1, input_bd.st.count, ("(n = " :+ strofreal(sum(input_bd.oi.freqs), "%12.0gc") :+ ")"))
              }

              if(input_bd.opt.pval_show > 0)
              {
                line = line, J(1, p_count, "")
              }

              table = table \ line
              breaks = breaks \ breaks[rows(breaks),.]
              header_row = header_row + 1

            /* Stats */

              line = J(1, input_bd.oi.count + input_bd.opt.ovo_total, input_bd.st.name_long)

              if(input_bd.opt.pval_show > 0) line = line, "Overall"

              if(input_bd.opt.pval_show == 2)
              {
                for(i=1; i<=input_bd.oi.count; i++)
                {
                  for(j=i+1; j<=input_bd.oi.count; j++)
                  {
                    line = line, (input_bd.oi.name_select[i,1] :+ char(10) :+ " vs " :+ char(10) :+ input_bd.oi.name_select[j,1])
                  }
                }
              }

              table = table \ line
          }

        /* Style */

          col = 2
          B.put_string(start_row, col, table)

          /* Merging (2) */

            for(i=1; i<rows(table); i++)
            {
              lvls = uniqrows(breaks[i,.]')

              for(j=1; j<=rows(lvls); j++)
              {
                vec_index = vectorIndex(selectindex(breaks[i,.] :== lvls[j,1])) :+ 1

                for(k=1; k<=rows(vec_index); k++)
                {
                  row = start_row + (i-1)
                  col_1 = vec_index[k,1]
                  col_2 = vec_index[k,2]

                  B.set_sheet_merge(sheet, (row, row), (col_1, col_2))
                }
              }
            }



          /* Setting Formats */

            B.set_fmtid((start_row, header_row), start_col, fmt_first_square)

            B.set_fmtid((start_row, header_row), (start_col + 1, p_col - 1), fmt_header_stats)

            if(input_bd.opt.pval_show > 0)
            {
              B.set_fmtid((start_row, header_row), (p_col, end_col), fmt_header_pvals)
            }

          /* Height & Width */

            /* Stats */

              B.set_column_width(start_col + 1, p_col - 1, width + 1)

            /* P-Values */

              if(input_bd.opt.pval_show == 2)
              {
                width = max(udstrlen(input_bd.oi.name_select)) + 1
                B.set_column_width(p_col + 1, end_col, width)
                B.set_row_height(header_row, header_row, 45)
              }

      /* Variable Names & Formats */

        row = header_row + 1

        /* Values */

          table        = J(1,1,"")
          style        = J(1,2,"")
          format_table = formats

          for(i=1; i<=input_bd.vi.var_count; i++)
          {
            /* Series Name */

              if(input_bd.vi.series_position[1,i] == 1)
              {
                series = input_bd.vi.display_series[1,i]

                style = style \ ("main", "blank")
                table = table \ series

                if(input_bd.res[1,i].binary == 1 & input_bd.opt.dis_percent == 1)
                {
                  format_table = format_table \ subinstr(subinstr(formats, "fmt_stat_num", "fmt_stat_pct"), "fmt_stat_ci", "fmt_stat_ci_pct")
                }
                else
                {
                  format_table = format_table \ formats
                }
              }

            /* Variable Name */

              table = table \ input_bd.vi.display_name[1,i]

              if(input_bd.vi.series_position[1,i] == .) style = style \ ("main", "")
              else                                      style = style \ ("sub", "")

              if(input_bd.res[1,i].binary == 1 & input_bd.opt.dis_percent == 1)
              {
                format_table = format_table \ subinstr(subinstr(formats, "fmt_stat_num", "fmt_stat_pct"), "fmt_stat_ci", "fmt_stat_ci_pct")
              }
              else
              {
                format_table = format_table \ formats
              }

            /* Separator */

              if(input_bd.vi.series_position[1,i] == -1)
              {
                style[rows(style), 2] = "sep"
              }

              if(i != input_bd.vi.var_count)
              {
                if(input_bd.vi.series_position[1,i] == . & input_bd.vi.series_position[1,i+1] != .)
                {
                  style[rows(style), 2] = "sep"
                }
              }
          }

          table        = table[(2..rows(table)), .]
          style        = style[(2..rows(style)), .]
          format_table = format_table[(2..rows(format_table)), .]

          end_row = row + rows(table) - 1

          B.put_string(row, start_col, table)

        /* Style */

          if(sum(style[.,1] :== "main") > 0)
          {
            select_rows = vectorIndex(selectindex(style[.,1] :== "main")') :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_fmtid((select_rows[i,1], select_rows[i,2]), start_col, fmt_varname_main)
            }
          }

          if(sum(style[.,1] :== "sub") > 0)
          {
            select_rows = vectorIndex(selectindex(style[.,1] :== "sub")') :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_fmtid((select_rows[i,1], select_rows[i,2]), start_col, fmt_varname_sub)
            }
          }

          width = max((10 \ udstrlen(table))) + 2
          B.set_column_width(start_col, start_col, width)

      /* Statistics */

        /* Placing Values */

          excelResultsWide(input_bd, B, row, end_row)

        /* Formatting */

          /* fmt_stat_obs */

            if(sum(format_table :== "fmt_stat_obs") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_obs"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_obs"))) :+ 1

              /* Creating Format */

                fmt_stat_obs = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_obs, "center")
                B.fmtid_set_number_format(fmt_stat_obs, "#,##0")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_obs)
                  }
                }
            }

          /* fmt_stat_num */

            if(sum(format_table :== "fmt_stat_num") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_num"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_num"))) :+ 1

              /* Creating Format */

                fmt_stat_num = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_num, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0")
                B.fmtid_set_number_format(fmt_stat_num, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_num)
                  }
                }
            }

          /* fmt_stat_pct */

            if(sum(format_table :== "fmt_stat_pct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_pct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_pct"))) :+ 1

              /* Creating Format */

                fmt_stat_pct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_pct, "center")
                fmt = "0" + ((input_bd.opt.dis_round > 0) * ".") + (input_bd.opt.dis_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_pct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_pct)
                  }
                }
            }

          /* fmt_stat_ci */

            if(sum(format_table :== "fmt_stat_ci") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_ci"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_ci"))) :+ 1

              /* Creating Format */

                fmt_stat_ci = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_ci, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0")
                B.fmtid_set_number_format(fmt_stat_ci, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_ci)
                  }
                }
            }

          /* fmt_stat_cipct */

            if(sum(format_table :== "fmt_stat_cipct") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_cipct"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_cipct"))) :+ 1

              /* Creating Format */

                fmt_stat_cipct = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_cipct, "center")
                fmt = "0" + ((input_bd.opt.ci_round > 0) * ".") + (input_bd.opt.ci_round * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_cipct, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_cipct)
                  }
                }
            }

          /* fmt_stat_minmax */

            if(sum(formats :== "fmt_stat_minmax") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_minmax"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_minmax"))) :+ 1

              /* Creating Format */

                fmt_stat_minmax = B.add_fmtid()

                B.fmtid_set_horizontal_align(fmt_stat_minmax, "center")

                if(sum(stats :== "min") > 0) fmt = selectindex(input_bd.st.name_short :== "min")[1,1]
                else                         fmt = selectindex(input_bd.st.name_short :== "max")[1,1]

                fmt = strtoreal(substr(input_bd.st.col_format[1,fmt], -2, 1))
                fmt = "0" + ((fmt > 0) * ".") + (fmt * "0") + (input_bd.opt.dis_percent * "%")
                B.fmtid_set_number_format(fmt_stat_minmax, fmt)

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_minmax)
                  }
                }
            }

          /* fmt_stat_str */

            if(sum(formats :== "fmt_stat_str") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_str"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_str"))) :+ 1

              /* Creating Format */

                fmt_stat_str = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_str, "center")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_str)
                  }
                }
            }

          /* fmt_stat_pval */

            if(sum(format_table :== "fmt_stat_pval") > 0)
            {
              rows = vectorIndex(selectindex(rowmax(format_table :== "fmt_stat_pval"))') :+ (row - 1)
              cols = vectorIndex(selectindex(colmax(format_table :== "fmt_stat_pval"))) :+ 1

              /* Creating Format */

                fmt_stat_pval = B.add_fmtid()
                B.fmtid_set_horizontal_align(fmt_stat_pval, "center")
                B.fmtid_set_number_format(fmt_stat_pval, "0.0000")

              /* Setting Format */

                for(i=1; i<=rows(rows); i++)
                {
                  for(j=1; j<=rows(cols); j++)
                  {
                    B.set_fmtid((rows[i,1], rows[i,2]), (cols[j,1], cols[j,2]), fmt_stat_pval)
                  }
                }
            }

      /* Footer */

        table = J(0,0,"")

        if(inlist("mean", input_bd.st.name_short) & cols(input_bd.opt.pval_stars) > 0)
        {
          for(i=1; i<=cols(input_bd.opt.pval_stars); i++)
          {
            line = i * uchar(735) + ((cols(input_bd.opt.pval_stars) - i + 1) * " ")
            line = line + "p(overall) < 0" + strofreal(revorder(input_bd.opt.pval_stars)[1,i])

            if(cols(table) == 0) table = line
            else                 table = table \ line
          }
        }

        if(inlist("ci", input_bd.st.name_short) & input_bd.opt.pval_scripts != .)
        {
          line = input_bd.code_list[1,(1..input_bd.oi.count)]'
          line = line :+ `" sig. diff. from ""' :+ input_bd.oi.name_select :+ `"" (p < 0"' :+ strofreal(input_bd.opt.pval_scripts) :+ ")"

          if(cols(table) == 0) table = line
          else                 table = table \ line
        }

        if(cols(table) > 0)
        {
          B.put_string(end_row + 1, start_col, table)
        }

      /* Styles (2) */

        /* Blank Cells */

          if(sum(style[.,2] :== "blank") > 0)
          {
            select_rows = selectindex(style[.,2] :== "blank") :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.put_string(select_rows[i,1], start_col + 1, J(1, end_col - start_col, ""))
            }
          }

        /* Separator */

          if(sum(style[.,2] :== "sep") > 0)
          {
            select_rows = selectindex(style[.,2] :== "sep") :+ (row - 1)

            for(i=1; i<=rows(select_rows); i++)
            {
              B.set_bottom_border((select_rows[i,1], select_rows[i,1]), (start_col, end_col), "thin")
            }
          }

        /* Stat Borders */

          for(i=2; i<=group_count; i++)
          {
            col = 2 + ((i-1) * input_bd.st.count)
            B.set_left_border((start_row, end_row), col, "medium")
          }

        /* Overall Borders */

          B.set_top_border((start_row, start_row), (start_col, end_col), "medium")
          B.set_bottom_border((header_row - 1, header_row - 1), (start_col + 1, end_col), "thin")
          B.set_bottom_border((header_row, header_row), (start_col, end_col), "medium")
          B.set_bottom_border((end_row, end_row), (start_col, end_col), "medium")

          B.set_right_border((start_row, end_row), (end_col, end_col), "medium")

          if(input_bd.opt.pval_show > 0)
          {
            B.set_left_border((start_row, end_row), (p_col, p_col), "medium")
          }
    }

  /* excelResultsWide() */

    void excelResultsWide(struct braddev scalar input_bd,
                          class  xl      scalar B,
                          real           scalar start_row,
                          real           scalar end_row)
    {
      /* Getting Type */

        stat_types = (input_bd.st.name_short :!= "ci") :* "numeric"

        if(cols(input_bd.opt.pval_stars) > 0 & inlist("mean", input_bd.st.name_short))
        {
          stat_types[1,selectindex(input_bd.st.name_short :== "mean")[1,1]] = ""
        }

        string_pos = selectindex(stat_types :== "")

        for(i=1; i<=cols(string_pos); i++)
        {
          stat_types[1,string_pos[1,i]] = "string"
        }

      /* Creating Numeric Table */

        group_count = input_bd.oi.count + input_bd.opt.ovo_total
        p_count     = (input_bd.opt.pval_show > 0) + ((input_bd.opt.pval_show == 2) * (factorial(input_bd.oi.count)/(factorial(2) * factorial(input_bd.oi.count-2))))

        rows = group_count
        cols = input_bd.st.count

        full_table = J(0,0,.)

        for(i=1; i<=cols(input_bd.res); i++)
        {
          if(input_bd.res[1,i].series_position == 1)
          {
            if(i == 1) full_table = J(1, (group_count * input_bd.st.count) + p_count, .)
            else       full_table = full_table \ J(1, (group_count * input_bd.st.count) + p_count, .)
          }

          table = J(rows, cols, .)

          /* Stats */

            for(j=1; j<=input_bd.st.count; j++)
            {
              if(stat_types[1,j] == "string") continue

              /* Getting Stat */

                if(input_bd.st.name_short[1,j] == "obs")  values = input_bd.res[1,i].vals_obs
                if(input_bd.st.name_short[1,j] == "nyes") values = input_bd.res[1,i].vals_nyes
                if(input_bd.st.name_short[1,j] == "mean") values = input_bd.res[1,i].vals_mean
                if(input_bd.st.name_short[1,j] == "se")   values = input_bd.res[1,i].vals_se
                if(input_bd.st.name_short[1,j] == "lci")  values = input_bd.res[1,i].vals_ci[.,1]
                if(input_bd.st.name_short[1,j] == "uci")  values = input_bd.res[1,i].vals_ci[.,2]
                if(input_bd.st.name_short[1,j] == "sd")   values = input_bd.res[1,i].vals_sd
                if(input_bd.st.name_short[1,j] == "var")  values = input_bd.res[1,i].vals_var
                if(input_bd.st.name_short[1,j] == "min")  values = input_bd.res[1,i].vals_minmax[.,1]
                if(input_bd.st.name_short[1,j] == "max")  values = input_bd.res[1,i].vals_minmax[.,2]

              /* Adding to Table */

                table[.,j] = values
            }

            table = rowshape(table, 1)

          /* P-Values */

            if(input_bd.opt.pval_show > 0)
            {
              if(nonmissing(input_bd.res[1,i].vals_povr) == 0)
              {
                table = table, .
              }
              else
              {
                values = select(input_bd.res[1,i].vals_povr, (input_bd.res[1,i].vals_povr :!= .))[1,1]
                table = table, values
              }
            }

            if(input_bd.opt.pval_show == 2)
            {
              values = input_bd.res[1,i].vals_pind

              values = vech(values[(2..rows(values)-input_bd.opt.ovo_total),(1..(cols(values)-1))])'

              table = table, values
            }

          /* Adding to Full Table */

            if(cols(full_table) == 0) full_table = table
            else                      full_table = full_table \ table
        }

        B.put_number(start_row, 2, full_table)

      /* Creating String Table */

        full_table = J(0,0,"")

        if(cols(string_pos) > 0)
        {
          for(i=1; i<=cols(input_bd.res); i++)
          {
            if(input_bd.res[1,i].series_position == 1)
            {
              if(i == 1) full_table = J(1, (group_count * input_bd.st.count), "")
              else       full_table = full_table \ J(1, (group_count * input_bd.st.count), "")
            }

            table = J(rows, cols, "")

            /* Stats */

              for(j=1; j<=input_bd.st.count; j++)
              {
                if(stat_types[1,j] != "string") continue

                /* Getting Stat */

                  if(input_bd.st.name_short[1,j] == "mean") values = input_bd.res[1,i].vals_mean
                  if(input_bd.st.name_short[1,j] == "ci")   values = input_bd.res[1,i].vals_ci

                /* Specific (Mean) */

                  if(input_bd.st.name_short[1,j] == "mean" & cols(input_bd.opt.pval_stars) > 0)
                  {
                    if(input_bd.res[1,i].binary & input_bd.opt.dis_percent)
                    {
                      values = values :* 100
                    }

                    values = strofreal(values, input_bd.st.col_format[1,j])

                    if(input_bd.res[1,i].binary & input_bd.opt.dis_percent)
                    {
                      values = values :+ ((values :!= ".") :* "%")
                    }

                    values = ((values :!= ".") :* values) :+ ((values :== ".") :* "-")

                    values = values :+ input_bd.res[1,i].ps_stars
                  }

                /* Specific (CI) */

                  if(input_bd.st.name_short[1,j] == "ci")
                  {
                    if(input_bd.res[1,i].binary & input_bd.opt.dis_percent)
                    {
                      values = values :* 100
                    }
                    values = strofreal(values, input_bd.st.col_format[1,j])

                    if(input_bd.res[1,i].binary & input_bd.opt.dis_percent)
                    {
                      values = values :+ ((values :!= ".") :* "%")
                    }

                    values = (values[.,1] :!= ".") :* (substr(input_bd.opt.ci_notation,1,1) :+ values[.,1] :+ input_bd.opt.ci_separator :+ values[.,2] :+ substr(input_bd.opt.ci_notation,2,1))
                    values = values :+ ((values :== "") :* "-")

                    if(input_bd.opt.pval_scripts != .) values = values :+ input_bd.res[1,i].ps_scripts
                  }

                /* Adding to Table */

                  table[.,j] = values
              }

            table = rowshape(table, 1)

            /* Adding to Full Table */

              if(cols(full_table) == 0) full_table = table
              else                      full_table = full_table \ table
          }

          vec_index = vectorIndex(selectindex(J(1, group_count, stat_types) :== "string"))

          for(i=1; i<=rows(vec_index); i++)
          {
            B.put_string(start_row, 1 + vec_index[i,1], full_table[.,(vec_index[i,1]..vec_index[i,2])])
          }
        }
    }

  end

**======================================================================**
**   Mata Functions - general                                           **
**======================================================================**;

  #delimit cr

  mata:

  /* commonString() */

    string scalar commonString(string rowvector input_strings)
    {
      /* Longest String */

        longest = udstrlen(input_strings)
        maxindex(longest, 1, start, range)

        longest = input_strings[1,start[1,1]]

      /* Prefix */

        prefix = 0
        for(i=udstrlen(longest); i>=1; i--)
        {
          if(allof(substr(input_strings, 1, i), substr(longest, 1, i)))
          {
            prefix = i
            break
          }
        }

      /* Suffix */

        suffix = 0
        for(i=udstrlen(longest); i>=1; i--)
        {
          if(allof(substr(strreverse(input_strings), 1, i), substr(strreverse(longest), 1, i)))
          {
            suffix = i
            break
          }
        }

      /* Setting Name */

        if(prefix == 0 & suffix == 0)
        {
          return(invtokens(input_strings, ","))
        }

        series = ""
        if(prefix > 0) series = series + substr(longest, 1, prefix)
        series = series + "*"
        if(suffix > 0) series = series + strreverse(substr(strreverse(longest), 1, suffix))

      return(series)
    }

  /* inlist() */

    real rowvector inlist(transmorphic rowvector needle,
                          transmorphic matrix    haystack)
    {
      values = J(1, cols(needle), .)

      for(i=1; i<=cols(needle); i++)
      {
        values[1,i] = max(haystack :== needle[1,i])
      }

      return(values)
    }

  /* vectorIndex() */

    real matrix vectorIndex(real matrix input_vec)
    {
      real matrix vec_index

      vec_index = input_vec[1,1], input_vec[1,1]

      pos = 1
      for(i=2; i<=cols(input_vec); i++)
      {
        if(input_vec[1,i] == input_vec[1,i-1] + 1) { vec_index[pos,2] = input_vec[1,i]; }
        else                                       { vec_index = vec_index \ (input_vec[1,i], input_vec[1,i]); pos = pos + 1; }
      }

      return(vec_index)
    }

  /* getTitle() */

    string scalar getTitle(struct braddev scalar input_bd)
    {
      title = ""

      /* None */

        if(input_bd.opt.dis_title == "none")
        {
          return(title)
        }

      /* Pre-selected Title */

        if(input_bd.opt.dis_title != "")
        {
          title = input_bd.opt.dis_title
          return(title)
        }

      /* 1 Variable */

        if(input_bd.vi.var_count == 1)
        {
          title = input_bd.vi.name_original[1,1] + " - " + st_varlabel(input_bd.vi.name_original[1,1])
          return(title)
        }

      /* 1 Series (XI or Series) */

        if(input_bd.vi.series_count == 1)
        {
          title = input_bd.vi.series_name[1,1] + " - " + input_bd.vi.display_series[1,1]
          return(title)
        }

      /* Multiple Series */

        st_local("series_list", invtokens(char(34) :+ input_bd.vi.series_name :+ char(34)))
        stata("local series_list : list uniq series_list")

        title = invtokens(tokens(st_local("series_list")), ", ")

      return(title)
    }

  /* tableColumns() */

    real matrix tableColumns(real scalar name_width,
                             real scalar linesize,
                             real matrix columns)
    {
      table_list = 1, .
      table_size = name_width + columns[1,1]

      row = 1
      for(i=2; i<=cols(columns); i++)
      {
        if((table_size + columns[1,i]) >= linesize)
        {
          table_list = table_list \ (i,.)
          table_size = name_width + columns[1,i]
          row = row + 1
        }
        else
        {
          table_list[row,2] = i
          table_size        = table_size + columns[1,i]
        }
      }

      if(table_list[rows(table_list),2] == .)
      {
        table_list[rows(table_list),2] = cols(columns)
      }

      table_list = table_list :+ 1

      return(table_list)
    }

  end
