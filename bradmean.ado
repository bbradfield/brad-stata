version 14.0
#delimit;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Computes multiple independent means in single table  **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.5.4                                                **
**   Date:         03/12/2019                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions                                                    */
/*======================================================================*/

  program define bradmean, rclass sortpreserve byable(recall);
  syntax varlist(fv) [if] [in],
    [
      SVY
      SUBpop(varname numeric)

      OVER(varlist)
      OVEROPT(string)
      TEST(string)

      DISplay(string)
      TITLE(string)
      SORT(string)
      STats(string)
      FORMAT(string)
      EXCEL(string)
    ];

  *----------------------------------------------------------*
  *   01. Creating Sample Marker                             *
  *----------------------------------------------------------*;

    tempname touse;

    mark `touse' `if' `in';

    if(_by())
    {;
      quietly replace `touse' = 0 if `_byindex' != _byindex();
    };

  *----------------------------------------------------------*
  *   02. Using Mata Functions                               *
  *----------------------------------------------------------*;

    mata: bd     = braddev();
    mata: bd.opt = getOptions();
    mata: bd.vi  = getVarInfo(bd);
    mata: bd.oi  = getOverInfo(bd);
    mata: bd.si  = getStatInfo(bd);
    mata: gatherResults(bd);
    mata: printer(bd);
    mata: createExcel(bd);

  *----------------------------------------------------------*
  *   03. Cleaning Up                                        *
  *----------------------------------------------------------*;

    mata: mata drop bd;

end;

#delimit cr

mata:

/*======================================================================*/
/*   Mata Functions - general                                           */
/*======================================================================*/

  /* function : inlist() */

    real matrix inlist(transmorphic matrix needle,
                       transmorphic matrix haystack,
                      |real         scalar sums)
    {
      rows = rows(needle)
      cols = cols(needle)

      /* Summary */

        if(args() == 3)
        {
          for(i=rows; i; i--)
          {
            for(j=cols; j; j--)
            {
              if(anyof(haystack, needle[i,j])) return(1)
            }
          }
          return(0)
        }

      /* All */

        values = J(rows, cols, .)

        for(i=rows; i; i--)
        {
          for(j=cols; j; j--)
          {
            values[i,j] = anyof(haystack, needle[i,j])
          }
        }

      return(values)
    }

  /* function : rangex() */

    real colvector rangex(real scalar start,
                          real scalar steps,
                          real scalar interval)
    {
      return(range(start, start + ((steps - 1) * interval), interval))
    }

/*======================================================================*/
/*   Mata Structure - braddev                                           */
/*======================================================================*/

  /* struct : braddev */

    struct braddev
    {
      struct options  scalar    opt
      struct varInfo  rowvector vi
      struct overInfo scalar    oi
      struct statInfo scalar    si
    }

/*======================================================================*/
/*   Mata Structure - options                                           */
/*======================================================================*/

  /* struct : options */

    struct options
    {
      struct options_display scalar display
      struct options_over    scalar over
      struct options_test    scalar test
      struct options_weight  scalar weight
      struct options_excel   scalar excel
    }

  /* struct : options_display */

    struct options_display
    {
      /* Xi & Series */
      real   scalar xi_values,     xi_variables
      real   scalar series_values, series_variables

      /* Sort */
      string scalar sort_direction, sort_statistic

      /* Table */
      real   scalar print
      real   scalar separator
      real   scalar statnames
      real   scalar wide
      string scalar align
    }

  /* struct : options_over */

    struct options_over
    {
      /* Estimation */
      real scalar miss
      real scalar total

      /* Display */
      real scalar group_n
      real scalar labels
      real scalar legend
    }

  /* struct : options_test */

    struct options_test
    {
      /* P-Values */
      real   scalar    overall, individual, statistic

      /* Chi2 Test */
      real   scalar    chi_overall

      /* T-Test */
      real   scalar    t_overall, t_individual

      /* F-Test */
      real   scalar    f_overall, f_individual
      string scalar    f_mtest

      /* Significance Notation */
      real   scalar    footer
      real   scalar    force
      real   rowvector stars
      real   scalar    scripts
      string rowvector letters
    }

  /* struct : options_weight */

    struct options_weight
    {
      real   scalar survey
      string scalar subpop
    }

  /* struct : options_excel */

    struct options_excel
    {
      /* Command Information */
      real   scalar output
      real   scalar bookreplace, sheetreplace

      /* File Information */
      string scalar file_path, sheet

      /* Style Information */
      string rowvector color
      string scalar    font_face
      real   scalar    font_size
    }

  /* function : getOptions() */

    struct options scalar getOptions()
    {
      struct options scalar opt

      /* Getting Options */

        opt.display = getOptionsDisplay()
        opt.over    = getOptionsOver()
        opt.test    = getOptionsTest()
        opt.weight  = getOptionsWeight()
        opt.excel   = getOptionsExcel()

      /* Cleaning Up */

        if(opt.weight.survey) opt.test.chi_overall = 0

      return(opt)
    }

  /* function : getOptionsDisplay() */

    struct options_display scalar getOptionsDisplay()
    {
      struct options_display scalar display

      /* Defaults */

        /* Xi & Series */
        display.xi_values     = display.xi_variables     = 1
        display.series_values = display.series_variables = 0

        /* Sort */
        display.sort_direction = "-"
        display.sort_statistic = ""

        /* Table */
        display.print     = 1
        display.wide      = 0
        display.separator = 1
        display.statnames = 1
        display.align     = "lalign"

      /* Parsing 'display' */

        input_string = st_local("display")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          /* Xi & Series */

            if(strpos(token, "xival") != 0) { display.xi_values    = strpos(token, "no") != 1; continue; }
            if(strpos(token, "xivar") != 0) { display.xi_variables = strpos(token, "no") != 1; continue; }
            if(strpos(token, "xi")    != 0) { display.xi_variables = display.xi_values = strpos(token, "no") != 1; continue; }

            if(strpos(token, "seriesval") != 0) { display.series_values    = strpos(token, "no") != 1; continue; }
            if(strpos(token, "seriesvar") != 0) { display.series_variables = strpos(token, "no") != 1; continue; }
            if(strpos(token, "series")    != 0) { display.series_variables = display.series_values = strpos(token, "no") != 1; continue; }

          /* Table */

            if(strpos(token, "noprint") != 0) { display.print     = 0; continue; }
            if(strpos(token, "nosep")   != 0) { display.separator = 0; continue; }
            if(strpos(token, "nostat")  != 0) { display.statnames = 0; continue; }

            if(token == "wide") { display.wide = 1; continue; }

            if(strpos(token, "al") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)

                if(strpos(token, "l") == 1)      display.align = "lalign"
                else if(strpos(token, "c") == 1) display.align = "center"
                else if(strpos(token, "r") == 1) display.align = "ralign"
              }
            }
        }

      /* Parsing 'sort' */

        input_string = strlower(st_local("sort"))

        if(substr(input_string, 1, 1) == "+" | substr(input_string, 1, 1) == "-")
        {
          display.sort_direction = substr(input_string, 1, 1)
          input_string = substr(input_string, 2)
        }

        if(inlist(input_string, tokens("obs nyes mean se sd var min max"), 1))
        {
          display.sort_statistic = input_string
        }

      return(display)
    }

  /* function : getOptionsOver() */

    struct options_over scalar getOptionsOver()
    {
      struct options_over scalar over

      /* Defaults */

        /* Estimation */
        over.miss  = 1
        over.total = 0

        /* Display */
        over.group_n = 0
        over.labels  = 1
        over.legend  = 1

      /* Parsing */

        input_string = st_local("overopt")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          /* Estimation */

            if(strpos(token, "nomi") != 0) { over.miss  = 0; continue; }
            if(strpos(token, "tot")  != 0) { over.total = 1; continue; }

          /* Display */

            if(strpos(token, "group") != 0) { over.group_n = 1; continue; }
            if(strpos(token, "nolab") != 0) { over.labels  = 0; continue; }
            if(strpos(token, "noleg") != 0) { over.legend  = 0; continue; }
        }

      return(over)
    }

  /* function : getOptionsTest() */

    struct options_test scalar getOptionsTest()
    {
      struct options_test scalar test

      /* Defaults */

        /* P-Values */
        test.overall    = 0
        test.individual = 0
        test.statistic  = 0

        /* F-Test */
        test.f_overall    = 0
        test.f_individual = 0
        test.f_mtest      = "noadjust"

        /* Chi2 Test */
        test.chi_overall = 0

        /* T-Test */
        test.t_overall    = 0
        test.t_individual = 0

        /* Significance Notation */
        test.footer  = 1
        test.force   = 0
        test.stars   = J(0, 0, .)
        test.scripts = .
        test.letters = uchar((7468,7470,7472,7473,7475,7476,7477,7478,7479,7480,7481,7482,7484,7486,7487,7488,7489,7490))

      /* Parsing */

        input_string = st_local("test")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          /* P-Values */

            if(strpos(token, "stat")  != 0) { test.statistic = 1; continue; }
            if(strpos(token, "foot")  == 1) { test.footer    = 1; continue; }
            if(strpos(token, "nofo")  == 1) { test.footer    = 0; continue; }
            if(strpos(token, "force") == 1) { test.force     = 1; continue; }

          /* F-Test */

            if(strpos(token, "ftest") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                sub_option = tokenget(t)
                sub_option = substr(sub_option, 2, strlen(sub_option) - 2)

                input_string = tokenrest(t)

                tokenset(t, sub_option)

                while((token = tokenget(t)) != "")
                {
                  if(strpos(token, "a") == 1) { test.f_overall    = test.f_individual = 1; continue; }
                  if(strpos(token, "o") == 1) { test.f_overall    = 1; continue; }
                  if(strpos(token, "i") == 1) { test.f_individual = 1; continue; }

                  if(token == "mtest")
                  {
                    if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                    {
                      token = tokenget(t)
                      token = substr(token, 2, strlen(token) - 2)
                      if(strpos(token, "b") == 1) { test.f_mtest = "bonferroni"; continue; }
                      if(strpos(token, "h") == 1) { test.f_mtest = "holm";       continue; }
                      if(strpos(token, "s") == 1) { test.f_mtest = "sidak";      continue; }
                    }
                    continue
                  }
                }

                tokenset(t, input_string)
              }
              else
              {
                test.f_overall = 1
              }
              continue
            }

          /* Chi2 Test */

            if(strpos(token, "chi") == 1) { test.chi_overall = 1; continue; }

          /* T-Test */

            if(strpos(token, "ttest") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                if(strpos(token, "a") == 1) { test.t_overall    = test.t_individual = 1; continue; }
                if(strpos(token, "o") == 1) { test.t_overall    = 1; continue; }
                if(strpos(token, "i") == 1) { test.t_individual = 1; continue; }
              }
              else
              {
                test.t_overall = 1
              }
              continue
            }

          /* Significance Notation */

            if(strpos(token, "star") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)

                values = strtoreal(tokens(token))
                values = select(values, ((values :> 0) :& (values :< 1)))
                values = sort(values', 1)'

                if(cols(values) > 3) values = values[1..3]

                if(cols(values) != 0) test.stars = values
                else                  test.stars = (0.01, 0.05)
              }
              else
              {
                test.stars = (0.01, 0.05)
              }
              continue
            }

            if(strpos(token, "script") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)

                values = strtoreal(tokens(token))
                values = select(values, ((values :> 0) :& (values :< 1)))

                if(cols(values) != 0) test.scripts = min(values)
                else                  test.scripts = 0.05
              }
              else
              {
                test.scripts = 0.05
              }
              continue
            }
        }

      /* Cleaning Up */

        if(test.t_overall)                               test.f_overall = 0
        else if(cols(test.stars) > 0 | test.chi_overall) test.f_overall = 1

        if(test.t_individual)      test.f_individual = 0
        else if(test.scripts != .) test.f_individual = 1

        test.overall    = (test.chi_overall | test.t_overall | test.f_overall)
        test.individual = (test.t_individual | test.f_individual)

      return(test)
    }

  /* function : getOptionsWeight() */

    struct options_weight scalar getOptionsWeight()
    {
      struct options_weight scalar weight

      /* Defaults */

        weight.survey = 0
        weight.subpop = ""

      /* Parsing */

        if(st_local("subpop") != "")
        {
          weight.survey = 1
          weight.subpop = st_local("subpop")
        }
        else if(st_local("svy") != "")
        {
          weight.survey = 1
        }

      /* Cleaning Up */

        if(weight.survey)
        {
          rc = _stata("_svy_newrule", 1)

          if(rc != 0) { errprintf("{error:Data is not svyset}\n"); exit(119); }
        }

        if(weight.subpop != "")
        {
          stata("cap count if !missing(" + weight.subpop + ") & (" + weight.subpop + " != 1) & (" + weight.subpop + " != 0)")

          if(st_numscalar("r(N)") != 0) { errprintf("{error:Subpop variable must be a 0/1 variable}\n"); exit(119); }
        }

        if(weight.survey) stata("svymarkout " + st_local("touse"))

      return(weight)
    }

  /* function : getOptionsExcel() */

    struct options_excel scalar getOptionsExcel()
    {
      struct options_excel scalar excel

      /* Defaults */

        /* Command Information */
        excel.output      = 0
        excel.bookreplace = excel.sheetreplace = 0

        /* File Information */
        excel.file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")

        /* Style Information */
        excel.color     = ("228 223 236", "238 236 225")
        excel.font_face = "Calibri"
        excel.font_size = 11

      /* Parsing */

        excel.output = st_local("excel") != ""

        input_string = st_local("excel")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          if(strpos(token, "rep")      == 1) { excel.bookreplace  = 1; continue; }
          if(strpos(token, "mod")      == 1) { excel.bookreplace  = excel.sheetreplace = 0; continue; }
          if(strpos(token, "sheetrep") == 1) { excel.sheetreplace = 1; continue; }

          /* File */

            if(strpos(token, "file") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                if(pathsuffix(token) == "")
                {
                  file_path = pathjoin(token, "bradmean_output.xlsx")
                }
                else
                {
                  if(pathsuffix(token) == ".xlsx" | pathsuffix(token) == ".xls") file_path = token
                  else                                                           file_path = pathrmsuffix(token) + ".xlsx"
                }

                pathsplit(file_path, path1, path2)
                if(!direxists(path1))
                {
                  printf("{error:Directory does not exist, defaulting to " + `"""' + c("pwd") + `"""' + "}\n")
                  file_path = pathjoin(c("pwd"), path2)
                }

                excel.file_path = file_path
              }
              continue
            }

          /* Sheet */

            if(strpos(token, "sheet") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                excel.sheet = token
              }
              continue
            }

          /* Colors */

            if(strpos(token, "color") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)

                if(token == "bradfield")       { excel.color = ("228 223 236", "238 236 225"); continue; }
                if(token == "material_red")    { excel.color = ("255 235 238", "235 255 252"); continue; }
                if(token == "material_purple") { excel.color = ("243 229 245", "231 245 229"); continue; }
                if(token == "material_indigo") { excel.color = ("232 234 246", "246 244 232"); continue; }
                if(token == "material_blue")   { excel.color = ("227 242 253", "253 238 227"); continue; }
                if(token == "material_green")  { excel.color = ("232 245 233", "245 232 244"); continue; }
                if(token == "material_orange") { excel.color = ("255 243 224", "224 236 255"); continue; }
                if(token == "monochrome")      { excel.color = ("255 255 255", "255 255 255"); continue; }
                if(token == "rti")             { excel.color = ("204 220 233", "233 217 204"); continue; }
              }
              continue
            }

          /* Font Face */

            if(strpos(token, "font") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)
                token = strlower(token)

                if(token == "arial")     { excel.font_face = "Arial";           continue; }
                if(token == "calibri")   { excel.font_face = "Calibri";         continue; }
                if(token == "garamond")  { excel.font_face = "Garamond";        continue; }
                if(token == "helvetica") { excel.font_face = "Helvetica";       continue; }
                if(token == "tnr")       { excel.font_face = "Times New Roman"; continue; }
                if(token == "verdana")   { excel.font_face = "Verdana";         continue; }
              }
              continue
            }

          /* Font Size */

            if(strpos(token, "size") == 1)
            {
              if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
              {
                token = tokenget(t)
                token = substr(token, 2, strlen(token) - 2)
                token = subinstr(token, `"""', "", .)
                token = strtoreal(token)

                if(token >= 9 & token <= 12) excel.font_size = token
              }
              continue
            }
        }

      return(excel)
    }

/*======================================================================*/
/*   Mata Structure - varInfo                                           */
/*======================================================================*/

  /* struct : varInfo */

    struct varInfo
    {
      /* Name */
      string scalar    term
      string rowvector varlist

      /* Information */
      string scalar type
      real   scalar binary

      /* Description */
      string rowvector labels
      real   rowvector levels
      string rowvector answers
      string scalar    question

      /* Results */
      struct results scalar results
    }

  /* function : getVarInfo() */

    struct varInfo rowvector getVarInfo(struct braddev scalar bd)
    {
      struct varInfo rowvector vi

      /* Cleaning Tokenlist */

        varlist = st_local("0")
        varlist = subinstr(varlist, ",", " , ")
        varlist = subinstr(varlist, "#", " # ")

        tokens = tokens(varlist)

        if(anyof(tokens, "if"))
        {
          pos = selectindex(tokens :== "if")[1] - 1
          tokens = tokens[1..pos]
        }

        if(anyof(tokens, "in"))
        {
          pos = selectindex(tokens :== "in")[1] - 1
          tokens = tokens[1..pos]
        }

        if(anyof(tokens, ","))
        {
          pos = selectindex(tokens :== ",")[1] - 1
          tokens = tokens[1..pos]
        }

        tokens = ((strpos(tokens, ".") :!= 0) :* "i.") :+ substr(tokens, strpos(tokens, ".") :+ 1)

      /* Getting Information */

        cols = cols(tokens)

        vi  = varInfo(1, cols)
        pos = J(1, cols, 1)

        for(i=cols; i; i--)
        {
          /* Term */

            vi[i].term = tokens[i]

          /* Varlist */

            stata("cap ds " + subinstr(vi[i].term, "i.", "") + ", has(type numeric)")
            vi[i].varlist = tokens(st_global("r(varlist)"))

            if(cols(vi[i].varlist) == 0)
            {
              pos[i] = 0
              continue
            }

          /* Type */

            vi[i].type = (cols(vi[i].varlist) > 1) ? "series" : (strpos(vi[i].term, "i.") == 1 ? "xi" : "individual")

          /* Other Information */

            if(vi[i].type == "individual")  vi[i] = getVarInfoIndividual(bd, vi[i])
            else if(vi[i].type == "xi")     vi[i] = getVarInfoXi(bd, vi[i])
            else if(vi[i].type == "series") vi[i] = getVarInfoSeries(bd, vi[i])
        }

      /* Limiting Informaton */

        vi = select(vi, pos)

      return(vi)
    }

  /* function : getVarInfoIndividual() */

    struct varInfo scalar getVarInfoIndividual(struct braddev scalar bd,
                                               struct varInfo scalar input_vi)
    {
      struct varInfo scalar vi
      vi = input_vi

      /* Binary */

        stata("cap count if !missing(" + vi.varlist[1] + ") & (" + vi.varlist[1] + " != 1) & (" + vi.varlist[1] + " != 0)")
        vi.binary = st_numscalar("r(N)") == 0

      /* Label */

        vi.labels = st_varlabel(vi.varlist[1])

      /* Question */

        vi.question = (vi.labels == "") ? vi.varlist : vi.labels

      /* Answer */

        vi.answers = bd.opt.display.series_variables ? vi.question : vi.varlist

      return(vi)
    }

  /* function : getVarInfoXi() */

    struct varInfo scalar getVarInfoXi(struct braddev scalar bd,
                                       struct varInfo scalar input_vi)
    {
      struct varInfo scalar vi
      vi = input_vi

      /* Binary */

        vi.binary = 1

      /* Label */

        vi.labels = st_varlabel(vi.varlist)

      /* Levels */

        stata("cap levelsof " + vi.varlist + ", local(levels)")

        vi.levels = strtoreal(tokens(st_local("levels")))

      /* Answers */

        varlab = st_varvaluelabel(vi.varlist)

        if(bd.opt.display.xi_values & varlab != "") vi.answers = st_vlmap(varlab, vi.levels)
        else                                        vi.answers = vi.varlist :+ " == " :+ strofreal(vi.levels)

        pos = selectindex(vi.answers :== "")
        if(length(pos) > 0) vi.answers[pos] = vi.varlist :+ " == " :+ strofreal(vi.levels[pos])

      /* Question */

        vi.question = (bd.opt.display.xi_variables & vi.labels != "") ? vi.labels : vi.term

      return(vi)
    }

  /* function : getVarInfoSeries() */

    struct varInfo scalar getVarInfoSeries(struct braddev scalar bd,
                                           struct varInfo scalar input_vi)
    {
      struct varInfo scalar vi
      vi = input_vi

      /* Binary */

        vi.binary = 1

        cols = cols(vi.varlist)

        for(i=cols; i; i--)
        {
          stata("cap count if !missing(" + vi.varlist[i] + ") & (" + vi.varlist[i] + " != 1) & (" + vi.varlist[i] + " != 0)")

          if(st_numscalar("r(N)") > 0) { vi.binary = 0; break; }
        }

      /* Labels */

        vi.labels = J(1, cols, "")

        for(i=cols; i; i--)
        {
          vi.labels[i] = st_varlabel(vi.varlist[i])
        }

      /* Answers */

        vi.answers = bd.opt.display.series_values ? substr(vi.labels, strpos(vi.labels, "[") :+ 1, strpos(vi.labels, "]") :- 2) : vi.varlist

        pos = selectindex(vi.answers :== "")
        if(length(pos) > 0) vi.answers[pos] = vi.varlist[pos]

      /* Question */

        if(!bd.opt.display.series_variables)
        {
          vi.question = vi.term
        }
        else
        {
          labels = strtrim(substr(vi.labels, strpos(vi.labels, "]") :+ 1))

          vi.question = select(labels, udstrlen(labels) :== max(udstrlen(labels)))[1]
        }

      return(vi)
    }

/*======================================================================*/
/*   Mata Structure - overInfo                                          */
/*======================================================================*/

  /* struct : overInfo */

    struct overInfo
    {
      /* Name */
      string scalar    name
      string rowvector varlist

      /* Levels & Frequencies */
      real   rowvector levels
      real   rowvector freqs
      string rowvector labels
    }

  /* function : getOverInfo() */

    struct overInfo scalar getOverInfo(struct braddev scalar bd)
    {
      struct overInfo scalar oi

      /* Marking Out Varlist */

        if(!bd.opt.over.miss)
        {
          cols = cols(bd.vi)

          for(i=cols; i; i--)
          {
            stata("cap markout " + st_local("touse") + " " + invtokens(subinstr(bd.vi[i].varlist, "i.", "")))
          }

          stata("cap count if " + st_local("touse"))

          if(st_numscalar("r(N)") == 0) { errprintf("{error:No observations}\n"); exit(2000); }
        }

      /* Empty Varlist */

        if(st_local("over") == "") return(oi)

      /* Getting Varlist */

        oi.varlist = tokens(st_local("over"))

      /* Marking Out Overlist */

        stata("cap markout " + st_local("touse") + " " + invtokens(oi.varlist) + ", strok")
        stata("cap count if " + st_local("touse"))

        if(st_numscalar("r(N)") == 0) { errprintf("{error:No observations}\n"); exit(2000); }

      /* Generating Variable */

        if(cols(oi.varlist) == 1 & st_isnumvar(oi.varlist))
        {
          oi.name = oi.varlist

          stata("cap levelsof " + oi.name + " if " + st_local("touse") + ", local(lvls)")
          levels = tokens(st_local("lvls"))

          if(cols(levels) == 1) { errprintf("{error:Only 1 level of over, treating as {it:if}}\n"); return(oi); }
        }
        else
        {
          oi.name = st_tempname()

          group_num = st_tempname()
          stata("cap egen " + group_num + " = group("  + st_local("over") + ") if " + st_local("touse"))

          stata("cap levelsof " + group_num + ", local(lvls)")
          levels = tokens(st_local("lvls"))

          if(cols(levels) == 1) { errprintf("{error:Only 1 level of over, treating as {it:if}}\n"); stata("cap drop " + group_num); return(oi); }

          group_str = st_tempname()
          stata("cap egen " + group_str + " = concat(" + st_local("over") + ") if " + st_local("touse") + `", decode punct(", ")"')

          num_format = "%0" + strofreal(max(udstrlen(levels))) + ".0f"
          num_format = char(34) + num_format + char(34)

          stata("cap replace " + group_str + " = string(" + group_num + ", " + num_format + ") + " + `"" ""' + " + " + group_str + " if " + st_local("touse"))
          stata("cap encode " + group_str + ", generate(" + oi.name + ")")

          stata("cap drop " + group_str)
          stata("cap drop " + group_num)
        }

      /* Getting Levels */

        matcell = st_tempname()
        matrow  = st_tempname()

        stata("cap tab " + oi.name + " if " + st_local("touse") + ", matcell(" + matcell + ") matrow(" + matrow + ")")

        oi.levels = st_matrix(matrow)'
        oi.freqs  = st_matrix(matcell)'

        vallab = st_varvaluelabel(oi.name)

        if(vallab != "")
        {
          oi.labels = st_vlmap(st_varvaluelabel(oi.name), oi.levels)
          if(oi.name != oi.varlist) oi.labels = strtrim(substr(oi.labels, strpos(oi.labels, " ") :+ 1))
        }
        else
        {
          oi.labels = strofreal(oi.levels)
          bd.opt.over.legend = 0
        }

        pos = selectindex(oi.labels :== "")
        if(cols(pos) > 0) oi.labels[pos] = "_over_" :+ strofreal(oi.levels[pos])

      /* Cleaning Options */

        if(cols(oi.levels) > 167)
        {
          bd.opt.test.individual = bd.opt.test.t_individual = bd.opt.test.f_individual = 0
        }

        if(cols(oi.levels) > 2 & bd.opt.test.t_overall)
        {
          bd.opt.test.f_overall = 1
          bd.opt.test.t_overall = 0
        }
        else if(cols(oi.levels) == 2)
        {
          if(bd.opt.test.t_individual)
          {
            bd.opt.test.t_overall    = bd.opt.test.overall    = 1
            bd.opt.test.t_individual = bd.opt.test.individual = 0
          }
          else if(bd.opt.test.f_individual)
          {
            bd.opt.test.f_overall    = bd.opt.test.overall    = 1
            bd.opt.test.f_individual = bd.opt.test.individual = 0
          }
        }

      return(oi)
    }

/*======================================================================*/
/*   Mata Structure - statInfo                                          */
/*======================================================================*/

  /* struct : statInfo */

    struct statInfo
    {
      /* Name */
      string rowvector name
      string rowvector label

      /* Format - Rounding */
      real rowvector roundc, roundi

      /* Format - Notation */
      real   rowvector comma
      real   rowvector percent
      real   rowvector symbol
      string rowvector notation

      /* Format - P-Values */
      real rowvector stars, scripts

      /* CI Specific */
      real   scalar ci_level
      real   scalar ci_proportion
      real   scalar ci_combined
      string scalar ci_separator
    }

  /* function : getStatInfo() */

    struct statInfo scalar getStatInfo(struct braddev scalar bd)
    {
      struct statInfo scalar si

      /* Getting Stat List */

        st_local("statlist", "obs nyes mean se sd var lci uci min max")
        st_local("stats", strlower(st_local("stats")))
        st_local("stats", subinword(st_local("stats"), "all", "obs nyes mean se sd var lci uci min max"))
        st_local("stats", subinword(st_local("stats"), "ci", "lci uci"))

        stata("local stats : list uniq stats")
        stata("local stats : list stats & statlist")

        statlist = tokens(st_local("stats"))

        if(cols(statlist) == 0)
        {
          statlist = bd.opt.display.wide ? "mean" : ("obs", "nyes", "mean", "se", "lci", "uci")
        }

      /* Filling Names & Labels */

        si.name = si.label = statlist

        statlist = ("obs", "nyes", "mean", "se", "sd", "var", "lci", "uci", "min", "max") \ ("Obs", "n(Yes)", "Mean", "Std Err", "Std Dev", "Variance", "Lower CI", "Upper CI", "Min", "Max")

        cols = cols(si.name)

        for(i=cols; i; i--)
        {
          si.label[i] = statlist[2,selectindex(si.name[i] :== statlist[1,.])]
        }

      /* Setting Defaults */

        /* Format - Rounding */
        si.roundc = si.roundi = J(1, cols, 7)

        /* Format - Notation */
        si.comma    = J(1, cols, 1)
        si.percent  = J(1, cols, 0)
        si.symbol   = J(1, cols, 1)
        si.notation = J(2, cols, "")

        /* Format - P-Values */
        si.stars = si.scripts = J(1, cols, 0)
        if(anyof(si.name, "mean")) si.stars[selectindex(si.name :== "mean")]   = 1
        if(anyof(si.name, "uci"))  si.scripts[selectindex(si.name :== "uci")]  = 1

        /* CI Specific */
        si.ci_level      = strtoreal(st_global("S_level"))
        si.ci_proportion = 0
        si.ci_combined   = 0
        si.ci_separator  = ","

      /* Parsing Format */

        statlist = ("count", "error", "minmax", "ci"), statlist[1,.]

        input_string = st_local("format")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          /* All */

            cols = cols(si.name)

            /* Round */

              if(token == "round")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundc = si.roundi = J(1, cols, token)
                }
                continue
              }

            /* RoundC */

              if(token == "roundc")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundc = J(1, cols, token)
                }
                continue
              }

            /* RoundI */

              if(token == "roundi")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundi = J(1, cols, token)
                }
                continue
              }

            /* Percent */

              if(token == "pct"   | strpos(token, "per") == 1)   { si.percent = J(1, cols, 1); continue; }
              if(token == "nopct" | strpos(token, "noper") == 1) { si.percent = J(1, cols, 0); continue; }

            /* Symbol */

              if(strpos(token, "sym") == 1)   { si.symbol = J(1, cols, 1); continue; }
              if(strpos(token, "nosym") == 1) { si.symbol = J(1, cols, 0); continue; }

          /* Specific Variables - Getting Positions */

            if(!anyof(statlist, token)) continue

            pos = selectindex(si.name :== token)

            if(cols(pos) == 0)
            {
              if(token == "ci")          pos = selectindex(inlist(si.name, ("lci", "uci")))
              else if(token == "count")  pos = selectindex(inlist(si.name, ("obs", "nyes")))
              else if(token == "error")  pos = selectindex(inlist(si.name, ("se", "sd", "var")))
              else if(token == "minmax") pos = selectindex(inlist(si.name, ("min", "max")))
            }

            cols = cols(pos)

            if(cols == 0) continue

          /* Specific Variables - Checking Options */

            if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
            {
              sub_option = tokenget(t)
              sub_option = substr(sub_option, 2, strlen(sub_option) - 2)

              input_string = tokenrest(t)

              tokenset(t, sub_option)
            }

            while((token = tokenget(t)) != "")
            {
              /* Round */

                if(token == "round")
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)
                    token = strtoreal(token)
                    if(token >= 0 & token <= 7) si.roundc[pos] = si.roundi[pos] = J(1, cols, token)
                  }
                  continue
                }

              /* RoundC */

                if(token == "roundc")
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)
                    token = strtoreal(token)
                    if(token >= 0 & token <= 7) si.roundc[pos] = J(1, cols, token)
                  }
                  continue
                }

              /* RoundI */

                if(token == "roundi")
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)
                    token = strtoreal(token)
                    if(token >= 0 & token <= 7) si.roundi[pos] = J(1, cols, token)
                  }
                  continue
                }

              /* Percent */

                if(token == "pct"   | strpos(token, "per") == 1)   { si.percent[pos] = J(1, cols, 1); continue; }
                if(token == "nopct" | strpos(token, "noper") == 1) { si.percent[pos] = J(1, cols, 0); continue; }

              /* Symbol */

                if(strpos(token, "sym") == 1)   { si.symbol[pos] = J(1, cols, 1); continue; }
                if(strpos(token, "nosym") == 1) { si.symbol[pos] = J(1, cols, 0); continue; }

              /* Comma */

                if(token == "comma")   { si.comma[pos] = J(1, cols, 1); continue; }
                if(token == "nocomma") { si.comma[pos] = J(1, cols, 0); continue; }

              /* Stars */

                if(strpos(token, "star") == 1)   { si.stars[pos] = J(1, cols, 1); continue; }
                if(strpos(token, "nostar") == 1) { si.stars[pos] = J(1, cols, 0); continue; }

              /* Scripts */

                if(strpos(token, "script") == 1)   { si.scripts[pos] = J(1, cols, 1); continue; }
                if(strpos(token, "noscript") == 1) { si.scripts[pos] = J(1, cols, 0); continue; }

              /* Notation */

                if(strpos(token, "not") == 1)
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)

                    if(strpos(token, "par") == 1) si.notation[.,pos] = J(1, cols, ("(" \ ")"))
                    if(strpos(token, "bra") == 1) si.notation[.,pos] = J(1, cols, ("[" \ "]"))
                  }
                  continue
                }

              /* CI Specific */

                if(inlist(si.name[pos], ("lci", "uci"), 1))
                {
                  /* Level */

                    if(strpos(token, "lev") == 1 | strpos(token, "lvl") == 1)
                    {
                      if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                      {
                        token = tokenget(t)
                        token = substr(token, 2, strlen(token) - 2)
                        token = trunc(strtoreal(token))
                        if(token > 0 & token < 100) si.ci_level = token
                      }
                      continue
                    }

                  /* Proportion */

                    if(strpos(token, "prop") == 1) { si.ci_proportion = 1; continue; }

                  /* Combined */

                    if(strpos(token, "comb") == 1) { si.ci_combined = 1; continue; }

                  /* Separator */

                    if(strpos(token, "sep") == 1)
                    {
                      if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                      {
                        token = tokenget(t)
                        token = substr(token, 2, strlen(token) - 2)
                        if(token == "-" | token == ",") si.ci_separator = token
                      }
                      continue
                    }
                }
            }

          tokenset(t, input_string)
        }

      /* Cleaning Formats */

        /* Comma */

          pos  = selectindex(!inlist(si.name, ("obs", "nyes", "min", "max")))
          cols = cols(pos)
          if(cols > 0) si.comma[pos] = J(1, cols, 0)

        /* Percent, Symbol, Round */

          pos = selectindex(inlist(si.name, ("obs", "nyes")))
          cols = cols(pos)
          if(cols > 0)
          {
            si.percent[pos] = J(1, cols, 0)
            si.symbol[pos]  = J(1, cols, 0)
            si.roundc[pos]  = J(1, cols, 0)
            si.roundi[pos]  = J(1, cols, 0)
          }

        /* Scripts */

          if(cols(bd.oi.levels) > 18) si.scripts = J(1, cols(si.name), 0)

        /* Combined CI */

          if(si.ci_combined & anyof(si.name, "uci"))
          {
            pos = selectindex(si.name :== "uci")

            si.name[pos]  = "ci"
            si.label[pos] = "Confidence Interval"

            if(si.notation[1,pos] == "") si.notation[.,pos] = ("[" \ "]")

            pos = selectindex(si.name :!= "lci")

            si.name     = si.name[pos]
            si.label    = si.label[pos]
            si.roundc   = si.roundc[pos]
            si.roundi   = si.roundi[pos]
            si.comma    = si.comma[pos]
            si.percent  = si.percent[pos]
            si.symbol   = si.symbol[pos]
            si.notation = si.notation[.,pos]
            si.stars    = si.stars[pos]
            si.scripts  = si.scripts[pos]
          }

        /* Stat Names */

          if(cols(si.name) > 1) bd.opt.display.statnames = 1

      return(si)
    }

/*======================================================================*/
/*   Mata Structure - results                                           */
/*======================================================================*/

  /* struct : results */

    struct results
    {
      /* Count */
      real matrix obs
      real matrix nyes

      /* Mean */
      real matrix mean

      /* Error */
      real matrix lci, uci
      real matrix se
      real matrix sd
      real matrix var

      /* MinMax */
      real matrix min, max

      /* Calculation Only */
      real matrix t
      real matrix df

      /* P-Values */
      real matrix ovr_statistic, ovr_pvalue
      real matrix ind_statistic, ind_pvalue

      /* Sort Order */
      real rowvector sort_order
    }

  /* function : gatherResults() */

    void gatherResults(struct braddev scalar bd)
    {
      cols   = cols(bd.vi)
      groups = length(bd.oi.levels)

      if(groups == 0)
      {
        for(i=cols; i; i--)
        {
          if(bd.vi[i].type != "xi") bd.vi[i].results = calculateSeriesNoOver(bd, bd.vi[i])
          else                      bd.vi[i].results = calculateXiNoOver(bd, bd.vi[i])
        }
      }
      else
      {
        for(i=cols; i; i--)
        {
          if(bd.vi[i].type != "xi") bd.vi[i].results = calculateSeriesOver(bd, bd.vi[i])
          else                      bd.vi[i].results = calculateXiOver(bd, bd.vi[i])
        }
      }
    }

  /* function : calculateSeriesNoOver() */

    struct results scalar calculateSeriesNoOver(struct braddev scalar bd,
                                                struct varInfo scalar vi)
    {
      struct results scalar res

      /* Defining Results */

        vars = cols(vi.answers)

        res.obs = J(1, vars, 0)
        res.nyes = res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(1, vars, .)
        res.sort_order = range(1, vars, 1)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean_pre = "cap svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean_pre = "cap svy: mean "
          else                           cmd_mean_pre = "cap mean "

          cmd_mean_suf = " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ")"

        /* Count, Min, Max */

          cmd_count_pre = "cap tabstat "

          cmd_count_suf = " if " + st_local("touse")
          if(bd.opt.weight.subpop != "") cmd_count_suf = cmd_count_suf + " & " + bd.opt.weight.subpop + " == 1"
          cmd_count_suf = cmd_count_suf + ", stat(sum min max) save"

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Mean */

            stata(cmd_mean_pre + vi.varlist[i] + cmd_mean_suf)
            mat_results = st_matrix("r(table)")

            if(cols(mat_results) == 0) continue

            res.mean[i] = mat_results[1]
            res.se[i]   = mat_results[2]
            res.t[i]    = mat_results[3]
            res.lci[i]  = mat_results[5]
            res.uci[i]  = mat_results[6]
            res.df[i]   = mat_results[7]

            if(bd.opt.weight.subpop == "") res.obs[i] = st_matrix("e(_N)")
            else                           res.obs[i] = st_matrix("e(_N_subp)")

          /* SD & Var */

            if(inlist(bd.si.name, ("sd", "var"), 1))
            {
              stata("cap estat sd")

              res.sd[i]  = st_matrix("r(sd)")
              res.var[i] = st_matrix("r(variance)")
            }

          /* Count, Min, Max */

            if(inlist(bd.si.name, ("nyes", "min", "max"), 1))
            {
              stata(cmd_count_pre + vi.varlist[i] + cmd_count_suf)
              mat_results = st_matrix("r(StatTotal)")

              if(vi.binary) res.nyes[i] = mat_results[1]
              res.min[i] = mat_results[2]
              res.max[i] = mat_results[3]
            }
        }

      /* Logit CI */

        if(bd.si.ci_proportion & vi.binary)
        {
          if(!bd.opt.weight.survey) temp_se = sqrt((res.mean :* (1 :- res.mean)) :/ res.obs)
          else                      temp_se = res.se

          res.lci = invlogit(logit(res.mean) :- invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
          res.uci = invlogit(logit(res.mean) :+ invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
        }

      /* Sort Order */

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          res.sort_order = getSortOrder(getResults(res, bd.opt.display.sort_statistic), bd.opt.display.sort_direction)
        }

      return(res)
    }

  /* function : calculateXiNoOver() */

    struct results scalar calculateXiNoOver(struct braddev scalar bd,
                                            struct varInfo scalar vi)
    {
      struct results scalar res

      /* Defining Results */

        vars = cols(vi.answers)

        res.obs = J(1, vars, 0)
        res.nyes = res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(1, vars, .)
        res.sort_order = range(1, vars, 1)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean_pre = "cap xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean_pre = "cap xi, noomit: svy: mean "
          else                           cmd_mean_pre = "cap xi, noomit: mean "

          cmd_mean_suf = " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ")"

        /* Count, Min, Max */

          cmd_count_pre = "cap xi, noomit: tabstat "

          cmd_count_suf = " if " + st_local("touse")
          if(bd.opt.weight.subpop != "") cmd_count_suf = cmd_count_suf + " & " + bd.opt.weight.subpop + " == 1"
          cmd_count_suf = cmd_count_suf + ", stat(sum min max) save"

      /* Calculating Results */

        /* Mean */

          stata(cmd_mean_pre + vi.term + cmd_mean_suf)
          mat_results = st_matrix("r(table)")

          if(cols(mat_results) == 0) return(res)

          res.mean = mat_results[1,.]
          res.se   = mat_results[2,.]
          res.t    = mat_results[3,.]
          res.lci  = mat_results[5,.]
          res.uci  = mat_results[6,.]
          res.df   = mat_results[7,.]

          if(bd.opt.weight.subpop == "") res.obs = st_matrix("e(_N)")
          else                           res.obs = st_matrix("e(_N_subp)")

        /* SD & Var */

          if(inlist(bd.si.name, ("sd", "var"), 1))
          {
            stata("cap estat sd")

            res.sd  = st_matrix("r(sd)")
            res.var = st_matrix("r(variance)")
          }

        /* Count, Min, Max */

          if(inlist(bd.si.name, ("nyes", "min", "max"), 1))
          {
            stata(cmd_count_pre + vi.term + cmd_count_suf)
            mat_results = st_matrix("r(StatTotal)")

            res.nyes = mat_results[1,.]
            res.min  = mat_results[2,.]
            res.max  = mat_results[3,.]
          }

      /* Logit CI */

        if(bd.si.ci_proportion & vi.binary)
        {
          if(!bd.opt.weight.survey) temp_se = sqrt((res.mean :* (1 :- res.mean)) :/ res.obs)
          else                      temp_se = res.se

          res.lci = invlogit(logit(res.mean) :- invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
          res.uci = invlogit(logit(res.mean) :+ invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
        }

      /* Sort Order */

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          res.sort_order = getSortOrder(getResults(res, bd.opt.display.sort_statistic), bd.opt.display.sort_direction)
        }

      return(res)
    }

  /* function : calculateSeriesOver() */

    struct results scalar calculateSeriesOver(struct braddev scalar bd,
                                              struct varInfo scalar vi)
    {
      struct results scalar res

      /* Defining Results */

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total
        vars   = cols(vi.answers)

        res.obs = J(groups, vars, 0)
        res.nyes = res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(groups, vars, .)
        res.sort_order = range(1, vars, 1)

        res.ovr_statistic = res.ovr_pvalue = J(1, vars, .)
        if(bd.opt.test.individual) res.ind_statistic = res.ind_pvalue = J(factorial(lvls)/(2 * factorial(lvls - 2)), vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean_pre = "cap svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean_pre = "cap svy: mean "
          else                           cmd_mean_pre = "cap mean "

          cmd_mean_suf_1 = " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ")"
          cmd_mean_suf_2 = "over(" + bd.oi.name + ", nolabel)"

        /* Count, Min, Max */

          cmd_count_pre   = "cap tabstat "

          cmd_count_suf_1 = " if " + st_local("touse")
          if(bd.opt.weight.subpop != "") cmd_count_suf_1 = cmd_count_suf_1 + " & " + bd.opt.weight.subpop + " == 1"
          cmd_count_suf_1 = cmd_count_suf_1 + ", stat(sum min max) save"

          cmd_count_suf_2 = " by(" + bd.oi.name + ")"

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Groups */

            /* Mean */

              stata(cmd_mean_pre + vi.varlist[i] + cmd_mean_suf_1 + cmd_mean_suf_2)
              mat_results = st_matrix("r(table)")'

              if(cols(mat_results) == 0) continue

              pos = strtoreal(tokens(st_global("e(over_namelist)")))
              pos = selectindex(inlist(bd.oi.levels, pos))
              num = cols(pos)

              res.mean[pos,i] = mat_results[.,1]
              res.se[pos,i]   = mat_results[.,2]
              res.t[pos,i]    = mat_results[.,3]
              res.lci[pos,i]  = mat_results[.,5]
              res.uci[pos,i]  = mat_results[.,6]
              res.df[pos,i]   = mat_results[.,7]

              if(bd.opt.weight.subpop == "") res.obs[pos,i] = st_matrix("e(_N)")'
              else                           res.obs[pos,i] = st_matrix("e(_N_subp)")'

            /* SD & Var */

              if(inlist(bd.si.name, ("sd", "var"), 1))
              {
                stata("cap estat sd")

                res.sd[pos,i]  = st_matrix("r(sd)")'
                res.var[pos,i] = st_matrix("r(variance)")'
              }

            /* Count, Min, Max */

              if(inlist(bd.si.name, ("nyes", "min", "max"), 1))
              {
                stata(cmd_count_pre + vi.varlist[i] + cmd_count_suf_1 + cmd_count_suf_2)

                for(j=lvls; j; j--)
                {
                  mat_results = st_matrix("r(Stat" + strofreal(j) + ")")

                  if(vi.binary) res.nyes[j,i] = mat_results[1]
                  res.min[j,i] = mat_results[2]
                  res.max[j,i] = mat_results[3]
                }
              }

            /* Testing */

              if(num > 1 & (bd.opt.test.overall | bd.opt.test.individual))
              {
                /* Positions */

                  col_1 = J(lvls, 1, range(1, lvls, 1)')
                  col_1 = vech(lowertriangle(col_1,0))
                  col_1 = select(col_1, col_1 :!= 0)

                  col_2 = J(1, lvls, range(1, lvls, 1))
                  col_2 = vech(lowertriangle(col_2,0))
                  col_2 = select(col_2, col_2 :!= 0)

                  test_pos = inlist(col_1, pos) :& inlist(col_2, pos)
                  test_num = select(bd.oi.levels[col_1]', test_pos), select(bd.oi.levels[col_2]', test_pos)
                  test_pos = selectindex(test_pos)

                /* F-Test */

                  if(bd.opt.test.f_overall | bd.opt.test.f_individual)
                  {
                    term = ("[" + vi.varlist[i] + "]") :+ strofreal(test_num)
                    term = "(" :+ term[.,1] :+ " == " :+ term[.,2] :+ ")"

                    stata("cap test " + invtokens(term') + ", mtest(" + bd.opt.test.f_mtest + ")")

                    if(bd.opt.test.f_overall)
                    {
                      res.ovr_statistic[i] = st_numscalar("r(F)")
                      res.ovr_pvalue[i]    = st_numscalar("r(p)")
                    }

                    if(bd.opt.test.f_individual)
                    {
                      mat_results = st_matrix("r(mtest)")
                      res.ind_statistic[test_pos,i] = mat_results[.,1]
                      res.ind_pvalue[test_pos,i] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3] : mat_results[.,4]
                    }
                  }

                /* T-Test - Overall */

                  if(bd.opt.test.t_overall)
                  {
                    term = "[" + vi.varlist[i] + "]"
                    stata("cap lincom " + term + strofreal(bd.oi.levels[1]) + " - " + term + strofreal(bd.oi.levels[2]))

                    res.ovr_statistic[i] = st_numscalar("r(t)")
                    res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }

                /* T-Test - Individual */

                  if(bd.opt.test.t_individual)
                  {
                    term = ("[" + vi.varlist[i] + "]") :+ strofreal(test_num)
                    term = term[.,1] :+ " - " :+ term[.,2]

                    rows = rows(term)
                    for(j=rows; j; j--)
                    {
                      stata("cap lincom " + term[j])

                      res.ind_statistic[test_pos[j],i] = st_numscalar("r(t)")
                      res.ind_pvalue[test_pos[j],i]    = st_numscalar("r(p)")
                    }
                  }
              }

          /* Total */

            if(!bd.opt.over.total) continue

            /* Mean */

              stata(cmd_mean_pre + vi.varlist[i] + cmd_mean_suf_1)
              mat_results = st_matrix("r(table)")

              if(cols(mat_results) == 0) continue

              res.mean[groups,i] = mat_results[1]
              res.se[groups,i]   = mat_results[2]
              res.t[groups,i]    = mat_results[3]
              res.lci[groups,i]  = mat_results[5]
              res.uci[groups,i]  = mat_results[6]
              res.df[groups,i]   = mat_results[7]

              if(bd.opt.weight.subpop == "") res.obs[groups,i] = st_matrix("e(_N)")
              else                           res.obs[groups,i] = st_matrix("e(_N_subp)")

            /* SD & Var */

              if(inlist(bd.si.name, ("sd", "var"), 1))
              {
                stata("cap estat sd")

                res.sd[groups,i]  = st_matrix("r(sd)")
                res.var[groups,i] = st_matrix("r(variance)")
              }

            /* Count, Min, Max */

              if(vi.binary) res.nyes[groups,i] = colsum(res.nyes[.,i])
              res.min[groups,i]  = colmin(res.min[.,i])
              res.max[groups,i]  = colmax(res.max[.,i])
        }

      /* Logit CI */

        if(bd.si.ci_proportion & vi.binary)
        {
          if(!bd.opt.weight.survey) temp_se = sqrt((res.mean :* (1 :- res.mean)) :/ res.obs)
          else                      temp_se = res.se

          res.lci = invlogit(logit(res.mean) :- invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
          res.uci = invlogit(logit(res.mean) :+ invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
        }

      /* Sort Order */

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          res.sort_order = getSortOrder(getResults(res, bd.opt.display.sort_statistic), bd.opt.display.sort_direction)
        }

      return(res)
    }

  /* function : calculateXiOver() */

    struct results scalar calculateXiOver(struct braddev scalar bd,
                                          struct varInfo scalar vi)
    {
      struct results scalar res

      /* Defining Results */

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total
        vars   = cols(vi.answers)

        res.obs = J(groups, vars, 0)
        res.nyes = res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(groups, vars, .)
        res.sort_order = range(1, vars, 1)

        res.ovr_statistic = res.ovr_pvalue = J(1, vars, .)
        if(bd.opt.test.individual) res.ind_statistic = res.ind_pvalue = J(factorial(lvls)/(2 * factorial(lvls - 2)), vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean_pre = "cap xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean_pre = "cap xi, noomit: svy: mean "
          else                           cmd_mean_pre = "cap xi, noomit: mean "

          cmd_mean_suf_1 = " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ")"
          cmd_mean_suf_2 = "over(" + bd.oi.name + ", nolabel)"

        /* Count, Min, Max */

          cmd_count_pre   = "cap xi, noomit: tabstat "

          cmd_count_suf_1 = " if " + st_local("touse")
          if(bd.opt.weight.subpop != "") cmd_count_suf_1 = cmd_count_suf_1 + " & " + bd.opt.weight.subpop + " == 1"
          cmd_count_suf_1 = cmd_count_suf_1 + ", stat(sum min max) save"

          cmd_count_suf_2 = " by(" + bd.oi.name + ")"

      /* Calculating Results */

        /* Groups */

          /* Mean */

            stata(cmd_mean_pre + vi.term + cmd_mean_suf_1 + cmd_mean_suf_2)
            mat_results = st_matrix("r(table)")

            if(cols(mat_results) == 0) return(res)

            varlist = tokens(st_global("e(varlist)"))
            pos     = strtoreal(tokens(st_global("e(over_namelist)")))
            pos     = selectindex(inlist(bd.oi.levels, pos))
            num     = cols(pos)

            res.mean[pos,.] = rowshape(mat_results[1,.],vars)'
            res.se[pos,.]   = rowshape(mat_results[2,.],vars)'
            res.t[pos,.]    = rowshape(mat_results[3,.],vars)'
            res.lci[pos,.]  = rowshape(mat_results[5,.],vars)'
            res.uci[pos,.]  = rowshape(mat_results[6,.],vars)'
            res.df[pos,.]   = rowshape(mat_results[7,.],vars)'

            if(bd.opt.weight.subpop == "") res.obs[pos,.] = rowshape(st_matrix("e(_N)"), vars)'
            else                           res.obs[pos,.] = rowshape(st_matrix("e(_N_subp)"), vars)'

          /* SD & Var */

            if(inlist(bd.si.name, ("sd", "var"), 1))
            {
              stata("cap estat sd")

              res.sd[pos,.]  = rowshape(st_matrix("r(sd)"),vars)'
              res.var[pos,.] = rowshape(st_matrix("r(variance)"),vars)'
            }

          /* Count, Min, Max */

            if(inlist(bd.si.name, ("nyes", "min", "max"), 1))
            {
              stata(cmd_count_pre + vi.term + cmd_count_suf_1 + cmd_count_suf_2)

              for(i=lvls; i; i--)
              {
                mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

                res.nyes[i,.] = mat_results[1,.]
                res.min[i,.]  = mat_results[2,.]
                res.max[i,.]  = mat_results[3,.]
              }
            }

          /* Testing */

            if(num > 1 & (bd.opt.test.overall | bd.opt.test.individual))
            {
              /* Positions */

                col_1 = J(lvls, 1, range(1, lvls, 1)')
                col_1 = vech(lowertriangle(col_1,0))
                col_1 = select(col_1, col_1 :!= 0)

                col_2 = J(1, lvls, range(1, lvls, 1))
                col_2 = vech(lowertriangle(col_2,0))
                col_2 = select(col_2, col_2 :!= 0)

                test_pos = inlist(col_1, pos) :& inlist(col_2, pos)
                test_num = select(bd.oi.levels[col_1]', test_pos), select(bd.oi.levels[col_2]', test_pos)
                test_pos = selectindex(test_pos)

              /* Chi2 */

                if(bd.opt.test.chi_overall)
                {
                  stata("cap tab " + vi.varlist + " " + bd.oi.name + ", chi2")

                  res.ovr_statistic = J(1, vars, st_numscalar("r(chi2)"))
                  res.ovr_pvalue    = J(1, vars, st_numscalar("r(p)"))
                }

              /* F-Test */

                if((bd.opt.test.f_overall & !bd.opt.test.chi_overall) | bd.opt.test.f_individual)
                {
                  for(i=vars; i; i--)
                  {
                    term = ("[" + varlist[i] + "]") :+ strofreal(test_num)
                    term = "(" :+ term[.,1] :+ " == " :+ term[.,2] :+ ")"

                    stata("cap test " + invtokens(term') + ", mtest(" + bd.opt.test.f_mtest + ")")

                    if(bd.opt.test.f_overall & !bd.opt.test.chi_overall)
                    {
                      res.ovr_statistic[i] = st_numscalar("r(F)")
                      res.ovr_pvalue[i]    = st_numscalar("r(p)")
                    }

                    if(bd.opt.test.f_individual)
                    {
                      mat_results = st_matrix("r(mtest)")
                      res.ind_statistic[test_pos,i] = mat_results[.,1]
                      res.ind_pvalue[test_pos,i] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3] : mat_results[.,4]
                    }
                  }
                }

              /* T-Test - Overall */

                if(bd.opt.test.t_overall & !bd.opt.test.chi_overall)
                {
                  for(i=vars; i; i--)
                  {
                    term = "[" + varlist[i] + "]"
                    stata("cap lincom " + term + strofreal(bd.oi.levels[1]) + " - " + term + strofreal(bd.oi.levels[2]))

                    res.ovr_statistic[i] = st_numscalar("r(t)")
                    res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }
                }

              /* T-Test - Individual */

                if(bd.opt.test.t_individual)
                {
                  for(i=vars; i; i--)
                  {
                    term = ("[" + varlist[i] + "]") :+ strofreal(test_num)
                    term = term[.,1] :+ " - " :+ term[.,2]

                    rows = rows(term)
                    for(j=rows; j; j--)
                    {
                      stata("cap lincom " + term[j])

                      res.ind_statistic[test_pos[j],i] = st_numscalar("r(t)")
                      res.ind_pvalue[test_pos[j],i]    = st_numscalar("r(p)")
                    }
                  }
                }
            }

        /* Total */

          if(bd.opt.over.total)
          {
            /* Mean */

              stata(cmd_mean_pre + vi.term + cmd_mean_suf_1)
              mat_results = st_matrix("r(table)")

              if(cols(mat_results) == 0) return(res)

              res.mean[groups,.] = mat_results[1,.]
              res.se[groups,.]   = mat_results[2,.]
              res.t[groups,.]    = mat_results[3,.]
              res.lci[groups,.]  = mat_results[5,.]
              res.uci[groups,.]  = mat_results[6,.]
              res.df[groups,.]   = mat_results[7,.]

              if(bd.opt.weight.subpop == "") res.obs[groups,.] = st_matrix("e(_N)")
              else                           res.obs[groups,.] = st_matrix("e(_N_subp)")

            /* SD & Var */

              if(inlist(bd.si.name, ("sd", "var"), 1))
              {
                stata("cap estat sd")

                res.sd[groups,.]  = st_matrix("r(sd)")
                res.var[groups,.] = st_matrix("r(variance)")
              }

            /* Count, Min, Max */

              res.nyes[groups,.] = colsum(res.nyes)
              res.min[groups,.]  = colmin(res.min)
              res.max[groups,.]  = colmax(res.max)
          }

      /* Logit CI */

        if(bd.si.ci_proportion & vi.binary)
        {
          if(!bd.opt.weight.survey) temp_se = sqrt((res.mean :* (1 :- res.mean)) :/ res.obs)
          else                      temp_se = res.se

          res.lci = invlogit(logit(res.mean) :- invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
          res.uci = invlogit(logit(res.mean) :+ invttail(res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (res.mean :* (1 :- res.mean))))
        }

      /* Sort Order */

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          res.sort_order = getSortOrder(getResults(res, bd.opt.display.sort_statistic), bd.opt.display.sort_direction)
        }

      return(res)
    }

  /* function : getResults() */

    real matrix getResults(struct results scalar res,
                           string         scalar stat)
    {
      if(stat == "mean") return(res.mean)
      if(stat == "obs")  return(res.obs)
      if(stat == "nyes") return(res.nyes)
      if(stat == "sd")   return(res.sd)
      if(stat == "se")   return(res.se)
      if(stat == "lci")  return(res.lci)
      if(stat == "uci")  return(res.uci)
      if(stat == "var")  return(res.var)
      if(stat == "min")  return(res.min)
      if(stat == "max")  return(res.max)

      return(J(0,0,.))
    }

  /* function : getSortOrder() */

    real rowvector getSortOrder(real   matrix values,
                                string scalar direction)
    {
      vals = range(1, cols(values), 1), values'

      if(direction == "+") vals = sort(vals, 2)
      else                 vals = sort(vals, -2)

      if(rows(selectindex(vals[.,2] :== .)) > 0) sort_order = vals[selectindex(vals[.,2] :!= .),1] \ vals[selectindex(vals[.,2] :== .),1]
      else                                       sort_order = vals[.,1]

      return(sort_order')
    }

/*======================================================================*/
/*   Mata Structure - printer                                           */
/*======================================================================*/

  /* function : printer() */

    void printer(struct braddev scalar bd)
    {
      if(!bd.opt.display.print) return(J(0,0,.))

      lvls = cols(bd.oi.levels)

      /* Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          printf("\n{title:" + subinstr(subinstr(title, "%", "%%"), "\", "\\") + "}\n")
        }

      /* Legend */

        if(lvls > 0 & bd.opt.over.legend)
        {
          legend = getLegend(bd.oi)
          printf("\n")
          display(legend)
        }

      /* Table */

        printf("\n")

        if(lvls == 0)   printLongNoOver(bd)
        else if(!bd.opt.display.wide) printLongOver(bd)
        else                          printWide(bd)

      /* Footer */

        if(lvls > 1)
        {
          if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & any(bd.si.stars) & bd.opt.test.footer)
          {
            legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
            legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'
            display(legend)
          }

          if(bd.opt.test.individual & bd.opt.test.scripts != . & any(bd.si.scripts) & bd.opt.test.footer)
          {
            legend = bd.opt.test.letters[1..lvls]' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"
            display(legend)
          }
        }

      printf("\n")
    }

  /* function : getTitle() */

    string scalar getTitle(struct varInfo rowvector vi)
    {
      title = st_local("title")

      cols = cols(vi)

      if(strlower(title) == "none")
      {
        return("")
      }
      else if(title != "")
      {
        return(title)
      }
      else if(cols == 1)
      {
        if(vi.term == vi.question) return(vi.term)
        else                       return(vi.term + " - " + vi.question)
      }
      else
      {
        title = J(1, cols, "")

        for(i=1; i<=cols; i++)
        {
          title[i] = vi[i].term
        }

        return(invtokens(title, ", "))
      }

      return("")
    }

  /* function : getLegend() */

    string colvector getLegend(struct overInfo scalar oi)
    {
      legend = "_over_" :+ strofreal(oi.levels)'
      legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} {c |} "
      legend = legend :+ char(34) :+ invtokens(oi.varlist, ", ") :+ char(34) :+ " = "
      legend = legend :+ char(34) :+ oi.labels' :+ char(34)

      legend = subinstr(subinstr(legend, "%", "%%"), "\", "\\")

      return(legend)
    }

  /* function : printLongNoOver() */

    void function printLongNoOver(struct braddev scalar bd)
    {
      /* Creating Formats */

        stats = cols(bd.si.name)

        format_c = format_i = J(1, stats, "")

        for(i=stats; i; i--)
        {
          format_c[i] = "%32." + strofreal(bd.si.roundc[i]) + "f" + (bd.si.comma[i] * "c")
          format_i[i] = "%32." + strofreal(bd.si.roundi[i]) + "f" + (bd.si.comma[i] * "c")
        }

      /* Getting Results Table */

        series = cols(bd.vi)

        for(i=series; i; i--)
        {
          /* Creating Results Table */

            formats = bd.vi[i].binary ? format_i : format_c

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

            res_table = J(vars, stats, "")

          /* Statistics */

            for(j=stats; j; j--)
            {
              symbol = bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"

              if(bd.si.name[j] != "ci")
              {
                values = getResults(bd.vi[i].results, bd.si.name[j])

                if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                strings = bd.si.notation[1,j] :+ strofreal(values, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                miss = (values :== .)
              }
              else
              {
                values1 = getResults(bd.vi[i].results, "lci")
                values2 = getResults(bd.vi[i].results, "uci")

                if(bd.vi[i].binary & bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                strings = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ symbol :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                miss = (values1 :== .)
              }

              strings = (!miss :* strings) :+ (miss :* ".")

              res_table[.,j] = strings'
            }

          /* Labels */

            if(bd.vi[i].type == "individual" | series == 1)
            {
              res_table = J(vars, 1, "1"), bd.vi[i].answers', res_table
              res_table = res_table[sort_order,.]
            }
            else
            {
              res_table = J(vars, 1, "2"), bd.vi[i].answers', res_table
              res_table = res_table[sort_order,.]
              res_table = ("1", bd.vi[i].question, J(1, stats, "")) \ res_table
            }

          /* Separator */

            if(bd.opt.display.separator & series > 1)
            {
              res_table = J(1, stats + 2, "0") \ res_table
            }

          /* Adding to Final Table */

            if(series != i) fin_table = res_table \ fin_table
            else            fin_table = res_table
        }

      /* Formatting Results Table */

        /* Column Lengths */

          col_lengths = colmax(udstrlen(fin_table))

          pos  = selectindex(col_lengths :< 5)
          cols = cols(pos)
          if(cols > 0) col_lengths[pos] = J(1, cols, 5)

          if(col_lengths[2] :> 80)      col_lengths[2] = 80
          else if(col_lengths[2] :< 14) col_lengths[2] = 14

        /* Variable Names */

          pos = selectindex(fin_table[.,1] :== "1")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {lalign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

          pos = selectindex(fin_table[.,1] :== "2")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {ralign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

        /* Statistics */

          cols = cols(fin_table)
          pos  = range(3, cols, 1)

          fin_table[.,pos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[pos]) :+ ":" :+ fin_table[.,pos] :+ "} "

        /* Horizontal Separators */

          hsep    = "{hline " :+ strofreal(col_lengths :+ 2) :+ "}"
          hsep[2] = hsep[2] + "{c +}"

          if(fin_table[1,1] == "0") fin_table[1,.] = hsep
          else                      fin_table = hsep \ fin_table

          pos = rows(fin_table)
          if(fin_table[pos,1] == "0") fin_table[pos,.] = subinstr(hsep, "+", "BT")
          else                        fin_table        = fin_table \ subinstr(hsep, "+", "BT")

          if(!bd.opt.display.separator)
          {
            pos = selectindex(fin_table[.,1] :!= "0")

            fin_table = fin_table[pos,.]
          }
          else
          {
            pos  = selectindex(fin_table[.,1] :== "0")
            rows = rows(pos)
            if(rows > 0) fin_table[pos,.] = J(rows, 1, hsep)
          }

        /* Removing First Column */

          cols = cols(fin_table)

          fin_table   = fin_table[.,(2..cols)]
          col_lengths = col_lengths[2..cols] :+ 3

          cols = cols(fin_table)

        /* Setting Table Numbers */

          linesize = c("linesize")
          linesize = linesize < 120 ? 120 : linesize
          linesize = linesize > 250 ? 250 : linesize

          i = 1
          table_lens = runningsum(col_lengths)
          table_nums = J(1, cols, i)

          pos = selectindex(table_lens :>= linesize)
          while(cols(pos) > 0)
          {
            i = i + 1

            table_lens[pos] = runningsum(col_lengths[pos]) :+ col_lengths[1]
            table_nums[pos] = J(1, cols(pos), i)

            pos = selectindex(table_lens :>= linesize)
          }

          table_nums[1] = .
          table_count   = max(table_nums)

          col_lengths = col_lengths :- 3

        /* Statistic Names */

          labels = "{space " :+ strofreal(col_lengths :+ 2) :+ "}{c |}"

          labels[2..cols] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[2..cols]) :+ ":" :+ abbrev(bd.si.label, col_lengths[2..cols]) :+ "} "

          fin_table = labels \ fin_table

        /* Adding {res} */

          fin_table[.,1] = "{res}" :+ fin_table[.,1]

      /* Printing Table */

        for(i=1; i<=table_count; i++)
        {
          if(table_count > 1) printf("\n(" + strofreal(i) + "/" + strofreal(table_count) + ")\n")

          print_table = fin_table[.,1]

          pos  = selectindex(table_nums :== i)
          cols = cols(pos)

          for(j=1; j<=cols; j++)
          {
            print_table = print_table :+ fin_table[.,pos[j]]
          }

          display(print_table)
        }
    }

  /* function : printLongOver() */

    void function printLongOver(struct braddev scalar bd)
    {
      /* Creating Formats */

        stats = cols(bd.si.name)

        format_c = format_i = J(1, stats, "")

        for(i=stats; i; i--)
        {
          format_c[i] = "%32." + strofreal(bd.si.roundc[i]) + "f" + (bd.si.comma[i] * "c")
          format_i[i] = "%32." + strofreal(bd.si.roundi[i]) + "f" + (bd.si.comma[i] * "c")
        }

      /* General Dimensions */

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total

        p_overall    = bd.opt.test.overall    & (cols(bd.opt.test.stars) == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual & (bd.opt.test.scripts == .     | bd.opt.test.force)
        p_stars      = bd.opt.test.overall    & (cols(bd.opt.test.stars) != 0)
        p_scripts    = bd.opt.test.individual & (bd.opt.test.scripts != .)
        pvalues      = p_overall + (p_individual * lvls)

      /* Getting Results Table */

        series = cols(bd.vi)

        for(i=series; i; i--)
        {
          /* Creating Results Table */

            formats = bd.vi[i].binary ? format_i : format_c

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

            res_rows = vars * (groups + 2)
            res_cols = stats + (pvalues * (1 + bd.opt.test.statistic))

            res_table = J(res_rows, res_cols, "")

          /* Statistics */

            temp_table = J(groups + 2, vars, "")

            for(j=stats; j; j--)
            {
              /* Statistic */

                symbol = bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"

                if(bd.si.name[j] != "ci")
                {
                  values = getResults(bd.vi[i].results, bd.si.name[j])

                  if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                  strings = bd.si.notation[1,j] :+ strofreal(values, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                  miss = (values :== .)
                }
                else
                {
                  values1 = getResults(bd.vi[i].results, "lci")
                  values2 = getResults(bd.vi[i].results, "uci")

                  if(bd.vi[i].binary & bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                  strings = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ symbol :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                  miss = (values1 :== .)
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  cols = cols(bd.opt.test.stars)

                  for(k=cols; k; k--)
                  {
                    strings = strings :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* "*")
                  }
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  for(k=vars; k; k--)
                  {
                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,k])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    postscripts = ((pvals :< bd.opt.test.scripts) :* (bd.opt.test.letters[1..groups]))

                    for(l=1; l<=lvls; l++)
                    {
                      strings[.,k] = strings[.,k] :+ postscripts[.,l]
                    }
                  }
                }

              /* Reshaping & Placing */

                strings = (!miss :* strings) :+ (miss :* ".")

                temp_table[(2..groups+1),.] = strings

                res_table[.,j] = vec(temp_table[.,sort_order])
            }

            res_pos = stats + 1

          /* P-Values - Overall */

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ovr_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  temp_table[(2..groups+1),.] = J(groups, 1, strings)

                  res_table[.,res_pos] = vec(temp_table[.,sort_order])

                  res_pos = res_pos + 1
                }

              /* P-Values */

                strings = strofreal(bd.vi[i].results.ovr_pvalue, "%6.4f")

                temp_table[(2..groups+1),.] = J(groups, 1, strings)

                res_table[.,res_pos] = vec(temp_table[.,sort_order])

                res_pos = res_pos + 1
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  res_rows = rangex(2, groups, 1)
                  res_cols = rangex(res_pos, lvls, 2)

                  for(j=1; j<=vars; j++)
                  {
                    pos = sort_order[j]

                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_statistic[.,pos])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    strings = strofreal(pvals[.,(1..lvls)], "%32.2f")

                    pos = strings :!= "."

                    if(bd.opt.test.t_individual) strings = "t = " :+ strings
                    else                         strings = "F = " :+ strings

                    strings = (pos :* strings) :+ (!pos :* ".")

                    res_table[res_rows,res_cols] = strings

                    res_rows = res_rows :+ groups :+ 2
                  }

                  res_pos = res_pos + 1
                }

              /* P-Values */

                pvals = J(groups, groups, .)

                rows = range(2, lvls, 1)
                cols = range(1, lvls-1, 1)

                res_rows = rangex(2, groups, 1)
                res_cols = rangex(res_pos, lvls, 1 + bd.opt.test.statistic)

                for(j=1; j<=vars; j++)
                {
                  pos = sort_order[j]

                  pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,pos])
                  _diag(pvals, .)
                  _makesymmetric(pvals)

                  strings = strofreal(pvals[.,(1..lvls)], "%6.4f")

                  pos = strings :!= "."

                  strings = (pos :* strings) :+ (!pos :* ".")

                  res_table[res_rows,res_cols] = strings

                  res_rows = res_rows :+ groups :+ 2
                }
            }

          /* Labels */

            labels = J(groups + 2, vars, "")

            if(series == 1)
            {
              labels[1,.] = bd.vi[i].answers
            }
            else
            {
              if(bd.vi[i].type != "xi") labels[1,.] = bd.vi[i].varlist
              else                      labels[1,.] = bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels)
            }

            pos = rangex(2, lvls, 1)
            if(bd.opt.over.labels) labels[pos,.] = J(1, vars, bd.oi.labels')
            else                   labels[pos,.] = J(1, vars, ("_over_" :+ strofreal(bd.oi.levels)'))

            if(bd.opt.over.total) labels[1+groups,.] = J(1, vars, "Total")

            labels = vec(labels[.,sort_order])

          /* Indices */

            indices             = J(groups + 2, 1, "2")
            indices[1]          = "1"
            indices[groups + 2] = "0"

            indices = vec(J(1, vars, indices))

          /* Adding to Final Table */

            if(series != i) fin_table = (indices, labels, res_table) \ fin_table
            else            fin_table = (indices, labels, res_table)
        }

      /* Formatting Results Table */

        /* Column Lengths */

          col_lengths = colmax(udstrlen(fin_table))

          pos  = selectindex(col_lengths :< 5)
          cols = cols(pos)
          if(cols > 0) col_lengths[pos] = J(1, cols, 5)

          if(col_lengths[2] :> 80)      col_lengths[2] = 80
          else if(col_lengths[2] :< 14) col_lengths[2] = 14

        /* Removing Single-Series Variable Names */

          if(series == 1 & vars == 1)
          {
            pos = selectindex(fin_table[.,1] :!= "1")
            fin_table = fin_table[pos,.]

            pos = selectindex(fin_table[.,1] :== "2")
            fin_table[pos,1] = J(rows(pos), 1, "1")
          }

        /* Variable Names */

          pos = selectindex(fin_table[.,1] :== "1")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {lalign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

          pos = selectindex(fin_table[.,1] :== "2")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {ralign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

        /* Statistics */

          cols = cols(fin_table)
          pos  = range(3, cols, 1)

          fin_table[.,pos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[pos]) :+ ":" :+ fin_table[.,pos] :+ "} "

        /* Horizontal Separators */

          hsep    = "{hline " :+ strofreal(col_lengths :+ 2) :+ "}"
          hsep[2] = hsep[2] + "{c +}"

          if(fin_table[1,1] == "0") fin_table[1,.] = hsep
          else                      fin_table = hsep \ fin_table

          pos = rows(fin_table)
          if(fin_table[pos,1] == "0") fin_table[pos,.] = subinstr(hsep, "+", "BT")
          else                        fin_table        = fin_table \ subinstr(hsep, "+", "BT")

          if(!bd.opt.display.separator)
          {
            pos = selectindex(fin_table[.,1] :!= "0")

            fin_table = fin_table[pos,.]
          }
          else
          {
            pos  = selectindex(fin_table[.,1] :== "0")
            rows = rows(pos)
            if(rows > 0) fin_table[pos,.] = J(rows, 1, hsep)
          }

        /* Removing First Column */

          cols = cols(fin_table)

          fin_table   = fin_table[.,(2..cols)]
          col_lengths = col_lengths[2..cols] :+ 3

          cols = cols(fin_table)

        /* Setting Table Numbers */

          linesize = c("linesize")
          linesize = linesize < 120 ? 120 : linesize
          linesize = linesize > 250 ? 250 : linesize

          i = 1
          table_lens = runningsum(col_lengths)
          table_nums = J(1, cols, i)

          pos = selectindex(table_lens :>= linesize)
          while(cols(pos) > 0)
          {
            i = i + 1

            table_lens[pos] = runningsum(col_lengths[pos]) :+ col_lengths[1]
            table_nums[pos] = J(1, cols(pos), i)

            pos = selectindex(table_lens :>= linesize)
          }

          table_nums[1] = .
          table_count   = max(table_nums)

          col_lengths = col_lengths :- 3

        /* Setting Breaks */

          breaks = 1 + stats

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            breaks = breaks \ rangex(3 + stats, pvalues - 1, 2)
          }

        /* Setting Binds */

          binds_1 = range(1, cols, 1)'
          binds_2 = J(1, cols, .)

          if(pvalues > 0)
          {
            pos = range(2 + stats, cols, 1)
            binds_2[pos] = vec(J(1 + bd.opt.test.statistic, 1, rangex(stats + 1, pvalues, 1)'))'
          }

          for(i=2; i<cols; i++)
          {
            if((binds_2[i] == binds_2[i+1]) & (table_nums[i] == table_nums[i+1]))
            {
              binds_1[i+1] = binds_1[i]
            }
          }

        /* Header */

          header = 2

          /* Statistic Names */

            labels = "{space " :+ strofreal(col_lengths :+ 2) :+ "}{c |}"

            pos = rangex(2, stats, 1)
            labels[pos] = bd.si.label

            if(pvalues > 0)
            {
              pos = range(2 + stats, cols, 1)

              if(!bd.opt.test.statistic) labels[pos] = J(1, pvalues, "P-Val")
              else                       labels[pos] = J(1, pvalues, ("Stat", "P-Val"))
            }

            pos = range(2, cols, 1)
            labels[pos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[pos]) :+ ":" :+ abbrev(labels[pos], col_lengths[pos]) :+ "} "

            fin_table = labels \ fin_table

          /* Group Names */

            if(pvalues > 0)
            {
              labels    = "{space " :+ strofreal(col_lengths :+ 2) :+ "}"
              labels[1] = labels[1] + "{c |}"

              pos = rangex(2 + stats, 1 + bd.opt.test.statistic, 1)

              if(p_overall)
              {
                labels[pos] = J(1, 1 + bd.opt.test.statistic, "Overall")

                pos = pos :+ 1 :+ bd.opt.test.statistic
              }

              if(p_individual)
              {
                for(i=1; i<=lvls; i++)
                {
                  labels[pos] = J(1, 1 + bd.opt.test.statistic, "vs " + strofreal(bd.oi.levels[i]))

                  pos = pos :+ 1 :+ bd.opt.test.statistic
                }
              }

              for(i=2+stats; i<=cols; i++)
              {
                res_pos  = selectindex(binds_1 :== binds_1[i])
                res_cols = cols(res_pos)
                length   = sum(col_lengths[res_pos]) + ((res_cols - 1) * 2)

                labels[i] = " {center " :+ strofreal(length) + ":" + abbrev(labels[i], length) + "} "

                if(res_cols > 1)
                {
                  labels[res_pos[2..res_cols]] = J(1, res_cols - 1, "")
                }

                i = res_pos[res_cols]
              }

              fin_table = labels \ fin_table
            }

        /* Vertical Separators */

          if(rows(breaks) > 0)
          {
            cols = cols(table_nums)
            cols = selectindex(table_nums :== (table_nums[2..cols], .))

            breaks = breaks[selectindex(inlist(breaks, cols))]

            vsep = substr(fin_table[.,1], strrpos(fin_table[.,1], "{"))

            fin_table[.,breaks] = fin_table[.,breaks] :+ vsep
          }

        /* Adding {res} */

          fin_table[.,1] = "{res}" :+ fin_table[.,1]

      /* Printing Table */

        for(i=1; i<=table_count; i++)
        {
          if(table_count > 1) printf("\n(" + strofreal(i) + "/" + strofreal(table_count) + ")\n")

          print_table = fin_table[.,1]

          pos  = selectindex(table_nums :== i)
          cols = cols(pos)

          for(j=1; j<=cols; j++)
          {
            print_table = print_table :+ fin_table[.,pos[j]]
          }

          display(print_table)
        }
    }

  /* function : printWide() */

    void function printWide(struct braddev scalar bd)
    {
      /* Creating Formats */

        stats = cols(bd.si.name)

        format_c = format_i = J(1, stats, "")

        for(i=stats; i; i--)
        {
          format_c[i] = "%32." + strofreal(bd.si.roundc[i]) + "f" + (bd.si.comma[i] * "c")
          format_i[i] = "%32." + strofreal(bd.si.roundi[i]) + "f" + (bd.si.comma[i] * "c")
        }

      /* General Dimensions */

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total

        p_overall    = bd.opt.test.overall    & (cols(bd.opt.test.stars) == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual & (bd.opt.test.scripts == .     | bd.opt.test.force)
        p_stars      = bd.opt.test.overall    & (cols(bd.opt.test.stars) != 0)
        p_scripts    = bd.opt.test.individual & (bd.opt.test.scripts != .)
        pvalues      = p_overall + (p_individual * (factorial(lvls)/(2 * factorial(lvls - 2))))

      /* Getting Results Table */

        series = cols(bd.vi)

        for(i=series; i; i--)
        {
          /* Creating Results Table */

            formats = bd.vi[i].binary ? format_i : format_c

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

            res_rows = vars
            res_cols = (stats * groups) + (pvalues * (1 + bd.opt.test.statistic))

            res_table = J(res_rows, res_cols, "")

          /* Statistics */

            for(j=stats; j; j--)
            {
              /* Statistic */

                symbol = bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"

                if(bd.si.name[j] != "ci")
                {
                  values = getResults(bd.vi[i].results, bd.si.name[j])

                  if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                  strings = bd.si.notation[1,j] :+ strofreal(values, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                  miss = (values :== .)
                }
                else
                {
                  values1 = getResults(bd.vi[i].results, "lci")
                  values2 = getResults(bd.vi[i].results, "uci")

                  if(bd.vi[i].binary & bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                  strings = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ symbol :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ symbol :+ bd.si.notation[2,j]

                  miss = (values1 :== .)
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  cols = cols(bd.opt.test.stars)

                  for(k=cols; k; k--)
                  {
                    strings = strings :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* "*")
                  }
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  for(k=vars; k; k--)
                  {
                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,k])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    postscripts = ((pvals :< bd.opt.test.scripts) :* (bd.opt.test.letters[1..groups]))

                    for(l=1; l<=lvls; l++)
                    {
                      strings[.,k] = strings[.,k] :+ postscripts[.,l]
                    }
                  }
                }

              /* Reshaping & Placing */

                strings = (!miss :* strings) :+ (miss :* ".")

                pos = rangex(j, groups, stats)

                res_table[.,pos] = strings'
            }

            res_pos = (stats * groups) + 1

          /* P-Values - Overall */

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ovr_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  res_table[.,res_pos] = strings'

                  res_pos = res_pos + 1
                }

              /* P-Values */

                strings = strofreal(bd.vi[i].results.ovr_pvalue, "%6.4f")

                res_table[.,res_pos] = strings'

                res_pos = res_pos + 1
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              rows = factorial(lvls)/(2 * factorial(lvls - 2))

              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ind_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  res_table[.,rangex(res_pos,rows,2)] = strings'

                  res_pos = res_pos + 1
                }

              /* P-Values */

                strings = strofreal(bd.vi[i].results.ind_pvalue, "%6.4f")

                res_table[.,rangex(res_pos,rows,1+bd.opt.test.statistic)] = strings'
            }

          /* Labels */

            if(bd.vi[i].type == "individual" | series == 1)
            {
              res_table = J(vars, 1, "1"), bd.vi[i].answers', res_table
              res_table = res_table[sort_order,.]
            }
            else
            {
              res_table = J(vars, 1, "2"), bd.vi[i].answers', res_table
              res_table = res_table[sort_order,.]
              res_table = ("1", bd.vi[i].question, J(1, res_cols, "")) \ res_table
            }

          /* Separator */

            if(bd.opt.display.separator & series > 1)
            {
              res_table = J(1, res_cols + 2, "0") \ res_table
            }

          /* Adding to Final Table */

            if(series != i) fin_table = res_table \ fin_table
            else            fin_table = res_table
        }

      /* Formatting Results Table */

        /* Column Lengths */

          col_lengths = colmax(udstrlen(fin_table))

          pos  = selectindex(col_lengths :< 5)
          cols = cols(pos)
          if(cols > 0) col_lengths[pos] = J(1, cols, 5)

          if(col_lengths[2] :> 80)      col_lengths[2] = 80
          else if(col_lengths[2] :< 14) col_lengths[2] = 14

        /* Variable Names */

          pos = selectindex(fin_table[.,1] :== "1")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {lalign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

          pos = selectindex(fin_table[.,1] :== "2")
          if(length(pos) > 0)
          {
            fin_table[pos,2] = " {ralign " :+ strofreal(col_lengths[2]) :+ ":" :+ substr(fin_table[pos,2], 1, col_lengths[2]) :+ "} {c |}"
          }

        /* Statistics */

          cols = cols(fin_table)
          pos  = range(3, cols, 1)

          fin_table[.,pos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[pos]) :+ ":" :+ fin_table[.,pos] :+ "} "

        /* Horizontal Separators */

          hsep    = "{hline " :+ strofreal(col_lengths :+ 2) :+ "}"
          hsep[2] = hsep[2] + "{c +}"

          if(fin_table[1,1] == "0") fin_table[1,.] = hsep
          else                      fin_table = hsep \ fin_table

          pos = rows(fin_table)
          if(fin_table[pos,1] == "0") fin_table[pos,.] = subinstr(hsep, "+", "BT")
          else                        fin_table        = fin_table \ subinstr(hsep, "+", "BT")

          if(!bd.opt.display.separator)
          {
            pos = selectindex(fin_table[.,1] :!= "0")

            fin_table = fin_table[pos,.]
          }
          else
          {
            pos  = selectindex(fin_table[.,1] :== "0")
            rows = rows(pos)
            if(rows > 0)
            {
              fin_table[pos,.] = J(rows, 1, hsep)
            }
          }

        /* Removing First Column */

          cols = cols(fin_table)

          fin_table   = fin_table[.,(2..cols)]
          col_lengths = col_lengths[2..cols] :+ 3

          cols = cols(fin_table)

        /* Setting Table Numbers */

          linesize = c("linesize")
          linesize = linesize < 120 ? 120 : linesize
          linesize = linesize > 250 ? 250 : linesize

          i = 1
          table_lens = runningsum(col_lengths)
          table_nums = J(1, cols, i)

          pos = selectindex(table_lens :>= linesize)
          while(cols(pos) > 0)
          {
            i = i + 1

            table_lens[pos] = runningsum(col_lengths[pos]) :+ col_lengths[1]
            table_nums[pos] = J(1, cols(pos), i)

            pos = selectindex(table_lens :>= linesize)
          }

          table_nums[1] = .
          table_count   = max(table_nums)

          col_lengths = col_lengths :- 3

        /* Setting Breaks */

          breaks = rangex(stats + 1, groups, stats)

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            breaks = breaks \ rangex((stats * groups) + 3, pvalues - 1, 2)
          }

        /* Setting Binds */

          binds_1 = range(1, cols, 1)'
          binds_2 = J(1, cols, .)

          pos = rangex(2, stats * groups, 1)
          binds_2[pos] = vec(J(stats, 1, range(1, groups, 1)'))'

          if(pvalues > 0)
          {
            pos = range(2 + (stats * groups), cols, 1)
            binds_2[pos] = vec(J(1 + bd.opt.test.statistic, 1, rangex(groups + 1, pvalues, 1)'))'
          }

          for(i=2; i<cols; i++)
          {
            if((binds_2[i] == binds_2[i+1]) & (table_nums[i] == table_nums[i+1]))
            {
              binds_1[i+1] = binds_1[i]
            }
          }

        /* Header Dimensions */

          stat_statnames = bd.opt.display.statnames
          stat_groupn    = bd.opt.over.group_n
          stat_grouplab  = 1

          pval_statnames = (bd.opt.display.statnames | bd.opt.test.statistic) & (pvalues > 0)
          pval_grouplab  = pvalues > 0

          header = (stat_statnames + stat_groupn + stat_grouplab), (pval_statnames + pval_grouplab)
          header = max(header)

          bound_cols = J(header, cols, 0)

          labels = J(header, 1, ("{space " :+ strofreal(col_lengths :+ 2) :+ "}"))

        /* Header */

          for(i=header; i; i--)
          {
            /* Statistics */

              pos = rangex(2, stats * groups, 1)

              if(stat_statnames)
              {
                labels[i,pos] = J(1, groups, bd.si.label)
                bound_cols[i,pos] = J(1, stats * groups, 1)

                stat_statnames = 0
              }
              else if(stat_groupn)
              {
                res_table = "(n = " :+ strofreal(bd.oi.freqs, "%32.0fc") :+ ")"

                if(bd.opt.over.total) res_table = res_table, ("(n = " :+ strofreal(sum(bd.oi.freqs), "%32.0fc") :+ ")")

                labels[i,pos] = vec(J(stats, 1, res_table))'
                bound_cols[i,pos] = J(1, stats * groups, 1 + (stats > 1))

                stat_groupn = 0
              }
              else if(stat_grouplab)
              {
                if(bd.opt.over.labels) res_table = bd.oi.labels
                else                   res_table = "_over_" :+ strofreal(bd.oi.levels)

                if(bd.opt.over.total) res_table = res_table, "Total"

                labels[i,pos] = vec(J(stats, 1, res_table))'
                bound_cols[i,pos] = J(1, stats * groups, 1 + (stats > 1))

                stat_grouplab = 0
              }

            /* P-Values */

              if(pval_statnames)
              {
                pos = range(2 + (stats * groups), cols, 1)

                if(!bd.opt.test.statistic)
                {
                  labels[i,pos] = J(1, pvalues, "P-Val")
                  bound_cols[i,pos] = J(1, pvalues, 1)
                }
                else
                {
                  labels[i,pos]     = J(1, pvalues, ("Stat", "P-Val"))
                  bound_cols[i,pos] = J(1, pvalues * 2, 1)
                }

                pval_statnames = 0
              }
              else if(pval_grouplab)
              {
                pos = range(2 + (stats * groups), cols, 1)

                res_table = J(1, pvalues, "")
                res_pos   = 1

                if(p_overall)
                {
                  res_table[res_pos] = "Overall"
                  res_pos = res_pos + 1
                }

                if(p_individual)
                {
                  for(j=1; j<lvls; j++)
                  {
                    for(k=j+1; k<=lvls; k++)
                    {
                      res_table[res_pos] = strofreal(bd.oi.levels[j]) + "v" + strofreal(bd.oi.levels[k])
                      res_pos = res_pos + 1
                    }
                  }
                }

                if(!bd.opt.test.statistic)
                {
                  labels[i,pos] = res_table
                  bound_cols[i,pos] = J(1, pvalues, 1)
                }
                else
                {
                  labels[i,pos]     = vec(J(2, 1, res_table))'
                  bound_cols[i,pos] = J(1, pvalues * 2, 2)
                }

                pval_grouplab = 0
              }
          }

        /* Binding Columns */

          bound_cols[.,1] = J(header, 1, .)

          for(i=1; i<=header; i++)
          {
            /* First Column */

              labels[i,1] = labels[i,1] + "{c |}"

            /* Empty Columns */

              pos  = selectindex(bound_cols[i,.] :== 0)
              cols = cols(pos)

              if(cols > 0)
              {
                labels[i,pos] = "{space " :+ strofreal(col_lengths[pos] :+ 2) :+ "}"
              }

            /* Unbound Columns */

              pos  = selectindex(bound_cols[i,.] :== 1)
              cols = cols(pos)

              if(cols > 0)
              {
                labels[i,pos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[pos]) :+ ":" :+ abbrev(labels[i,pos], col_lengths[pos]) :+ "} "
              }

            /* Bound Columns */

              pos  = selectindex(bound_cols[i,.] :== 2)
              cols = cols(pos)

              if(cols > 0)
              {
                for(j=1; j<=cols; j++)
                {
                  temp_pos = pos[j]
                  res_pos  = selectindex(binds_1 :== binds_1[temp_pos])
                  res_cols = cols(res_pos)
                  length   = sum(col_lengths[res_pos]) + ((res_cols - 1) * 2)

                  labels[i,temp_pos] = " {center " :+ strofreal(length) + ":" + abbrev(labels[i,temp_pos], length) + "} "

                  if(res_cols > 1)
                  {
                    labels[i,res_pos[2..res_cols]] = J(1, res_cols - 1, "")
                  }

                  j = j + res_cols - 1
                }
              }
          }

          fin_table = labels \ fin_table

        /* Vertical Separators */

          if(rows(breaks) > 0)
          {
            cols = cols(table_nums)
            cols = selectindex(table_nums :== (table_nums[2..cols], .))

            breaks = breaks[selectindex(inlist(breaks, cols))]

            vsep = substr(fin_table[.,1], strrpos(fin_table[.,1], "{"))

            fin_table[.,breaks] = fin_table[.,breaks] :+ vsep
          }

        /* Adding {res} */

          fin_table[.,1] = "{res}" :+ fin_table[.,1]

      /* Printing Table */

        for(i=1; i<=table_count; i++)
        {
          if(table_count > 1) printf("\n(" + strofreal(i) + "/" + strofreal(table_count) + ")\n")

          print_table = fin_table[.,1]

          pos  = selectindex(table_nums :== i)
          cols = cols(pos)

          for(j=1; j<=cols; j++)
          {
            print_table = print_table :+ fin_table[.,pos[j]]
          }

          display(print_table)
        }
    }

/*======================================================================*/
/*   Mata Structure - excel                                             */
/*======================================================================*/

  /* function : createExcel() */

    void createExcel(struct braddev scalar bd)
    {
      if(!bd.opt.excel.output) return(J(0,0,.))

      /* Initializing Object */

        class xl scalar B
        B = xl()

      /* Loading Book & Setting Worksheet */

        if(fileexists(bd.opt.excel.file_path))
        {
          if(bd.opt.excel.bookreplace) B.clear_book(bd.opt.excel.file_path)

          B.load_book(bd.opt.excel.file_path)

          if(bd.opt.excel.sheet == "")
          {
            B.set_sheet(B.get_sheets()[1])
          }
          else
          {
            if(anyof(B.get_sheets(), bd.opt.excel.sheet)) B.set_sheet(bd.opt.excel.sheet)
            else                                          B.add_sheet(bd.opt.excel.sheet)

            if(bd.opt.excel.bookreplace & bd.opt.excel.sheet != "Sheet1") B.delete_sheet("Sheet1")
          }

          if(bd.opt.excel.sheetreplace) B.clear_sheet(B.query("sheetname"))
        }
        else
        {
          if(bd.opt.excel.sheet == "") bd.opt.excel.sheet = "Sheet1"

          B.create_book(bd.opt.excel.file_path, bd.opt.excel.sheet)
        }

        B.set_mode("open")

        B.set_missing(".")

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

      /* Creating Table */

        if(cols(bd.oi.levels) == 0)   excelLongNoOver(bd, B, row)
        else if(!bd.opt.display.wide) excelLongOver(bd, B, row)
        else                          excelWide(bd, B, row)

      B.close_book()
    }

  /* function : excelLongNoOver() */

    void excelLongNoOver(struct braddev scalar bd,
                         class  xl      scalar B,
                         real           scalar input_row)
    {
      /* General Dimensions */

        stats = cols(bd.si.name)

        row       = input_row
        start_row = row

        end_col = 1 + stats

      /* Creating Formats */

        /* Fonts */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Header */

          fmt_header = B.add_fmtid()
          B.fmtid_set_fontid(fmt_header, font_bold)
          B.fmtid_set_horizontal_align(fmt_header, "center")
          B.fmtid_set_fill_pattern(fmt_header, "solid", bd.opt.excel.color[1])

        /* Variables */

          fmt_question = B.add_fmtid()
          B.fmtid_set_fontid(fmt_question, font_bold)
          B.fmtid_set_horizontal_align(fmt_question, "left")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

        /* Statistics - Numeric Format */

          txt_stats_c = txt_stats_i = J(1, stats, "")

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              txt_stats_c[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundc[i] > 0 ? ("." + (bd.si.roundc[i] * "0")) : "")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_c[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_c[i] + char(34) + bd.si.notation[2,i] + char(34)
              }

            /* Binary */

              txt_stats_i[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundi[i] > 0 ? ("." + (bd.si.roundi[i] * "0")) : "") + (bd.si.percent[i] * bd.si.symbol[i] * "%")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_i[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_i[i] + char(34) + bd.si.notation[2,i] + char(34)
              }
          }

        /* Statistics - Format IDs */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_c[i] :== txt_stats_c[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")

                if(txt_stats_c[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
                }
              }
              else
              {
                fmt_stats_c[i] = fmt_stats_c[pos[1]]
              }

            /* Binary */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_i[i] :== txt_stats_i[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")

                if(txt_stats_i[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
                }
              }
              else
              {
                fmt_stats_i[i] = fmt_stats_i[pos[1]]
              }
          }

      /* Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, 1, title)
          B.set_fmtid(row, 1, fmt_title)

          row       = row + 1
          start_row = row
        }

      /* Header */

        B.put_string(row, 2, bd.si.label)
        B.set_fmtid(row, (2,end_col), fmt_header)

        B.set_top_border(row, (1,end_col), "medium")
        B.set_bottom_border(row, (1,end_col), "medium")

        row = row + 1

      /* Variables & Statistics */

        series = cols(bd.vi)

        for(i=1; i<=series; i++)
        {
          /* General Information */

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

          /* Variable Names */

            if(bd.vi[i].type == "individual" | series == 1)
            {
              pos = row + vars - 1

              B.put_string(row, 1, bd.vi[i].answers'[sort_order])
              B.set_fmtid((row,pos), 1, fmt_question)
            }
            else
            {
              B.put_string(row, 1, bd.vi[i].question)
              B.set_fmtid(row, 1, fmt_question)

              row = row + 1
              pos = row + vars - 1

              B.put_string(row, 1, bd.vi[i].answers'[sort_order])
              B.set_fmtid((row,pos), 1, fmt_answer)
            }

          /* Statistics */

            for(j=stats; j; j--)
            {
              if(bd.si.name[j] != "ci")
              {
                values = getResults(bd.vi[i].results, bd.si.name[j])

                if(bd.vi[i].binary & bd.si.percent[j] & !bd.si.symbol[j]) values = values * 100

                B.put_number(row, 1+j, values[sort_order]')
              }
              else
              {
                values1 = getResults(bd.vi[i].results, "lci")
                values2 = getResults(bd.vi[i].results, "uci")

                if(bd.vi[i].binary)
                {
                  if(bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                  strings = strofreal(values1, format_i[j]) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%") :+ bd.si.ci_separator
                  strings = strings :+ strofreal(values2, format_i[j]) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                }
                else
                {
                  strings = strofreal(values1, format_c[j]) :+ bd.si.ci_separator :+ strofreal(values2, format_c[j])
                }

                strings = bd.si.notation[1,j] :+ strings :+ bd.si.notation[2,j]
                strings = ((values1 :!= .) :* strings) :+ ((values1 :== .) :* ".")

                B.put_string(row, 1+j, values[sort_order]')
              }

              if(!bd.vi[i].binary) B.set_fmtid((row,pos), 1+j, fmt_stats_c[j])
              else                 B.set_fmtid((row,pos), 1+j, fmt_stats_i[j])
            }

          /* Separator */

            if(series > 1 & i != series)
            {
              B.set_bottom_border(pos, (1,end_col), "thin")
            }

          row = row + vars
        }

        row = row - 1

      /* Borders */

        B.set_left_border((start_row,row), 1, "medium")
        B.set_right_border((start_row,row), 1, "medium")
        B.set_right_border((start_row,row), end_col, "medium")
        B.set_bottom_border(row, (1,end_col), "medium")
    }

  /* function : excelLongOver() */

    void excelLongOver(struct braddev scalar bd,
                       class  xl      scalar B,
                       real           scalar input_row)
    {
      /* General Dimensions */

        sheet = B.query("sheetname")

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total

        p_overall    = bd.opt.test.overall    & (cols(bd.opt.test.stars) == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual & (bd.opt.test.scripts == .     | bd.opt.test.force)
        p_stars      = bd.opt.test.overall    & (cols(bd.opt.test.stars) != 0)
        p_scripts    = bd.opt.test.individual & (bd.opt.test.scripts != .)
        pvalues      = p_overall + (p_individual * lvls)

        stats = cols(bd.si.name)

        row       = input_row
        start_row = row

        end_col = 1 + stats + (pvalues * (1 + bd.opt.test.statistic))

      /* Creating Formats */

        /* Fonts */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Whitespace */

          fmt_whitespace = B.add_fmtid()
          B.fmtid_set_fill_pattern(fmt_whitespace, "solid", "white")

        /* Header */

          fmt_header = B.add_fmtid()
          B.fmtid_set_fontid(fmt_header, font_bold)
          B.fmtid_set_horizontal_align(fmt_header, "center")
          B.fmtid_set_fill_pattern(fmt_header, "solid", bd.opt.excel.color[1])

        /* Variables */

          fmt_question = B.add_fmtid()
          B.fmtid_set_fontid(fmt_question, font_bold)
          B.fmtid_set_horizontal_align(fmt_question, "left")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

        /* Statistics - Numeric Format */

          txt_stats_c = txt_stats_i = J(1, stats, "")

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              txt_stats_c[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundc[i] > 0 ? ("." + (bd.si.roundc[i] * "0")) : "")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_c[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_c[i] + char(34) + bd.si.notation[2,i] + char(34)
              }

            /* Binary */

              txt_stats_i[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundi[i] > 0 ? ("." + (bd.si.roundi[i] * "0")) : "") + (bd.si.percent[i] * bd.si.symbol[i] * "%")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_i[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_i[i] + char(34) + bd.si.notation[2,i] + char(34)
              }
          }

        /* Statistics - Format IDs */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_c[i] :== txt_stats_c[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")

                if(txt_stats_c[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
                }
              }
              else
              {
                fmt_stats_c[i] = fmt_stats_c[pos[1]]
              }

            /* Binary */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_i[i] :== txt_stats_i[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")

                if(txt_stats_i[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
                }
              }
              else
              {
                fmt_stats_i[i] = fmt_stats_i[pos[1]]
              }
          }

        /* P-Values */

          if(pvalues > 0)
          {
            fmt_pval = B.add_fmtid()
            B.fmtid_set_fontid(fmt_pval, font)
            B.fmtid_set_vertical_align(fmt_pval, "center")
            B.fmtid_set_horizontal_align(fmt_pval, "center")
            B.fmtid_set_number_format(fmt_pval, "0.0000")

            if(bd.opt.test.statistic)
            {
              fmt_pstat = B.add_fmtid()
              B.fmtid_set_fontid(fmt_pstat, font)
              B.fmtid_set_vertical_align(fmt_pstat, "center")
              B.fmtid_set_horizontal_align(fmt_pstat, "center")
            }
          }

      /* Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, 1, title)
          B.set_fmtid(row, 1, fmt_title)

          row       = row + 1
          start_row = row
        }

      /* Header - Row 1 */

        cols = 1 + stats + (pvalues * (1 + bd.opt.test.statistic))

        if(pvalues > 0)
        {
          col = 2 + cols(bd.si.label)

          /* Placing Values */

            if(p_overall)
            {
              B.put_string(row, col, J(1, 1 + bd.opt.test.statistic, "Overall"))

              col = col + 1 + bd.opt.test.statistic
            }

            if(p_individual)
            {
              if(bd.opt.over.labels) strings = "vs " :+ bd.oi.labels
              else                   strings = "vs " :+ strofreal(bd.oi.levels)

              B.put_string(row, col, vec(J(1 + bd.opt.test.statistic, 1, strings))')
            }

          /* Merging Cells */

            if(bd.opt.test.statistic)
            {
              col = 2 + cols(bd.si.label)

              for(i=1; i<=pvalues; i++)
              {
                B.set_sheet_merge(sheet, (row,row), (col,col+1))

                col = col + 2
              }
            }

          row = row + 1
        }

      /* Header - Row 2 */

        B.put_string(row, 2, bd.si.label)

        if(pvalues > 0)
        {
          col = 2 + stats

          if(!bd.opt.test.statistic) B.put_string(row, col, J(1, pvalues, "P-Val"))
          else                       B.put_string(row, col, J(1, pvalues, ("Stat", "P-Val")))
        }

        header_row = row
        row        = row + 1

      /* Group Labels */

        if(bd.opt.over.labels) labels = bd.oi.labels'
        else                   labels = "_over_" :+ strofreal(bd.oi.levels)'

        if(bd.opt.over.total) labels = labels \ "Total"

      /* Variables & Statistics */

        series = cols(bd.vi)

        for(i=1; i<=series; i++)
        {
          /* General Information */

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

          /* Variable Names */

            if(series == 1)
            {
              strings = bd.vi[i].answers
            }
            else
            {
              if(bd.vi[i].type != "xi") strings = bd.vi[i].varlist
              else                      strings = bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels)
            }

            strings = strings \ J(1, vars, labels)

            B.put_string(row, 1, vec(strings[.,sort_order]))

          /* Statistics */

            for(j=stats; j; j--)
            {
              postscripts = J(groups, vars, "")

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  cols = cols(bd.opt.test.stars)

                  for(k=cols; k; k--)
                  {
                    postscripts = postscripts :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* "*")
                  }
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  for(k=vars; k; k--)
                  {
                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,k])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    strings = ((pvals :< bd.opt.test.scripts) :* (bd.opt.test.letters[1..groups]))

                    for(l=1; l<=lvls; l++)
                    {
                      postscripts[.,k] = postscripts[.,k] :+ strings[.,l]
                    }
                  }
                }

              /* Statistic */

                pvals = sum(postscripts :!= "")

                if(bd.si.name[j] != "ci")
                {
                  values = getResults(bd.vi[i].results, bd.si.name[j])

                  if(pvals == 0)
                  {
                    if(bd.vi[i].binary & bd.si.percent[j] & !bd.si.symbol[j]) values = values * 100

                    values = J(1, vars, .) \ values[.,sort_order]

                    B.put_number(row, 1+j, vec(values))
                  }
                  else
                  {
                    if(bd.vi[i].binary)
                    {
                      if(bd.si.percent[j]) values = values :* 100

                      fmt = "%32." + strofreal(bd.si.roundi[j]) + "f" + (bd.si.comma[j] * "c")

                      strings = strofreal(values, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                    }
                    else
                    {
                      fmt = "%32." + strofreal(bd.si.roundc[j]) + "f" + (bd.si.comma[j] * "c")

                      strings = strofreal(values, fmt)
                    }

                    strings = bd.si.notation[1,j] :+ strings :+ bd.si.notation[2,j] :+ postscripts

                    pos = (values :!= .)

                    strings = (pos :* strings) :+ (!pos :* ".")
                    strings = J(1, vars, "") \ strings[.,sort_order]

                    B.put_string(row, 1+j, vec(strings))
                  }
                }
                else
                {
                  values1 = getResults(bd.vi[i].results, "lci")
                  values2 = getResults(bd.vi[i].results, "uci")

                  if(bd.vi[i].binary)
                  {
                    if(bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                    fmt = "%32." + strofreal(bd.si.roundi[j]) + "f" + (bd.si.comma[j] * "c")

                    strings = strofreal(values1, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%") :+ bd.si.ci_separator
                    strings = strings :+ strofreal(values2, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                  }
                  else
                  {
                    fmt = "%32." + strofreal(bd.si.roundc[j]) + "f" + (bd.si.comma[j] * "c")

                    strings = strofreal(values1, fmt) :+ bd.si.ci_separator :+ strofreal(values2, fmt)
                  }

                  strings = bd.si.notation[1,j] :+ strings :+ bd.si.notation[2,j] :+ postscripts

                  pos = (values1 :!= .)

                  strings = (pos :* strings) :+ (!pos :* ".")
                  strings = J(1, vars, "") \ strings[.,sort_order]

                  B.put_string(row, 1+j, vec(strings))
                }

              /* Formatting */

                pos = row + (groups * vars) + vars - 1

                if(!bd.vi[i].binary) B.set_fmtid((row,pos), 1+j, fmt_stats_c[j])
                else                 B.set_fmtid((row,pos), 1+j, fmt_stats_i[j])
            }

          /* P-Values - Overall */

            pos = row + (groups * vars) + vars - 1
            col = 2 + stats

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ovr_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  strings = J(1, vars, "") \ J(groups, 1, strings[sort_order])

                  B.put_string(row, col, vec(strings))
                  B.set_fmtid((row,pos), col, fmt_pstat)

                  col = col + 1
                }

              /* P-Values */

                values = bd.vi[i].results.ovr_pvalue
                values = J(1, vars, .) \ J(groups, 1, values[sort_order])

                B.put_number(row, col, vec(values))
                B.set_fmtid((row,pos), col, fmt_pval)

                col = col + 1
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  res_table = J((vars * groups) + vars, groups, "")
                  res_rows = rangex(2, groups, 1)

                  for(j=1; j<=vars; j++)
                  {
                    pos = sort_order[j]

                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_statistic[.,pos])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    strings = strofreal(pvals, "%32.2f")

                    pos = strings :!= "."

                    if(bd.opt.test.t_individual) strings = "t = " :+ strings
                    else                         strings = "F = " :+ strings

                    strings = (pos :* strings) :+ (!pos :* ".")

                    res_table[res_rows,.] = strings

                    res_rows = res_rows :+ groups :+ 1
                  }

                  pos = row + (groups * vars) + vars - 1

                  for(j=1; j<=lvls; j++)
                  {
                    B.put_string(row, col, res_table[.,j])
                    B.set_fmtid((row,pos), col, fmt_pstat)

                    col = col + 1 + bd.opt.test.statistic
                  }
                }

              /* P-Values */

                col = 2 + stats + (p_overall * (1 + bd.opt.test.statistic)) + bd.opt.test.statistic

                pvals = J(groups, groups, .)

                rows = range(2, lvls, 1)
                cols = range(1, lvls-1, 1)

                res_table = J((vars * groups) + vars, groups, .)
                res_rows = rangex(2, groups, 1)

                for(j=1; j<=vars; j++)
                {
                  pos = sort_order[j]

                  pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,pos])
                  _diag(pvals, .)
                  _makesymmetric(pvals)

                  res_table[res_rows,.] = pvals

                  res_rows = res_rows :+ groups :+ 1
                }

                pos = row + (groups * vars) + vars - 1

                for(j=1; j<=lvls; j++)
                {
                  B.put_number(row, col, res_table[.,j])
                  B.set_fmtid((row,pos), col, fmt_pval)

                  col = col + 1 + bd.opt.test.statistic
                }
            }

          row = row + vars + (vars * groups)
        }

        row = row - 1

      /* General Formatting */

        /* Header */

          B.set_fmtid((start_row,header_row), 1, fmt_whitespace)
          B.set_fmtid((start_row,header_row), (2,end_col), fmt_header)
          B.set_top_border(start_row, (1,end_col), "medium")
          B.set_bottom_border(header_row, (1,end_col), "medium")

        /* Variable Names & Separators */

          vars = (row - header_row) / (groups + 1)
          row  = header_row + 1

          for(i=1; i<=vars; i++)
          {
            /* Variable Name */

              B.set_fmtid(row, 1, fmt_question)

              row = row + 1

            /* Group Names */

              pos = row + groups - 1

              B.set_fmtid((row,pos), 1, fmt_answer)

              row = pos

            /* Separator */

              B.set_bottom_border(pos, (1,end_col), "thin")

              row = row + 1
          }

          row = row - 1

        /* Overall Borders */

          B.set_left_border((start_row,row), 1, "medium")
          B.set_right_border((start_row,row), 1, "medium")
          B.set_right_border((start_row,row), end_col, "medium")
          B.set_bottom_border(row, (1,end_col), "medium")

        /* P-Value Borders */

          col = 1 + stats

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            for(i=1; i<=pvalues; i++)
            {
              B.set_right_border((start_row,row), col, "thin")

              col = col + 2
            }
          }
          else if(pvalues > 0)
          {
            B.set_right_border((start_row,row), col, "thin")
          }

      /* Footer */

        row = row + 1

        if(p_stars & bd.opt.test.footer)
        {
          legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
          legend = legend :+ " p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'

          pos = row + cols(bd.opt.test.stars) - 1

          B.put_string(row, 1, legend)
          B.set_fmtid((row,pos), 1, fmt_title)

          row = pos + 1
        }

        if(p_scripts & bd.opt.test.footer)
        {
          legend = bd.opt.test.letters[1..cols(bd.oi.levels)]' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          pos = row + cols(bd.oi.levels) - 1

          B.put_string(row, 1, legend)
          B.set_fmtid((row,pos), 1, fmt_title)

          row = pos + 1
        }
    }

  /* function : excelWide() */

    void excelWide(struct braddev scalar bd,
                   class  xl      scalar B,
                   real           scalar input_row)
    {
      /* General Dimensions */

        sheet = B.query("sheetname")

        lvls   = cols(bd.oi.levels)
        groups = lvls + bd.opt.over.total

        p_overall    = bd.opt.test.overall    & (cols(bd.opt.test.stars) == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual & (bd.opt.test.scripts == .     | bd.opt.test.force)
        p_stars      = bd.opt.test.overall    & (cols(bd.opt.test.stars) != 0)
        p_scripts    = bd.opt.test.individual & (bd.opt.test.scripts != .)
        pvalues      = p_overall + (p_individual * (factorial(lvls)/(2 * factorial(lvls - 2))))

        stats = cols(bd.si.name)

        row       = input_row
        start_row = row

        end_col = 1 + (stats * groups) + (pvalues * (1 + bd.opt.test.statistic))

      /* Creating Formats */

        /* Fonts */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Whitespace */

          fmt_whitespace = B.add_fmtid()
          B.fmtid_set_fill_pattern(fmt_whitespace, "solid", "white")

        /* Header */

          fmt_header = B.add_fmtid()
          B.fmtid_set_fontid(fmt_header, font_bold)
          B.fmtid_set_horizontal_align(fmt_header, "center")
          B.fmtid_set_fill_pattern(fmt_header, "solid", bd.opt.excel.color[1])

        /* Variables */

          fmt_question = B.add_fmtid()
          B.fmtid_set_fontid(fmt_question, font_bold)
          B.fmtid_set_horizontal_align(fmt_question, "left")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

        /* Statistics - Numeric Format */


          txt_stats_c = txt_stats_i = J(1, stats, "")

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              txt_stats_c[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundc[i] > 0 ? ("." + (bd.si.roundc[i] * "0")) : "")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_c[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_c[i] + char(34) + bd.si.notation[2,i] + char(34)
              }

            /* Binary */

              txt_stats_i[i] = (bd.si.comma[i] * "#,##") + "0" + (bd.si.roundi[i] > 0 ? ("." + (bd.si.roundi[i] * "0")) : "") + (bd.si.percent[i] * bd.si.symbol[i] * "%")

              if(bd.si.notation[1,i] != "")
              {
                txt_stats_i[i] = char(34) + bd.si.notation[1,i] + char(34) + txt_stats_i[i] + char(34) + bd.si.notation[2,i] + char(34)
              }
          }

        /* Statistics - Format IDs */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            /* Continuous */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_c[i] :== txt_stats_c[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")

                if(txt_stats_c[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
                }
              }
              else
              {
                fmt_stats_c[i] = fmt_stats_c[pos[1]]
              }

            /* Binary */

              if(i == 1)
              {
                pos = J(0,0,.)
              }
              else
              {
                pos = range(1, i-1, 1)
                pos = selectindex(txt_stats_i[i] :== txt_stats_i[pos])
              }

              if(cols(pos) == 0)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")

                if(txt_stats_i[i] != "")
                {
                  B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
                }
              }
              else
              {
                fmt_stats_i[i] = fmt_stats_i[pos[1]]
              }
          }

        /* P-Values */

          if(pvalues > 0)
          {
            fmt_pval = B.add_fmtid()
            B.fmtid_set_fontid(fmt_pval, font)
            B.fmtid_set_vertical_align(fmt_pval, "center")
            B.fmtid_set_horizontal_align(fmt_pval, "center")
            B.fmtid_set_number_format(fmt_pval, "0.0000")

            if(bd.opt.test.statistic)
            {
              fmt_pstat = B.add_fmtid()
              B.fmtid_set_fontid(fmt_pstat, font)
              B.fmtid_set_vertical_align(fmt_pstat, "center")
              B.fmtid_set_horizontal_align(fmt_pstat, "center")
            }
          }

      /* Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, 1, title)
          B.set_fmtid(row, 1, fmt_title)

          row       = row + 1
          start_row = row
        }

      /* Header - Dimensions */

        stat_statnames = bd.opt.display.statnames
        stat_groupn    = bd.opt.over.group_n
        stat_grouplab  = 1

        pval_statnames = (bd.opt.display.statnames | bd.opt.test.statistic) & (pvalues > 0)
        pval_grouplab  = pvalues > 0

        header = (stat_statnames + stat_groupn + stat_grouplab), (pval_statnames + pval_grouplab)
        header = max(header)

      /* Header - Values */

        for(i=header; i; i--)
        {
          row = start_row + i - 1

          /* Statistics */

            col = 2

            if(stat_statnames)
            {
              labels = J(1, groups, bd.si.label)

              B.put_string(row, 2, labels)

              stat_statnames = 0
            }
            else if(stat_groupn)
            {
              labels = "(n = " :+ strofreal(bd.oi.freqs, "%32.0fc") :+ ")"

              if(bd.opt.over.total) labels = labels, ("(n = " :+ strofreal(sum(bd.oi.freqs), "%32.0fc") :+ ")")

              if(stats == 1)
              {
                B.put_string(row, 2, labels)
              }
              else
              {
                B.put_string(row, 2, vec(J(stats, 1, labels))')

                for(j=1; j<=groups; j++)
                {
                  pos = col + stats - 1

                  B.set_sheet_merge(sheet, (row,row), (col,pos))

                  col = pos + 1
                }
              }

              stat_groupn = 0
            }
            else if(stat_grouplab)
            {
              if(bd.opt.over.labels) labels = bd.oi.labels
              else                   labels = "_over_" :+ strofreal(bd.oi.levels)

              if(bd.opt.over.total) labels = labels, "Total"

              if(stats == 1)
              {
                B.put_string(row, 2, labels)
              }
              else
              {
                B.put_string(row, 2, vec(J(stats, 1, labels))')

                for(j=1; j<=groups; j++)
                {
                  pos = col + stats - 1

                  B.set_sheet_merge(sheet, (row,row), (col,pos))

                  col = pos + 1
                }
              }

              stat_grouplab = 0
            }

          /* P-Values */

            col = 2 + (stats * groups)

            if(pval_statnames)
            {
              if(!bd.opt.test.statistic) B.put_string(row, col, J(1, pvalues, "P-Val"))
              else                       B.put_string(row, col, J(1, pvalues, ("Stat", "P-Val")))

              pval_statnames = 0
            }
            else if(pval_grouplab)
            {
              labels = J(1, pvalues, "")

              pos = 1

              if(p_overall)
              {
                labels[pos] = "Overall"
                pos         = pos + 1
              }

              if(p_individual)
              {
                for(j=1; j<lvls; j++)
                {
                  for(k=j+1; k<=lvls; k++)
                  {
                    labels[pos] = strofreal(bd.oi.levels[j]) + "v" + strofreal(bd.oi.levels[k])
                    pos         = pos + 1
                  }
                }
              }

              if(!bd.opt.test.statistic)
              {
                B.put_string(row, col, labels)
              }
              else
              {
                B.put_string(row, col, vec(J(2, 1, labels))')

                for(j=1; j<=pvalues; j++)
                {
                  pos = col + 1

                  B.set_sheet_merge(sheet, (row,row), (col,pos))

                  col = pos + 1
                }
              }

              pval_grouplab = 0
            }
        }

        header_row = start_row + header - 1
        row        = header_row + 1

      /* Variables & Statistics */

        series = cols(bd.vi)

        for(i=1; i<=series; i++)
        {
          /* General Information */

            sort_order = bd.vi[i].results.sort_order

            vars = cols(bd.vi[i].answers)

          /* Variable Names */

            if(bd.vi[i].type == "individual" | series == 1)
            {
              pos = row + vars - 1

              B.put_string(row, 1, bd.vi[i].answers'[sort_order])
              B.set_fmtid((row,pos), 1, fmt_question)
            }
            else
            {
              B.put_string(row, 1, bd.vi[i].question)
              B.set_fmtid(row, 1, fmt_question)

              row = row + 1
              pos = row + vars - 1

              B.put_string(row, 1, bd.vi[i].answers'[sort_order])
              B.set_fmtid((row,pos), 1, fmt_answer)
            }

          /* Statistics */

            for(j=stats; j; j--)
            {
              postscripts = J(groups, vars, "")

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  cols = cols(bd.opt.test.stars)

                  for(k=cols; k; k--)
                  {
                    postscripts = postscripts :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* "*")
                  }
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  pvals = J(groups, groups, .)

                  rows = range(2, lvls, 1)
                  cols = range(1, lvls-1, 1)

                  for(k=vars; k; k--)
                  {
                    pvals[rows,cols] = invvech(bd.vi[i].results.ind_pvalue[.,k])
                    _diag(pvals, .)
                    _makesymmetric(pvals)

                    strings = ((pvals :< bd.opt.test.scripts) :* (bd.opt.test.letters[1..groups]))

                    for(l=1; l<=lvls; l++)
                    {
                      postscripts[.,k] = postscripts[.,k] :+ strings[.,l]
                    }
                  }
                }

              /* Statistic */

                pvals  = sum(postscripts :!= "")
                isnum  = 1

                if(bd.si.name[j] != "ci")
                {
                  values = getResults(bd.vi[i].results, bd.si.name[j])

                  if(pvals == 0)
                  {
                    if(bd.vi[i].binary & bd.si.percent[j] & !bd.si.symbol[j]) values = values * 100

                    values = values[.,sort_order]'
                  }
                  else
                  {
                    if(bd.vi[i].binary)
                    {
                      if(bd.si.percent[j]) values = values :* 100

                      fmt = "%32." + strofreal(bd.si.roundi[j]) + "f" + (bd.si.comma[j] * "c")

                      strings = strofreal(values, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                    }
                    else
                    {
                      fmt = "%32." + strofreal(bd.si.roundc[j]) + "f" + (bd.si.comma[j] * "c")

                      strings = strofreal(values, fmt)
                    }

                    strings = bd.si.notation[1,j] :+ strings :+ bd.si.notation[2,j] :+ postscripts

                    pos = (values :!= .)

                    strings = (pos :* strings) :+ (!pos :* ".")
                    strings = strings[.,sort_order]'

                    isnum = 0
                  }
                }
                else
                {
                  values1 = getResults(bd.vi[i].results, "lci")
                  values2 = getResults(bd.vi[i].results, "uci")

                  if(bd.vi[i].binary)
                  {
                    if(bd.si.percent[j]) { values1 = values1 :* 100; values2 = values2 :* 100; }

                    fmt = "%32." + strofreal(bd.si.roundi[j]) + "f" + (bd.si.comma[j] * "c")

                    strings = strofreal(values1, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%") :+ bd.si.ci_separator
                    strings = strings :+ strofreal(values2, fmt) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                  }
                  else
                  {
                    fmt = "%32." + strofreal(bd.si.roundc[j]) + "f" + (bd.si.comma[j] * "c")

                    strings = strofreal(values1, fmt) :+ bd.si.ci_separator :+ strofreal(values2, fmt)
                  }

                  strings = bd.si.notation[1,j] :+ strings :+ bd.si.notation[2,j] :+ postscripts

                  pos = (values1 :!= .)

                  strings = (pos :* strings) :+ (!pos :* ".")
                  strings = strings[.,sort_order]'

                  isnum = 0
                }

              /* Placing Statistic */

                pos = row + vars - 1
                col = rangex(1 + j, groups, stats)

                for(k=1; k<=groups; k++)
                {
                  if(isnum) B.put_number(row, col[k], values[.,k])
                  else      B.put_string(row, col[k], strings[.,k])

                  if(!bd.vi[i].binary) B.set_fmtid((row,pos), col[k], fmt_stats_c[j])
                  else                 B.set_fmtid((row,pos), col[k], fmt_stats_i[j])
                }
            }

          /* P-Values - Overall */

            col = 2 + (stats * groups)

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ovr_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  B.put_string(row, col, strings[sort_order]')
                  B.set_fmtid((row,pos), col, fmt_pstat)

                  col = col + 1
                }

              /* P-Values */

                values = bd.vi[i].results.ovr_pvalue

                B.put_number(row, col, values[sort_order]')
                B.set_fmtid((row,pos), col, fmt_pval)

                col = col + 1
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  strings = strofreal(bd.vi[i].results.ind_statistic, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") strings = "Chi2 = " :+ strings
                  else if(bd.opt.test.t_overall)                      strings = "t = " :+ strings
                  else if(bd.opt.test.f_overall)                      strings = "F = " :+ strings

                  strings = strings[.,sort_order]'

                  cols = cols(strings)
                  for(k=1; k<=cols; k++)
                  {
                    B.put_string(row, col, strings[.,k])
                    B.set_fmtid((row,pos), col, fmt_pstat)

                    col = col + 2
                  }

                  col = col - ((groups + 1) * 2) + 1
                }

              /* P-Values */

                values = bd.vi[i].results.ind_pvalue
                values = values[.,sort_order]'

                if(!bd.opt.test.statistic)
                {
                  B.put_number(row, col, values)
                  B.set_fmtid((row,pos), (col,end_col), fmt_pval)
                }
                else
                {
                  cols = cols(values)
                  for(k=1; k<=cols; k++)
                  {
                    B.put_number(row, col, values[.,k])
                    B.set_fmtid((row,pos), col, fmt_pval)

                    col = col + 2
                  }
                }
            }

          /* Separator */

            if(series > 1 & i != series)
            {
              B.set_bottom_border(pos, (1,end_col), "thin")
            }

          row = row + vars
        }

        row = row - 1

      /* General Formatting */

        /* Header */

          B.set_fmtid((start_row,header_row), 1, fmt_whitespace)
          B.set_fmtid((start_row,header_row), (2,end_col), fmt_header)
          B.set_top_border(start_row, (1,end_col), "medium")
          B.set_bottom_border(header_row, (1,end_col), "medium")

        /* Overall Borders */

          B.set_left_border((start_row,row), 1, "medium")
          B.set_right_border((start_row,row), 1, "medium")
          B.set_right_border((start_row,row), end_col, "medium")
          B.set_bottom_border(row, (1,end_col), "medium")

        /* Group Borders */

          if(stats > 1)
          {
            col = 1 + stats

            for(i=1; i<groups; i++)
            {
              B.set_right_border((start_row,row), col, "thin")

              col = col + stats
            }
          }

        /* P-Value Borders */

          col = 1 + (stats * groups)

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            for(i=1; i<=pvalues; i++)
            {
              B.set_right_border((start_row,row), col, "thin")

              col = col + 2
            }
          }
          else if(pvalues > 0)
          {
            B.set_right_border((start_row,row), col, "thin")
          }

      /* Footer */

        row = row + 1

        if(p_stars & bd.opt.test.footer)
        {
          legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
          legend = legend :+ " p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'

          pos = row + cols(bd.opt.test.stars) - 1

          B.put_string(row, 1, legend)
          B.set_fmtid((row,pos), 1, fmt_title)

          row = pos + 1
        }

        if(p_scripts & bd.opt.test.footer)
        {
          legend = bd.opt.test.letters[1..cols(bd.oi.levels)]' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          pos = row + cols(bd.oi.levels) - 1

          B.put_string(row, 1, legend)
          B.set_fmtid((row,pos), 1, fmt_title)

          row = pos + 1
        }
    }

end
