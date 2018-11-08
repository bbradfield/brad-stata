version 14.0
#delimit;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Computes multiple independent means in single table  **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.5.0                                                **
**   Date:         11/08/2018                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions                                                    */
/*======================================================================*/

  program define braddev, rclass sortpreserve byable(recall);
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
                       transmorphic matrix haystack)
    {
      values = J(rows(needle), cols(needle), .)

      for(i=1; i<=rows(needle); i++)
      {
        for(j=1; j<=cols(needle); j++)
        {
          values[i,j] = anyof(haystack, needle[i,j])
        }
      }

      return(values)
    }

/*======================================================================*/
/*   Mata Structure - braddev                                           */
/*======================================================================*/

  /* struct : braddev */

    struct braddev
    {
      struct varInfo  scalar vi
      struct statInfo scalar si
      struct options  scalar opt
      struct overInfo scalar oi
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
      /* XI & Series */
      real   scalar xi_values, xi_variables
      real   scalar series_values, series_variables

      /* Sort */
      string scalar sort_direction, sort_statistic

      /* Table */
      real   scalar print, wide, separator
      string scalar align
    }

  /* struct : options_over */

    struct options_over
    {
      /* Estimation */
      real scalar miss, total

      /* Display */
      real scalar group_n, labels, legend
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
      real   scalar    footer, force
      real   rowvector stars
      real   scalar    scripts
      real   matrix    letters
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
      string matrix color
      string scalar font_face
      real   scalar font_size
    }

  /* function : getOptions() */

    struct options scalar getOptions()
    {
      struct options scalar opt

      /* Display */

        /* Defaults */

          /* XI & Series */
          opt.display.xi_values    = opt.display.series_values    = 1
          opt.display.xi_variables = opt.display.series_variables = 0

          /* Sort */
          opt.display.sort_direction = "-"
          opt.display.sort_statistic = ""

          /* Table */
          opt.display.print     = 1
          opt.display.wide      = 0
          opt.display.separator = 1
          opt.display.align     = "lalign"

        /* Parsing - display */

          input_string = st_local("display")

          t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
          tokenset(t, input_string)

          while((token = tokenget(t)) != "")
          {
            token = strlower(token)

            /* XI & Series */

              if(token == "xi")   { opt.display.xi_values = opt.display.xi_variables = 1; continue; }
              if(token == "noxi") { opt.display.xi_values = opt.display.xi_variables = 0; continue; }

              if(strpos(token, "xival") == 1)   { opt.display.xi_values = 1;    continue; }
              if(strpos(token, "noxival") == 1) { opt.display.xi_values = 0;    continue; }
              if(strpos(token, "xivar") == 1)   { opt.display.xi_variables = 1; continue; }
              if(strpos(token, "noxivar") == 1) { opt.display.xi_variables = 0; continue; }

              if(token == "series")   { opt.display.series_values = opt.display.series_variables = 1; continue; }
              if(token == "noseries") { opt.display.series_values = opt.display.series_variables = 0; continue; }

              if(strpos(token, "seriesval") == 1)   { opt.display.series_values = 1;    continue; }
              if(strpos(token, "noseriesval") == 1) { opt.display.series_values = 0;    continue; }
              if(strpos(token, "seriesvar") == 1)   { opt.display.series_variables = 1; continue; }
              if(strpos(token, "noseriesvar") == 1) { opt.display.series_variables = 0; continue; }

            /* Table */

              if(token == "print")   { opt.display.print = 1; continue; }
              if(token == "noprint") { opt.display.print = 0; continue; }

              if(strpos(token, "sep") == 1)   { opt.display.separator = 1; continue; }
              if(strpos(token, "nosep") == 1) { opt.display.separator = 0; continue; }

              if(token == "wide") { opt.display.wide = 1; continue; }
              if(token == "long") { opt.display.wide = 0; continue; }

              if(strpos(token, "al") == 1)
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  if(strpos(token, "l") == 1) opt.display.align = "lalign"
                  if(strpos(token, "c") == 1) opt.display.align = "center"
                  if(strpos(token, "r") == 1) opt.display.align = "ralign"
                }
                continue
              }
          }

        /* Parsing - sort */

          input_string = strlower(st_local("sort"))

          if(substr(input_string, 1, 1) == "+" | substr(input_string, 1, 1) == "-")
          {
            opt.display.sort_direction = substr(input_string, 1, 1)
            input_string = substr(input_string, 2)
          }

          if(inlist(input_string, tokens("obs nyes mean se sd var min max")))
          {
            opt.display.sort_statistic = input_string
          }

      /* Over */

        /* Defaults */

          /* Estimation */
          opt.over.miss  = 1
          opt.over.total = 0

          /* Display */
          opt.over.group_n = 0
          opt.over.labels  = 1
          opt.over.legend  = 1

        /* Parsing */

          input_string = st_local("overopt")

          t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
          tokenset(t, input_string)

          while((token = tokenget(t)) != "")
          {
            token = strlower(token)

            /* Estimation */

              if(strpos(token, "mi") == 1)   { opt.over.miss = 1; continue; }
              if(strpos(token, "nomi") == 1) { opt.over.miss = 0; continue; }

              if(strpos(token, "tot") == 1)   { opt.over.total = 1; continue; }
              if(strpos(token, "notot") == 1) { opt.over.total = 0; continue; }

            /* Display */

              if(strpos(token, "group") == 1)   { opt.over.group_n = 1; continue; }
              if(strpos(token, "nogroup") == 1) { opt.over.group_n = 0; continue; }

              if(strpos(token, "lab") == 1)   { opt.over.labels = 1; continue; }
              if(strpos(token, "nolab") == 1) { opt.over.labels = 0; continue; }

              if(strpos(token, "leg") == 1)   { opt.over.legend = 1; continue; }
              if(strpos(token, "noleg") == 1) { opt.over.legend = 0; continue; }
          }

      /* Test */

        /* Defaults */

          /* P-Values */
          opt.test.overall    = 0
          opt.test.individual = 0
          opt.test.statistic  = 0

          /* Chi2 Test */
          opt.test.chi_overall = 0

          /* T-Test */
          opt.test.t_overall = opt.test.t_individual = 0

          /* F-Test */
          opt.test.f_overall    = 0
          opt.test.f_individual = 0
          opt.test.f_mtest      = "noadjust"

          /* Significance Notation */
          opt.test.footer  = 1
          opt.test.force   = 0
          opt.test.stars   = J(0, 0, .)
          opt.test.scripts = .
          opt.test.letters = (7468,7470,7472,7473,7475,7476,7477,7478,7479,7480,7481,7482,7484,7486,7487,7488,7489,7490)

        /* Parsing */

          input_string = st_local("test")

          t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
          tokenset(t, input_string)

          while((token = tokenget(t)) != "")
          {
            token = strlower(token)

            /* P-Values */

              if(strpos(token, "stat") == 1)   { opt.test.statistic = 1; continue; }
              if(strpos(token, "nostat") == 1) { opt.test.statistic = 0; continue; }
              if(strpos(token, "foot")  == 1)  { opt.test.footer    = 1; continue; }
              if(strpos(token, "nofo")  == 1)  { opt.test.footer    = 0; continue; }
              if(strpos(token, "force") == 1)  { opt.test.force     = 1; continue; }

            /* Chi2 Test */

              if(strpos(token, "chi") == 1) { opt.test.chi_overall = 1; continue; }

            /* T-Test */

              if(strpos(token, "ttest") == 1)
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)

                  if(strpos(token, "a") == 1) { opt.test.t_overall = opt.test.t_individual = 1; continue; }
                  if(strpos(token, "o") == 1) { opt.test.t_overall                         = 1; continue; }
                  if(strpos(token, "i") == 1) { opt.test.t_individual                      = 1; continue; }
                }
                else
                {
                  opt.test.t_overall = 1
                }
                continue
              }

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
                    if(strpos(token, "a") == 1) { opt.test.f_overall = opt.test.f_individual = 1; continue; }
                    if(strpos(token, "o") == 1) { opt.test.f_overall                         = 1; continue; }
                    if(strpos(token, "i") == 1) { opt.test.f_individual                      = 1; continue; }

                    if(token == "mtest")
                    {
                      if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                      {
                        token = tokenget(t)
                        token = substr(token, 2, strlen(token) - 2)
                        if(strpos(subtoken, "b") == 1) { opt.test.f_mtest = "bonferroni"; continue; }
                        if(strpos(subtoken, "h") == 1) { opt.test.f_mtest = "holm";       continue; }
                        if(strpos(subtoken, "s") == 1) { opt.test.f_mtest = "sidak";      continue; }
                      }
                      continue
                    }
                  }

                  tokenset(t, input_string)
                }
                else
                {
                  opt.test.f_overall = 1
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

                  if(cols(values) > 3)  opt.test.stars = values[., (1..3)]

                  if(cols(values) != 0) opt.test.stars = values
                  else                  opt.test.stars = (0.01, 0.05)
                }
                else
                {
                  opt.test.stars = (0.01, 0.05)
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

                  if(cols(values) != 0) opt.test.scripts = min(values)
                  else                  opt.test.scripts = 0.05
                }
                else
                {
                  opt.test.scripts = 0.05
                }
                continue
              }
          }

        /* Cleaning Up */

          if(cols(opt.test.stars) > 0)
          {
            opt.test.f_overall = 1
          }

          if(opt.test.chi_overall | opt.test.t_overall)
          {
            opt.test.f_overall = 1
          }

          if(opt.test.chi_overall | opt.test.t_overall | opt.test.f_overall)
          {
            opt.test.overall = 1
          }

          if(opt.test.scripts != . & !opt.test.t_individual & !opt.test.f_individual)
          {
            opt.test.f_individual = 1
          }

          if(opt.test.t_individual | opt.test.f_individual)
          {
            opt.test.individual = 1
          }

      /* Weight */

        /* Defaults */

          opt.weight.survey = 0
          opt.weight.subpop = ""

        /* Parsing */

          if(st_local("svy") != "")
          {
            opt.weight.survey = 1
          }

          if(st_local("subpop") != "")
          {
            opt.weight.survey = 1
            opt.weight.subpop = st_local("subpop")
          }

        /* Cleaning Up */

          /* Survey */

            if(opt.weight.survey)
            {
              stata("cap noi _svy_newrule")

              stata("local _ec = _rc")
              if(st_local("_ec") != "0")
              {
                printf("{error:Data is not svyset}\n")
                exit(119)
              }
            }

          /* Subpop */

            if(opt.weight.subpop != "")
            {
              stata("cap levelsof " + opt.weight.subpop + ", local(lvls)")

              levels = st_local("lvls")
              if(levels != "0 1" & levels != "1")
              {
                printf("{error:Subpop variable must be a 0/1 variable}\n")
                exit(119)
              }
            }

          /* Marker */

            if(opt.weight.survey)
            {
              stata("svymarkout " + st_local("touse"))
            }

      /* Excel */

        /* Defaults */

          /* Command Information */
          opt.excel.output      = 0
          opt.excel.bookreplace = opt.excel.sheetreplace = 0

          /* File Information */
          opt.excel.file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")

          /* Style Information */
          opt.excel.color     = ("228 223 236", "238 236 225")
          opt.excel.font_face = "Calibri"
          opt.excel.font_size = 11

        /* Parsing */

          opt.excel.output = st_local("excel") != ""

          input_string = st_local("excel")

          t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
          tokenset(t, input_string)

          while((token = tokenget(t)) != "")
          {
            token = strlower(token)

            if(strpos(token, "rep")      == 1) { opt.excel.bookreplace  = 1;                          continue; }
            if(strpos(token, "mod")      == 1) { opt.excel.bookreplace  = opt.excel.sheetreplace = 0; continue; }
            if(strpos(token, "sheetrep") == 1) { opt.excel.sheetreplace = 1;                          continue; }

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
                  if(direxists(path1) == 0)
                  {
                    printf("{error:Directory does not exist, defaulting to " + `"""' + c("pwd") + `"""' + "}\n")
                    file_path = pathjoin(c("pwd"), path2)
                  }

                  opt.excel.file_path = file_path
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

                  opt.excel.sheet = token
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

                  if(token == "bradfield")       { opt.excel.color = ("228 223 236", "238 236 225"); continue; }
                  if(token == "material_red")    { opt.excel.color = ("255 235 238", "235 255 252"); continue; }
                  if(token == "material_purple") { opt.excel.color = ("243 229 245", "231 245 229"); continue; }
                  if(token == "material_indigo") { opt.excel.color = ("232 234 246", "246 244 232"); continue; }
                  if(token == "material_blue")   { opt.excel.color = ("227 242 253", "253 238 227"); continue; }
                  if(token == "material_green")  { opt.excel.color = ("232 245 233", "245 232 244"); continue; }
                  if(token == "material_orange") { opt.excel.color = ("255 243 224", "224 236 255"); continue; }
                  if(token == "monochrome")      { opt.excel.color = ("255 255 255", "255 255 255"); continue; }
                  if(token == "rti")             { opt.excel.color = ("204 220 233", "233 217 204"); continue; }
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

                  if(token == "arial")     { opt.excel.font_face = "Arial";           continue; }
                  if(token == "calibri")   { opt.excel.font_face = "Calibri";         continue; }
                  if(token == "garamond")  { opt.excel.font_face = "Garamond";        continue; }
                  if(token == "helvetica") { opt.excel.font_face = "Helvetica";       continue; }
                  if(token == "tnr")       { opt.excel.font_face = "Times New Roman"; continue; }
                  if(token == "verdana")   { opt.excel.font_face = "Verdana";         continue; }
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

                  if(token >= 9 & token <= 12) opt.excel.font_size = token
                }
                continue
              }

          }

      return(opt)
    }

/*======================================================================*/
/*   Mata Structure - varInfo                                           */
/*======================================================================*/

  /* struct : varInfo */

    struct varInfo
    {
      /* Name */
      string scalar term
      string matrix varlist

      /* Information */
      string scalar type
      real   scalar binary

      /* Description */
      string matrix answers
      string scalar question

      /* Results */
      struct results scalar results
    }

  /* function : getVarInfo() */

    struct varInfo matrix getVarInfo(struct braddev scalar bd)
    {
      struct varInfo matrix vi
      vi = varInfo(cols(tokens(st_local("0"))), 1)

      positions = J(rows(vi), 1, 0)

      for(i=1; i<=rows(vi); i++)
      {
        /* Getting Token */

          token = strtrim(st_local(strofreal(i)))

          if(token == "if" | token == "in" | token == ",") break
          if(strpos(token, "#") != 0)                      continue

        /* Term */

          vi[i].term = subinstr(token, ",", "")

          if(strpos(vi[i].term, ".") != 0) vi[i].term = "i." + substr(vi[i].term, strpos(vi[i].term, ".") + 1)

        /* Varlist */

          stata("cap ds " + substr(vi[i].term, strpos(vi[i].term, ".") + 1) + ", has(type numeric)")
          vi[i].varlist = tokens(st_global("r(varlist)"))'

          if(rows(vi[i].varlist) == 0) continue

        /* Type */

          vi[i].type = (rows(vi[i].varlist) > 1) ? "series" : (strpos(vi[i].term, "i.") == 1 ? "xi" : "individual")

          if(strpos(vi[i].term, "i.") == 1) vi[i].type = "xi"

        /* Binary, Answers, and Question */

          /* Individual */

            if(vi[i].type == "individual")
            {
              stata("cap count if !missing(" + vi[i].varlist + ") & " + vi[i].varlist + " != 1 & " + vi[i].varlist + " != 0 ")
              vi[i].binary = st_numscalar("r(N)") == 0
              vi[i].answers = vi[i].varlist

              vi[i].question = st_varlabel(vi[i].varlist)
              if(vi[i].question == "") vi[i].question = vi[i].term
            }

          /* XI */

            if(vi[i].type == "xi")
            {
              vi[i].binary = 1

              vi[i].question = st_varlabel(vi[i].varlist)
              if(vi[i].question == "") vi[i].question = vi[i].term

              stata("cap levelsof " + vi[i].varlist + ", local(levels)")
              levels = tokens(st_local("levels"))'

              vi[i].varlist = vi[i].varlist :+ "==" :+ levels

              if(st_varvaluelabel(substr(vi[i].term, 3)) == "" | !bd.opt.display.xi_values)
              {
                vi[i].answers = vi[i].varlist
              }
              else
              {
                vi[i].answers = st_vlmap(st_varvaluelabel(substr(vi[i].term, 3)), strtoreal(levels))
                vi[i].answers[selectindex(vi[i].answers :== "")] = vi[i].varlist[selectindex(vi[i].answers :== "")]
              }
            }

          /* Series */

            if(vi[i].type == "series")
            {
              vi[i].binary = 1
              descriptions = J(rows(vi[i].varlist), 1, "")

              for(j=1; j<=rows(vi[i].varlist); j++)
              {
                descriptions[j] = st_varlabel(vi[i].varlist[j])

                if(vi[i].binary == 1)
                {
                  stata("cap count if !missing(" + vi[i].varlist[j] + ") & " + vi[i].varlist[j] + " != 1 & " + vi[i].varlist[j] + " != 0 ")
                  if(st_numscalar("r(N)") > 0) vi[i].binary = 0
                }
              }

              vi[i].answers = substr(descriptions, strpos(descriptions, "[") :+ 1, strpos(descriptions, "]") :- 2)
              vi[i].answers[selectindex(vi[i].answers :== "")] = vi[i].varlist[selectindex(vi[i].answers :== "")]

              descriptions = strtrim(substr(descriptions, strpos(descriptions, "]") :+ 1))

              vi[i].question = select(descriptions, udstrlen(descriptions) :== max(udstrlen(descriptions)))[1]

              if(!bd.opt.display.series_values) vi[i].answers = vi[i].varlist
            }

        /* Position */

          positions[i] = 1

          if(substr(token, -1, 1) == ",") break
      }

      vi = select(vi, positions)

      return(vi)
    }

/*======================================================================*/
/*   Mata Structure - overInfo                                          */
/*======================================================================*/

  /* struct : overInfo */

    struct overInfo
    {
      /* Name */
      string scalar name
      string matrix varlist

      /* Levels & Frequencies */
      real   matrix levels, freqs
      string matrix labels
    }

  /* function : getOverInfo() */

    struct overInfo scalar getOverInfo(struct braddev scalar bd)
    {
      struct overInfo scalar oi

      if(st_local("over") == "")
      {
        bd.opt.test.overall    = 0
        bd.opt.test.individual = 0
        bd.opt.display.wide    = 0
        return(oi)
      }

      oi.varlist = tokens(st_local("over"))
      oi.name    = st_tempname()

      /* Marking Out Variables */

        if(!bd.opt.over.miss)
        {
          varlist = ""
          for(i=1; i<=rows(bd.vi); i++)
          {
            if(bd.vi[i].type == "xi") varlist = varlist + " " + subinstr(bd.vi[i].term, "i.", "")
            else                      varlist = varlist + " " + invtokens(bd.vi[i].varlist')
          }

          stata("cap markout " + st_local("touse") + " " + varlist)
        }

        stata("cap replace " + st_local("touse") + " = 0 if missing(" + invtokens(oi.varlist, ",") + ")")

      /* Generating Variable */

        group_str = st_tempname()
        group_num = st_tempname()

        stata("cap egen " + group_str + " = concat(" + st_local("over") + ") if " + st_local("touse") + `", decode punct(", ")"')
        stata("cap egen " + group_num + " = group("  + st_local("over") + ") if " + st_local("touse"))

        stata("cap levelsof " + group_num + ", local(lvls)")
        levels = tokens(st_local("lvls"))

        num_format = "%0" + strofreal(max(udstrlen(levels))) + ".0f"
        num_format = char(34) + num_format + char(34)

        stata("cap replace " + group_str + " = string(" + group_num + ", " + num_format + ") + " + `"" ""' + " + " + group_str + " if " + st_local("touse"))
        stata("cap encode " + group_str + ", generate(" + oi.name + ")")

        stata("cap drop " + group_str)
        stata("cap drop " + group_num)

      /* Getting Levels */

        matcell = st_tempname()
        matrow  = st_tempname()

        stata("cap tab " + oi.name + ", matcell(" + matcell + ") matrow(" + matrow + ")")

        oi.levels = st_matrix(matrow)
        oi.freqs  = st_matrix(matcell)

        if(bd.opt.over.labels)
        {
          oi.labels = st_vlmap(oi.name, oi.levels)
          oi.labels = strtrim(substr(oi.labels, strpos(oi.labels, " ") :+ 1))
        }
        else
        {
          oi.labels = "_over_" :+ strofreal(oi.levels)
        }

        positions = selectindex(oi.labels :== "")

        if(rows(positions) > 0)
        {
          oi.labels[positions] = "_over_" :+ strofreal(oi.levels[positions])
        }

      /* Cleaning Up */

        if(rows(oi.levels) == 1)
        {
          bd.opt.over.total      = 0
          bd.opt.test.overall    = 0
          bd.opt.test.individual = 0
        }

        if(rows(oi.levels) != 2)
        {
          bd.opt.test.t_overall = 0
        }

        if(rows(oi.levels) == 2)
        {
          if(bd.opt.test.t_individual) bd.opt.test.t_overall = bd.opt.test.overall = 1
          if(bd.opt.test.f_individual) bd.opt.test.f_overall = bd.opt.test.overall = 1
          bd.opt.test.individual = 0
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
      string matrix name, label

      /* Format - Rounding */
      real   matrix roundc, roundi

      /* Format - Notation */
      real   matrix comma, percent, symbol
      string matrix notation

      /* Format - P-Values */
      real   matrix stars, scripts

      /* CI Specific */
      real   scalar ci_level, ci_proportion
      real   scalar ci_combined
      string scalar ci_separator
    }

  /* function : getStatInfo() */

    struct statInfo scalar getStatInfo(struct braddev scalar bd)
    {
      struct statInfo scalar si

      /* Getting Stat List */

        st_local("statlist", "obs nyes mean se sd var lci uci min max")
        st_local("stats", strlower(subinword(st_local("stats"), "all", "obs nyes mean se sd var lci uci min max")))
        st_local("stats", subinword(st_local("stats"), "ci", "lci uci"))

        stata("local stats : list uniq stats")
        stata("local stats : list stats & statlist")

        statlist = tokens(st_local("stats"))'

        if(rows(statlist) == 0)
        {
          if(bd.opt.display.wide) statlist = "mean"
          else                    statlist = tokens("obs nyes mean sd lci uci")'
        }

      /* Setting Defaults */

        /* Names */
        si.name  = tokens(st_local("statlist"))'
        si.label = ("Obs" \ "n(Yes)" \ "Mean" \ "Std Err" \ "Std Dev" \ "Variance" \ "Lower CI" \ "Upper CI" \ "Min" \ "Max")

        /* Format - Rounding */
        si.roundc = si.roundi = J(10, 1, 7)

        /* Format - Notation */
        si.comma    = J(10, 1, 1)
        si.percent  = J(10, 1, 0)
        si.symbol   = J(10, 1, 1)
        si.notation = J(10, 2, "")

        /* Format - P-Values */
        si.stars = si.scripts = J(10, 1, 0)
        si.stars[3] = 1
        si.scripts[8] = 1

        /* CI Specific */
        si.ci_level      = strtoreal(st_global("S_level"))
        si.ci_proportion = 0
        si.ci_combined   = 0
        si.ci_separator  = ","

      /* Parsing Format */

        input_string = st_local("format")
        name_list    = ("count", "error", "minmax", "ci", "obs", "nyes", "mean", "se", "sd", "var", "lci", "uci", "min", "max")

        t = tokeninit(" ", (""), (`""""', `"`""'"', "()"), 1)
        tokenset(t, input_string)

        while((token = tokenget(t)) != "")
        {
          token = strlower(token)

          /* All */

            /* Round */

              if(token == "round")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundc = si.roundi = J(10, 1, token)
                }
                continue
              }

            /* RoundC */

              if(token == "roundc" | token == "round_c")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundc = J(10, 1, token)
                }
                continue
              }

            /* RoundI */

              if(token == "roundi" | token == "round_i")
              {
                if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                {
                  token = tokenget(t)
                  token = substr(token, 2, strlen(token) - 2)
                  token = strtoreal(token)
                  if(token >= 0 & token <= 7) si.roundi = J(10, 1, token)
                }
                continue
              }

            /* Percent */

              if(token == "pct" | strpos(token, "per") == 1)
              {
                si.percent = J(10, 1, 1)
                continue
              }

              if(token == "nopct" | strpos(token, "noper") == 1)
              {
                si.percent = J(10, 1, 0)
                continue
              }

            /* Symbol */

              if(strpos(token, "sym") == 1)
              {
                si.symbol = J(10, 1, 1)
                continue
              }

              if(strpos(token, "nosym") == 1)
              {
                si.symbol = J(10, 1, 0)
                continue
              }

          /* Specific Variables - Getting Positions */

            if(!anyof(name_list, token)) continue
            else                         positions = J(0, 0, .)

            if(token == "count")  positions = (1 \ 2)
            if(token == "error")  positions = (4 \ 5 \ 6)
            if(token == "ci")     positions = (7 \ 8)
            if(token == "minmax") positions = (9 \ 10)

            if(rows(positions) == 0 & cols(positions) == 0)
            {
              positions = selectindex(si.name :== token)
            }

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
                    if(token >= 0 & token <= 7) si.roundc[positions] = si.roundi[positions] = J(rows(positions), cols(positions), token)
                  }
                  continue
                }

              /* RoundC */

                if(token == "roundc" | token == "round_c")
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)
                    token = strtoreal(token)
                    if(token >= 0 & token <= 7) si.roundc[positions] = J(rows(positions), cols(positions), token)
                  }
                  continue
                }

              /* RoundI */

                if(token == "roundi" | token == "round_i")
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)
                    token = strtoreal(token)
                    if(token >= 0 & token <= 7) si.roundi[positions] = J(rows(positions), cols(positions), token)
                  }
                  continue
                }

              /* Percent */

                if(token == "pct" | strpos(token, "per") == 1)
                {
                  si.percent[positions] = J(rows(positions), cols(positions), 1)
                  continue
                }

                if(token == "nopct" | strpos(token, "noper") == 1)
                {
                  si.percent[positions] = J(rows(positions), cols(positions), 0)
                  continue
                }

              /* Symbol */

                if(strpos(token, "sym") == 1)
                {
                  si.symbol[positions] = J(rows(positions), cols(positions), 1)
                  continue
                }

                if(strpos(token, "nosym") == 1)
                {
                  si.symbol[positions] = J(rows(positions), cols(positions), 0)
                  continue
                }

              /* Comma */

                if(token == "comma")
                {
                  si.comma[positions] = J(rows(positions), cols(positions), 1)
                  continue
                }

                if(token == "nocomma")
                {
                  si.comma[positions] = J(rows(positions), cols(positions), 0)
                  continue
                }

              /* Stars */

                if(strpos(token, "star") == 1)
                {
                  si.stars[positions] = J(rows(positions), cols(positions), 1)
                  continue
                }

                if(strpos(token, "nostar") == 1)
                {
                  si.stars[positions] = J(rows(positions), cols(positions), 0)
                  continue
                }

              /* Scripts */

                if(strpos(token, "script") == 1)
                {
                  si.scripts[positions] = J(rows(positions), cols(positions), 1)
                  continue
                }

                if(strpos(token, "noscript") == 1)
                {
                  si.scripts[positions] = J(rows(positions), cols(positions), 0)
                  continue
                }

              /* Notation */

                if(strpos(token, "not") == 1)
                {
                  if(substr(tokenpeek(t), 1, 1) == "(" & substr(tokenpeek(t), -1, 1) == ")")
                  {
                    token = tokenget(t)
                    token = substr(token, 2, strlen(token) - 2)

                    if(strpos(token, "par") == 1) si.notation[positions,.] = J(rows(positions), 1, ("(", ")"))
                    if(strpos(token, "bra") == 1) si.notation[positions,.] = J(rows(positions), 1, ("[", "]"))
                  }
                  continue
                }

              /* CI Specific */

                if(anyof(positions, 7) | anyof(positions, 8))
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

                    if(strpos(token, "prop") == 1)
                    {
                      si.ci_proportion = 1
                      continue
                    }

                  /* Combined */

                    if(strpos(token, "comb") == 1)
                    {
                      si.ci_combined = 1
                      continue
                    }

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

      /* Cleaning Format */

        si.comma[(3..8)]   = J(6, 1, 0)

        si.percent[(1..2)] = si.symbol[(1..2)] = J(2, 1, 0)
        si.roundc[(1..2)]  = si.roundi[(1..2)] = J(2, 1, 0)

        if(rows(bd.oi.levels) > 18)
        {
          si.scripts = J(10, 1, 0)
        }

      /* Selecting Statistics */

        positions = J(rows(statlist), 1, .)

        for(i=1; i<=rows(positions); i++)
        {
          positions[i] = selectindex(si.name :== statlist[i])
        }

        if(si.ci_combined)
        {
          positions = select(positions, positions :!= 7)
        }

        si.name     = si.name[positions]
        si.label    = si.label[positions]
        si.roundc   = si.roundc[positions]
        si.roundi   = si.roundi[positions]
        si.comma    = si.comma[positions]
        si.percent  = si.percent[positions]
        si.symbol   = si.symbol[positions]
        si.notation = si.notation[positions, .]
        si.stars    = si.stars[positions]
        si.scripts  = si.scripts[positions]

        if(si.ci_combined)
        {
          positions = selectindex(si.name :== "uci")
          if(rows(positions) > 0)
          {
            si.name[positions] = "ci"
            si.label[positions] = "Confidence Interval"
            if(si.notation[positions,1] == "") si.notation[positions,.] = ("[", "]")
          }
        }

      return(si)
    }

/*======================================================================*/
/*   Mata Structure - results                                           */
/*======================================================================*/

  /* struct : results */

    struct results
    {
      /* Count */
      real matrix obs, nyes

      /* Mean */
      real matrix mean

      /* Error */
      real matrix lci, uci, se, sd, var

      /* MinMax */
      real matrix min, max

      /* Calculation only */
      real matrix t, df

      /* P-Values */
      real matrix ovr_statistic, ovr_pvalue
      real matrix ind_statistic, ind_pvalue
    }

  /* function : gatherResults() */

    void gatherResults(struct braddev scalar bd)
    {
      for(i=1; i<=rows(bd.vi); i++)
      {
        if(bd.vi[i].type == "xi") bd.vi[i].results = calculateXI(bd, i)
        else                      bd.vi[i].results = calculateSeries(bd, i)
      }

      if(bd.opt.weight.survey)
      {
        bd.opt.test.chi_overall = 0
      }
    }

  /* function : calculateXI() */

    struct results scalar calculateXI(struct braddev scalar bd,
                                      real           scalar vi_position)
    {
      struct varInfo scalar vi
      struct results scalar res

      vi = bd.vi[vi_position]

      /* Defining Results */

        rows = rows(vi.answers)

        if(bd.oi.name == "") groups = 1
        else                 groups = rows(bd.oi.levels) + bd.opt.over.total

        res.obs = res.nyes = J(rows, groups, 0)
        res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(rows, groups, .)

      /* Defining Commands */

        /* Mean */

          cmd_mean = "cap xi, noomit: "

          if(bd.opt.weight.survey)
          {
            if(bd.opt.weight.subpop != "") cmd_mean = cmd_mean + "svy, subpop(" + bd.opt.weight.subpop + "): "
            else                           cmd_mean = cmd_mean + "svy: "
          }

          cmd_mean = cmd_mean + "mean " + vi.term + " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ") "

          if(groups > 1)
          {
            cmd_mean = cmd_mean + "over(" + bd.oi.name + ", nolabel)"
          }

        /* Count, Min, & Max */

          cmd_count = "cap xi, noomit: tabstat " + vi.term + " if " + st_local("touse") + ", stat(sum min max) save"
          if(groups > 1) cmd_count = cmd_count + " by(" + bd.oi.name + ")"

      /* Calculating Results (1) */

        /* Mean */

          stata(cmd_mean)

          pos = 1
          if(groups > 1) pos = strtoreal(tokens(st_global("e(over_namelist)")))

          res.mean[.,pos] = rowshape(st_matrix("r(table)")[1,.], rows)
          res.se[.,pos]   = rowshape(st_matrix("r(table)")[2,.], rows)
          res.t[.,pos]    = rowshape(st_matrix("r(table)")[3,.], rows)
          res.lci[.,pos]  = rowshape(st_matrix("r(table)")[5,.], rows)
          res.uci[.,pos]  = rowshape(st_matrix("r(table)")[6,.], rows)
          res.df[.,pos]   = rowshape(st_matrix("r(table)")[7,.], rows)

          if(bd.opt.weight.subpop == "") res.obs[.,pos] = rowshape(st_matrix("e(_N)"), rows)
          else                           res.obs[.,pos] = rowshape(st_matrix("e(_N_subp)"), rows)

        /* SD & Var */

          stata("cap estat sd")

          res.sd[.,pos]  = rowshape(st_matrix("r(sd)"), rows)
          res.var[.,pos] = rowshape(st_matrix("r(variance)"), rows)

        /* Count, Min, & Max */

          stata(cmd_count)

          if(groups == 1)
          {
            res.nyes[.,1] = st_matrix("r(StatTotal)")'[.,1]
            res.min[.,1]  = st_matrix("r(StatTotal)")'[.,2]
            res.max[.,1]  = st_matrix("r(StatTotal)")'[.,3]
          }
          else
          {
            for(i=1; i<=rows(bd.oi.levels); i++)
            {
              res.nyes[.,i] = st_matrix("r(Stat" + strofreal(i) + ")")'[.,1]
              res.min[.,i]  = st_matrix("r(Stat" + strofreal(i) + ")")'[.,2]
              res.max[.,i]  = st_matrix("r(Stat" + strofreal(i) + ")")'[.,3]
            }

            if(bd.opt.over.total)
            {
              res.nyes[.,groups] = st_matrix("r(StatTotal)")'[.,1]
              res.min[.,groups]  = st_matrix("r(StatTotal)")'[.,2]
              res.max[.,groups]  = st_matrix("r(StatTotal)")'[.,3]
            }
          }

        /* Testing - Individual */

          levels = rows(bd.oi.levels)

          if(bd.opt.test.individual)
          {
            res.ind_statistic = res.ind_pvalue = J(rows, rows(bd.oi.levels) * rows(bd.oi.levels), .)

            if(bd.opt.test.t_individual)
            {
              for(i=1; i<=rows; i++)
              {
                for(j=1; j<=cols(pos); j++)
                {
                  for(k=j+1; k<=cols(pos); k++)
                  {
                    stata("cap ttesti " + strofreal(res.obs[i,pos[j]]) + " " + strofreal(res.mean[i,pos[j]]) + " " + strofreal(res.sd[i,pos[j]]) + " " + strofreal(res.obs[i,pos[k]]) + " " + strofreal(res.mean[i,pos[k]]) + " " + strofreal(res.sd[i,pos[k]]) + ", unequal level(" + strofreal(bd.si.ci_level) + ") ")
                    res.ind_statistic[i,((pos[j] - 1) * levels) + pos[k]] = res.ind_statistic[i,((pos[k] - 1) * levels) + pos[j]] = st_numscalar("r(t)")
                    res.ind_pvalue[i,((pos[j] - 1) * levels) + pos[k]]    = res.ind_pvalue[i,((pos[k] - 1) * levels) + pos[j]]    = st_numscalar("r(p)")
                  }
                }
              }
            }
            else
            {
              varlist = tokens(st_global("e(varlist)"))
              for(i=1; i<=rows; i++)
              {
                for(j=1; j<=levels; j++)
                {
                  stata("cap test " + "[" + varlist[i] + "]" + strofreal(pos[j]) + " == " + invtokens(("[" + varlist[i] + "]") :+ strofreal(pos), " == ") + ", mtest(" + bd.opt.test.f_mtest + ")")
                  res.ind_statistic[i,((pos[j] - 1) * levels) :+ pos] = st_matrix("r(mtest)")[.,1]'
                  res.ind_pvalue[i,((pos[j] - 1) * levels) :+ pos]    = st_matrix("r(mtest)")[.,3]'
                }
              }
            }
          }

        /* Testing - Overall */

          if(bd.opt.test.overall)
          {
            if(bd.opt.test.chi_overall)
            {
              if(bd.opt.weight.survey)
              {
                if(bd.opt.weight.subpop != "") stata("cap svy, subpop(" + bd.opt.weight.subpop + "): tab " + substr(vi.term, 3) + " " + bd.oi.name)
                else                           stata("cap svy: tab " + substr(vi.term, 3) + " " + bd.oi.name)

                res.ovr_statistic = J(rows, 1, st_numscalar("e(F_Penl)"))
                res.ovr_pvalue    = J(rows, 1, st_numscalar("e(p_Penl)"))
              }
              else
              {
                stata("cap tab " + substr(vi.term, 3) + " " + bd.oi.name + ", chi2")
                res.ovr_statistic = J(rows, 1, st_numscalar("r(chi2)"))
                res.ovr_pvalue    = J(rows, 1, st_numscalar("r(p)"))
              }
            }
            else
            {
              res.ovr_statistic = res.ovr_pvalue = J(rows, 1, .)

              if(bd.opt.test.t_overall)
              {
                for(i=1; i<=rows; i++)
                {
                  stata("cap ttesti " + strofreal(res.obs[i,1]) + " " + strofreal(res.mean[i,1]) + " " + strofreal(res.sd[i,1]) + " " + strofreal(res.obs[i,2]) + " " + strofreal(res.mean[i,2]) + " " + strofreal(res.sd[i,2]) + ", unequal level(" + strofreal(bd.si.ci_level) + ") ")
                  res.ovr_statistic[i] = st_numscalar("r(t)")
                  res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
              }
              else
              {
                varlist = tokens(st_global("e(varlist)"))
                for(i=1; i<=rows; i++)
                {
                  stata("cap test " + invtokens(("[" + varlist[i] + "]") :+ strofreal(pos), " == ") + ", mtest(" + bd.opt.test.f_mtest + ")")
                  res.ovr_statistic[i] = st_numscalar("r(F)")
                  res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
              }
            }
          }

      /* Calculating Results (2) */

        if(bd.opt.over.total == 1)
        {
          pos = rows(bd.oi.levels) + bd.opt.over.total

          /* Mean */

            cmd_mean = "cap xi, noomit: "

            if(bd.opt.weight.survey)
            {
              if(bd.opt.weight.subpop != "") cmd_mean = cmd_mean + "svy, subpop(" + bd.opt.weight.subpop + "): "
              else                           cmd_mean = cmd_mean + "svy: "
            }

            cmd_mean = cmd_mean + "mean " + vi.term + " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ") "

            stata(cmd_mean)

            res.mean[.,pos] = rowshape(st_matrix("r(table)")[1,.], rows)
            res.se[.,pos]   = rowshape(st_matrix("r(table)")[2,.], rows)
            res.t[.,pos]    = rowshape(st_matrix("r(table)")[3,.], rows)
            res.lci[.,pos]  = rowshape(st_matrix("r(table)")[5,.], rows)
            res.uci[.,pos]  = rowshape(st_matrix("r(table)")[6,.], rows)
            res.df[.,pos]   = rowshape(st_matrix("r(table)")[7,.], rows)

            if(bd.opt.weight.subpop == "") res.obs[.,pos] = rowshape(st_matrix("e(_N)"), rows)
            else                           res.obs[.,pos] = rowshape(st_matrix("e(_N_subp)"), rows)

          /* SD & Var */

            stata("cap estat sd")

            res.sd[.,pos]  = rowshape(st_matrix("r(sd)"), rows)
            res.var[.,pos] = rowshape(st_matrix("r(variance)"), rows)
        }

      /* Logit CIs */

        if(bd.si.ci_proportion)
        {
          res.lci = invlogit(logit(res.mean) :- invttail(max(res.df), ((100 - bd.si.ci_level) / 200)) :* (res.se :/ (res.mean :* (1 :- res.mean))))
          res.uci = invlogit(logit(res.mean) :+ invttail(max(res.df), ((100 - bd.si.ci_level) / 200)) :* (res.se :/ (res.mean :* (1 :- res.mean))))
        }

      /* Sorting */

        if(bd.opt.display.sort_statistic != "")
        {
          if(bd.opt.display.sort_statistic == "obs")  values = res.obs
          if(bd.opt.display.sort_statistic == "nyes") values = res.nyes
          if(bd.opt.display.sort_statistic == "mean") values = res.mean
          if(bd.opt.display.sort_statistic == "se")   values = res.se
          if(bd.opt.display.sort_statistic == "sd")   values = res.sd
          if(bd.opt.display.sort_statistic == "var")  values = res.var
          if(bd.opt.display.sort_statistic == "min")  values = res.min
          if(bd.opt.display.sort_statistic == "max")  values = res.max

          if((bd.opt.display.sort_statistic == "min" | bd.opt.display.sort_statistic == "max") & vi.binary == 0)
          {
            return(res)
          }

          values = range(1, rows, 1), values

          if(bd.opt.display.sort_direction == "+") values = sort(values, 2)
          else                                     values = sort(values, -2)

          if(rows(selectindex(values[.,2] :== .)) > 0)
          {
            positions = values[selectindex(values[.,2] :!= .),1] \ values[selectindex(values[.,2] :== .),1]
          }
          else
          {
            positions = values[.,1]
          }

          bd.vi[vi_position].varlist = bd.vi[vi_position].varlist[positions]
          bd.vi[vi_position].answers = bd.vi[vi_position].answers[positions]

          res.obs  = res.obs[positions,.]
          res.nyes = res.nyes[positions,.]
          res.mean = res.mean[positions,.]
          res.lci  = res.lci[positions,.]
          res.uci  = res.uci[positions,.]
          res.se   = res.se[positions,.]
          res.sd   = res.sd[positions,.]
          res.var  = res.var[positions,.]
          res.min  = res.min[positions,.]
          res.max  = res.max[positions,.]
          res.t    = res.t[positions,.]
          res.df   = res.df[positions,.]

          if(rows(res.ovr_statistic) > 0) res.ovr_statistic = res.ovr_statistic[positions,.]
          if(rows(res.ovr_pvalue)    > 0) res.ovr_pvalue    = res.ovr_pvalue[positions,.]
          if(rows(res.ind_statistic) > 0) res.ind_statistic = res.ind_statistic[positions,.]
          if(rows(res.ind_pvalue)    > 0) res.ind_pvalue    = res.ind_pvalue[positions,.]
        }

      return(res)
    }

  /* function : calculateSeries() */

    struct results scalar calculateSeries(struct braddev scalar bd,
                                          real           scalar vi_position)
    {
      struct varInfo scalar vi
      struct results scalar res

      vi = bd.vi[vi_position]

      /* Defining Results */

        rows = rows(vi.answers)

        if(bd.oi.name == "") groups = 1
        else                 groups = rows(bd.oi.levels) + bd.opt.over.total

        res.obs = J(rows, groups, 0)
        res.mean = res.lci = res.uci = res.se = res.sd = res.var = res.min = res.max = res.t = res.df = J(rows, groups, .)

        if(vi.binary) res.nyes = res.obs
        else          res.nyes = res.mean

        if(bd.opt.test.overall)
        {
          res.ovr_statistic = res.ovr_pvalue = J(rows, 1, .)
        }

        if(bd.opt.test.individual)
        {
          res.ind_statistic = res.ind_pvalue = J(rows, rows(bd.oi.levels) * rows(bd.oi.levels), .)
        }

      /* Defining Commands */

        /* Mean */

          cmd_mean = J(1, 4, "")

          cmd_mean[1] = "cap xi, noomit: "
          if(bd.opt.weight.survey)
          {
            if(bd.opt.weight.subpop != "") cmd_mean[1] = cmd_mean[1] + "svy, subpop(" + bd.opt.weight.subpop + "): "
            else                           cmd_mean[1] = cmd_mean[1] + "svy: "
          }
          cmd_mean[1] = cmd_mean[1] + "mean"

          cmd_mean[3] = " if " + st_local("touse") + ", level(" + strofreal(bd.si.ci_level) + ") "

          if(groups > 1) cmd_mean[4] = "over(" + bd.oi.name + ", nolabel)"

        /* Count, Min, & Max */

          cmd_count = J(1, 4, "")

          cmd_count[1] = "cap xi, noomit: tabstat"
          cmd_count[3] = "if " + st_local("touse") + ", stat(sum min max) save"
          if(groups > 1) cmd_count[4] = "by(" + bd.oi.name + ")"

      /* Calculating Results */

        for(i=1; i<=rows; i++)
        {
          cmd_mean[2] = cmd_count[2] = vi.varlist[i]

          /* Non-Total */

            /* Mean */

              stata(invtokens(cmd_mean))
              stata("local _rc = _rc")

              if(st_local("_rc") != "0")
              {
                continue
              }

              pos = 1
              if(groups > 1) pos = strtoreal(tokens(st_global("e(over_namelist)")))

              res.mean[i,pos] = st_matrix("r(table)")[1,.]
              res.se[i,pos]   = st_matrix("r(table)")[2,.]
              res.t[i,pos]    = st_matrix("r(table)")[3,.]
              res.lci[i,pos]  = st_matrix("r(table)")[5,.]
              res.uci[i,pos]  = st_matrix("r(table)")[6,.]
              res.df[i,pos]   = st_matrix("r(table)")[7,.]

              if(bd.opt.weight.subpop == "") res.obs[i,pos] = st_matrix("e(_N)")
              else                           res.obs[i,pos] = st_matrix("e(_N_subp)")

            /* SD & Var */

              stata("cap estat sd")

              res.sd[i,pos]  = st_matrix("r(sd)")
              res.var[i,pos] = st_matrix("r(variance)")

            /* Count, Min, & Max */

              stata(invtokens(cmd_count))

              if(groups == 1)
              {
                if(vi.binary) res.nyes[i,1] = st_matrix("r(StatTotal)")'[.,1]

                res.min[i,1] = st_matrix("r(StatTotal)")'[.,2]
                res.max[i,1] = st_matrix("r(StatTotal)")'[.,3]
              }
              else
              {
                for(j=1; j<=rows(bd.oi.levels); j++)
                {
                  if(vi.binary) res.nyes[i,j] = st_matrix("r(Stat" + strofreal(j) + ")")'[.,1]

                  res.min[i,j] = st_matrix("r(Stat" + strofreal(j) + ")")'[.,2]
                  res.max[i,j] = st_matrix("r(Stat" + strofreal(j) + ")")'[.,3]
                }

                if(bd.opt.over.total)
                {
                  if(vi.binary) res.nyes[i,groups] = st_matrix("r(StatTotal)")'[.,1]

                  res.min[i,groups] = st_matrix("r(StatTotal)")'[.,2]
                  res.max[i,groups] = st_matrix("r(StatTotal)")'[.,3]
                }
              }

            /* Testing - Overall */

              if(bd.opt.test.overall)
              {
                if(bd.opt.test.t_overall)
                {
                  stata("cap ttesti " + strofreal(res.obs[i,1]) + " " + strofreal(res.mean[i,1]) + " " + strofreal(res.sd[i,1]) + " " + strofreal(res.obs[i,2]) + " " + strofreal(res.mean[i,2]) + " " + strofreal(res.sd[i,2]) + ", unequal level(" + strofreal(bd.si.ci_level) + ") ")
                  res.ovr_statistic[i] = st_numscalar("r(t)")
                  res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
                else
                {
                  varlist = st_global("e(varlist)")
                  stata("cap test " + invtokens(("[" + varlist + "]") :+ strofreal(pos), " == ") + ", mtest(" + bd.opt.test.f_mtest + ")")
                  res.ovr_statistic[i] = st_numscalar("r(F)")
                  res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
              }

            /* Testing - Individual */

              levels = rows(bd.oi.levels)

              if(bd.opt.test.individual)
              {
                if(bd.opt.test.t_individual)
                {
                  for(j=1; j<=cols(pos); j++)
                  {
                    for(k=j+1; k<=cols(pos); k++)
                    {
                      stata("cap ttesti " + strofreal(res.obs[i,pos[j]]) + " " + strofreal(res.mean[i,pos[j]]) + " " + strofreal(res.sd[i,pos[j]]) + " " + strofreal(res.obs[i,pos[k]]) + " " + strofreal(res.mean[i,pos[k]]) + " " + strofreal(res.sd[i,pos[k]]) + ", unequal level(" + strofreal(bd.si.ci_level) + ") ")
                      res.ind_statistic[i,((pos[j] - 1) * levels) + pos[k]] = res.ind_statistic[i,((pos[k] - 1) * levels) + pos[j]] = st_numscalar("r(t)")
                      res.ind_pvalue[i,((pos[j] - 1) * levels) + pos[k]]    = res.ind_pvalue[i,((pos[k] - 1) * levels) + pos[j]]    = st_numscalar("r(p)")
                    }
                  }
                }
                else
                {
                  varlist = st_global("e(varlist)")
                  for(j=1; j<=cols(pos); j++)
                  {
                    stata("cap test " + "[" + varlist + "]" + strofreal(pos[j]) + " == " + invtokens(("[" + varlist + "]") :+ strofreal(pos), " == ") + ", mtest(" + bd.opt.test.f_mtest + ")")
                    res.ind_statistic[i,((pos[j] - 1) * levels) :+ pos] = st_matrix("r(mtest)")[.,1]'
                    res.ind_pvalue[i,((pos[j] - 1) * levels) :+ pos]    = st_matrix("r(mtest)")[.,3]'
                  }
                }
              }

          /* Total */

            if(bd.opt.over.total)
            {

            /* Mean */

              stata(invtokens(cmd_mean[1..3]))

              pos = groups

              res.mean[i,pos] = st_matrix("r(table)")[1,.]
              res.se[i,pos]   = st_matrix("r(table)")[2,.]
              res.t[i,pos]    = st_matrix("r(table)")[3,.]
              res.lci[i,pos]  = st_matrix("r(table)")[5,.]
              res.uci[i,pos]  = st_matrix("r(table)")[6,.]
              res.df[i,pos]   = st_matrix("r(table)")[7,.]

              if(bd.opt.weight.subpop == "") res.obs[i,pos] = st_matrix("e(_N)")
              else                           res.obs[i,pos] = st_matrix("e(_N_subp)")

            /* SD & Var */

              stata("cap estat sd")

              res.sd[i,pos]  = st_matrix("r(sd)")
              res.var[i,pos] = st_matrix("r(variance)")

            }

          /* Logit CIs */

            if(bd.si.ci_proportion)
            {
              res.lci[i,.] = invlogit(logit(res.mean[i,.]) :- invttail(max(res.df[i,.]), ((100 - bd.si.ci_level) / 200)) :* (res.se[i,.] :/ (res.mean[i,.] :* (1 :- res.mean[i,.]))))
              res.uci[i,.] = invlogit(logit(res.mean[i,.]) :+ invttail(max(res.df[i,.]), ((100 - bd.si.ci_level) / 200)) :* (res.se[i,.] :/ (res.mean[i,.] :* (1 :- res.mean[i,.]))))
            }
        }

      /* Sorting */

        if(bd.opt.display.sort_statistic != "")
        {
          if(bd.opt.display.sort_statistic == "obs")  values = res.obs
          if(bd.opt.display.sort_statistic == "nyes") values = res.nyes
          if(bd.opt.display.sort_statistic == "mean") values = res.mean
          if(bd.opt.display.sort_statistic == "se")   values = res.se
          if(bd.opt.display.sort_statistic == "sd")   values = res.sd
          if(bd.opt.display.sort_statistic == "var")  values = res.var
          if(bd.opt.display.sort_statistic == "min")  values = res.min
          if(bd.opt.display.sort_statistic == "max")  values = res.max

          if((bd.opt.display.sort_statistic == "min" | bd.opt.display.sort_statistic == "max") & vi.binary == 0)
          {
            return(res)
          }

          values = range(1, rows, 1), values

          if(bd.opt.display.sort_direction == "+") values = sort(values, 2)
          else                                     values = sort(values, -2)

          if(rows(selectindex(values[.,2] :== .)) > 0)
          {
            positions = values[selectindex(values[.,2] :!= .),1] \ values[selectindex(values[.,2] :== .),1]
          }
          else
          {
            positions = values[.,1]
          }

          bd.vi[vi_position].varlist = bd.vi[vi_position].varlist[positions]
          bd.vi[vi_position].answers = bd.vi[vi_position].answers[positions]

          res.obs  = res.obs[positions,.]
          res.nyes = res.nyes[positions,.]
          res.mean = res.mean[positions,.]
          res.lci  = res.lci[positions,.]
          res.uci  = res.uci[positions,.]
          res.se   = res.se[positions,.]
          res.sd   = res.sd[positions,.]
          res.var  = res.var[positions,.]
          res.min  = res.min[positions,.]
          res.max  = res.max[positions,.]
          res.t    = res.t[positions,.]
          res.df   = res.df[positions,.]

          if(rows(res.ovr_statistic) > 0) res.ovr_statistic = res.ovr_statistic[positions,.]
          if(rows(res.ovr_pvalue)    > 0) res.ovr_pvalue    = res.ovr_pvalue[positions,.]
          if(rows(res.ind_statistic) > 0) res.ind_statistic = res.ind_statistic[positions,.]
          if(rows(res.ind_pvalue)    > 0) res.ind_pvalue    = res.ind_pvalue[positions,.]
        }

      return(res)
    }

/*======================================================================*/
/*   Mata Structure - printer                                           */
/*======================================================================*/

  /* function : printer() */

    void printer(struct braddev scalar bd)
    {
      if(bd.opt.display.print == 0) return(J(0, 0, .))

      /* Title */

        title = getTitle(bd.vi)

        if(title != "") printf("\n{title:" + title + "}\n")

      /* Legend */

        if(rows(bd.oi.levels) > 0 & bd.opt.over.legend)
        {
          labels = st_vlmap(bd.oi.name, bd.oi.levels)
          labels = strtrim(substr(labels, strpos(labels, " ") :+ 1))

          legend = "_over_" :+ strofreal(bd.oi.levels)
          legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} {c |} " :+ invtokens(bd.oi.varlist, ", ") :+ " = " :+ char(34) :+ labels :+ char(34)

          printf("\n")
          display(legend)
        }

      /* Table */

        printf("\n")

        if(bd.opt.display.wide) printTableWide(bd)
        else                    printTableLong(bd)

      /* Footer */

        if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & any(bd.si.stars) & bd.opt.test.footer)
        {
          legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
          legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} p(overall) < 0" :+ strofreal(bd.opt.test.stars)'
          display(legend)
        }

        if(bd.opt.test.individual & bd.opt.test.scripts != . & any(bd.si.scripts) & bd.opt.test.footer)
        {
          legend = uchar(bd.opt.test.letters[1..rows(bd.oi.levels)])' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"
          display(legend)
        }

      printf("\n")
    }

  /* function : getTitle() */

    string scalar getTitle(struct varInfo matrix vi)
    {
      title = st_local("title")

      if(title == "none") return("")

      if(title == "")
      {
        if(rows(vi) == 1)
        {
          title = vi.term + " - " + vi.question
        }
        else
        {
          title = J(1, rows(vi), "")
          for(i=1; i<=rows(vi); i++)
          {
            title[i] = vi[i].term
          }
          title = invtokens(title, ", ")
        }
      }

      title = subinstr(subinstr(title, "%", "%%"), "\", "\\")

      return(title)
    }

  /* function : seriesTable() */

    string matrix seriesTable(struct varInfo scalar vi,
                              struct braddev scalar bd)
    {
      /* Getting Stats */

        groups = rows(bd.oi.levels) == 0 ? 1 : rows(bd.oi.levels) + bd.opt.over.total

        stats_table = J(rows(vi.answers), groups * rows(bd.si.name), "")

        for(i=1; i<=rows(bd.si.name); i++)
        {
          positions = range(1, groups, 1)
          positions = i :+ ((positions :- 1) :* rows(bd.si.name))

          if(bd.si.name[i] != "ci")
          {
            if(bd.si.name[i] == "obs")  values1 = vi.results.obs
            if(bd.si.name[i] == "nyes") values1 = vi.results.nyes
            if(bd.si.name[i] == "mean") values1 = vi.results.mean
            if(bd.si.name[i] == "se")   values1 = vi.results.se
            if(bd.si.name[i] == "sd")   values1 = vi.results.sd
            if(bd.si.name[i] == "var")  values1 = vi.results.var
            if(bd.si.name[i] == "lci")  values1 = vi.results.lci
            if(bd.si.name[i] == "uci")  values1 = vi.results.uci
            if(bd.si.name[i] == "min")  values1 = vi.results.min
            if(bd.si.name[i] == "max")  values1 = vi.results.max

            if(vi.binary & bd.si.percent[i]) values1 = values1 :* 100

            format = "%32." + (vi.binary ? strofreal(bd.si.roundi[i]) : strofreal(bd.si.roundc[i])) + "f" + (bd.si.comma[i] * "c")

            strings = strofreal(values1, format) :+ (vi.binary * bd.si.percent[i] * bd.si.symbol[i] * "%")
            strings = bd.si.notation[i,1] :+ strings :+ bd.si.notation[i,2]
            strings = ((values1 :!= .) :* strings)

            stats_table[.,positions] = strings

            continue
          }

          if(bd.si.name[i] == "ci")
          {
            format = "%32." + (vi.binary ? strofreal(bd.si.roundi[i]) : strofreal(bd.si.roundc[i])) + "f"

            values1 = vi.results.lci
            values2 = vi.results.uci

            if(vi.binary & bd.si.percent[i])
            {
              values1 = values1 :* 100
              values2 = values2 :* 100
            }

            strings = vi.binary * bd.si.percent[i] * bd.si.symbol[i] * "%"
            strings = (strofreal(values1, format) :+ strings) :+ bd.si.ci_separator :+ " " :+ (strofreal(values2, format) :+ strings)
            strings = bd.si.notation[i,1] :+ strings :+ bd.si.notation[i,2]
            strings = ((values1 :!= .) :* strings)

            stats_table[.,positions] = strings

            continue
          }
        }

        nonmissing = (stats_table :!= "")

      /* P-Values - Stars */

        if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & any(bd.si.stars))
        {
          postscripts = J(rows(vi.answers), 1, "")

          for(i=1; i<=cols(bd.opt.test.stars); i++)
          {
            postscripts = postscripts :+ ((vi.results.ovr_pvalue :< bd.opt.test.stars[i]) :* uchar(735))
          }

          positions = selectindex(J(groups, 1, bd.si.stars))
          stats_table[.,positions] = stats_table[.,positions] :+ postscripts
        }

      /* P-Values - Scripts */

        if(bd.opt.test.individual & bd.opt.test.scripts != . & any(bd.si.scripts))
        {
          postscripts = J(rows(vi.answers), rows(bd.oi.levels), "")

          for(i=1; i<=rows(bd.oi.levels); i++)
          {
            positions = range(i, i + ((rows(bd.oi.levels) - 1) * rows(bd.oi.levels)), rows(bd.oi.levels))
            postscripts = postscripts :+ ((vi.results.ind_pvalue[.,positions] :< bd.opt.test.scripts) :* uchar(bd.opt.test.letters[i]))
          }

          for(i=1; i<=sum(bd.si.scripts); i++)
          {
            positions = range(0, rows(bd.oi.levels) - 1, 1) :* rows(bd.si.name) :+ selectindex(bd.si.scripts)[i]
            stats_table[.,positions] = stats_table[.,positions] :+ postscripts
          }
        }

      /* Removing Missing Values */

        stats_table = (nonmissing :* stats_table) :+ (!nonmissing :* ".")

      /* Reshaping */

        if(!bd.opt.display.wide) stats_table = colshape(stats_table, rows(bd.si.name))

      /* P-Values - Overall */

        if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
        {
          strings = J(rows(vi.answers), 1 + bd.opt.test.statistic, "")

          if(bd.opt.test.statistic)
          {
            nonmissing = selectindex(vi.results.ovr_statistic :!= .)

            strings[.,1] = strofreal(vi.results.ovr_statistic, "%32.2f")

            if(vi.type == "xi" & bd.opt.test.chi_overall)
            {
              strings[nonmissing,1] = "Chi2 = " :+ strings[nonmissing,1]
            }
            else
            {
              if(bd.opt.test.t_overall) strings[nonmissing,1] = "t = " :+ strings[nonmissing,1]
              else                      strings[nonmissing,1] = "F = " :+ strings[nonmissing,1]
            }
          }

          strings[.,cols(strings)] = strofreal(vi.results.ovr_pvalue, "%6.4f")

          if(bd.opt.display.wide) stats_table = stats_table, strings
          else                    stats_table = stats_table, colshape(J(1, groups, strings), cols(strings))
        }

      /* P-Values - Individual */

        if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
        {
          if(bd.opt.display.wide)
          {
            cols = 1 + bd.opt.test.statistic

            positions = range(1, rows(bd.oi.levels) * rows(bd.oi.levels), 1)
            positions = rowshape(positions, rows(bd.oi.levels))'
            positions = vech(positions[(2..rows(bd.oi.levels)),(1..(rows(bd.oi.levels)-1))])'

            strings = J(rows(vi.answers), cols(positions) * cols, "")

            if(bd.opt.test.statistic)
            {
              nonmissing = (vi.results.ind_statistic[.,positions] :!= .)

              if(bd.opt.test.t_individual) values1 = "t = " :+ strofreal(vi.results.ind_statistic[.,positions], "%32.2f")
              else                         values1 = "F = " :+ strofreal(vi.results.ind_statistic[.,positions], "%32.2f")

              values1 = (values1 :* nonmissing) :+ ("." :* !nonmissing)

              strings[.,range(1, (cols(positions) * cols) - 1, cols)] = values1
            }

            strings[.,range(cols, (cols(positions) * cols), cols)] = strofreal(vi.results.ind_pvalue[.,positions], "%6.4f")

            stats_table = stats_table, strings
          }
          else
          {
            cols = 1 + bd.opt.test.statistic

            values1 = J(rows(vi.answers), cols(vi.results.ind_statistic) * cols, "")

            if(bd.opt.test.statistic)
            {
              nonmissing = (vi.results.ind_statistic :!= .)

              if(bd.opt.test.t_individual) values2 = "t = " :+ strofreal(vi.results.ind_statistic, "%32.2f")
              else                         values2 = "F = " :+ strofreal(vi.results.ind_statistic, "%32.2f")

              values2 = (values2 :* nonmissing) :+ ("." :* !nonmissing)

              values1[.,range(1, (cols(vi.results.ind_statistic) * cols) - 1, cols)] = values2
            }

            values2 = strofreal(vi.results.ind_pvalue, "%6.4f")

            values1[.,range(cols, (cols(vi.results.ind_statistic) * cols), cols)] = values2

            strings = J(rows(vi.answers) * groups, rows(bd.oi.levels) * cols, ".")

            for(i=1; i<=rows(vi.answers); i++)
            {
              positions = range(1, rows(bd.oi.levels), 1) :+ ((i - 1) * groups)

              strings[positions,.] = rowshape(values1[i,.], rows(bd.oi.levels))
            }

            stats_table = stats_table, strings
          }
        }

      /* Finishing - Wide & Long, No Over*/

        if(bd.opt.display.wide | groups == 1)
        {
          if(vi.type != "individual")
          {
            stats_table = ("{ralign 80:" :+ substr(vi.answers, 1, 80) :+ "}"), stats_table
            stats_table = J(1, cols(stats_table), "") \ stats_table

            if((vi.type == "xi" & bd.opt.display.xi_variables) | (vi.type == "series" & bd.opt.display.series_variables))
            {
              stats_table[1,1] = "{lalign 80:" + substr(vi.question, 1, 80) + "}"
            }
            else
            {
              stats_table[1,1] = "{lalign 80:" + substr(vi.term, 1, 80) + "}"
            }
          }
          else
          {
            stats_table = ("{lalign 80:" + substr(vi.term, 1, 80) + "}"), stats_table
          }

          stats_table = J(1, cols(stats_table), "") \ stats_table

          return(stats_table)
        }

      /* Finishing - Long, Over */

        if(!bd.opt.display.wide & groups > 1)
        {
          if(bd.opt.over.labels) strings = "{ralign 80:" :+ substr(bd.oi.labels, 1, 80) :+ "}"
          else                   strings = "{ralign 80:" :+ substr("_over_" :+ strofreal(bd.oi.levels), 1, 80) :+ "}"

          if(bd.opt.over.total) strings = strings \ ("{ralign 80:Total}")

          stats_table = J(rows(vi.answers), 1, strings), stats_table

          return_table = J(rows(stats_table) + (2 * rows(vi.answers)), cols(stats_table), "")

          for(i=1; i<=rows(vi.answers); i++)
          {
            pos1 = (i*2) + ((i-1) * groups)
            pos2 = range(pos1 + 1, pos1 + groups, 1)
            pos3 = range(1, groups, 1) :+ ((i-1) * groups)

            return_table[pos1,1] = "{lalign 80:" :+ vi.varlist[i] :+ "}"
            return_table[pos2,.] = stats_table[pos3,.]
          }

          return(return_table)
        }

      return("")
    }

  /* function : printTableWide() */

    void printTableWide(struct braddev scalar bd)
    {
      /* Results */

        for(i=1; i<=rows(bd.vi); i++)
        {
          if(i==1) results_table = seriesTable(bd.vi[i], bd)
          else     results_table = results_table \ seriesTable(bd.vi[i], bd)
        }

      /* Separators (1) */

        if(!bd.opt.display.separator)
        {
          pos1 = selectindex(rowmax(results_table :!= ""))
          results_table = J(1, cols(results_table), "") \ results_table[pos1,.]
        }

      /* Formatting Lengths */

        /* Name */

          length = max(udstrlen(substr(results_table[.,1], 12, udstrlen(results_table[.,1])))) - 1

          results_table[.,1] = subinstr(results_table[.,1], "align 80:", "align " + strofreal(length) + ":")

        /* Stats */

          pos1 = selectindex(rowmax(results_table :!= ""))

          col_lengths = colmax(udstrlen(results_table[.,(2..cols(results_table))]))

          pos2 = selectindex(col_lengths :< 5)
          if(cols(pos2) > 0)
          {
            col_lengths[pos2] = J(1, cols(pos2), 5)
          }

          results_table[pos1,(2..cols(results_table))] = "{" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths) :+ ":" :+ results_table[pos1,(2..cols(results_table))] :+ "}"

          results_table[pos1,.] = " " :+ results_table[pos1,.] :+ " "

        col_lengths = length, col_lengths

        col_lengths = col_lengths :+ 2

      /* Table Columns */

        breaks = rows(bd.oi.levels) + bd.opt.over.total

        if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
        {
          breaks = breaks + 1
        }

        if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
        {
          breaks = breaks + factorial(rows(bd.oi.levels)) / (2 * factorial(rows(bd.oi.levels)-2))
        }

        linesize = c("linesize")
        if(linesize < 140) linesize = 140
        if(linesize > 240) linesize = 240

        table_columns = J(1, cols(col_lengths), .)

        length = col_lengths[1] + 2 + breaks

        j = 1
        for(i=2; i<=cols(table_columns); i++)
        {
          if((length + col_lengths[i] + 2) > linesize)
          {
            j = j + 1
            length = col_lengths[1] + 2 + breaks
          }

          length = length + col_lengths[i] + 2

          table_columns[i] = j
        }

      /* Header - First Row */

        pos1 = cols(results_table)

        groups = bd.opt.display.wide ? (rows(bd.oi.levels) + bd.opt.over.total) : 1

        header = "{space " + strofreal(col_lengths[1]) + "}"
        header = header, J(1, groups, bd.si.label')

        pvalues = (pos1 - cols(header)) / (1 + bd.opt.test.statistic)

        if(pvalues > 0)
        {
          if(bd.opt.test.statistic) header = header, J(1, pvalues, ("Stat", "P-Val"))
          else                      header = header, J(1, pvalues, "P-Val")
        }

        header[2..pos1] = abbrev(header[2..pos1], col_lengths[2..pos1] :- 2)
        header[2..pos1] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[2..pos1] :- 2) :+ ":" :+ header[2..pos1] :+ "} "

        results_table = header \ results_table

      /* Header - Second Row */

        header = J(1, cols(col_lengths), "")

        /* Group Labels */

          if(bd.opt.over.group_n)
          {
            labels = bd.oi.freqs
            if(bd.opt.over.total) labels = labels \ sum(labels)

            labels = "(n = " :+ strtrim(strofreal(labels, "%32.0fc")) :+ ")"
          }
          else
          {
            labels = bd.oi.labels
            if(bd.opt.over.total) labels = labels \ "Total"
          }

          for(i=1; i<=groups; i++)
          {
            pos1 = 2 + ((i-1) * rows(bd.si.name))
            pos2 = pos1 + rows(bd.si.name) - 1

            header[pos1..pos2] = J(1, (pos2 - pos1 + 1), labels[i])
          }

        /* P-Values */

          if(pvalues > 0)
          {
            pos1 = pos2 + 1
            pos2 = pos2 + 1 + bd.opt.test.statistic

            if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
            {
              header[pos1..pos2] = J(1, 1 + bd.opt.test.statistic, "Overall")

              pos1 = pos2 + 1
              pos2 = pos2 + 1 + bd.opt.test.statistic
            }

            if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
            {
              for(i=1; i<=rows(bd.oi.labels); i++)
              {
                for(j=i+1; j<=rows(bd.oi.labels); j++)
                {
                  header[pos1..pos2] = J(1, 1 + bd.opt.test.statistic, strofreal(i) + "v" + strofreal(j))

                  pos1 = pos2 + 1
                  pos2 = pos2 + 1 + bd.opt.test.statistic
                }
              }
            }
          }

        new_lengths = J(1, cols(col_lengths), 0)

        for(i=2; i<=cols(header); i++)
        {
          for(j=i; j<=cols(header); j++)
          {
            if(header[i] == header[j] & table_columns[i] == table_columns[j])
            {
              new_lengths[i] = new_lengths[i] + col_lengths[j]
              if(j == cols(header)) i = j + 1
            }
            else
            {
              i = j - 1
              break
            }
          }
        }

        pos1 = selectindex(new_lengths :> 0)
        pos2 = selectindex(new_lengths :== 0)

        new_lengths[pos1] = new_lengths[pos1] :- 2

        pos1 = selectindex(new_lengths :> 0 :& new_lengths :<= 32)
        if(cols(pos1) > 0) header[pos1] = abbrev(header[pos1], new_lengths[pos1])

        pos1 = selectindex(new_lengths :> 32)
        if(cols(pos1) > 0) header[pos1] = substr(header[pos1], 1, new_lengths[pos1])

        pos1 = selectindex(new_lengths :> 0)

        header[pos1] = " {center " :+ strofreal(new_lengths[pos1]) :+ ":" :+ strtrim(header[pos1]) :+ "} "
        header[pos2] = header[pos2] :* 0

        header[1] = "{space " :+ strofreal(col_lengths[1]) :+ "}"

        results_table = header \ results_table

      /* Header - Third Row */

        if(bd.opt.over.group_n)
        {
          header = J(1, cols(col_lengths), "")

          /* Group Labels */

            labels = bd.oi.labels
            if(bd.opt.over.total) labels = labels \ "Total"

            for(i=1; i<=groups; i++)
            {
              pos1 = 2 + ((i-1) * rows(bd.si.name))
              pos2 = pos1 + rows(bd.si.name) - 1

              header[pos1..pos2] = J(1, (pos2 - pos1 + 1), labels[i])
            }

          new_lengths = J(1, cols(col_lengths), 0)

          for(i=2; i<=cols(header); i++)
          {
            for(j=i; j<=cols(header); j++)
            {
              if(header[i] == header[j] & table_columns[i] == table_columns[j])
              {
                new_lengths[i] = new_lengths[i] + col_lengths[j]
                if(j == cols(header)) i = j + 1
              }
              else
              {
                i = j - 1
                break
              }
            }
          }

          pos1 = selectindex(new_lengths :> 0)
          pos2 = selectindex(new_lengths :== 0)

          new_lengths[pos1] = new_lengths[pos1] :- 2

          pos1 = selectindex(new_lengths :> 0 :& new_lengths :<= 32)
          if(cols(pos1) > 0) header[pos1] = abbrev(header[pos1], new_lengths[pos1])

          pos1 = selectindex(new_lengths :> 32)
          if(cols(pos1) > 0) header[pos1] = substr(header[pos1], 1, new_lengths[pos1])

          pos1 = selectindex(new_lengths :> 0)

          header[pos1] = " {center " :+ strofreal(new_lengths[pos1]) :+ ":" :+ strtrim(header[pos1]) :+ "} "
          header[pos2] = header[pos2] :* 0

          if(pvalues > 0)
          {
            pos2 = 2 + (rows(bd.si.name) * groups)
            header[pos2..cols(header)] = "{space " :+ strofreal(col_lengths[pos2..cols(header)]) :+ "}"
          }

          header[1] = "{space " :+ strofreal(col_lengths[1]) :+ "}"

          results_table = header \ results_table
        }

      /* Separators (2) */

        /* Horizontal */

          pos1 = selectindex(!rowmax(results_table :!= ""))

          h_separator = "{hline " :+ strofreal(col_lengths) :+ "}"

          if(rows(pos1) > 0) results_table[pos1,.] = J(rows(pos1), 1, h_separator)

          results_table = results_table \ h_separator

        /* Vertical */

          breaks = range(1, groups + 1, 1)
          breaks = 2 :+ ((breaks :- 1) :* rows(bd.si.name))

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            labels = range(1, pvalues + 1, 1)
            breaks = breaks \ breaks[rows(breaks)] :+ ((labels :- 1) :* 2)
          }

          v_separator = J(rows(results_table), 1, "{c |}")

          if(rows(pos1) > 0)
          {
            v_separator[pos1] = J(rows(pos1), 1, "{c +}")
          }

          v_separator[rows(v_separator)] = "{c BT}"

      /* Printing */

        for(i=1; i<=max(table_columns); i++)
        {
          pos1 = selectindex(table_columns :== i)

          if(max(table_columns) > 1)
          {
            printf("\n(" + strofreal(i) + "/" + strofreal(max(table_columns)) + ")\n")
          }

          print_table = "{res}" :+ results_table[.,1]

          for(j=1; j<=cols(pos1); j++)
          {
            if(j == 1 | anyof(breaks, pos1[j]))
            {
              print_table = print_table :+ v_separator
            }

            print_table = print_table :+ results_table[.,pos1[j]]
          }

          display(print_table)
        }
    }

  /* function : printTableLong() */

    void printTableLong(struct braddev scalar bd)
    {
      /* Results */

        for(i=1; i<=rows(bd.vi); i++)
        {
          if(i==1) results_table = seriesTable(bd.vi[i], bd)
          else     results_table = results_table \ seriesTable(bd.vi[i], bd)
        }

      /* Separators (1) */

        if(!bd.opt.display.separator)
        {
          pos1 = selectindex(rowmax(results_table :!= ""))
          results_table = J(1, cols(results_table), "") \ results_table[pos1,.]
        }

      /* Formatting Lengths */

        /* Name */

          length = max(udstrlen(substr(results_table[.,1], 12, udstrlen(results_table[.,1])))) - 1

          results_table[.,1] = subinstr(results_table[.,1], "align 80:", "align " + strofreal(length) + ":")

        /* Stats */

          pos1 = selectindex(rowmax(results_table :!= ""))

          col_lengths = colmax(udstrlen(results_table[.,(2..cols(results_table))]))

          pos2 = selectindex(col_lengths :< 5)
          if(cols(pos2) > 0)
          {
            col_lengths[pos2] = J(1, cols(pos2), 5)
          }

          results_table[pos1,(2..cols(results_table))] = "{" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths) :+ ":" :+ results_table[pos1,(2..cols(results_table))] :+ "}"

          results_table[pos1,.] = " " :+ results_table[pos1,.] :+ " "

        col_lengths = length, col_lengths

        col_lengths = col_lengths :+ 2

      /* Table Columns */

        breaks = 1

        if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
        {
          breaks = breaks + 1
        }

        if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
        {
          breaks = breaks + rows(bd.oi.levels)
        }

        linesize = c("linesize")
        if(linesize < 140) linesize = 140
        if(linesize > 240) linesize = 240

        table_columns = J(1, cols(col_lengths), .)

        length = col_lengths[1] + 2 + breaks

        j = 1
        for(i=2; i<=cols(table_columns); i++)
        {
          if((length + col_lengths[i] + 2) > linesize)
          {
            j = j + 1
            length = col_lengths[1] + 2 + breaks
          }

          length = length + col_lengths[i] + 2

          table_columns[i] = j
        }

      /* Header - First Row */

        pos1 = cols(results_table)

        header = ("{space " + strofreal(col_lengths[1]) + "}"), bd.si.label'

        pvalues = (pos1 - cols(header)) / (1 + bd.opt.test.statistic)

        if(pvalues > 0)
        {
          if(bd.opt.test.statistic) header = header, J(1, pvalues, ("Stat", "P-Val"))
          else                      header = header, J(1, pvalues, "P-Val")
        }

        header[2..pos1] = abbrev(header[2..pos1], col_lengths[2..pos1] :- 2)
        header[2..pos1] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[2..pos1] :- 2) :+ ":" :+ header[2..pos1] :+ "} "

        results_table = header \ results_table

      /* Header - Second Row */

        if(pvalues > 0)
        {
          /* P-Values */

            header = J(1, cols(col_lengths), "")

            pos1 = 2 + rows(bd.si.name)
            pos2 = pos1 + bd.opt.test.statistic

            if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
            {
              header[pos1..pos2] = J(1, 1 + bd.opt.test.statistic, "Overall")

              pos1 = pos2 + 1
              pos2 = pos2 + 1 + bd.opt.test.statistic
            }

            if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
            {
              for(i=1; i<=rows(bd.oi.labels); i++)
              {
                header[pos1..pos2] = J(1, 1 + bd.opt.test.statistic, "vs " + strofreal(i))

                pos1 = pos2 + 1
                pos2 = pos2 + 1 + bd.opt.test.statistic
              }
            }

          new_lengths = J(1, cols(col_lengths), 0)

          for(i=2; i<=cols(header); i++)
          {
            for(j=i; j<=cols(header); j++)
            {
              if(header[i] == header[j] & table_columns[i] == table_columns[j])
              {
                new_lengths[i] = new_lengths[i] + col_lengths[j]
                if(j == cols(header)) i = j + 1
              }
              else
              {
                i = j - 1
                break
              }
            }
          }

          pos1 = selectindex(new_lengths :> 0)
          pos2 = selectindex(new_lengths :== 0)

          new_lengths[pos1] = new_lengths[pos1] :- 2

          pos1 = selectindex(new_lengths :> 0 :& new_lengths :<= 32)
          if(cols(pos1) > 0) header[pos1] = abbrev(header[pos1], new_lengths[pos1])

          pos1 = selectindex(new_lengths :> 32)
          if(cols(pos1) > 0) header[pos1] = substr(header[pos1], 1, new_lengths[pos1])

          pos1 = selectindex(new_lengths :> 0)

          header[pos1] = " {center " :+ strofreal(new_lengths[pos1]) :+ ":" :+ strtrim(header[pos1]) :+ "} "
          header[pos2] = header[pos2] :* 0

          pos2 = 1 + rows(bd.si.name)
          header[1..pos2] = "{space " :+ strofreal(col_lengths[1..pos2]) :+ "}"

          results_table = header \ results_table
        }

      /* Separators (2) */

        /* Horizontal */

          pos1 = selectindex(!rowmax(results_table :!= ""))

          h_separator = "{hline " :+ strofreal(col_lengths) :+ "}"

          if(rows(pos1) > 0) results_table[pos1,.] = J(rows(pos1), 1, h_separator)

          results_table = results_table \ h_separator

        /* Vertical */

          breaks = 2 + rows(bd.si.name)

          if(pvalues > 1 & bd.opt.test.statistic)
          {
            labels = range(1, pvalues + 1, 1)
            breaks = breaks \ breaks[rows(breaks)] :+ ((labels :- 1) :* 2)
          }

          v_separator = J(rows(results_table), 1, "{c |}")

          if(rows(pos1) > 0)
          {
            v_separator[pos1] = J(rows(pos1), 1, "{c +}")
          }

          v_separator[rows(v_separator)] = "{c BT}"

      /* Printing */

        for(i=1; i<=max(table_columns); i++)
        {
          pos1 = selectindex(table_columns :== i)

          if(max(table_columns) > 1)
          {
            printf("\n(" + strofreal(i) + "/" + strofreal(max(table_columns)) + ")\n")
          }

          print_table = "{res}" :+ results_table[.,1]

          for(j=1; j<=cols(pos1); j++)
          {
            if(j == 1 | anyof(breaks, pos1[j]))
            {
              print_table = print_table :+ v_separator
            }

            print_table = print_table :+ results_table[.,pos1[j]]
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

        if(bd.opt.display.wide)
        {
          excelTableWide(bd, B, row)
        }
        else
        {
          if(rows(bd.oi.levels) > 0) excelTableLongOver(bd, B, row)
          else                       excelTableLongNoOver(bd, B, row)
        }


      B.close_book()
    }

  /* function : excelTableWide() */

    void excelTableWide(struct braddev scalar bd,
                        class  xl      scalar B,
                        real           scalar input_row)
    {
      /* Initializing P-Values */

        pvalues = 0

        if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
        {
          pvalues = pvalues + 1
        }

        if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
        {
          pvalues = pvalues + factorial(rows(bd.oi.levels))/(2 * factorial(rows(bd.oi.levels) - 2))
        }

      /* Creating Formats */

        B.set_missing(".")

        /* Font */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Stats - Text */

          txt_stats_c = txt_stats_i = J(rows(bd.si.label), 1, "")

          for(i=1; i<=rows(bd.si.label); i++)
          {
            /* Continuous */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundc[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundc[i])

              if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & bd.si.stars[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_c[i] = fmt

            /* Binary */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundi[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundi[i])
              if(bd.si.percent[i])    fmt = fmt + "%"

              if(bd.opt.test.individual & bd.opt.test.scripts != . & bd.si.scripts[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_i[i] = fmt
          }

        /* Stats - Format ID */

          /* Continuous */

            fmt_stats_c = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              fmt_stats_c[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_c[i], font)
              B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
              B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
            }

          /* Binary */

            fmt_stats_i = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              positions = selectindex(txt_stats_c :== txt_stats_i[i])
              if(rows(positions) > 0 & cols(positions) > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[positions[1]]
                continue
              }

              fmt_stats_i[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_i[i], font)
              B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
              B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
            }

        /* P-Values */

          fmt_pvalue = B.add_fmtid()
          B.fmtid_set_fontid(fmt_pvalue, font)
          B.fmtid_set_horizontal_align(fmt_pvalue, "center")

        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Whitespace */

          fmt_whitespace = B.add_fmtid()
          B.fmtid_set_left_border(fmt_whitespace, "medium")
          B.fmtid_set_right_border(fmt_whitespace, "medium")
          B.fmtid_set_fill_pattern(fmt_whitespace, "solid", "white")

        /* Header - Stats */

          fmt_header = B.add_fmtid()
          B.fmtid_set_fontid(fmt_header, font_bold)
          B.fmtid_set_horizontal_align(fmt_header, "center")
          B.fmtid_set_fill_pattern(fmt_header, "solid", bd.opt.excel.color[1])

        /* Variables */

          fmt_question = B.add_fmtid()
          B.fmtid_set_fontid(fmt_question, font_bold)
          B.fmtid_set_horizontal_align(fmt_question, "left")
          B.fmtid_set_left_border(fmt_question, "medium")
          B.fmtid_set_right_border(fmt_question, "medium")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_left_border(fmt_answer, "medium")
          B.fmtid_set_right_border(fmt_answer, "medium")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

      /* Creating Header */

        groups = rows(bd.oi.levels) + bd.opt.over.total

        row = 2 + bd.opt.over.group_n
        col = (groups * rows(bd.si.label)) + (pvalues * (1 + bd.opt.test.statistic))

        header = J(row, col, "")

        /* Stat Columns */

          pos1 = 1
          pos2 = groups * rows(bd.si.label)

          /* Bottom Row */

            header[row,(pos1..pos2)] = rowshape(J(1, groups, bd.si.label'), 1)

          /* Middle Row */

            if(bd.opt.over.group_n)
            {
              values = bd.oi.freqs

              if(bd.opt.over.total) values = values \ sum(values)

              strings = "(n = " :+ strtrim(strofreal(values, "%32.0fc")) :+ ")"

              header[(row-1),(pos1..pos2)] = rowshape(J(1, rows(bd.si.name), strings), 1)
            }

          /* Top Row */

            strings = bd.oi.labels

            if(bd.opt.over.total) strings = strings \ "Total"

            header[1,(pos1..pos2)] = rowshape(J(1, rows(bd.si.name), strings), 1)

        /* P-Value Columns */

          pos1 = pos2 + 1
          pos2 = cols(header)

          /* Bottom Row */

            if(pvalues > 0)
            {
              if(bd.opt.test.statistic) header[row,(pos1..pos2)] = J(1, pvalues, ("Stat", "P-Value"))
              else                      header[row,(pos1..pos2)] = J(1, pvalues, "P-Value")
            }

          /* Middle & Top Row */

            if(pvalues > 0)
            {
              pos2 = pos1 + bd.opt.test.statistic

              if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
              {
                header[(row-1),(pos1..pos2)] = J(1, (1 + bd.opt.test.statistic), "Overall")

                pos1 = pos2 + 1
                pos2 = pos1 + bd.opt.test.statistic
              }

              if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
              {
                for(i=1; i<=rows(bd.oi.labels); i++)
                {
                  for(j=i+1; j<=rows(bd.oi.labels); j++)
                  {
                    header[(row-1),(pos1..pos2)] = J(1, (1 + bd.opt.test.statistic), (char(34) :+ bd.oi.labels[i] :+ char(34) :+ " vs " :+ char(34) :+ bd.oi.labels[j] :+ char(34)))

                    pos1 = pos2 + 1
                    pos2 = pos1 + bd.opt.test.statistic
                  }
                }
              }
            }

      /* Initializing Table */

        row = input_row
        col = 1

        start_row = row

        end_col = 1 + cols(header)

      /* Table - Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, col, title)
          B.set_fmtid(row, col, fmt_title)

          row = row + 1
          start_row = row
        }

      /* Table - Header */

        B.put_string(row, (col+1), header)

        sheet = B.query("sheetname")

        /* Merging Groups - Stats */

          for(i=1; i<=groups; i++)
          {
            pos1 = 2 + ((i - 1) * rows(bd.si.label))
            pos2 = pos1 + rows(bd.si.label) - 1

            B.set_sheet_merge(sheet, (row,row), (pos1,pos2))
            if(rows(header) == 3) B.set_sheet_merge(sheet, (row+1,row+1), (pos1,pos2))
          }

        /* Formatting - Stats */

          B.set_fmtid((row,(row+rows(header)-1)), (2,end_col), fmt_header)

        /* Merging Groups - P-Values */

          if(pvalues > 0 & bd.opt.test.statistic)
          {
            pos1 = 2 + (groups * rows(bd.si.label))

            for(i=1; i<=pvalues; i++)
            {
              pos2 = pos1 + 1

              B.set_sheet_merge(sheet, (row,row), (pos1,pos2))
              if(rows(header) == 3) B.set_sheet_merge(sheet, (row+1,row+1), (pos1,pos2))

              pos1 = pos2 + 1
            }
          }

        /* Formatting - Whitespace */

          pos1 = start_row
          pos2 = start_row + rows(header) - 1

          B.set_fmtid((pos1,pos2), 1, fmt_whitespace)

        row = start_row + rows(header)

      /* Table - Contents */

        for(i=1; i<=rows(bd.vi); i++)
        {
          col = 1

          /* Names */

            if(bd.vi[i].type == "individual")
            {
              B.put_string(row, col, bd.vi[i].varlist)
              B.set_fmtid(row, col, fmt_question)
            }
            else
            {
              B.put_string(row, col, bd.vi[i].question)
              B.set_fmtid(row, col, fmt_question)

              row = row + 1

              pos1 = row
              pos2 = row + rows(bd.vi[i].answers) - 1

              B.put_string(row, col, bd.vi[i].answers)
              B.set_fmtid((pos1,pos2), col, fmt_answer)
            }

          /* Statistics */

            end_row = row + rows(bd.vi[i].answers) - 1

            for(j=1; j<=rows(bd.si.label); j++)
            {
              col = col + 1

              /* Numeric */

                if(bd.si.name[j] != "ci" & ((bd.vi[i].binary & txt_stats_i[j] != "text") | (!bd.vi[i].binary & txt_stats_c[j] != "text")))
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  for(k=1; k<=groups; k++)
                  {
                    pos1 = col + ((k - 1) * rows(bd.si.label))

                    B.put_number(row, pos1, values[.,k])

                    if(bd.vi[i].binary) B.set_fmtid((row,end_row), pos1, fmt_stats_i[j])
                    else                B.set_fmtid((row,end_row), pos1, fmt_stats_c[j])
                  }

                  continue
                }

              /* Text */

                if(bd.si.name[j] != "ci")
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = strofreal(values, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                }
                else
                {
                  values1 = bd.vi[i].results.lci
                  values2 = bd.vi[i].results.uci

                  if(bd.vi[i].binary & bd.si.percent[j])
                  {
                    values1 = values1 :* 100
                    values2 = values2 :* 100
                  }

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = (strofreal(values1, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")) :+ bd.si.ci_separator :+ (strofreal(values2, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"))
                  values  = values1
                }

                if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & bd.si.stars[j])
                {
                  postscripts = J(rows(bd.vi[i].varlist), 1, "")
                  for(k=1; k<=cols(bd.opt.test.stars); k++)
                  {
                    postscripts = postscripts :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* uchar(735))
                  }
                  strings = strings :+ postscripts
                }

                if(bd.opt.test.individual & bd.opt.test.scripts != . & bd.si.scripts[j])
                {
                  postscripts = J(rows(bd.vi[i].varlist), rows(bd.oi.levels), "")
                  for(k=1; k<=rows(bd.oi.levels); k++)
                  {
                    positions = range(k, k + ((rows(bd.oi.levels) - 1) * rows(bd.oi.levels)), rows(bd.oi.levels))
                    postscripts = postscripts :+ ((bd.vi[i].results.ind_pvalue[.,positions] :< bd.opt.test.scripts) :* uchar(bd.opt.test.letters[k]))
                  }
                  strings[.,(1..rows(bd.oi.levels))] = strings[.,(1..rows(bd.oi.levels))] :+ postscripts
                }

                strings = bd.si.notation[j,1] :+ strings :+ bd.si.notation[j,2]
                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                for(k=1; k<=groups; k++)
                {
                  pos1 = col + ((k - 1) * rows(bd.si.name))

                  B.put_string(row, pos1, strings[.,k])

                  if(bd.vi[i].binary) B.set_fmtid((row,end_row), pos1, fmt_stats_i[j])
                  else                B.set_fmtid((row,end_row), pos1, fmt_stats_c[j])
                }
            }

          /* P-Values - Overall */

            col = 2 + (rows(bd.si.label) * groups)

            if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
            {
              if(bd.opt.test.statistic)
              {
                if(bd.opt.test.t_overall)                           strings = "t = "
                else                                                strings = "F = "
                if(bd.vi[i].type == "xi" & bd.opt.test.chi_overall) strings = "Chi2 = "

                values  = bd.vi[i].results.ovr_statistic
                strings = strings :+ strtrim(strofreal(values, "%32.2f"))
                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                B.put_string(row, col, strings)
                B.set_fmtid((row,end_row), col, fmt_pvalue)

                col = col + 1
              }

              values  = bd.vi[i].results.ovr_pvalue
              strings = "P = " :+ strtrim(strofreal(values, "%32.3f"))

              pos1 = selectindex(values :>= 0.01)
              if(rows(pos1) > 0) strings[pos1] = "P = " :+ strtrim(strofreal(values[pos1], "%32.2f"))

              pos1 = selectindex(values :< 0.001)
              if(rows(pos1) > 0) strings[pos1] = J(rows(pos1), 1, "P < 0.001")

              strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

              B.put_string(row, col, strings)
              B.set_fmtid((row,end_row), col, fmt_pvalue)

              col = col + 1
            }

          /* P-Values - Individual */

            if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
            {
              for(j=1; j<=rows(bd.oi.levels); j++)
              {
                for(k=j+1; k<=rows(bd.oi.levels); k++)
                {
                  pos2 = (j + ((j-1) * rows(bd.oi.levels))) + (k - j)

                  if(bd.opt.test.statistic)
                  {
                    if(bd.opt.test.t_individual) strings = "t = "
                    else                         strings = "F = "

                    values  = bd.vi[i].results.ind_statistic[.,pos2]
                    strings = strings :+ strtrim(strofreal(values, "%32.2f"))
                    strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                    B.put_string(row, col, strings)
                    B.set_fmtid((row,end_row), col, fmt_pvalue)

                    col = col + 1
                  }

                  values  = bd.vi[i].results.ind_pvalue[.,pos2]
                  strings = "P = " :+ strtrim(strofreal(values, "%32.3f"))

                  pos1 = selectindex(values :>= 0.01)
                  if(rows(pos1) > 0) strings[pos1] = "P = " :+ strtrim(strofreal(values[pos1], "%32.2f"))

                  pos1 = selectindex(values :< 0.001)
                  if(rows(pos1) > 0) strings[pos1] = J(rows(pos1), 1, "P < 0.001")

                  strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                  B.put_string(row, col, strings)
                  B.set_fmtid((row,end_row), col, fmt_pvalue)

                  col = col + 1
                }
              }
            }

          /* Setting Bottom Border */

            if(i < rows(bd.vi))
            {
              B.set_bottom_border(end_row, (1,end_col), "thin")
            }

          row = end_row + 1
        }

      /* Formatting - Stats & P-Values */

        /* Stats */

          for(i=1; i<=groups; i++)
          {
            pos1 = 1 + (i * rows(bd.si.label))

            B.set_right_border((start_row,end_row), pos1, "thin")
          }

        /* P-Values */

          if(pvalues > 0 & bd.opt.test.statistic)
          {
            pos1 = 1 + (groups * rows(bd.si.label))

            for(i=1; i<=pvalues; i++)
            {
              pos1 = pos1 + 2

              B.set_right_border((start_row,end_row), pos1, "thin")
            }
          }

      /* Formatting - Borders */

        pos1 = 1
        pos2 = 1 + cols(header)

        /* Header */

          row = start_row + rows(header)

          B.set_top_border(row, (pos1,pos2), "medium")

        /* Overall Table */

          B.set_top_border(start_row, (pos1,pos2), "medium")
          B.set_bottom_border(end_row, (pos1,pos2), "medium")

          B.set_right_border((start_row,end_row), pos2, "medium")

      /* Footer */

        row = end_row + 1

        if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & any(bd.si.stars) & bd.opt.test.footer)
        {
          spaces = " " :* (cols(bd.opt.test.stars) :- (range(1, cols(bd.opt.test.stars), 1)))

          legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
          legend = legend :+ spaces :+ " p(overall) < 0" :+ strofreal(bd.opt.test.stars)'

          B.put_string(row, 1, legend)

          row = row + rows(legend)
        }

        if(bd.opt.test.individual & bd.opt.test.scripts != . & any(bd.si.scripts) & bd.opt.test.footer)
        {
          legend = uchar(bd.opt.test.letters[1..rows(bd.oi.levels)])' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          B.put_string(row, 1, legend)
        }
    }

  /* function : excelTableLongOver() */

    void excelTableLongOver(struct braddev scalar bd,
                            class  xl      scalar B,
                            real           scalar input_row)
    {
      /* Initializing P-Values */

        pvalues = 0

        if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
        {
          pvalues = pvalues + 1
        }

        if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
        {
          pvalues = pvalues + rows(bd.oi.levels)
        }

      /* Creating Formats */

        B.set_missing(".")

        /* Font */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Stats - Text */

          txt_stats_c = txt_stats_i = J(rows(bd.si.label), 1, "")

          for(i=1; i<=rows(bd.si.label); i++)
          {
            /* Continuous */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundc[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundc[i])

              if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & bd.si.stars[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_c[i] = fmt

            /* Binary */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundi[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundi[i])
              if(bd.si.percent[i])    fmt = fmt + "%"

              if(bd.opt.test.individual & bd.opt.test.scripts != . & bd.si.scripts[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_i[i] = fmt
          }

        /* Stats - Format ID */

          /* Continuous */

            fmt_stats_c = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              fmt_stats_c[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_c[i], font)
              B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
              B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
            }

          /* Binary */

            fmt_stats_i = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              positions = selectindex(txt_stats_c :== txt_stats_i[i])
              if(rows(positions) > 0 & cols(positions) > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[positions[1]]
                continue
              }

              fmt_stats_i[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_i[i], font)
              B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
              B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
            }

        /* P-Values */

          fmt_pvalue = B.add_fmtid()
          B.fmtid_set_fontid(fmt_pvalue, font)
          B.fmtid_set_horizontal_align(fmt_pvalue, "center")

        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Whitespace */

          fmt_whitespace = B.add_fmtid()
          B.fmtid_set_left_border(fmt_whitespace, "medium")
          B.fmtid_set_right_border(fmt_whitespace, "medium")
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
          B.fmtid_set_left_border(fmt_question, "medium")
          B.fmtid_set_right_border(fmt_question, "medium")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_left_border(fmt_answer, "medium")
          B.fmtid_set_right_border(fmt_answer, "medium")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

      /* Creating Header */

        row = 1 + (pvalues > 0)
        col = rows(bd.si.label) + (pvalues * (1 + bd.opt.test.statistic))

        header = J(row, col, "")

        /* Stat Columns */

          pos1 = 1
          pos2 = rows(bd.si.label)

          header[row,(pos1..pos2)] = bd.si.label'

        /* P-Value Columns */

          pos1 = pos2 + 1
          pos2 = cols(header)

          /* Bottom Row */

            if(pvalues > 0)
            {
              if(bd.opt.test.statistic) header[row,(pos1..pos2)] = J(1, pvalues, ("Stat", "P-Value"))
              else                      header[row,(pos1..pos2)] = J(1, pvalues, "P-Value")
            }

          /* Top Row */

            if(pvalues > 0)
            {
              pnames = J(pvalues, 1, "")

              row = 1

              if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
              {
                pnames[row] = "Overall"

                row = row + 1
              }

              if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
              {
                pnames[row..rows(pnames)] = "vs " :+ char(34) :+ bd.oi.labels :+ char(34)
              }

              header[1,(pos1..pos2)] = rowshape(J(1, (1 + bd.opt.test.statistic), pnames), 1)
            }

      /* Initializing Table */

        row = input_row
        col = 1

        start_row = row

        end_col = 1 + cols(header)

      /* Table - Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, col, title)
          B.set_fmtid(row, col, fmt_title)

          row = row + 1
          start_row = row
        }

      /* Table - Header */

        B.put_string(row, (col+1), header)

        /* Merging Groups */

          if(pvalues > 0 & bd.opt.test.statistic)
          {
            sheet = B.query("sheetname")

            col = 2 + rows(bd.si.label)

            for(i=1; i<=pvalues; i++)
            {
              B.set_sheet_merge(sheet, (row,row), (col,(col+1)))

              col = col + 2
            }
          }

        /* Formatting - Whitespace */

          pos1 = start_row
          pos2 = start_row + rows(header) - 1

          B.set_fmtid((pos1,pos2), 1, fmt_whitespace)

        /* Formatting - Stats */

          B.set_fmtid(row, (2,end_col), fmt_header)
          if(pvalues > 0) B.set_fmtid((row+1), (2,end_col), fmt_header)

        row = start_row + rows(header)

      /* Table - Contents */

        over_labels = bd.oi.labels'
        if(bd.opt.over.total) over_labels = over_labels, "Total"

        for(i=1; i<=rows(bd.vi); i++)
        {
          col = 1

          end_row = row + (rows(bd.vi[i].varlist) * (cols(over_labels) + 1)) - 1

          /* Name */

            B.put_string(row, col, colshape((bd.vi[i].varlist, J(rows(bd.vi[i].varlist), 1, over_labels)), 1))

          /* Statistics */

            for(j=1; j<=rows(bd.si.label); j++)
            {
              col = col + 1

              /* Numeric */

                if(bd.si.name[j] != "ci" & ((bd.vi[i].binary & txt_stats_i[j] != "text") | (!bd.vi[i].binary & txt_stats_c[j] != "text")))
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  values = J(rows(values), 1, .), values

                  B.put_number(row, col, colshape(values, 1))

                  if(bd.vi[i].binary) B.set_fmtid((row,end_row), col, fmt_stats_i[j])
                  else                B.set_fmtid((row,end_row), col, fmt_stats_c[j])

                  continue
                }

              /* Text */

                if(bd.si.name[j] != "ci")
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = strofreal(values, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                }
                else
                {
                  values1 = bd.vi[i].results.lci
                  values2 = bd.vi[i].results.uci

                  if(bd.vi[i].binary & bd.si.percent[j])
                  {
                    values1 = values1 :* 100
                    values2 = values2 :* 100
                  }

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = (strofreal(values1, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")) :+ bd.si.ci_separator :+ (strofreal(values2, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"))
                  values  = values1
                }

                if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & bd.si.stars[j])
                {
                  postscripts = J(rows(bd.vi[i].varlist), 1, "")
                  for(k=1; k<=cols(bd.opt.test.stars); k++)
                  {
                    postscripts = postscripts :+ ((bd.vi[i].results.ovr_pvalue :< bd.opt.test.stars[k]) :* uchar(735))
                  }
                  strings = strings :+ postscripts
                }

                if(bd.opt.test.individual & bd.opt.test.scripts != . & bd.si.scripts[j])
                {
                  postscripts = J(rows(bd.vi[i].varlist), rows(bd.oi.levels), "")
                  for(k=1; k<=rows(bd.oi.levels); k++)
                  {
                    positions = range(k, k + ((rows(bd.oi.levels) - 1) * rows(bd.oi.levels)), rows(bd.oi.levels))
                    postscripts = postscripts :+ ((bd.vi[i].results.ind_pvalue[.,positions] :< bd.opt.test.scripts) :* uchar(bd.opt.test.letters[k]))
                  }
                  strings[.,(1..rows(bd.oi.levels))] = strings[.,(1..rows(bd.oi.levels))] :+ postscripts
                }

                strings = bd.si.notation[j,1] :+ strings :+ bd.si.notation[j,2]
                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                strings = J(rows(strings), 1, ""), strings

                B.put_string(row, col, colshape(strings, 1))

                if(bd.vi[i].binary) B.set_fmtid((row,end_row), pos1, fmt_stats_i[j])
                else                B.set_fmtid((row,end_row), pos1, fmt_stats_c[j])
            }

          /* P-Values - Overall */

            col = col + 1

            if(bd.opt.test.overall & (cols(bd.opt.test.stars) == 0 | !any(bd.si.stars) | bd.opt.test.force))
            {
              if(bd.opt.test.statistic)
              {
                if(bd.opt.test.t_overall)                           strings = "t = "
                else                                                strings = "F = "
                if(bd.vi[i].type == "xi" & bd.opt.test.chi_overall) strings = "Chi2 = "

                values  = bd.vi[i].results.ovr_statistic
                strings = strings :+ strtrim(strofreal(values, "%32.2f"))
                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")
                strings = J(rows(strings), 1, ""), J(1, cols(over_labels), strings)

                B.put_string(row, col, colshape(strings, 1))
                B.set_fmtid((row,end_row), col, fmt_pvalue)

                col = col + 1
              }

              values  = bd.vi[i].results.ovr_pvalue
              strings = "P = " :+ strtrim(strofreal(values, "%32.3f"))

              pos1 = selectindex(values :>= 0.01)
              if(rows(pos1) > 0) strings[pos1] = "P = " :+ strtrim(strofreal(values[pos1], "%32.2f"))

              pos1 = selectindex(values :< 0.001)
              if(rows(pos1) > 0) strings[pos1] = J(rows(pos1), 1, "P < 0.001")

              strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")
              strings = J(rows(strings), 1, ""), J(1, cols(over_labels), strings)

              B.put_string(row, col, colshape(strings, 1))
              B.set_fmtid((row,end_row), col, fmt_pvalue)

              col = col + 1
            }

          /* P-Values - Individual */

            if(bd.opt.test.individual & (bd.opt.test.scripts == . | !any(bd.si.scripts) | bd.opt.test.force))
            {
              for(k=1; k<=rows(bd.oi.levels); k++)
              {
                positions = range(k, k + ((rows(bd.oi.levels) - 1) * rows(bd.oi.levels)), rows(bd.oi.levels))

                if(bd.opt.test.statistic)
                {
                  values = J(rows(bd.vi[i].varlist), 1, .), bd.vi[i].results.ind_statistic[.,positions]
                  if(bd.opt.over.total) values = values, J(rows(bd.vi[i].varlist), 1, .)

                  values = colshape(values, 1)

                  if(bd.opt.test.t_individual) strings = "t = " :+ strtrim(strofreal(values, "%32.2f"))
                  else                         strings = "F = " :+ strtrim(strofreal(values, "%32.2f"))

                  strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                  B.put_string(row, col, strings)
                  B.set_fmtid((row,end_row), col, fmt_pvalue)

                  col = col + 1
                }

                values = J(rows(bd.vi[i].varlist), 1, .), bd.vi[i].results.ind_pvalue[.,positions]
                if(bd.opt.over.total) values = values, J(rows(bd.vi[i].varlist), 1, .)

                values  = colshape(values, 1)
                strings = "P = " :+ strtrim(strofreal(values, "%32.3f"))

                pos1 = selectindex(values :>= 0.01)
                if(rows(pos1) > 0) strings[pos1] = "P = " :+ strtrim(strofreal(values[pos1], "%32.2f"))

                pos1 = selectindex(values :< 0.001)
                if(rows(pos1) > 0) strings[pos1] = J(rows(pos1), 1, "P < 0.001")

                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                B.put_string(row, col, strings)
                B.set_fmtid((row,end_row), col, fmt_pvalue)

                col = col + 1
              }
            }

          row = end_row + 1
        }

        end_row = row - 1
        end_col = 1 + cols(header)

      /* Formatting - Results */

        strings = J(1, cols(header), "")

        row = start_row + rows(header)

        for(i=1; i<=rows(bd.vi); i++)
        {
          for(j=1; j<=rows(bd.vi[i].varlist); j++)
          {
            /* Empty Row */

              B.put_string(row, 2, strings)

            /* Question */

              B.set_fmtid(row, 1, fmt_question)

            /* Groups */

              pos1 = row + 1
              pos2 = row + cols(over_labels)

              B.set_fmtid((pos1,pos2), 1, fmt_answer)

            /* Bottom Line */

              row = pos2

              pos1 = 1
              pos2 = 1 + cols(header)

              B.set_bottom_border(row, (pos1,pos2), "thin")

            row = row + 1
          }
        }

      /* Formatting - Borders */

        pos1 = 1
        pos2 = 1 + cols(header)

        /* Header */

          row = start_row + rows(header)

          B.set_top_border(row, (pos1,pos2), "medium")

        /* P-Values */

          if(pvalues > 0)
          {
            col = 1 + rows(bd.si.label)

            B.set_right_border((start_row,end_row), col, "thin")

            if(bd.opt.test.statistic)
            {
              for(i=1; i<=pvalues; i++)
              {
                col = col + 2

                B.set_right_border((start_row,end_row), col, "thin")
              }
            }
          }

        /* Overall Table */

          B.set_top_border(start_row, (pos1,pos2), "medium")
          B.set_bottom_border(end_row, (pos1,pos2), "medium")

          B.set_right_border((start_row,end_row), end_col, "medium")

      /* Footer */

        row = end_row + 1

        if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & any(bd.si.stars) & bd.opt.test.footer)
        {
          spaces = " " :* (cols(bd.opt.test.stars) :- (range(1, cols(bd.opt.test.stars), 1)))

          legend = range(1, cols(bd.opt.test.stars), 1) :* uchar(735)
          legend = legend :+ spaces :+ " p(overall) < 0" :+ strofreal(bd.opt.test.stars)'

          B.put_string(row, 1, legend)

          row = row + rows(legend)
        }

        if(bd.opt.test.individual & bd.opt.test.scripts != . & any(bd.si.scripts) & bd.opt.test.footer)
        {
          legend = uchar(bd.opt.test.letters[1..rows(bd.oi.levels)])' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          B.put_string(row, 1, legend)
        }
    }

  /* function : excelTableLongNoOver() */

    void excelTableLongNoOver(struct braddev scalar bd,
                              class  xl      scalar B,
                              real           scalar input_row)
    {
      /* Creating Formats */

        B.set_missing(".")

        /* Font */

          font = B.add_fontid()
          B.fontid_set_font(font, bd.opt.excel.font_face, bd.opt.excel.font_size)

          font_bold = B.add_fontid()
          B.fontid_set_font(font_bold, bd.opt.excel.font_face, bd.opt.excel.font_size)
          B.fontid_set_font_bold(font_bold, "on")

        /* Stats - Text */

          txt_stats_c = txt_stats_i = J(rows(bd.si.label), 1, "")

          for(i=1; i<=rows(bd.si.label); i++)
          {
            /* Continuous */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundc[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundc[i])

              if(bd.opt.test.overall & cols(bd.opt.test.stars) > 0 & bd.si.stars[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_c[i] = fmt

            /* Binary */

              fmt = bd.si.comma[i] ? "#,##0" : "0"

              if(bd.si.roundi[i] > 0) fmt = fmt + "." + ("0" * bd.si.roundi[i])
              if(bd.si.percent[i])    fmt = fmt + "%"

              if(bd.opt.test.individual & bd.opt.test.scripts != . & bd.si.scripts[i]) fmt = "text"

              if(!allof(bd.si.notation[i,.], "")) fmt = "text"

              txt_stats_i[i] = fmt
          }

        /* Stats - Format ID */

          /* Continuous */

            fmt_stats_c = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              fmt_stats_c[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_c[i], font)
              B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
              B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])
            }

          /* Binary */

            fmt_stats_i = J(rows(bd.si.label), 1, .)

            for(i=1; i<=rows(bd.si.label); i++)
            {
              positions = selectindex(txt_stats_c :== txt_stats_i[i])
              if(rows(positions) > 0 & cols(positions) > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[positions[1]]
                continue
              }

              fmt_stats_i[i] = B.add_fmtid()
              B.fmtid_set_fontid(fmt_stats_i[i], font)
              B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
              B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
              B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])
            }
        /* Title */

          fmt_title = B.add_fmtid()
          B.fmtid_set_fontid(fmt_title, font_bold)

        /* Whitespace */

          fmt_whitespace = B.add_fmtid()
          B.fmtid_set_border(fmt_whitespace, "medium")
          B.fmtid_set_fill_pattern(fmt_whitespace, "solid", "white")

        /* Header */

          fmt_header = B.add_fmtid()
          B.fmtid_set_fontid(fmt_header, font_bold)
          B.fmtid_set_horizontal_align(fmt_header, "center")
          B.fmtid_set_top_border(fmt_header, "medium")
          B.fmtid_set_bottom_border(fmt_header, "medium")
          B.fmtid_set_fill_pattern(fmt_header, "solid", bd.opt.excel.color[1])

        /* Variables */

          fmt_question = B.add_fmtid()
          B.fmtid_set_fontid(fmt_question, font_bold)
          B.fmtid_set_horizontal_align(fmt_question, "left")
          B.fmtid_set_left_border(fmt_question, "medium")
          B.fmtid_set_right_border(fmt_question, "medium")
          B.fmtid_set_fill_pattern(fmt_question, "solid", bd.opt.excel.color[2])

          fmt_answer = B.add_fmtid()
          B.fmtid_set_fontid(fmt_answer, font_bold)
          B.fmtid_set_horizontal_align(fmt_answer, "right")
          B.fmtid_set_left_border(fmt_answer, "medium")
          B.fmtid_set_right_border(fmt_answer, "medium")
          B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])

      /* Creating Header */

        header = bd.si.label'

      /* Initializing Table */

        row = input_row
        col = 1

        start_row = row

        end_col = 1 + cols(header)

      /* Table - Title */

        title = getTitle(bd.vi)

        if(title != "")
        {
          B.put_string(row, col, title)
          B.set_fmtid(row, col, fmt_title)

          row = row + 1
          start_row = row
        }

      /* Table - Header */

        B.set_fmtid(row, 1, fmt_whitespace)

        B.put_string(row, (col+1), header)
        B.set_fmtid(row, (2,end_col), fmt_header)

        row = row + 1

      /* Table - Contents */

        for(i=1; i<=rows(bd.vi); i++)
        {
          col = 1

          /* Names */

            if(bd.vi[i].type == "individual")
            {
              B.put_string(row, col, bd.vi[i].varlist)
              B.set_fmtid(row, col, fmt_question)
            }
            else
            {
              B.put_string(row, col, bd.vi[i].question)
              B.set_fmtid(row, col, fmt_question)

              row = row + 1

              pos1 = row
              pos2 = row + rows(bd.vi[i].answers) - 1

              B.put_string(row, col, bd.vi[i].answers)
              B.set_fmtid((pos1,pos2), col, fmt_answer)
            }

          /* Statistics */

            end_row = row + rows(bd.vi[i].answers) - 1

            for(j=1; j<=rows(bd.si.label); j++)
            {
              col = col + 1

              /* Numeric */

                if(bd.si.name[j] != "ci" & ((bd.vi[i].binary & txt_stats_i[j] != "text") | (!bd.vi[i].binary & txt_stats_c[j] != "text")))
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  B.put_number(row, col, values)

                  if(bd.vi[i].binary) B.set_fmtid((row,end_row), col, fmt_stats_i[j])
                  else                B.set_fmtid((row,end_row), col, fmt_stats_c[j])

                  continue
                }

              /* Text */

                if(bd.si.name[j] != "ci")
                {
                  if(bd.si.name[j] == "obs")  values = bd.vi[i].results.obs
                  if(bd.si.name[j] == "nyes") values = bd.vi[i].results.nyes
                  if(bd.si.name[j] == "mean") values = bd.vi[i].results.mean
                  if(bd.si.name[j] == "se")   values = bd.vi[i].results.se
                  if(bd.si.name[j] == "sd")   values = bd.vi[i].results.sd
                  if(bd.si.name[j] == "var")  values = bd.vi[i].results.var
                  if(bd.si.name[j] == "lci")  values = bd.vi[i].results.lci
                  if(bd.si.name[j] == "uci")  values = bd.vi[i].results.uci
                  if(bd.si.name[j] == "min")  values = bd.vi[i].results.min
                  if(bd.si.name[j] == "max")  values = bd.vi[i].results.max

                  if(bd.vi[i].binary & bd.si.percent[j]) values = values :* 100

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = strofreal(values, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")
                }
                else
                {
                  values1 = bd.vi[i].results.lci
                  values2 = bd.vi[i].results.uci

                  if(bd.vi[i].binary & bd.si.percent[j])
                  {
                    values1 = values1 :* 100
                    values2 = values2 :* 100
                  }

                  format = "%32." + (bd.vi[i].binary ? strofreal(bd.si.roundi[j]) : strofreal(bd.si.roundc[j])) + "f" + (bd.si.comma[j] * "c")

                  strings = (strofreal(values1, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%")) :+ bd.si.ci_separator :+ (strofreal(values2, format) :+ (bd.vi[i].binary * bd.si.percent[j] * bd.si.symbol[j] * "%"))
                  values  = values1
                }

                strings = bd.si.notation[j,1] :+ strings :+ bd.si.notation[j,2]
                strings = ((values :!= .) :* strings) :+ ((values :== .) :* ".")

                B.put_string(row, col, strings)

                if(bd.vi[i].binary) B.set_fmtid((row,end_row), col, fmt_stats_i[j])
                else                B.set_fmtid((row,end_row), col, fmt_stats_c[j])
            }

          /* Setting Bottom Border */

            if(i < rows(bd.vi))
            {
              B.set_bottom_border(end_row, (1,end_col), "thin")
            }

          row = end_row + 1
        }

        end_row = row - 1

      /* Formatting - Borders */

        B.set_right_border((start_row,end_row), end_col, "medium")

        B.set_bottom_border(end_row, (1,end_col), "medium")
    }

end
