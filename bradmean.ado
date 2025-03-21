version 15.1
#delimit;
include bradsuite.mata, adopath;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradmean.ado                                         **
**   Purpose:      Computes multiple independent means in single table  **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.8.2                                                **
**   Date:         03/07/2025                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions - bradmean                                         */
/*======================================================================*/

  program define bradmean, nclass sortpreserve byable(recall);
  syntax varlist(fv) [if] [in] [fweight aweight pweight iweight/],
    [
      SVY
      SUBpop(varname numeric)

      OVER(varlist)
      OVEROPT(string)
      TEST(string)

      STats(string)
      FORMAT(string)
      SORT(string)
      VCE(passthru)

      DISplay(string)
      TITLE(string)
      EXCEL(string)
      COLLECT(string)
    ];

  *----------------------------------------------------------*
  *   01. Creating Sample Marker for Estimation              *
  *----------------------------------------------------------*;

    marksample touse, novarlist strok;

    if("`over'" != "")
    {;
      markout `touse' `over', strok;
    };

    if(_by())
    {;
      qui replace `touse' = 0 if `_byindex' != _byindex();
    };

  *----------------------------------------------------------*
  *   02. Parsing Weight & VCE                               *
  *----------------------------------------------------------*;

    if("`weight'" != "")
    {;
      local weight = "[`weight'=`exp']";
      local exp;
    };

    if("`vce'" != "")
    {;
      _vce_parse `touse', opt(ANALYTIC BOOTstrap JACKknife) argopt(CLuster) pwallowed(analytic bootstrap jackknife cluster) : `weight', `vce';
      local vce = "`r(vceopt)'";
    };

  *----------------------------------------------------------*
  *   03. Initializing Bradmean & Getting Options            *
  *----------------------------------------------------------*;

    mata: mata set matastrict on;
    mata: bd = bradmean();
    mata: initOptions(bd.opt);

  *----------------------------------------------------------*
  *   04. Creating Sample Marker for Counts                  *
  *----------------------------------------------------------*;

    tempvar touse_cnt;

    if("`subpop'" == "")
    {;
      mark `touse_cnt' if `touse';
    };
    else
    {;
      mark `touse_cnt' if `touse' & `subpop' != 0;
    };

  *----------------------------------------------------------*
  *   05. Getting Results                                    *
  *----------------------------------------------------------*;

    mata: initVarInfo(bd);
    mata: initOverInfo(bd);
    mata: initStatInfo(bd);
    mata: gatherResults(bd);

  *----------------------------------------------------------*
  *   06. Exporting Results                                  *
  *----------------------------------------------------------*;

    mata: printer(bd);

    if("`collect'" == "")
    {;
      mata: createExcelSingle(bd);
    };

    if(inlist("`collect'","save","export"))
    {;
      cap mata: length(bd_vec);

      if(_rc != 0)
      {;
        mata: bd_vec = combBradmean(bd);
      };
      else
      {;
        mata: bd_vec = combBradmean(bd, bd_vec);
      };
    };

    if("`collect'" == "export")
    {;
      mata: createExcelMultiple(bd_vec);

      mata: mata drop bd_vec;
    };

  *----------------------------------------------------------*
  *   07. Cleaning Up                                        *
  *----------------------------------------------------------*;

    mata: mata drop bd;

  end;

/*======================================================================*/
/*   Stata Functions - bd_format                                        */
/*======================================================================*/

  program define bd_format, sclass;
  syntax,
    [
      ROUND(integer -1)
      ROUNDC(integer -1)
      ROUNDI(integer -1)

      PCT
      NPCT

      PERcent
      NPERcent

      SYMbol
      NSYMbol

      COMMA
      NCOMMA

      STARs
      SCRIPTs

      NOTation(string)

      Level(cilevel)
      PROPortion
      COMBined
      SEParator(string)
      *
    ];

    sreturn local round         = "";
    sreturn local roundc        = "";
    sreturn local roundi        = "";
    sreturn local percent       = "";
    sreturn local symbol        = "";
    sreturn local comma         = "";
    sreturn local stars         = "";
    sreturn local scripts       = "";
    sreturn local notation      = "";
    sreturn local ci_proportion = "";
    sreturn local ci_combined   = "";
    sreturn local ci_separator  = "";
    sreturn local ci_level      = "";

    if(`round'  >= 0 & `round'  <= 7) sreturn local round  = `round';
    if(`roundc' >= 0 & `roundc' <= 7) sreturn local roundc = `roundc';
    if(`roundi' >= 0 & `roundi' <= 7) sreturn local roundi = `roundi';

    if("`pct'"  != "") sreturn local percent = 1;
    if("`npct'" != "") sreturn local percent = 0;

    if("`percent'"  != "") sreturn local percent = 1;
    if("`npercent'" != "") sreturn local percent = 0;

    if("`symbol'"  != "") sreturn local symbol = 1;
    if("`nsymbol'" != "") sreturn local symbol = 0;

    if("`comma'"  != "") sreturn local comma = 1;
    if("`ncomma'" != "") sreturn local comma = 0;

    if("`stars'"  != "")  sreturn local stars   = 1;
    if("`scripts'" != "") sreturn local scripts = 1;

    local notation = strlower(substr("`notation'", 1, 3));
    if("`notation'" == "par") sreturn local notation = "( )";
    if("`notation'" == "bra") sreturn local notation = "[ ]";

    if("`proportion'" != "")                 sreturn local ci_proportion = 1;
    if("`combined'"   != "")                 sreturn local ci_combined   = 1;
    if(inlist("`separator'", " ", ",", "-")) sreturn local ci_separator  = "`separator'";

    sreturn local ci_level = `level';

  end;

#delimit cr

mata:

/*======================================================================*/
/*   Mata Structures                                                    */
/*======================================================================*/

  /* struct : bradmean */

    struct bradmean
    {
      struct options  scalar    opt
      struct varInfo  rowvector vi
      struct overInfo scalar    oi
      struct statInfo scalar    si
    }

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
      `Boolean' xi_values,     xi_variables
      `Boolean' series_values, series_variables

      /* Sort */
      `String'  sort_direction, sort_statistic

      /* Table */
      `Boolean' print
      `String'  title
      `Boolean' separator
      `Boolean' statnames
      `Boolean' wide
      `String'  align
    }

  /* struct : options_over */

    struct options_over
    {
      /* Estimation */
      `Boolean' miss
      `Boolean' row
      `Boolean' total

      /* Display */
      `Boolean' group_n
      `Boolean' labels
      `Boolean' legend
    }

  /* struct : options_test */

    struct options_test
    {
      /* P-Values */
      `Boolean'   overall, individual, statistic

      /* Chi2 Test */
      `Boolean'   chi_overall

      /* T-Test */
      `Boolean'   t_overall, t_individual

      /* F-Test */
      `Boolean'   f_overall, f_individual
      `String'    f_mtest

      /* Significance Notation */
      `Boolean'   footer
      `Boolean'   force
      `RealVec'   stars
      `Real'      scripts
      `StringVec' letters
    }

  /* struct : options_weight */

    struct options_weight
    {
      /* General */
      `Boolean' any

      /* Direct Weights */
      `String'  cmd
      `String'  vce

      /* Survey Weights */
      `Boolean' survey
      `String'  subpop
    }

  /* struct : options_excel */

    struct options_excel
    {
      /* Command Information */
      `Boolean'   output
      `Boolean'   bookreplace, sheetreplace

      /* File Information */
      `String'    file_path, sheet

      /* Style Information */
      `StringVec' color
      `String'    font_face
      `Real'      font_size
    }

  /* struct : varInfo */

    struct varInfo
    {
      /* Name */
      `String'    term
      `StringVec' varlist

      /* Information */
      `String'    type
      `Boolean'   binary

      /* Description */
      `RealVec'   levels
      `StringVec' answers
      `String'    question

      /* Results */
      struct results scalar res
    }

  /* struct : overInfo */

    struct overInfo
    {
      /* Name */
      `String'    name
      `StringVec' varlist

      /* Levels & Frequencies */
      `RealVec'   levels, freqs
      `StringVec' labels
    }

  /* struct : statInfo */

    struct statInfo
    {
      /* Name */
      `StringVec' name, label

      /* Format - Rounding */
      `RealVec'   roundc, roundi

      /* Format - Notation */
      `RealVec'   comma, percent, symbol
      `StringMat' notation

      /* Format - P-Values */
      `RealVec'   stars, scripts

      /* CI Specific */
      `Real'      ci_level
      `Boolean'   ci_combined, ci_proportion
      `String'    ci_separator
    }

  /* struct : results */

    struct results
    {
      /* Count */
      `RealMat' obs
      `RealMat' nyes

      /* Mean */
      `RealMat' mean

      /* Error */
      `RealMat' lci, uci
      `RealMat' se
      `RealMat' sd
      `RealMat' var

      /* MinMax */
      `RealMat' min, max

      /* Pctiles */
      `RealMat' p25, p50, p75

      /* Calculation Only */
      `RealMat' t
      `RealMat' df

      /* P-Values */
      `RealMat' ovr_statistic, ovr_pvalue
      `RealMat' ind_statistic, ind_pvalue
    }

/*======================================================================*/
/*   Mata Functions - Initializing Bradmean                             */
/*======================================================================*/

  /* function : initOptions() */

    void function initOptions(struct options scalar opt)
    {
      /* Default Options */

        /* Display */

          /* XI & Series */
          opt.display.xi_values     = opt.display.xi_variables     = 1
          opt.display.series_values = opt.display.series_variables = 0

          /* Sort */
          opt.display.sort_direction = "-"
          opt.display.sort_statistic = ""

          /* Table */
          opt.display.print     = 1
          opt.display.wide      = 0
          opt.display.separator = 1
          opt.display.statnames = 1
          opt.display.align     = "lalign"

        /* Over */

          /* Estimation */
          opt.over.miss  = 1
          opt.over.row   = 0
          opt.over.total = 0

          /* Display */
          opt.over.group_n = 0
          opt.over.labels  = 1
          opt.over.legend  = 1

        /* Test */

          /* P-Values */
          opt.test.overall = opt.test.individual = opt.test.statistic = 0

          /* F-Test */
          opt.test.f_overall = opt.test.f_individual = 0
          opt.test.f_mtest   = "noadjust"

          /* Chi2 Test */
          opt.test.chi_overall = 0

          /* T-Test */
          opt.test.t_overall = opt.test.t_individual = 0

          /* Significance Notation */
          opt.test.footer  = 1
          opt.test.force   = 0
          opt.test.stars   = J(0, 0, .)
          opt.test.scripts = .
          opt.test.letters = uchar((7468,7470,7472,7473,7475,7476,7477,7478,7479,7480,7481,7482,7484,7486,7487,7488,7489,7490))

        /* Weight */

          opt.weight.cmd    = st_local("weight")
          opt.weight.vce    = st_local("vce")
          opt.weight.survey = st_local("svy") != "" | st_local("subpop") != ""
          opt.weight.subpop = st_local("subpop")
          opt.weight.any    = opt.weight.survey | opt.weight.cmd != ""

        /* Excel */

          /* Command Information */
          opt.excel.output      = 0
          opt.excel.bookreplace = opt.excel.sheetreplace = 0

          /* File Information */
          opt.excel.file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")

          /* Style Information */
          opt.excel.color     = ("228 223 236", "238 236 225")
          opt.excel.font_face = "Calibri"
          opt.excel.font_size = 11

      /* Getting Options */

        parseSort(opt.display)
        parseDisplay(opt.display)
        parseOver(opt.over)
        parseTest(opt.test)
        parseWeight(opt.weight)
        parseExcel(opt.excel)
    }

  /* function : parseSort() */

    void function parseSort(struct options_display scalar dis)
    {
      `String' input_string, word

      if((input_string = strlower(st_local("sort"))) == "") return

      /* Direction */

        word = substr(input_string, 1, 1)

        if(word == "+" | word == "-")
        {
          dis.sort_direction = word
          input_string = substr(input_string, 2)
        }

      /* Statistic */

        if(anyof(tokens("obs nyes mean se sd var min max p25 p50 p75"), input_string)) dis.sort_statistic = input_string
    }

  /* function : parseDisplay() */

    void function parseDisplay(struct options_display scalar dis)
    {
      `Tokens' tokens
      `String' input_string, word
      `Pos'    pos

      if((input_string = strlower(st_local("display"))) == "") return

      /* Getting Tokens */

        tokens = tokenbind(input_string)

      /* XI & Series */

        if(anyof(tokens, "noxi"))               dis.xi_values    = dis.xi_variables = 0
        if(anyof(strpos(tokens, "noxival"), 1)) dis.xi_values    = 0
        if(anyof(strpos(tokens, "noxivar"), 1)) dis.xi_variables = 0

        if(anyof(tokens, "series"))               dis.series_values    = dis.series_variables = 1
        if(anyof(strpos(tokens, "seriesval"), 1)) dis.series_values    = 1
        if(anyof(strpos(tokens, "seriesvar"), 1)) dis.series_variables = 1

      /* Table */

        if(anyof(tokens, "noprint"))           dis.print     = 0
        if(anyof(tokens, "wide"))              dis.wide      = 1
        if(anyof(strpos(tokens, "nosep"), 1))  dis.separator = 0
        if(anyof(strpos(tokens, "nostat"), 1)) dis.statnames = 0

        if(length(pos = selectindex(strpos(tokens, "al") :== 1)) > 0)
        {
          word = substr(insidepar(tokens[pos[1]], "(", ")"), 1, 1)

          if(word == "c")      dis.align = "center"
          else if(word == "r") dis.align = "ralign"
        }
    }

  /* function : parseOver() */

    void function parseOver(struct options_over scalar over)
    {
      `Tokens' tokens
      `String' input_string

      if((input_string = strlower(st_local("overopt"))) == "") return

      /* Getting Tokens */

        tokens = tokenbind(input_string)

      /* Estimation */

        if(anyof(strpos(tokens, "nomi"), 1)) over.miss  = 0
        if(anyof(tokens, "row"))             over.row   = 1
        if(anyof(strpos(tokens, "tot"), 1))  over.total = 1

      /* Display */

        if(anyof(strpos(tokens, "group"), 1)) over.group_n = 1
        if(anyof(strpos(tokens, "nolab"), 1)) over.labels  = 0
        if(anyof(strpos(tokens, "noleg"), 1)) over.legend  = 0

      /* Cleaning Up */

        if(over.row) over.total = 0
    }

  /* function : parseTest() */

    void function parseTest(struct options_test scalar test)
    {
      `Tokens'  tokens, subtokens
      `String'  input_string, word
      `RealVec' values
      `Pos'     pos

      if((input_string = strlower(st_local("test"))) == "") return

      /* Getting Tokens */

        tokens = tokenbind(input_string)

      /* P-Values */

        if(anyof(strpos(tokens, "stat"), 1))  test.statistic = 1
        if(anyof(strpos(tokens, "nofo"), 1))  test.footer    = 0
        if(anyof(strpos(tokens, "force"), 1)) test.force     = 1

      /* F-Test */

        if(anyof(strpos(tokens, "all"), 1))  test.f_overall    = test.f_individual = 1
        if(anyof(strpos(tokens, "over"), 1)) test.f_overall    = 1
        if(anyof(strpos(tokens, "ind"), 1))  test.f_individual = 1

        if(length(pos = selectindex(strpos(tokens, "ftest") :== 1)) > 0)
        {
          subtokens = tokens(insidepar(tokens[pos[1]], "(", ")"))

          if(length(subtokens) == 0)
          {
            test.f_overall = 1
          }
          else
          {
            if(anyof(strpos(subtokens, "all"), 1))  test.f_overall    = test.f_individual = 1
            if(anyof(strpos(subtokens, "over"), 1)) test.f_overall    = 1
            if(anyof(strpos(subtokens, "ind"), 1))  test.f_individual = 1

            if(length(pos = selectindex(strpos(subtokens, "mtest") :== 1)) > 0)
            {
              word = substr(subtokens[pos[1]], strpos(subtokens[pos[1]], "(") :+ 1, 1)
              if(anyof(("b", "h", "s"), word)) test.f_mtest = word
            }
          }
        }

      /* Chi2 Test */

        if(anyof(strpos(tokens, "chi"), 1)) test.chi_overall = 1

      /* T-Test */

        if(length(pos = selectindex(strpos(tokens, "ttest") :== 1)) > 0)
        {
          subtokens = tokens(insidepar(tokens[pos[1]], "(", ")"))

          if(length(subtokens) == 0)
          {
            test.t_overall = 1
          }
          else
          {
            if(anyof(strpos(subtokens, "all"), 1))  test.t_overall    = test.t_individual = 1
            if(anyof(strpos(subtokens, "over"), 1)) test.t_overall    = 1
            if(anyof(strpos(subtokens, "ind"), 1))  test.t_individual = 1
          }
        }

      /* Significance Notation - Stars */

        if(length(pos = selectindex(strpos(tokens, "star") :== 1)) > 0)
        {
          values = strtoreal(tokens(insidepar(tokens[pos[1]], "(", ")")))
          values = sort(values[selectindex((values :> 0) :& (values :< 1))]', 1)'

          if(length(values) == 0) test.stars = (0.01, 0.05)
          else                    test.stars = values[|1\min((length(values),3))|]
        }

      /* Significance Notation - Scripts */

        if(length(pos = selectindex(strpos(tokens, "script") :== 1)) > 0)
        {
          values = strtoreal(tokens(insidepar(tokens[pos[1]], "(", ")")))
          values = sort(values[selectindex((values :> 0) :& (values :< 1))]', 1)'

          if(length(values) == 0) test.scripts = 0.05
          else                    test.scripts = values[1]
        }

      /* Cleaning Up */

        if(length(test.stars) > 0 | test.chi_overall | test.t_overall) test.f_overall = 1
        test.overall = (test.chi_overall | test.t_overall | test.f_overall)

        if(test.scripts != . | test.t_individual) test.f_individual = 1
        test.individual = (test.t_individual | test.f_individual)
    }

  /* function : parseWeight() */

    void function parseWeight(struct options_weight scalar weight)
    {
      `Real' rc

      if(weight.survey)
      {
        checkerr(rc = _stata("_svy_newrule", 1))

        checkerr(rc = _stata("svymarkout " + st_local("touse"), 1))
      }

      if(weight.survey & weight.cmd != "") checkerr(101)
      if(weight.survey & weight.vce != "") checkerr(198)
    }

  /* function : parseExcel() */

    void function parseExcel(struct options_excel scalar excel)
    {
      `Tokens' tokens
      `String' input_string, word, path1, path2
      `Real'   value
      `Pos'    pos

      if((input_string = st_local("excel")) == "") return

      excel.output = 1

      /* Getting Tokens */

        tokens = tokenbind(input_string)

      /* Mode */

        if(anyof(strpos(tokens, "rep"), 1))      excel.bookreplace  = 1
        if(anyof(strpos(tokens, "sheetrep"), 1)) excel.sheetreplace = 1
        if(anyof(strpos(tokens, "mod"), 1))      excel.bookreplace  = excel.sheetreplace = 0

      /* File */

        if(length(pos = selectindex(strpos(tokens, "file") :== 1)) > 0)
        {
          word = subinstr(insidepar(tokens[pos[1]], "(", ")"), `"""', "")

          pathsplit(word, path1, path2)

          if(path1 == "")
          {
            path1 = c("pwd")
          }
          else if(!direxists(path1))
          {
            printf("{error:Directory does not exist, defaulting to " + `"""' + c("pwd") + `"""' + "}\n")
            path1 = c("pwd")
          }

          if(path2 == "")
          {
            path2 = "bradmean_output.xlsx"
          }
          else if(pathsuffix(path2) != ".xlsx" | pathsuffix(path2) != ".xls")
          {
            path2 = pathrmsuffix(path2) + ".xlsx"
          }

          excel.file_path = pathjoin(path1, path2)
        }

      /* Sheet */

        if(length(pos = selectindex(strpos(tokens, "sheet") :== 1)) > 0)
        {
          excel.sheet = insidepar(tokens[pos[1]], "(", ")")
        }

      /* Colors */

        if(length(pos = selectindex(strpos(tokens, "color") :== 1)) > 0)
        {
          word = insidepar(tokens[pos[1]], "(", ")")

          if(word == "bradfield")            excel.color = ("228 223 236", "238 236 225")
          else if(word == "material_red")    excel.color = ("255 235 238", "235 255 252")
          else if(word == "material_purple") excel.color = ("243 229 245", "231 245 229")
          else if(word == "material_indigo") excel.color = ("232 234 246", "246 244 232")
          else if(word == "material_blue")   excel.color = ("227 242 253", "253 238 227")
          else if(word == "material_green")  excel.color = ("232 245 233", "245 232 244")
          else if(word == "material_orange") excel.color = ("255 243 224", "224 236 255")
          else if(word == "monochrome")      excel.color = ("255 255 255", "255 255 255")
          else if(word == "rti")             excel.color = ("204 220 233", "233 217 204")
        }

      /* Font Face */

        if(length(pos = selectindex(strpos(tokens, "font") :== 1)) > 0)
        {
          word = insidepar(tokens[pos[1]], "(", ")")

          if(word == "arial")          excel.font_face = "Arial"
          else if(word == "calibri")   excel.font_face = "Calibri"
          else if(word == "garamond")  excel.font_face = "Garamond"
          else if(word == "helvetica") excel.font_face = "Helvetica"
          else if(word == "tnr")       excel.font_face = "Times New Roman"
          else if(word == "verdana")   excel.font_face = "Verdana"
        }

      /* Font Size */

        if(length(pos = selectindex(strpos(tokens, "size") :== 1)) > 0)
        {
          value = strtoreal(insidepar(tokens[pos[1]], "(", ")"))

          if(value >= 9 & value <= 12) excel.font_size = value
        }
    }

  /* function : initVarInfo() */

    void function initVarInfo(struct bradmean scalar bd)
    {
      `Tokens'  termlist, labels
      `Pos'     pos
      `RealVec' sel_terms, sel_vars
      `Integer' len, vars
      `Integer' rc, i, j

      /* Cleaning Tokens */

        termlist = st_local("0")

        pos = strpos(termlist, " if "), strpos(termlist, " in "), strpos(termlist, ",")

        if(!allof(pos, 0))
        {
          pos = min(pos[selectindex(pos :!= 0)])

          termlist = substr(termlist, 1, pos - 1)
        }

        if((pos = strpos(termlist, "[")) != 0)
        {
          termlist = substr(termlist, 1, pos - 1)
        }

        termlist = gentokens(termlist)
        termlist = ((strpos(termlist, ".") :!= 0) :* "i.") :+ substr(termlist, strpos(termlist, ".") :+ 1)

      /* Getting Information */

        bd.vi     = varInfo(len = length(termlist))
        sel_terms = J(1, len, 1)

        for(i=len; i; i--)
        {
          /* Varlist */

            bd.vi[i].term    = termlist[i]
            rc               = _stata("ds " + subinstr(termlist[i], "i.", "") + ", has(type numeric)", 1)
            bd.vi[i].varlist = tokens(st_global("r(varlist)"))

            if((vars = length(bd.vi[i].varlist)) == 0)
            {
              sel_terms[i] = 0
              continue
            }

          /* Type - XI */

            if(strpos(termlist[i], "i.") :== 1)
            {
              /* Checking for Any Values */

                rc = _stata("count if " + st_local("touse_cnt") + " & !missing(" + bd.vi[i].varlist + ")", 1)

                if(st_numscalar("r(N)") == 0)
                {
                  sel_terms[i] = 0
                  continue
                }

              /* Type & Binary */

                bd.vi[i].type   = "xi"
                bd.vi[i].binary = 1

              /* Question */

                if(bd.opt.display.xi_variables) bd.vi[i].question = st_varlabel(bd.vi[i].varlist)
                if(bd.vi[i].question == "")     bd.vi[i].question = bd.vi[i].term

              /* Levels & Answers */

                matrow = st_tempname()
                rc     = _stata("tab " + bd.vi[i].varlist + ", matrow(" + matrow + ")", 1)

                bd.vi[i].levels = st_matrix(matrow)'

                if(bd.opt.display.xi_values & (labels = st_varvaluelabel(bd.vi[i].varlist)) != "")
                {
                  bd.vi[i].answers = st_vlmap(labels, bd.vi[i].levels)
                  if(length(pos = selectindex(bd.vi[i].answers :== "")) > 0) bd.vi[i].answers[pos] = bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels[pos])
                }
                else
                {
                  bd.vi[i].answers = bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels)
                }

              /* Marking Out Missing */

                if(!bd.opt.over.miss)
                {
                  checkerr(rc = _stata("markout " + st_local("touse")     + " " + invtokens(bd.vi[i].varlist), 1))
                  checkerr(rc = _stata("markout " + st_local("touse_cnt") + " " + invtokens(bd.vi[i].varlist), 1))
                }

              continue
            }

          /* Type - Series */

            /* Checking for Any Values */

              sel_vars = J(1, vars, 1)

              for(j=vars; j; j--)
              {
                rc = _stata("count if " + st_local("touse_cnt") + " &  !missing(" + bd.vi[i].varlist[j] + ")", 1)

                sel_vars[j] = st_numscalar("r(N)") != 0
              }

              if(max(sel_vars) == 0)
              {
                sel_terms[i] = 0
                continue
              }
              else
              {
                bd.vi[i].varlist = bd.vi[i].varlist[selectindex(sel_vars)]
                vars             = length(bd.vi[i].varlist)
              }

            /* Type & Binary */

              bd.vi[i].type   = "series"
              rc              = _stata("assert " + invtokens("(missing(" :+ bd.vi[i].varlist :+ ") | inlist(" :+ bd.vi[i].varlist :+ ",0,1))", " & "), 1)
              bd.vi[i].binary = rc == 0

            /* Question & Answers */

              labels = J(1, vars, "")
              for(j=vars; j; j--) labels[j] = st_varlabel(bd.vi[i].varlist[j])

              if(vars > 1)
              {
                bd.vi[i].answers = bd.opt.display.series_values ? insidepar(labels, "[", "]") : bd.vi[i].varlist

                if(bd.opt.display.series_variables)
                {
                  labels = strtrim(insidepar(labels, "]", ""))
                  bd.vi[i].question = labels[selectindex(udstrlen(labels) :== max(udstrlen(labels)))[1]]
                }
                else
                {
                  bd.vi[i].question = bd.vi[i].term
                }
              }
              else
              {
                bd.vi[i].question = bd.opt.display.series_variables ? labels[1] : bd.vi[i].varlist
                bd.vi[i].answers  = bd.opt.display.series_variables ? labels[1] : bd.vi[i].varlist
              }

              if(bd.vi[i].question == "") bd.vi[i].question = bd.vi[i].term
              if(length(pos = selectindex(bd.vi[i].answers :== "")) > 0) bd.vi[i].answers[pos] = bd.vi[i].varlist[pos]

            /* Marking Out Missing */

              if(!bd.opt.over.miss)
              {
                checkerr(rc = _stata("markout " + st_local("touse")     + " " + invtokens(bd.vi[i].varlist), 1))
                checkerr(rc = _stata("markout " + st_local("touse_cnt") + " " + invtokens(bd.vi[i].varlist), 1))
              }
        }

      /* Selecting Terms & Getting Title */

        bd.vi = bd.vi[selectindex(sel_terms)]

        if(length(bd.vi) == 0) checkerr(102)

        bd.opt.display.title = getTitle(bd.vi)
    }

  /* function : getTitle() */

    `String' getTitle(struct varInfo rowvector vi)
    {
      `StringVec' title
      `Integer'   terms
      `Integer'   i

      title = st_local("title")
      terms = length(vi)

      if(strlower(title) == "none") return("")
      else if(title != "")          return(title)

      if(terms == 1)
      {
        return((vi.term == vi.question) ? vi.term : vi.term + " - " + vi.question)
      }
      else
      {
        title = J(1, terms, "")

        for(i=terms; i; i--) title[i] = vi[i].term

        return(invtokens(title, ", "))
      }

      return("")
    }

  /* function : initOverInfo() */

    void function initOverInfo(struct bradmean scalar bd)
    {
      `String'  matcell, matrow
      `String'  group_num, group_str
      `Integer' len
      `Integer' rc

      /* Empty Varlist */

        if(st_local("over") == "") return

      /* Getting Varlist */

        bd.oi.varlist = tokens(st_local("over"))

      /* Generating Variables */

        bd.oi.name = st_tempname()
        group_num  = st_tempname()
        group_str  = st_tempname()

        checkerr(rc = _stata("egen    " + group_num + " = group("  + st_local("over") + ") if " + st_local("touse_cnt"), 1))
        checkerr(rc = _stata("egen    " + group_str + " = concat(" + st_local("over") + ") if " + st_local("touse_cnt") + `", decode punct(", ")"', 1))
        checkerr(rc = _stata("replace " + group_str + " = string(" + group_num + `", "%05.0f""' + `") + " " + "' + group_str + " if " + st_local("touse_cnt"), 1))
        checkerr(rc = _stata("encode  " + group_str + ", generate(" + bd.oi.name + ")", 1))

      /* Getting Levels */

        matcell = st_tempname()
        matrow  = st_tempname()

        checkerr(rc = _stata("tab " + bd.oi.name + " if " + st_local("touse_cnt") + ", matcell(" + matcell + ") matrow(" + matrow + ")", 1))

        bd.oi.levels = st_matrix(matrow)'
        bd.oi.freqs  = st_matrix(matcell)'
        bd.oi.labels = bd.opt.over.labels ? st_vlmap(st_varvaluelabel(bd.oi.name), bd.oi.levels) : "_over_" :+ strofreal(bd.oi.levels)
        bd.oi.labels = substr(bd.oi.labels, strpos(bd.oi.labels, " ") :+ 1)

        if(length(pos = selectindex(bd.oi.labels :== "")) > 0) bd.oi.labels[pos] = "_over_" :+ strofreal(bd.oi.levels[pos])

      /* Cleaning Options - Overall */

        if((bd.opt.weight.cmd != "" & strpos(bd.opt.weight.cmd, "[fw") != 1) | bd.opt.weight.vce != "")
        {
          bd.opt.test.chi_overall = 0
        }

        if((len = length(bd.oi.levels)) > 2 & bd.opt.test.t_overall)
        {
          bd.opt.test.f_overall = 1
          bd.opt.test.t_overall = 0
        }
        else if(len == 2 & (bd.opt.test.t_individual | bd.opt.test.f_individual))
        {
          if(bd.opt.test.t_individual) { bd.opt.test.t_overall = 1; bd.opt.test.t_individual = 0; }
          if(bd.opt.test.f_individual) { bd.opt.test.f_overall = 1; bd.opt.test.f_individual = 0; }

          bd.opt.test.overall    = 1
          bd.opt.test.individual = 0
        }
        else if(len == 1)
        {
          bd.opt.display.wide = 0

          bd.oi.levels = J(0,0,.)

          errprintf("{error:Only 1 level of over, treating as {helpb if}}\n")
        }

      /* Cleaning Options - Individual */

        if(bd.opt.test.individual)
        {
          if(bd.opt.over.row)
          {
            bd.opt.test.overall    = bd.opt.test.f_overall = 1
            bd.opt.test.t_overall  = 0
            bd.opt.test.individual = 0

            errprintf("{error:Individual testing not allowed with row percentages}\n")
          }
          else
          {
            if(len > 167)
            {
              bd.opt.test.overall    = bd.opt.test.f_overall = 1
              bd.opt.test.individual = 0

              errprintf("{error:Individual testing only allows up to 167 levels}\n")
            }

            if(bd.opt.test.scripts != .)
            {
              if(len > 18)
              {
                bd.opt.test.scripts = .

                errprintf("{error:Scripts available only up to 18 levels}\n")
              }
              else
              {
                bd.opt.test.letters = bd.opt.test.letters[1..len]
              }
            }
          }
        }
    }

  /* function : initStatInfo() */

    void function initStatInfo(struct bradmean scalar bd)
    {
      `Tokens'  statlist, tokens, subtokens
      `String'  word
      `Pos'     pos
      `RealVec' sel
      `Integer' len
      `Integer' rc, i

      /* Getting Stat List */

        st_local("statlist", "obs nyes mean se sd var lci uci min max p25 p50 p75")
        st_local("stats", subinword(subinword(strlower(st_local("stats")), "ci", "lci uci"), "all", st_local("statlist")))

        rc = _stata("local stats : list stats & statlist")

        if((len = length(bd.si.name = tokens(st_local("stats")))) == 0)
        {
          len = length(bd.si.name = bd.opt.display.wide ? "mean" : ("obs", "nyes", "mean", "se", "lci", "uci"))
        }

      /* Filling Names & Labels */

        bd.si.label = bd.si.name

        statlist = ("obs", "nyes"  , "mean", "se"     , "sd"     , "var"     , "lci"     , "uci"     , "min", "max", "p25"        , "p50"        , "p75"        ) \
                   ("Obs", "n(Yes)", "Mean", "Std Err", "Std Dev", "Variance", "Lower CI", "Upper CI", "Min", "Max", "25th Pctile", "50th Pctile", "75th Pctile")

        for(i=len; i; i--) bd.si.label[i] = statlist[2, selectindex(bd.si.name[i] :== statlist[1,.])]

      /* Setting Default Formats */

        /* Rounding */
        bd.si.roundc = bd.si.roundi = J(1, len, 7)

        /* Notation */
        bd.si.comma    = J(1, len, 1)
        bd.si.percent  = J(1, len, 0)
        bd.si.symbol   = J(1, len, 1)
        bd.si.notation = J(2, len, "")

        /* P-Values */
        bd.si.stars = bd.si.scripts = J(1, len, 0)

        /* CI Specific */
        bd.si.ci_level      = strtoreal(st_global("S_level"))
        bd.si.ci_proportion = 0
        bd.si.ci_combined   = 0
        bd.si.ci_separator  = ","

      /* Parsing Format */

        if((input_string = strlower(st_local("format"))) != "")
        {
          /* Subbing in 'N' for 'NO' */

            input_string = subinstr(subinstr(subinstr(subinstr(input_string,"lvl","level"),"nop","np"),"nosym","nsym"),"nocomma","ncomma")

          /* Setting Options - Overall */

            rc = _stata("bd_format, " + input_string)

            if((word = st_global("s(round)"))         != "") bd.si.roundc = bd.si.roundi = J(1, len, strtoreal(word))
            if((word = st_global("s(roundc)"))        != "") bd.si.roundc                = J(1, len, strtoreal(word))
            if((word = st_global("s(roundi)"))        != "") bd.si.roundi                = J(1, len, strtoreal(word))
            if((word = st_global("s(percent)"))       != "") bd.si.percent               = J(1, len, strtoreal(word))
            if((word = st_global("s(symbol)"))        != "") bd.si.symbol                = J(1, len, strtoreal(word))
            if((word = st_global("s(comma)"))         != "") bd.si.comma                 = J(1, len, strtoreal(word))
            if((word = st_global("s(notation)"))      != "") bd.si.notation              = J(1, len, tokens(word)')
            if((word = st_global("s(ci_proportion)")) != "") bd.si.ci_proportion         = strtoreal(word)
            if((word = st_global("s(ci_combined)"))   != "") bd.si.ci_combined           = strtoreal(word)
            if((word = st_global("s(ci_separator)"))  != "") bd.si.ci_separator          = word
            if((word = st_global("s(ci_level)"))      != "") bd.si.ci_level              = strtoreal(word)

          /* Setting Options - Specific Variables */

            statlist  = statlist[1,.], ("ci", "count", "error", "minmax")
            tokens    = tokenbind(input_string)
            subtokens = insidepar(tokens, "", "(")
            pos       = selectindex(inlist(subtokens, statlist))
            len       = length(subtokens = subtokens[pos])

            if(len > 0) tokens = insidepar(tokens[pos], "(", ")")

            for(i=len; i; i--)
            {
              sel = length(pos = selectindex(bd.si.name :== subtokens[i]))
              if(sel == 0)
              {
                if(subtokens[i] == "ci")     sel = length(pos = selectindex(inlist(bd.si.name, ("lci", "uci"))))
                if(subtokens[i] == "count")  sel = length(pos = selectindex(inlist(bd.si.name, ("obs", "nyes"))))
                if(subtokens[i] == "error")  sel = length(pos = selectindex(inlist(bd.si.name, ("se", "sd", "var"))))
                if(subtokens[i] == "minmax") sel = length(pos = selectindex(inlist(bd.si.name, ("min", "max"))))
              }

              rc = _stata("bd_format, " + tokens[i])

              if((word = st_global("s(round)"))    != "") bd.si.roundc[pos] = bd.si.roundi[pos] = J(1, sel, strtoreal(word))
              if((word = st_global("s(roundc)"))   != "") bd.si.roundc[pos]                     = J(1, sel, strtoreal(word))
              if((word = st_global("s(roundi)"))   != "") bd.si.roundi[pos]                     = J(1, sel, strtoreal(word))
              if((word = st_global("s(percent)"))  != "") bd.si.percent[pos]                    = J(1, sel, strtoreal(word))
              if((word = st_global("s(symbol)"))   != "") bd.si.symbol[pos]                     = J(1, sel, strtoreal(word))
              if((word = st_global("s(comma)"))    != "") bd.si.comma[pos]                      = J(1, sel, strtoreal(word))
              if((word = st_global("s(stars)"))    != "") bd.si.stars[pos]                      = J(1, sel, strtoreal(word))
              if((word = st_global("s(scripts)"))  != "") bd.si.scripts[pos]                    = J(1, sel, strtoreal(word))
              if((word = st_global("s(notation)")) != "") bd.si.notation[.,pos]                 = J(1, sel, tokens(word)')

              if(anylist(subtokens[i], ("lci", "uci", "ci")))
              {
                if((word = st_global("s(ci_proportion)")) != "") bd.si.ci_proportion = strtoreal(word)
                if((word = st_global("s(ci_combined)"))   != "") bd.si.ci_combined   = strtoreal(word)
                if((word = st_global("s(ci_separator)"))  != "") bd.si.ci_separator  = word
                if((word = st_global("s(ci_level)"))      != "") bd.si.ci_level      = strtoreal(word)
              }
            }
        }

      /* Cleaning Format */

        /* 'count' Stats */

          if((len = length(pos = selectindex(inlist(bd.si.name, ("obs", "nyes"))))) > 0)
          {
            bd.si.roundc[pos] = bd.si.roundi[pos] = bd.si.percent[pos] = bd.si.symbol[pos] = J(1, len, 0)
          }

        /* Stars & Scripts */

          if(length(bd.opt.test.stars) > 0 & length(bd.oi.levels) > 1 & !anyof(bd.si.stars, 1)   & length(pos = selectindex(bd.si.name :== "mean")) > 0) bd.si.stars[pos]   = 1
          if(bd.opt.test.scripts != .      & length(bd.oi.levels) > 2 & !anyof(bd.si.scripts, 1) & length(pos = selectindex(bd.si.name :== "uci"))  > 0) bd.si.scripts[pos] = 1

        /* Combined CI */

          if(bd.si.ci_combined & anyof(bd.si.name, "uci"))
          {
            bd.si.name     = bd.si.name[pos = selectindex(bd.si.name :!= "lci")]
            bd.si.label    = bd.si.label[pos]
            bd.si.roundc   = bd.si.roundc[pos]
            bd.si.roundi   = bd.si.roundi[pos]
            bd.si.comma    = bd.si.comma[pos]
            bd.si.percent  = bd.si.percent[pos]
            bd.si.symbol   = bd.si.symbol[pos]
            bd.si.notation = bd.si.notation[.,pos]
            bd.si.stars    = bd.si.stars[pos]
            bd.si.scripts  = bd.si.scripts[pos]

            pos = selectindex(bd.si.name :== "uci")
            bd.si.name[pos] = "ci"
            bd.si.label[pos] = "Confidence Interval"
            if(bd.si.notation[1,pos] == "") bd.si.notation[.,pos] = ("[" \ "]")
          }
    }

/*======================================================================*/
/*   Mata Functions - Getting Results - General                         */
/*======================================================================*/

  /* function : getResults() */

    `RealMat' getResults(struct results scalar in_res,
                         `String'              stat)
    {
      if(stat == "mean") return(in_res.mean)
      if(stat == "obs")  return(in_res.obs)
      if(stat == "nyes") return(in_res.nyes)
      if(stat == "sd")   return(in_res.sd)
      if(stat == "se")   return(in_res.se)
      if(stat == "lci")  return(in_res.lci)
      if(stat == "uci")  return(in_res.uci)
      if(stat == "var")  return(in_res.var)
      if(stat == "min")  return(in_res.min)
      if(stat == "max")  return(in_res.max)
      if(stat == "p25")  return(in_res.p25)
      if(stat == "p50")  return(in_res.p50)
      if(stat == "p75")  return(in_res.p75)

      return
    }

  /* function : logitTransform() */

    void function logitTransform(struct bradmean scalar bd,
                                 struct varInfo  scalar vi)
    {
      `RealMat' temp_se

      temp_se = bd.opt.weight.any ? vi.res.se : sqrt((vi.res.mean :* (1 :- vi.res.mean)) :/ vi.res.obs)

      vi.res.lci = invlogit(logit(vi.res.mean) :- invttail(vi.res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (vi.res.mean :* (1 :- vi.res.mean))))
      vi.res.uci = invlogit(logit(vi.res.mean) :+ invttail(vi.res.df, ((100 - bd.si.ci_level) / 200)) :* (temp_se :/ (vi.res.mean :* (1 :- vi.res.mean))))
    }

  /* function : sortResults() */

    void function sortResults(struct bradmean scalar bd,
                              struct varInfo  scalar vi,
                              `Boolean'              over)
    {
      `Pos' sort_order

      sort_order = order(getResults(vi.res, bd.opt.display.sort_statistic)', (bd.opt.display.sort_direction == "+") ? 1 : -1)'

      if(over == 0)
      {
        if(vi.type != "xi") vi.varlist = vi.varlist[sort_order]
        else                vi.levels  = vi.levels[sort_order]

        vi.answers  = vi.answers[sort_order]
        vi.res.obs  = vi.res.obs[sort_order]
        vi.res.nyes = vi.res.nyes[sort_order]
        vi.res.mean = vi.res.mean[sort_order]
        vi.res.lci  = vi.res.lci[sort_order]
        vi.res.uci  = vi.res.uci[sort_order]
        vi.res.se   = vi.res.se[sort_order]
        vi.res.sd   = vi.res.sd[sort_order]
        vi.res.var  = vi.res.var[sort_order]
        vi.res.min  = vi.res.min[sort_order]
        vi.res.max  = vi.res.max[sort_order]
        vi.res.p25  = vi.res.p25[sort_order]
        vi.res.p50  = vi.res.p50[sort_order]
        vi.res.p75  = vi.res.p75[sort_order]
        vi.res.t    = vi.res.t[sort_order]
        vi.res.df   = vi.res.df[sort_order]
      }
      else
      {
        if(vi.type != "xi") vi.varlist = vi.varlist[.,sort_order]
        else                vi.levels  = vi.levels[.,sort_order]

        vi.answers           = vi.answers[.,sort_order]
        vi.res.obs           = vi.res.obs[.,sort_order]
        vi.res.nyes          = vi.res.nyes[.,sort_order]
        vi.res.mean          = vi.res.mean[.,sort_order]
        vi.res.lci           = vi.res.lci[.,sort_order]
        vi.res.uci           = vi.res.uci[.,sort_order]
        vi.res.se            = vi.res.se[.,sort_order]
        vi.res.sd            = vi.res.sd[.,sort_order]
        vi.res.var           = vi.res.var[.,sort_order]
        vi.res.min           = vi.res.min[.,sort_order]
        vi.res.max           = vi.res.max[.,sort_order]
        vi.res.p25           = vi.res.p25[.,sort_order]
        vi.res.p50           = vi.res.p50[.,sort_order]
        vi.res.p75           = vi.res.p75[.,sort_order]
        vi.res.t             = vi.res.t[.,sort_order]
        vi.res.df            = vi.res.df[.,sort_order]
        vi.res.ovr_statistic = vi.res.ovr_statistic[.,sort_order]
        vi.res.ovr_pvalue    = vi.res.ovr_pvalue[.,sort_order]
        vi.res.ind_statistic = vi.res.ind_statistic[.,sort_order]
        vi.res.ind_pvalue    = vi.res.ind_pvalue[.,sort_order]
      }
    }

  /* function : gatherResults() */

    void function gatherResults(struct bradmean scalar bd)
    {
      `Integer' len, groups
      `Integer' i

      len    = length(bd.vi)
      groups = length(bd.oi.levels)

      /* Version 15.1 */

        if(c("version") == 15.1)
        {
          if(groups == 0)
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesNoOver15(bd, bd.vi[i])
              else                      calculateXiNoOver15(bd, bd.vi[i])
            }
          }
          else if(!bd.opt.over.row)
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesOverCol15(bd, bd.vi[i])
              else                      calculateXiOverCol15(bd, bd.vi[i])
            }
          }
          else
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesOverRow15(bd, bd.vi[i])
              else                      calculateXiOverRow15(bd, bd.vi[i])
            }
          }
        }

      /* Version 16.0+ */

        if(c("version") >= 16)
        {
          if(groups == 0)
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesNoOver16(bd, bd.vi[i])
              else                      calculateXiNoOver16(bd, bd.vi[i])
            }
          }
          else if(!bd.opt.over.row)
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesOverCol16(bd, bd.vi[i])
              else                      calculateXiOverCol16(bd, bd.vi[i])
            }
          }
          else
          {
            for(i=len; i; i--)
            {
              if(bd.vi[i].type != "xi") calculateSeriesOverRow16(bd, bd.vi[i])
              else                      calculateXiOverRow16(bd, bd.vi[i])
            }
          }
        }
    }

/*======================================================================*/
/*   Mata Functions - Calculating Results - Version 15.1                */
/*======================================================================*/

  /* function : calculateSeriesNoOver15() */

    void function calculateSeriesNoOver15(struct bradmean scalar bd,
                                          struct varInfo  scalar vi)
    {
      `Integer' vars
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_count
      `RealMat' mat_results
      `Integer' rc, i

      /* Getting Information */

        vars = length(vi.answers)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(1, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Mean */

            rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2], 1)

            if(rc != 0) continue

            mat_results = st_matrix("r(table)")

            vi.res.mean[i] = mat_results[1]
            vi.res.se[i]   = mat_results[2]
            vi.res.t[i]    = mat_results[3]
            vi.res.lci[i]  = mat_results[5]
            vi.res.uci[i]  = mat_results[6]
            vi.res.df[i]   = mat_results[7]
            vi.res.obs[i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

          /* SD & Var */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[i]  = st_matrix("r(sd)")
              vi.res.var[i] = st_matrix("r(variance)")
            }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2], 1))
            mat_results = st_matrix("r(StatTotal)")

            if(vi.binary) vi.res.nyes = mat_results[1,.]
            vi.res.min = mat_results[2,.]
            vi.res.max = mat_results[3,.]
            vi.res.p25 = mat_results[4,.]
            vi.res.p50 = mat_results[5,.]
            vi.res.p75 = mat_results[6,.]
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 0)
        }
    }

  /* function : calculateXiNoOver15() */

    void function calculateXiNoOver15(struct bradmean scalar bd,
                                      struct varInfo  scalar vi)
    {
      `Integer' vars
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_count, varlist
      `RealMat' mat_results
      `Integer' rc, i

      /* Getting Information */

        vars = length(vi.answers)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(1, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "xi, noomit: svy: mean "
          else                           cmd_mean = "xi, noomit: mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

        /* Count */

          cmd_count = ("xi, noomit: tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save")

      /* Calculating Results */

        /* Mean */

          rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2], 1)

          if(rc != 0) return

          varlist     = tokens(st_global("e(varlist)"))
          mat_results = st_matrix("r(table)")

          vi.res.mean = mat_results[1,.]
          vi.res.se   = mat_results[2,.]
          vi.res.t    = mat_results[3,.]
          vi.res.lci  = mat_results[5,.]
          vi.res.uci  = mat_results[6,.]
          vi.res.df   = mat_results[7,.]
          vi.res.obs  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

        /* SD & Var */

          if(dosd)
          {
            checkerr(rc = _stata("estat sd", 1))

            vi.res.sd  = st_matrix("r(sd)")
            vi.res.var = st_matrix("r(variance)")
          }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + vi.term + cmd_count[2], 1))
            mat_results = st_matrix("r(StatTotal)")

            vi.res.nyes = mat_results[1,.]
            vi.res.min  = mat_results[2,.]
            vi.res.max  = mat_results[3,.]
            vi.res.p25  = mat_results[4,.]
            vi.res.p50  = mat_results[5,.]
            vi.res.p75  = mat_results[6,.]
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 0)
        }

      rc = _stata("drop " + invtokens(varlist))
    }

  /* function : calculateSeriesOverCol15() */

    void function calculateSeriesOverCol15(struct bradmean scalar bd,
                                           struct varInfo  scalar vi)
    {
      `Integer' vars, lvls, groups, len
      `Boolean' dosd, dotab, doovr, doind
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, term
      `RealMat' mat_results, tmp_stats, tmp_pvals
      `RealVec' over_num
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)
        vi.res.ind_statistic = vi.res.ind_pvalue = J(lvls * lvls, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce), (" over(" + bd.oi.name + ", nolabel)")

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Groups */

            /* Mean */

              rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2] + cmd_mean[3], 1)

              if(rc != 0) continue

              mat_results = st_matrix("r(table)")'

              over_num = strtoreal(tokens(st_global("e(over_namelist)")))

              vi.res.mean[over_num,i] = mat_results[.,1]
              vi.res.se[over_num,i]   = mat_results[.,2]
              vi.res.t[over_num,i]    = mat_results[.,3]
              vi.res.lci[over_num,i]  = mat_results[.,5]
              vi.res.uci[over_num,i]  = mat_results[.,6]
              vi.res.df[over_num,i]   = mat_results[.,7]
              vi.res.obs[over_num,i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)")' : st_matrix("e(_N_subp)")'

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[over_num,i]  = st_matrix("r(sd)")'
                vi.res.var[over_num,i] = st_matrix("r(variance)")'
              }

            /* Testing (Individual) */

              doind = bd.opt.test.individual

              if((len = length(over_num)) > 1 & doind)
              {
                tmp_stats = tmp_pvals = J(lvls, lvls, .)

                term = ("[" :+ vi.varlist[i] :+ "]") :+ strofreal(over_num)

                /* T-Test */

                  if(bd.opt.test.t_individual)
                  {
                    for(j=len; j; j--)
                    {
                      for(k=j-1; k; k--)
                      {
                        checkerr(rc = _stata("lincom " + term[j] + " - " + term[k], 1))

                        tmp_stats[over_num[j],over_num[k]] = tmp_stats[over_num[k],over_num[j]] = st_numscalar("r(t)")
                        tmp_pvals[over_num[j],over_num[k]] = tmp_pvals[over_num[k],over_num[j]] = st_numscalar("r(p)")
                      }
                    }

                    doind = 0
                  }

                /* F-Test */

                  if(bd.opt.test.f_individual & doind)
                  {
                    for(j=len; j; j--)
                    {
                      checkerr(rc = _stata("test " + term[j] + " == " + invtokens(term, " == ") + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                      mat_results = st_matrix("r(mtest)")

                      tmp_stats[over_num[j],over_num] = mat_results[.,1]'
                      tmp_pvals[over_num[j],over_num] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3]' : mat_results[.,4]'
                    }
                  }

                vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
              }

            /* Testing (Overall) */

              doovr = bd.opt.test.overall

              if((len = length(over_num)) > 1 & doovr)
              {
                /* Chi2 */

                  if(bd.opt.test.chi_overall & bd.vi.binary)
                  {
                    checkerr(rc = _stata(cmd_tab2[1] + vi.varlist[i] + cmd_tab2[2], 1))

                    if(bd.opt.weight.survey)
                    {
                      if(st_numscalar("e(r)") > 1)
                      {
                        vi.res.ovr_statistic[i] = st_numscalar("e(F_Pear)")
                        vi.res.ovr_pvalue[i]    = st_numscalar("e(p_Pear)")
                      }
                    }
                    else
                    {
                      if(st_numscalar("r(r)") > 1)
                      {
                        vi.res.ovr_statistic[i] = st_numscalar("r(chi2)")
                        vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                      }
                    }

                    doovr = 0
                  }

                /* T-Test */

                  if(bd.opt.test.t_overall & doovr)
                  {
                    term = "[" :+ vi.varlist[i] :+ "]"

                    checkerr(rc = _stata("lincom " + term :+ strofreal(over_num[1]) :+ " - " :+ term :+ strofreal(over_num[2]), 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(t)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")

                    doovr = 0
                  }

                /* F-Test */

                  if(bd.opt.test.f_overall & doovr)
                  {
                    term = ("[" :+ vi.varlist[i] :+ "]") :+ strofreal(over_num)

                    checkerr(rc = _stata("test " + invtokens(term, " == ") + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(F)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }
              }

          /* Total */

            if(!bd.opt.over.total) continue

            /* Mean */

              rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2], 1)

              if(rc != 0) continue

              mat_results = st_matrix("r(table)")

              vi.res.mean[groups,i] = mat_results[1]
              vi.res.se[groups,i]   = mat_results[2]
              vi.res.t[groups,i]    = mat_results[3]
              vi.res.lci[groups,i]  = mat_results[5]
              vi.res.uci[groups,i]  = mat_results[6]
              vi.res.df[groups,i]   = mat_results[7]
              vi.res.obs[groups,i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[groups,i]  = st_matrix("r(sd)")
                vi.res.var[groups,i] = st_matrix("r(variance)")
              }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2] + cmd_count[3], 1))

            for(j=lvls; j; j--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(j) + ")")

              if(vi.binary) vi.res.nyes[j,.] = mat_results[1,.]
              vi.res.min[j,.] = mat_results[2,.]
              vi.res.max[j,.] = mat_results[3,.]
              vi.res.p25[j,.] = mat_results[4,.]
              vi.res.p50[j,.] = mat_results[5,.]
              vi.res.p75[j,.] = mat_results[6,.]
            }

            if(bd.opt.over.total)
            {
              mat_results = st_matrix("r(StatTotal)")

              if(vi.binary) vi.res.nyes[groups,.] = mat_results[1,.]
              vi.res.min[groups,.] = mat_results[2,.]
              vi.res.max[groups,.] = mat_results[3,.]
              vi.res.p25[groups,.] = mat_results[4,.]
              vi.res.p50[groups,.] = mat_results[5,.]
              vi.res.p75[groups,.] = mat_results[6,.]
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }
    }

  /* function : calculateXiOverCol15() */

    void function calculateXiOverCol15(struct bradmean scalar bd,
                                       struct varInfo  scalar vi)
    {
      `Integer' vars, lvls, groups, len
      `Boolean' dosd, dotab, doovr, doind
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, term, varlist
      `RealMat' mat_results, tmp_stats, tmp_pvals
      `RealVec' over_num
      `Pos'     over_pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)
        vi.res.ind_statistic = vi.res.ind_pvalue = J(lvls * lvls, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "xi, noomit: svy: mean "
          else                           cmd_mean = "xi, noomit: mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce), (" over(" + bd.oi.name + ", nolabel)")

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("xi, noomit: tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        /* Groups */

          /* Mean */

            rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2] + cmd_mean[3], 1)

            if(rc != 0) return

            mat_results = st_matrix("r(table)")

            varlist  = tokens(st_global("e(varlist)"))
            over_num = strtoreal(tokens(st_global("e(over_namelist)")))
            over_pos = selectindex(inlist(bd.oi.levels, over_num))
            len      = length(over_num)

            vi.res.mean[over_pos,.] = colshape(mat_results[1,.], len)'
            vi.res.se[over_pos,.]   = colshape(mat_results[2,.], len)'
            vi.res.t[over_pos,.]    = colshape(mat_results[3,.], len)'
            vi.res.lci[over_pos,.]  = colshape(mat_results[5,.], len)'
            vi.res.uci[over_pos,.]  = colshape(mat_results[6,.], len)'
            vi.res.df[over_pos,.]   = colshape(mat_results[7,.], len)'
            vi.res.obs[over_pos,.]  = (bd.opt.weight.subpop == "") ? colshape(st_matrix("e(_N)"), len)' : colshape(st_matrix("e(_N_subp)"), len)'

          /* SD & Var */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[over_pos,.]  = colshape(st_matrix("r(sd)"), len)'
              vi.res.var[over_pos,.] = colshape(st_matrix("r(variance)"), len)'
            }

          /* Count, Min, Max */

            if(dotab)
            {
              checkerr(rc = _stata(cmd_count[1] + vi.term + cmd_count[2] + cmd_count[3], 1))

              for(i=lvls; i; i--)
              {
                mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

                vi.res.nyes[i,.] = mat_results[1,.]
                vi.res.min[i,.]  = mat_results[2,.]
                vi.res.max[i,.]  = mat_results[3,.]
                vi.res.p25[i,.] = mat_results[4,.]
                vi.res.p50[i,.] = mat_results[5,.]
                vi.res.p75[i,.] = mat_results[6,.]
              }

              if(bd.opt.over.total)
              {
                mat_results = st_matrix("r(StatTotal)")

                vi.res.nyes[groups,.] = mat_results[1,.]
                vi.res.min[groups,.]  = mat_results[2,.]
                vi.res.max[groups,.]  = mat_results[3,.]
                vi.res.p25[groups,.]  = mat_results[4,.]
                vi.res.p50[groups,.]  = mat_results[5,.]
                vi.res.p75[groups,.]  = mat_results[6,.]
              }
            }

          /* Testing (Individual) */

            doind = bd.opt.test.individual

            if(len > 1 & doind)
            {
              tmp_stats = tmp_pvals = J(lvls, lvls, .)

              /* T-Test */

                if(bd.opt.test.t_individual)
                {
                  for(i=vars; i; i--)
                  {
                    tmp_stats = tmp_pvals = J(lvls, lvls, .)
                    term      = "[" :+ varlist[i] :+ "]" :+ strofreal(over_num)

                    for(j=len; j; j--)
                    {
                      for(k=j-1; k; k--)
                      {
                        checkerr(rc = _stata("lincom " + term[j] + " - " + term[k], 1))

                        tmp_stats[over_pos[j],over_pos[k]] = tmp_stats[over_pos[k],over_pos[j]] = st_numscalar("r(t)")
                        tmp_pvals[over_pos[j],over_pos[k]] = tmp_pvals[over_pos[k],over_pos[j]] = st_numscalar("r(p)")
                      }
                    }

                    vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                    vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
                  }

                  doind = 0
                }

              /* F-Test */

                if(bd.opt.test.f_individual & doind)
                {
                  for(i=vars; i; i--)
                  {
                    tmp_stats = tmp_pvals = J(lvls, lvls, .)
                    term      = "[" :+ varlist[i] :+ "]" :+ strofreal(over_num)

                    for(j=len; j; j--)
                    {
                      checkerr(rc = _stata("test " + term[j] + " == " + invtokens(term, " == ") + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                      mat_results = st_matrix("r(mtest)")

                      tmp_stats[over_pos[j],over_pos] = mat_results[.,1]'
                      tmp_pvals[over_pos[j],over_pos] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3]' : mat_results[.,4]'
                    }

                    vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                    vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
                  }
                }
            }

          /* Testing (Overall) */

            doovr = bd.opt.test.overall

            if(len > 1 & doovr)
            {
              /* Chi2 */

                if(bd.opt.test.chi_overall)
                {
                  checkerr(rc = _stata(cmd_tab2[1] + vi.varlist + cmd_tab2[2], 1))

                  if(bd.opt.weight.survey)
                  {
                    if(st_numscalar("e(r)") > 1)
                    {
                      vi.res.ovr_statistic = J(1, vars, st_numscalar("e(F_Pear)"))
                      vi.res.ovr_pvalue    = J(1, vars, st_numscalar("e(p_Pear)"))
                    }
                  }
                  else
                  {
                    if(st_numscalar("r(r)") > 1)
                    {
                      vi.res.ovr_statistic = J(1, vars, st_numscalar("r(chi2)"))
                      vi.res.ovr_pvalue    = J(1, vars, st_numscalar("r(p)"))
                    }
                  }

                  doovr = 0
                }

              /* T-Test */

                if(bd.opt.test.t_overall & doovr)
                {
                  term = "[" :+ varlist' :+ "]"
                  term = "lincom " :+ term :+ strofreal(over_num[1]) :+ " - " :+ term :+ strofreal(over_num[2])

                  for(i=vars; i; i--)
                  {
                    checkerr(rc = _stata(term[i], 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(t)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }

                  doovr = 0
                }

              /* F-Test */

                if(bd.opt.test.f_overall & doovr)
                {
                  for(i=vars; i; i--)
                  {
                    term = "[" :+ varlist[i] :+ "]" :+ strofreal(over_num)
                    term = invtokens(term, " == ")

                    checkerr(rc = _stata("test " + term + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(F)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }
                }
            }

        /* Total */

          if(bd.opt.over.total)
          {
            /* Mean */

              rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2], 1)

              if(rc != 0) return

              mat_results = st_matrix("r(table)")

              vi.res.mean[groups,.] = mat_results[1,.]
              vi.res.se[groups,.]   = mat_results[2,.]
              vi.res.t[groups,.]    = mat_results[3,.]
              vi.res.lci[groups,.]  = mat_results[5,.]
              vi.res.uci[groups,.]  = mat_results[6,.]
              vi.res.df[groups,.]   = mat_results[7,.]
              vi.res.obs[groups,.]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[groups,.]  = st_matrix("r(sd)")
                vi.res.var[groups,.] = st_matrix("r(variance)")
              }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }

      rc = _stata("drop " + invtokens(varlist))
    }

  /* function : calculateSeriesOverRow15() */

    void function calculateSeriesOverRow15(struct bradmean scalar bd,
                                           struct varInfo  scalar vi)
    {
      `Integer' vars, groups
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, term, varlist
      `String'  cmd_tab1, matcell, matrow
      `RealMat' mat_results
      `RealVec' over_num
      `Pos'     pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = length(bd.oi.levels)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)

        if(!vi.binary) return

      /* Defining Commands */

        /* Mean */

          cmd_mean = ("i." + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

          if(bd.opt.weight.subpop != "") cmd_mean = "xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean " + cmd_mean
          else if(bd.opt.weight.survey)  cmd_mean = "xi, noomit: svy: mean " + cmd_mean
          else                           cmd_mean = "xi, noomit: mean " + cmd_mean

        /* Tabulate Oneway */

          matcell  = st_tempname()
          matrow   = st_tempname()
          cmd_tab1 = "tab " + bd.oi.name + " if e(sample), nolabel matcell(" + matcell + ") matrow(" + matrow + ")"

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Mean (1) */

            rc = _stata(cmd_mean + " over(" + vi.varlist[i] + ", nolabel)", 1)

            if(rc != 0) continue

            mat_results = st_matrix("r(table)")'
            over_num    = strtoreal(st_matrixcolstripe("r(table)")[.,2])
            varlist     = tokens(st_global("e(varlist)"))

          /* Observations */

            rc = _stata(cmd_tab1, 1)

            pos               = st_matrix(matrow)
            vi.res.obs[pos,i] = st_matrix(matcell)

          /* Mean (2) */

            if(st_global("e(over_nameslist)") == "0")
            {
              vi.res.mean[.,i] = J(groups, 1, 0)
              vi.res.se[.,i]   = J(groups, 1, 0)
              vi.res.sd[.,i]   = J(groups, 1, 0)
              vi.res.var[.,i]  = J(groups, 1, 0)

              continue
            }

            pos = selectindex(over_num :== 1)

            vi.res.mean[.,i] = mat_results[pos,1]
            vi.res.se[.,i]   = mat_results[pos,2]
            vi.res.t[.,i]    = mat_results[pos,3]
            vi.res.lci[.,i]  = mat_results[pos,5]
            vi.res.uci[.,i]  = mat_results[pos,6]
            vi.res.df[.,i]   = mat_results[pos,7]

          /* Standard Deviation & Variance */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[.,i]  = st_matrix("r(sd)")'[pos]
              vi.res.var[.,i] = st_matrix("r(variance)")'[pos]
            }

          /* Testing */

            if(bd.opt.test.chi_overall)
            {
              checkerr(rc = _stata(cmd_tab2[1] + vi.varlist[i] + cmd_tab2[2], 1))

              if(bd.opt.weight.survey)
              {
                if(st_numscalar("e(r)") > 1)
                {
                  vi.res.ovr_statistic[i] = st_numscalar("e(F_Pear)")
                  vi.res.ovr_pvalue[i]    = st_numscalar("e(p_Pear)")
                }
              }
              else
              {
                if(st_numscalar("r(r)") > 1)
                {
                  vi.res.ovr_statistic[i] = st_numscalar("r(chi2)")
                  vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
              }
            }
            else if(bd.opt.test.f_overall)
            {
              term = "test " + invtokens("[" :+ varlist :+ "]1", " == ") + ", mtest(" + bd.opt.test.f_mtest + ")"

              checkerr(rc = _stata(term, 1))

              vi.res.ovr_statistic[i] = st_numscalar("r(F)")
              vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
            }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2] + cmd_count[3], 1))

            for(i=groups; i; i--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

              vi.res.nyes[i,.] = mat_results[1,.]
              vi.res.min[i,.]  = mat_results[2,.]
              vi.res.max[i,.]  = mat_results[3,.]
              vi.res.p25[i,.]  = mat_results[4,.]
              vi.res.p50[i,.]  = mat_results[5,.]
              vi.res.p75[i,.]  = mat_results[6,.]
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }

      rc = _stata("drop " + invtokens(varlist))
    }

  /* function : calculateXiOverRow15() */

    void function calculateXiOverRow15(struct bradmean scalar bd,
                                       struct varInfo  scalar vi)
    {
      `Integer' vars, groups, len
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, term, varlist
      `String'  cmd_tab1, matcell, matrow
      `RealMat' mat_results
      `DataVec' over_names, over_num
      `Pos'     over_pos, pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = length(bd.oi.levels)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "xi, noomit: svy: mean "
          else                           cmd_mean = "xi, noomit: mean "

          cmd_mean = cmd_mean + "i." + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") over(" + vi.term + ") " + bd.opt.weight.vce

        /* Tabulate Oneway */

          matcell  = st_tempname()
          matrow   = st_tempname()
          cmd_tab1 = "tab " + bd.oi.name + " if e(sample), nolabel matcell(" + matcell + ") matrow(" + matrow + ")"

        /* Tabulate Twoway */

          cmd_tab2 = "tab " + vi.varlist + " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = "svy, subpop(" + bd.opt.weight.subpop + "): " + cmd_tab2 + ", pearson"
          else if(bd.opt.weight.survey)  cmd_tab2 = "svy: "                                       + cmd_tab2 + ", pearson"
          else                           cmd_tab2 =                                                 cmd_tab2 + ", chi2"

        /* Count */

          cmd_count = "xi, noomit: tabstat " + vi.term + " if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save by(" + bd.oi.name + ")"

      /* Calculating Results */

        /* Mean */

          rc = _stata(cmd_mean, 1)

          if(rc != 0) return

          mat_results = st_matrix("r(table)")
          varlist     = tokens(st_global("e(varlist)"))
          over_names  = tokens(st_global("e(over)"))
          over_num    = tokens(st_global("e(over_labels)"))
          len         = length(over_num)
          over_pos    = J(1, len, .)

          for(i=len; i; i--) over_pos[i] = selectindex(strtoreal(tokens(over_num[i])))

          over_num = tokens(st_global("e(over_namelist)"))

          vi.res.mean[.,over_pos] = colshape(mat_results[1,.], len)
          vi.res.se[.,over_pos]   = colshape(mat_results[2,.], len)
          vi.res.t[.,over_pos]    = colshape(mat_results[3,.], len)
          vi.res.lci[.,over_pos]  = colshape(mat_results[5,.], len)
          vi.res.uci[.,over_pos]  = colshape(mat_results[6,.], len)
          vi.res.df[.,over_pos]   = colshape(mat_results[7,.], len)

        /* Observations */

          rc = _stata(cmd_tab1, 1)

          pos               = st_matrix(matrow)
          vi.res.obs[pos,.] = J(1, vars, st_matrix(matcell))

        /* Standard Deviation & Variance */

          if(dosd)
          {
            checkerr(rc = _stata("estat sd", 1))

            vi.res.sd[.,over_pos]  = colshape(st_matrix("r(sd)"), len)
            vi.res.var[.,over_pos] = colshape(st_matrix("r(variance)"), len)
          }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count, 1))

            for(i=groups; i; i--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

              vi.res.nyes[i,.] = mat_results[1,.]
              vi.res.min[i,.]  = mat_results[2,.]
              vi.res.max[i,.]  = mat_results[3,.]
              vi.res.p25[i,.]  = mat_results[4,.]
              vi.res.p50[i,.]  = mat_results[5,.]
              vi.res.p75[i,.]  = mat_results[6,.]
            }
          }

        /* Testing */

          if(bd.opt.test.chi_overall)
          {
            checkerr(rc = _stata(cmd_tab2, 1))

            if(bd.opt.weight.survey)
            {
              if(st_numscalar("e(r)") > 1)
              {
                vi.res.ovr_statistic[.,over_pos] = J(1, len, st_numscalar("e(F_Pear)"))
                vi.res.ovr_pvalue[.,over_pos]    = J(1, len, st_numscalar("e(p_Pear)"))
              }
            }
            else
            {
              if(st_numscalar("r(r)") > 1)
              {
                vi.res.ovr_statistic[.,over_pos] = J(1, len, st_numscalar("r(chi2)"))
                vi.res.ovr_pvalue[.,over_pos]    = J(1, len, st_numscalar("r(p)"))
              }
            }
          }
          else if(bd.opt.test.f_overall)
          {
            for(i=len; i; i--)
            {
              term = "test " + invtokens("[" :+ varlist :+ "]" :+ over_num[i], " == ") + ", mtest(" + bd.opt.test.f_mtest + ")"

              checkerr(rc = _stata(term, 1))

              vi.res.ovr_statistic[over_pos[i]] = st_numscalar("r(F)")
              vi.res.ovr_pvalue[over_pos[i]]    = st_numscalar("r(p)")
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }

      rc = _stata("drop " + invtokens(over_names), 1)
    }

/*======================================================================*/
/*   Mata Functions - Calculating Results - Version 16.0+               */
/*======================================================================*/

  /* function : calculateSeriesNoOver16() */

    void function calculateSeriesNoOver16(struct bradmean scalar bd,
                                          struct varInfo  scalar vi)
    {
      `Integer' vars
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_count
      `RealMat' mat_results
      `Integer' rc, i

      /* Getting Information */

        vars = length(vi.answers)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(1, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Mean */

            rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2], 1)

            if(rc != 0) continue

            mat_results = st_matrix("r(table)")

            vi.res.mean[i] = mat_results[1]
            vi.res.se[i]   = mat_results[2]
            vi.res.t[i]    = mat_results[3]
            vi.res.lci[i]  = mat_results[5]
            vi.res.uci[i]  = mat_results[6]
            vi.res.df[i]   = mat_results[7]
            vi.res.obs[i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

          /* SD & Var */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[i]  = st_matrix("r(sd)")
              vi.res.var[i] = st_matrix("r(variance)")
            }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2], 1))
            mat_results = st_matrix("r(StatTotal)")

            if(vi.binary) vi.res.nyes = mat_results[1,.]
            vi.res.min = mat_results[2,.]
            vi.res.max = mat_results[3,.]
            vi.res.p25 = mat_results[4,.]
            vi.res.p50 = mat_results[5,.]
            vi.res.p75 = mat_results[6,.]
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 0)
        }
    }

  /* function : calculateXiNoOver16() */

    void function calculateXiNoOver16(struct bradmean scalar bd,
                                      struct varInfo  scalar vi)
    {
      `Integer' vars
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_count
      `RealMat' mat_results
      `RealVec' pos
      `Integer' rc, i

      /* Getting Information */

        vars = length(vi.answers)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = vi.res.mean = vi.res.se = vi.res.sd = vi.res.var = J(1, vars, 0)
        vi.res.nyes = vi.res.lci = vi.res.uci = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

        /* Count */

          cmd_count = ("xi, noomit: tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save")

      /* Calculating Results */

        /* Mean */

          rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2], 1)

          if(rc != 0) return

          mat_results = st_matrix("r(table)")
          pos         = strtoreal(subinstr(subinstr(insidepar(tokens(st_global("e(varlist)")), "", "."), "bn", ""), "o", ""))
          pos         = poslist(vi.levels, pos)

          vi.res.mean[pos] = mat_results[1,.]
          vi.res.se[pos]   = mat_results[2,.]
          vi.res.t[pos]    = mat_results[3,.]
          vi.res.lci[pos]  = mat_results[5,.]
          vi.res.uci[pos]  = mat_results[6,.]
          vi.res.df[pos]   = mat_results[7,.]
          vi.res.obs       = (bd.opt.weight.subpop == "") ? J(1, vars, st_matrix("e(_N)")[1]) : J(1, vars, st_matrix("e(_N_subp)")[1])

        /* SD & Var */

          if(dosd)
          {
            checkerr(rc = _stata("estat sd", 1))

            vi.res.sd[pos]  = st_matrix("r(sd)")
            vi.res.var[pos] = st_matrix("r(variance)")
          }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + vi.term + cmd_count[2], 1))
            mat_results = st_matrix("r(StatTotal)")

            vi.res.nyes = mat_results[1,.]
            vi.res.min  = mat_results[2,.]
            vi.res.max  = mat_results[3,.]
            vi.res.p25  = mat_results[4,.]
            vi.res.p50  = mat_results[5,.]
            vi.res.p75  = mat_results[6,.]

            rc = _stata("drop `_dta[__xi__Vars__To__Drop__]'", 1)
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 0)
        }
    }

  /* function : calculateSeriesOverCol16() */

    void function calculateSeriesOverCol16(struct bradmean scalar bd,
                                           struct varInfo  scalar vi)
    {
      `Integer' vars, lvls, groups, len
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, over_names, term
      `RealMat' mat_results, tmp_stats, tmp_pvals
      `RealVec' over_num
      `Integer' rc, i, j, k

      /* Getting Information */

        vars   = length(vi.answers)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)
        vi.res.ind_statistic = vi.res.ind_pvalue = J(lvls * lvls, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce), (" over(" + bd.oi.name + ", nolabel)")

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Groups */

            /* Mean */

              rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2] + cmd_mean[3], 1)

              if(rc != 0) continue

              mat_results = st_matrix("r(table)")'
              over_names  = st_matrixcolstripe("r(table)")[.,2]
              over_num    = strtoreal(subinstr(subinstr(insidepar(over_names, "@", "."), "bn", ""), "o", ""))

              vi.res.mean[over_num,i] = mat_results[.,1]
              vi.res.se[over_num,i]   = mat_results[.,2]
              vi.res.t[over_num,i]    = mat_results[.,3]
              vi.res.lci[over_num,i]  = mat_results[.,5]
              vi.res.uci[over_num,i]  = mat_results[.,6]
              vi.res.df[over_num,i]   = mat_results[.,7]
              vi.res.obs[over_num,i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)")' : st_matrix("e(_N_subp)")'

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[over_num,i]  = st_matrix("r(sd)")'
                vi.res.var[over_num,i] = st_matrix("r(variance)")'
              }

            /* Testing (Individual) */

              doind = bd.opt.test.individual

              if((len = length(over_num)) > 1 & doind)
              {
                tmp_stats = tmp_pvals = J(lvls, lvls, .)

                /* T-Test */

                  if(bd.opt.test.t_individual)
                  {
                    for(j=len; j; j--)
                    {
                      for(k=j-1; k; k--)
                      {
                        checkerr(rc = _stata("lincom " + over_names[j] + " - " + over_names[k], 1))

                        tmp_stats[over_num[j],over_num[k]] = tmp_stats[over_num[k],over_num[j]] = st_numscalar("r(t)")
                        tmp_pvals[over_num[j],over_num[k]] = tmp_pvals[over_num[k],over_num[j]] = st_numscalar("r(p)")
                      }
                    }

                    doind = 0
                  }

                /* F-Test */

                  if(bd.opt.test.f_individual & doind)
                  {
                    term = invtokens(over_names', " == ")

                    for(j=len; j; j--)
                    {
                      checkerr(rc = _stata("test " + over_names[j] + " == " + term + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                      mat_results = st_matrix("r(mtest)")

                      tmp_stats[over_num[j],over_num] = mat_results[.,1]'
                      tmp_pvals[over_num[j],over_num] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3]' : mat_results[.,4]'
                    }
                  }

                vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
              }

            /* Testing (Overall) */

              doovr = bd.opt.test.overall

              if((len = length(over_num)) > 1 & doovr)
              {
                /* Chi2 */

                  if(bd.opt.test.chi_overall & bd.vi.binary)
                  {
                    checkerr(rc = _stata(cmd_tab2[1] + vi.varlist[i] + cmd_tab2[2], 1))

                    if(bd.opt.weight.survey)
                    {
                      if(st_numscalar("e(r)") > 1)
                      {
                        vi.res.ovr_statistic[i] = st_numscalar("e(F_Pear)")
                        vi.res.ovr_pvalue[i]    = st_numscalar("e(p_Pear)")
                      }
                    }
                    else
                    {
                      if(st_numscalar("r(r)") > 1)
                      {
                        vi.res.ovr_statistic[i] = st_numscalar("r(chi2)")
                        vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                      }
                    }

                    doovr = 0
                  }

                /* T-Test */

                  if(bd.opt.test.t_overall & doovr)
                  {
                    checkerr(rc = _stata("lincom " + over_names[1] + " - " + over_names[2], 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(t)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")

                    doovr = 0
                  }

                /* F-Test */

                  if(bd.opt.test.f_overall & doovr)
                  {
                    checkerr(rc = _stata("test " + invtokens(over_names', " == ") + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(F)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }
              }

          /* Total */

            if(!bd.opt.over.total) continue

            /* Mean */

              rc = _stata(cmd_mean[1] + vi.varlist[i] + cmd_mean[2], 1)

              if(rc != 0) continue

              mat_results = st_matrix("r(table)")

              vi.res.mean[groups,i] = mat_results[1]
              vi.res.se[groups,i]   = mat_results[2]
              vi.res.t[groups,i]    = mat_results[3]
              vi.res.lci[groups,i]  = mat_results[5]
              vi.res.uci[groups,i]  = mat_results[6]
              vi.res.df[groups,i]   = mat_results[7]
              vi.res.obs[groups,i]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[groups,i]  = st_matrix("r(sd)")
                vi.res.var[groups,i] = st_matrix("r(variance)")
              }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2] + cmd_count[3], 1))

            for(j=lvls; j; j--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(j) + ")")

              if(vi.binary) vi.res.nyes[j,.] = mat_results[1,.]
              vi.res.min[j,.] = mat_results[2,.]
              vi.res.max[j,.] = mat_results[3,.]
              vi.res.p25[j,.] = mat_results[4,.]
              vi.res.p50[j,.] = mat_results[5,.]
              vi.res.p75[j,.] = mat_results[6,.]
            }

            if(bd.opt.over.total)
            {
              mat_results = st_matrix("r(StatTotal)")

              if(vi.binary) vi.res.nyes[groups,.] = mat_results[1,.]
              vi.res.min[groups,.] = mat_results[2,.]
              vi.res.max[groups,.] = mat_results[3,.]
              vi.res.p25[groups,.] = mat_results[4,.]
              vi.res.p50[groups,.] = mat_results[5,.]
              vi.res.p75[groups,.] = mat_results[6,.]
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }
    }

  /* function : calculateXiOverCol16() */

    void function calculateXiOverCol16(struct bradmean scalar bd,
                                       struct varInfo  scalar vi)
    {
      `Integer' vars, lvls, groups, len
      `Boolean' dosd, dotab, doovr, doind
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, over_names, term, varlist
      `RealMat' mat_results, tmp_stats, tmp_pvals
      `RealVec' over_num
      `Pos'     over_pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)
        vi.res.ind_statistic = vi.res.ind_pvalue = J(lvls * lvls, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "xi, noomit: svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "xi, noomit: svy: mean "
          else                           cmd_mean = "xi, noomit: mean "

          cmd_mean = cmd_mean, (" if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce), (" over(" + bd.oi.name + ", nolabel)")

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("xi, noomit: tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        /* Groups */

          /* Mean */

            rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2] + cmd_mean[3], 1)

            if(rc != 0) return

            varlist     = tokens(st_global("e(varlist)"))
            mat_results = st_matrix("r(table)")
            over_names  = st_matrixcolstripe("r(table)")[.,2]
            over_num    = strtoreal(subinstr(subinstr(insidepar(over_names, "@", "."), "bn", ""), "o", ""))
            over_pos    = selectindex(inlist(bd.oi.levels, uniqrows(over_num)))
            len         = length(over_pos)
            over_num    = rangex(1, len, 1)'

            vi.res.mean[over_pos,.] = colshape(mat_results[1,.], len)'
            vi.res.se[over_pos,.]   = colshape(mat_results[2,.], len)'
            vi.res.t[over_pos,.]    = colshape(mat_results[3,.], len)'
            vi.res.lci[over_pos,.]  = colshape(mat_results[5,.], len)'
            vi.res.uci[over_pos,.]  = colshape(mat_results[6,.], len)'
            vi.res.df[over_pos,.]   = colshape(mat_results[7,.], len)'
            vi.res.obs[over_pos,.]  = (bd.opt.weight.subpop == "") ? colshape(st_matrix("e(_N)"), len)' : colshape(st_matrix("e(_N_subp)"), len)'

          /* SD & Var */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[over_pos,.]  = colshape(st_matrix("r(sd)"), len)'
              vi.res.var[over_pos,.] = colshape(st_matrix("r(variance)"), len)'
            }

          /* Count, Min, Max */

            if(dotab)
            {
              checkerr(rc = _stata(cmd_count[1] + vi.term + cmd_count[2] + cmd_count[3], 1))

              for(i=lvls; i; i--)
              {
                mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

                vi.res.nyes[i,.] = mat_results[1,.]
                vi.res.min[i,.]  = mat_results[2,.]
                vi.res.max[i,.]  = mat_results[3,.]
                vi.res.p25[i,.]  = mat_results[4,.]
                vi.res.p50[i,.]  = mat_results[5,.]
                vi.res.p75[i,.]  = mat_results[6,.]
              }

              if(bd.opt.over.total)
              {
                mat_results = st_matrix("r(StatTotal)")

                vi.res.nyes[groups,.] = mat_results[1,.]
                vi.res.min[groups,.]  = mat_results[2,.]
                vi.res.max[groups,.]  = mat_results[3,.]
                vi.res.p25[groups,.]  = mat_results[4,.]
                vi.res.p50[groups,.]  = mat_results[5,.]
                vi.res.p75[groups,.]  = mat_results[6,.]
              }

              rc = _stata("drop `_dta[__xi__Vars__To__Drop__]'", 1)
            }

          /* Testing (Individual) */

            doind = bd.opt.test.individual

            if(len > 1 & doind)
            {
              tmp_stats = tmp_pvals = J(lvls, lvls, .)

              /* T-Test */

                if(bd.opt.test.t_individual)
                {
                  for(i=vars; i; i--)
                  {
                    tmp_stats = tmp_pvals = J(lvls, lvls, .)
                    term      = over_names[(over_num :+ ((i-1) :* len))]

                    for(j=len; j; j--)
                    {
                      for(k=j-1; k; k--)
                      {
                        checkerr(rc = _stata("lincom " + term[j] + " - " + term[k], 1))

                        tmp_stats[over_pos[j],over_pos[k]] = tmp_stats[over_pos[k],over_pos[j]] = st_numscalar("r(t)")
                        tmp_pvals[over_pos[j],over_pos[k]] = tmp_pvals[over_pos[k],over_pos[j]] = st_numscalar("r(p)")
                      }
                    }

                    vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                    vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
                  }

                  doind = 0
                }

              /* F-Test */

                if(bd.opt.test.f_individual & doind)
                {
                  for(i=vars; i; i--)
                  {
                    tmp_stats = tmp_pvals = J(lvls, lvls, .)
                    term      = over_names[(over_num :+ ((i-1) :* len))]

                    for(j=len; j; j--)
                    {
                      checkerr(rc = _stata("test " + term[j] + " == " + invtokens(term', " == ") + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                      mat_results = st_matrix("r(mtest)")

                      tmp_stats[over_pos[j],over_pos] = mat_results[.,1]'
                      tmp_pvals[over_pos[j],over_pos] = (bd.opt.test.f_mtest == "noadjust") ? mat_results[.,3]' : mat_results[.,4]'
                    }

                    vi.res.ind_statistic[.,i] = colshape(tmp_stats, 1)
                    vi.res.ind_pvalue[.,i]    = colshape(tmp_pvals, 1)
                  }
                }
            }

          /* Testing (Overall) */

            doovr = bd.opt.test.overall

            if(len > 1 & doovr)
            {
              /* Chi2 */

                if(bd.opt.test.chi_overall)
                {
                  checkerr(rc = _stata(cmd_tab2[1] + vi.varlist + cmd_tab2[2], 1))

                  if(bd.opt.weight.survey)
                  {
                    if(st_numscalar("e(r)") > 1)
                    {
                      vi.res.ovr_statistic = J(1, vars, st_numscalar("e(F_Pear)"))
                      vi.res.ovr_pvalue    = J(1, vars, st_numscalar("e(p_Pear)"))
                    }
                  }
                  else
                  {
                    if(st_numscalar("r(r)") > 1)
                    {
                      vi.res.ovr_statistic = J(1, vars, st_numscalar("r(chi2)"))
                      vi.res.ovr_pvalue    = J(1, vars, st_numscalar("r(p)"))
                    }
                  }

                  doovr = 0
                }

              /* T-Test */

                if(bd.opt.test.t_overall & doovr)
                {
                  term = colshape(over_names, 2)
                  term = "lincom " :+ term[.,1] :+ " - " :+ term[.,2]

                  for(i=vars; i; i--)
                  {
                    checkerr(rc = _stata(term[i], 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(t)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }

                  doovr = 0
                }

              /* F-Test */

                if(bd.opt.test.f_overall & doovr)
                {
                  for(i=vars; i; i--)
                  {
                    term = over_names[(over_num :+ ((i-1) :* len))]
                    term = invtokens(term', " == ")

                    checkerr(rc = _stata("test " + term + ", mtest(" + bd.opt.test.f_mtest + ")", 1))

                    vi.res.ovr_statistic[i] = st_numscalar("r(F)")
                    vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                  }
                }
            }

        /* Total */

          if(bd.opt.over.total)
          {
            /* Mean */

              rc = _stata(cmd_mean[1] + vi.term + cmd_mean[2], 1)

              if(rc != 0) return

              mat_results = st_matrix("r(table)")

              vi.res.mean[groups,.] = mat_results[1,.]
              vi.res.se[groups,.]   = mat_results[2,.]
              vi.res.t[groups,.]    = mat_results[3,.]
              vi.res.lci[groups,.]  = mat_results[5,.]
              vi.res.uci[groups,.]  = mat_results[6,.]
              vi.res.df[groups,.]   = mat_results[7,.]
              vi.res.obs[groups,.]  = (bd.opt.weight.subpop == "") ? st_matrix("e(_N)") : st_matrix("e(_N_subp)")

            /* SD & Var */

              if(dosd)
              {
                checkerr(rc = _stata("estat sd", 1))

                vi.res.sd[groups,.]  = st_matrix("r(sd)")
                vi.res.var[groups,.] = st_matrix("r(variance)")
              }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }

      rc = _stata("drop " + invtokens(varlist), 1)
    }

  /* function : calculateSeriesOverRow16() */

    void function calculateSeriesOverRow16(struct bradmean scalar bd,
                                           struct varInfo  scalar vi)
    {
      `Integer' vars, groups
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, over_names, term
      `String'  cmd_tab1, matcell, matrow
      `RealMat' mat_results
      `RealVec' over_num
      `Pos'     pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = length(bd.oi.levels)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)

        if(!vi.binary) return

      /* Defining Commands */

        /* Mean */

          cmd_mean = ("i." + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") " + bd.opt.weight.vce)

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean " + cmd_mean
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean " + cmd_mean
          else                           cmd_mean = "mean " + cmd_mean

        /* Tabulate Oneway */

          matcell  = st_tempname()
          matrow   = st_tempname()
          cmd_tab1 = "tab " + bd.oi.name + " if e(sample), nolabel matcell(" + matcell + ") matrow(" + matrow + ")"

        /* Tabulate Twoway */

          cmd_tab2 = " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = ("svy, subpop(" + bd.opt.weight.subpop + "): tab "), (cmd_tab2 + ", pearson")
          else if(bd.opt.weight.survey)  cmd_tab2 = ("svy: tab ")                                      , (cmd_tab2 + ", pearson")
          else                           cmd_tab2 = ("tab ")                                           , (cmd_tab2 + ", chi2")

        /* Count */

          cmd_count = ("tabstat "), (" if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save"), (" by(" + bd.oi.name + ")")

      /* Calculating Results */

        for(i=vars; i; i--)
        {
          /* Mean (1) */

            rc = _stata(cmd_mean + " over(" + vi.varlist[i] + ", nolabel)", 1)
            if(rc != 0) continue

            mat_results = st_matrix("r(table)")'
            over_names  = st_matrixcolstripe("r(table)")[.,2]
            over_num    = strtoreal(subinstr(subinstr(insidepar(over_names, "@", "."), "bn", ""), "o", ""))

          /* Observations */

            rc = _stata(cmd_tab1, 1)

            pos               = st_matrix(matrow)
            vi.res.obs[pos,i] = st_matrix(matcell)

          /* Mean (2) */

            if(allof(over_num, 0))
            {
              vi.res.mean[.,i] = J(groups, 1, 0)
              vi.res.se[.,i]   = J(groups, 1, 0)
              vi.res.sd[.,i]   = J(groups, 1, 0)
              vi.res.var[.,i]  = J(groups, 1, 0)

              continue
            }

            pos = selectindex(over_num :== 1)

            vi.res.mean[.,i] = mat_results[pos,1]
            vi.res.se[.,i]   = mat_results[pos,2]
            vi.res.t[.,i]    = mat_results[pos,3]
            vi.res.lci[.,i]  = mat_results[pos,5]
            vi.res.uci[.,i]  = mat_results[pos,6]
            vi.res.df[.,i]   = mat_results[pos,7]

          /* Standard Deviation & Variance */

            if(dosd)
            {
              checkerr(rc = _stata("estat sd", 1))

              vi.res.sd[.,i]  = st_matrix("r(sd)")'[pos]
              vi.res.var[.,i] = st_matrix("r(variance)")'[pos]
            }

          /* Testing */

            if(bd.opt.test.chi_overall)
            {
              checkerr(rc = _stata(cmd_tab2[1] + vi.varlist[i] + cmd_tab2[2], 1))

              if(bd.opt.weight.survey)
              {
                if(st_numscalar("e(r)") > 1)
                {
                  vi.res.ovr_statistic[i] = st_numscalar("e(F_Pear)")
                  vi.res.ovr_pvalue[i]    = st_numscalar("e(p_Pear)")
                }
              }
              else
              {
                if(st_numscalar("r(r)") > 1)
                {
                  vi.res.ovr_statistic[i] = st_numscalar("r(chi2)")
                  vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
                }
              }
            }
            else if(bd.opt.test.f_overall)
            {
              term = "test " + invtokens(over_names[pos]', " == ") + ", mtest(" + bd.opt.test.f_mtest + ")"

              checkerr(rc = _stata(term, 1))

              vi.res.ovr_statistic[i] = st_numscalar("r(F)")
              vi.res.ovr_pvalue[i]    = st_numscalar("r(p)")
            }
        }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count[1] + invtokens(vi.varlist) + cmd_count[2] + cmd_count[3], 1))

            for(i=groups; i; i--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

              vi.res.nyes[i,.] = mat_results[1,.]
              vi.res.min[i,.]  = mat_results[2,.]
              vi.res.max[i,.]  = mat_results[3,.]
              vi.res.p25[i,.]  = mat_results[4,.]
              vi.res.p50[i,.]  = mat_results[5,.]
              vi.res.p75[i,.]  = mat_results[6,.]
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion & vi.binary)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }
    }

  /* function : calculateXiOverRow16() */

    void function calculateXiOverRow16(struct bradmean scalar bd,
                                       struct varInfo  scalar vi)
    {
      `Integer' vars, groups, len
      `Boolean' dosd, dotab
      `Tokens'  cmd_mean, cmd_tab2, cmd_count, over_names, term
      `String'  cmd_tab1, matcell, matrow
      `RealMat' mat_results
      `DataVec' over_num
      `Pos'     over_pos, pos
      `Integer' rc, i, j

      /* Getting Information */

        vars   = length(vi.answers)
        groups = length(bd.oi.levels)

        dosd  = anylist(bd.si.name, ("sd", "var"))
        dotab = anylist(bd.si.name, ("nyes", "min", "max", "p25", "p50", "p75"))

      /* Defining Results */

        vi.res.obs = J(groups, vars, 0)
        vi.res.nyes = vi.res.mean = vi.res.lci = vi.res.uci = vi.res.se = vi.res.sd = vi.res.var = vi.res.min = vi.res.max = vi.res.p25 = vi.res.p50 = vi.res.p75 = vi.res.t = vi.res.df = J(groups, vars, .)

        vi.res.ovr_statistic = vi.res.ovr_pvalue = J(1, vars, .)

      /* Defining Commands */

        /* Mean */

          if(bd.opt.weight.subpop != "") cmd_mean = "svy, subpop(" + bd.opt.weight.subpop + "): mean "
          else if(bd.opt.weight.survey)  cmd_mean = "svy: mean "
          else                           cmd_mean = "mean "

          cmd_mean = cmd_mean + "i." + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd + ", level(" + strofreal(bd.si.ci_level) + ") over(" + vi.varlist + ") " + bd.opt.weight.vce

        /* Tabulate Oneway */

          matcell  = st_tempname()
          matrow   = st_tempname()
          cmd_tab1 = "tab " + bd.oi.name + " if e(sample), nolabel matcell(" + matcell + ") matrow(" + matrow + ")"

        /* Tabulate Twoway */

          cmd_tab2 = "tab " + vi.varlist + " " + bd.oi.name + " if " + st_local("touse") + " " + bd.opt.weight.cmd

          if(bd.opt.weight.subpop != "") cmd_tab2 = "svy, subpop(" + bd.opt.weight.subpop + "): " + cmd_tab2 + ", pearson"
          else if(bd.opt.weight.survey)  cmd_tab2 = "svy: "                                       + cmd_tab2 + ", pearson"
          else                           cmd_tab2 =                                                 cmd_tab2 + ", chi2"

        /* Count */

          cmd_count = "xi, noomit: tabstat " + vi.term + " if " + st_local("touse_cnt") + ", stat(sum min max p25 p50 p75) c(v) save by(" + bd.oi.name + ")"

      /* Calculating Results */

        /* Mean */

          rc = _stata(cmd_mean, 1)

          if(rc != 0) return

          mat_results = st_matrix("r(table)")
          over_names  = st_matrixcolstripe("r(table)")[.,2]
          over_num    = strtoreal(subinstr(subinstr(insidepar(over_names, "@", "."), "bn", ""), "o", ""))
          over_pos    = uniqrows(over_num)
          len         = length(over_pos)

          for(i=len; i; i--) over_pos[i] = selectindex(over_num :== over_pos[i])[1]

          vi.res.mean[.,over_pos] = colshape(mat_results[1,.], len)
          vi.res.se[.,over_pos]   = colshape(mat_results[2,.], len)
          vi.res.t[.,over_pos]    = colshape(mat_results[3,.], len)
          vi.res.lci[.,over_pos]  = colshape(mat_results[5,.], len)
          vi.res.uci[.,over_pos]  = colshape(mat_results[6,.], len)
          vi.res.df[.,over_pos]   = colshape(mat_results[7,.], len)

        /* Observations */

          rc = _stata(cmd_tab1, 1)

          pos               = st_matrix(matrow)
          vi.res.obs[pos,.] = J(1, vars, st_matrix(matcell))

        /* Standard Deviation & Variance */

          if(dosd)
          {
            checkerr(rc = _stata("estat sd", 1))

            vi.res.sd[.,over_pos]  = colshape(st_matrix("r(sd)"), len)
            vi.res.var[.,over_pos] = colshape(st_matrix("r(variance)"), len)
          }

        /* Count, Min, Max */

          if(dotab)
          {
            checkerr(rc = _stata(cmd_count, 1))

            for(i=groups; i; i--)
            {
              mat_results = st_matrix("r(Stat" + strofreal(i) + ")")

              vi.res.nyes[i,.] = mat_results[1,.]
              vi.res.min[i,.]  = mat_results[2,.]
              vi.res.max[i,.]  = mat_results[3,.]
              vi.res.p25[i,.]  = mat_results[4,.]
              vi.res.p50[i,.]  = mat_results[5,.]
              vi.res.p75[i,.]  = mat_results[6,.]
            }

            rc = _stata("drop `_dta[__xi__Vars__To__Drop__]'", 1)
          }

        /* Testing */

          if(bd.opt.test.chi_overall)
          {
            checkerr(rc = _stata(cmd_tab2, 1))

            if(bd.opt.weight.survey)
            {
              if(st_numscalar("e(r)") > 1)
              {
                vi.res.ovr_statistic[.,over_pos] = J(1, len, st_numscalar("e(F_Pear)"))
                vi.res.ovr_pvalue[.,over_pos]    = J(1, len, st_numscalar("e(p_Pear)"))
              }
            }
            else
            {
              if(st_numscalar("r(r)") > 1)
              {
                vi.res.ovr_statistic[.,over_pos] = J(1, len, st_numscalar("r(chi2)"))
                vi.res.ovr_pvalue[.,over_pos]    = J(1, len, st_numscalar("r(p)"))
              }
            }
          }
          else if(bd.opt.test.f_overall)
          {
            over_names = colshape(over_names, len)'

            for(i=len; i; i--)
            {
              term = "test " + invtokens(over_names[i,.], " == ") + ", mtest(" + bd.opt.test.f_mtest + ")"

              checkerr(rc = _stata(term, 1))

              vi.res.ovr_statistic[over_pos[i]] = st_numscalar("r(F)")
              vi.res.ovr_pvalue[over_pos[i]]    = st_numscalar("r(p)")
            }
          }

      /* Logit Transform & Sorting */

        if(bd.si.ci_proportion)
        {
          logitTransform(bd, vi)
        }

        if(anyof(bd.si.name, bd.opt.display.sort_statistic))
        {
          sortResults(bd, vi, 1)
        }
    }

/*======================================================================*/
/*   Mata Functions - Printer                                           */
/*======================================================================*/

  /* function : printer() */

    void function printer(struct bradmean scalar bd)
    {
      `StringMat' legend
      `String'    title
      `Integer'   lvls

      if(!bd.opt.display.print) return(J(0,0,.))

      /* Title */

        if(bd.opt.display.title != "") printf("\n{title:" + subinstr(subinstr(bd.opt.display.title, "%", "%%"), "\", "\\") + "}\n")

      /* Legend */

        if((lvls = length(bd.oi.levels)) > 0 & bd.opt.over.legend)
        {
          legend = getLegend(bd.oi)
          printf("\n")
          display(legend)
        }

      /* Table */

        printf("\n")

        if(lvls == 0)                 printLongNoOver(bd)
        else if(!bd.opt.display.wide) printLongOver(bd)
        else                          printWide(bd)

      /* Footer - Stars */

        if(lvls > 1 & bd.opt.test.overall & length(bd.opt.test.stars) > 0 & anyof(bd.si.stars, 1) & bd.opt.test.footer)
        {
          legend = range(1, length(bd.opt.test.stars), 1) :* uchar(735)
          legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'
          display(legend)
        }

      /* Footer - Scripts */

        if(lvls > 1 & bd.opt.test.individual & bd.opt.test.scripts != . & anyof(bd.si.scripts, 1) & bd.opt.test.footer)
        {
          legend = bd.opt.test.letters' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"
          display(legend)
        }
    }

  /* function : getLegend() */

    `StringVec' getLegend(struct overInfo scalar oi)
    {
      `StringMat' labels, legend

      labels = (st_varvaluelabel(oi.name) != "") ? st_vlmap(st_varvaluelabel(oi.name), oi.levels) : strofreal(oi.levels)

      if(oi.name != oi.varlist)                        labels      = substr(labels, strpos(labels, " ") :+ 1)
      if(length(pos = selectindex(labels :== "")) > 0) labels[pos] = strofreal(oi.levels[pos])

      legend = "_over_" :+ strofreal(oi.levels)'
      legend = "{lalign " :+ strofreal(max(udstrlen(legend))) :+ ":" :+ legend :+ "} {c |} "
      legend = legend :+ char(34) :+ invtokens(oi.varlist, ", ") :+ char(34) :+ " = " :+ char(34) :+ labels' :+ char(34)

      return(legend)
    }

  /* function : printLongNoOver() */

    void function printLongNoOver(struct bradmean scalar bd)
    {
      `Integer'   stats, terms, vars, len, rows, cols
      `StringVec' formats, sym, hsep, cur_vec
      `StringMat' res_table, cur_table
      `RealVec'   res_index, cur_index
      `RealMat'   values1, values2
      `Pos'       rpos, cpos
      `RealVec'   col_lengths, table_nums
      `Integer'   i, j

      /* Getting Information */

        stats = length(bd.si.name)
        terms = length(bd.vi)

        cols = 1 + stats

      /* Getting Results Table */

        res_table = J(0, cols, "")
        res_index = J(0, 1, .)

        for(i=1; i<=terms; i++)
        {
          /* Initializing Current Table */

            rows = vars = length(bd.vi[i].answers)

            cur_table = J(rows, cols, "")

            formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym     = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

          /* Labels */

            cur_table[.,1] = bd.vi[i].answers'
            cur_index      = J(rows, 1, 1 + (terms > 1 & vars > 1))

          /* Statistics */

            cpos = 1

            for(j=1; j<=stats; j++)
            {
              cpos++

              if(bd.si.name[j] != "ci")
              {
                values1 = getResults(bd.vi[i].res, bd.si.name[j])' :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                cur_vec = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
              }
              else
              {
                values1 = getResults(bd.vi[i].res, "lci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                values2 = getResults(bd.vi[i].res, "uci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                cur_vec = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
              }

              cur_table[.,cpos] = ((values1 :!= .) :* cur_vec) :+ ((values1 :== .) :* ".")
            }

          /* Adding to Complete Table */

            if(terms > 1 & vars > 1)
            {
              cur_table = (bd.vi[i].question, J(1, cols - 1, "")) \ cur_table
              cur_index = 1                                       \ cur_index
            }

            if(bd.opt.display.separator)
            {
              rows = terms > 1 & vars > 1
              if(i < terms) if(length(bd.vi[i+1].answers) > 1) rows = 1

              if(rows)
              {
                cur_table = cur_table \ J(1, cols, "")
                cur_index = cur_index \ 0
              }
            }

            res_table = res_table \ cur_table
            res_index = res_index \ cur_index
        }

      /* Formatting Results Table */

        /* Column Lengths */

          col_lengths = colmax(J(1, cols, 6) \ udstrlen(res_table))
          if(col_lengths[1] > 80) col_lengths[1] = 80

        /* Adding Statistic Names */

          cur_table = "", bd.si.label

          if(length(cpos = selectindex(udstrlen(cur_table) :> col_lengths)) > 0) cur_table[cpos] = abbrevx(cur_table[cpos], col_lengths[cpos])

          cur_table[1] = "{res}{space " + strofreal(col_lengths[1]) + "} {c |}"

          res_table = cur_table \ J(1, cols, "") \ res_table
          res_index = .         \ 0              \ res_index

        /* Statistics */

          rows = rows(res_table)
          cpos = range(2, cols, 1)

          res_table[|1,2\rows,cols|] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[cpos]) :+ ":" :+ res_table[|1,2\rows,cols|] :+ "} "

        /* Variable Names */

          if(length(rpos = selectindex(res_index :== 1)) > 0) res_table[rpos,1] = "{res}{lalign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"
          if(length(rpos = selectindex(res_index :== 2)) > 0) res_table[rpos,1] = "{res}{ralign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"

        /* Separator */

          hsep    =      "{hline " :+ strofreal(col_lengths    :+ 2) :+ "}"
          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c +}"

          if(length(rpos = selectindex(res_index :== 0)) > 0) res_table[rpos,.] = J(length(rpos), 1, hsep)

          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c BT}"

          if(res_index[rows] == 0) res_table[rows,.] = hsep
          else                     res_table         = res_table \ hsep

      /* Printing */

        len = max(table_nums = tablenums(col_lengths :+ 2))

        for(i=1; i<=len; i++)
        {
          if(len > 1)
          {
            if(i > 1) printf("\n")
            printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
          }

          cpos = selectindex(table_nums :== i)
          display(addcols((res_table[.,1], res_table[.,cpos])))
        }
    }

  /* function : printLongOver() */

    void function printLongOver(struct bradmean scalar bd)
    {
      `Integer'   stats, terms, vars, lvls, groups, len, rows, cols, comblen
      `Boolean'   p_overall, p_individual, p_stars, p_scripts, p_values
      `StringVec' labels, blank, formats, sym, hsep, vsep
      `StringMat' res_table, cur_table, temp_table
      `RealVec'   res_index, cur_index
      `RealMat'   values1, values2
      `Pos'       rpos, cpos
      `RealVec'   col_lengths, table_nums
      `Integer'   rc, i, j

      /* Getting Information */

        stats  = length(bd.si.name)
        terms  = length(bd.vi)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        p_stars      = bd.opt.test.overall    * anyof(bd.si.stars, 1)   * (length(bd.opt.test.stars) > 0)
        p_scripts    = bd.opt.test.individual * anyof(bd.si.scripts, 1) * (bd.opt.test.scripts != .)
        p_overall    = bd.opt.test.overall    * (p_stars   == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual * (p_scripts == 0 | bd.opt.test.force)
        p_values     = p_overall + (p_individual * lvls)

        cols = 1 + stats + (p_values * (1 + bd.opt.test.statistic))

      /* Getting Results Table */

        res_table = J(0, cols, "")
        res_index = J(0, 1, .)

        labels    = bd.oi.labels' \ J(bd.opt.over.total, 1, "Total") \ ""
        cur_index = 1 \ J(groups, 1, 2) \ 0

        for(i=1; i<=terms; i++)
        {
          /* Initializing Current Table */

            rows = (vars = length(bd.vi[i].answers)) * (2 + groups)

            cur_table = J(rows, cols, "")

            blank   = J(1, vars, "")
            formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym     = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

          /* Labels */

            if(terms == 1)                 cur_table[.,1] = vec(bd.vi[i].answers \ J(1, vars, labels))
            else if(bd.vi[i].type == "xi") cur_table[.,1] = vec((bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels)) \ J(1, vars, labels))
            else                           cur_table[.,1] = vec(bd.vi[i].varlist \ J(1, vars, labels))

          /* Statistics */

            cpos = 1

            for(j=1; j<=stats; j++)
            {
              cpos++

              /* Statistic */

                if(bd.si.name[j] != "ci")
                {
                  values1    = getResults(bd.vi[i].res, bd.si.name[j]) :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  temp_table = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }
                else
                {
                  values1    = getResults(bd.vi[i].res, "lci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  values2    = getResults(bd.vi[i].res, "uci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  temp_table = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  len = length(bd.opt.test.stars)
                  for(k=len; k; k--) temp_table = temp_table :+ ((bd.vi[i].res.ovr_pvalue :< bd.opt.test.stars[k]) :* uchar(735))
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  for(k=vars; k; k--) temp_table[|1,k\lvls,k|] = temp_table[|1,k\lvls,k|] :+ addcols((rowshape(bd.vi[i].res.ind_pvalue[.,k], lvls) :< bd.opt.test.scripts) :* bd.opt.test.letters)
                }

              /* Adding to Current Table */

                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                cur_table[.,cpos] = vec(blank \ temp_table \ blank)
            }

          /* P-Values - Overall */

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  cpos++

                  values1    = J(groups, 1, bd.vi[i].res.ovr_statistic)
                  temp_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") temp_table = (!bd.opt.weight.survey ? "Chi2 = " : "F = ") :+ temp_table
                  else if(bd.opt.test.t_overall)                      temp_table = "t = " :+ temp_table
                  else if(bd.opt.test.f_overall)                      temp_table = "F = " :+ temp_table

                  temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                  cur_table[.,cpos] = vec(blank \ temp_table \ blank)
                }

              /* P-Values */

                cpos++

                values1    = J(groups, 1, bd.vi[i].res.ovr_pvalue)
                temp_table = strofreal(values1, "%6.4f")
                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                cur_table[.,cpos] = vec(blank \ temp_table \ blank)
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  cpos = rangex(++cpos, lvls, 1 + bd.opt.test.statistic)
                  rpos = rangex(2, lvls, 1)

                  values1    = bd.vi[i].res.ind_statistic
                  temp_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.t_individual)      temp_table = "t = " :+ temp_table
                  else if(bd.opt.test.f_individual) temp_table = "F = " :+ temp_table

                  temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                  for(k=1; k<=vars; k++)
                  {
                    cur_table[rpos,cpos] = rowshape(temp_table[.,k], lvls)
                    rpos = rpos :+ 2 :+ groups
                  }
                }

              /* P-Values */

                cpos = rangex(++cpos[1], lvls, 1 + bd.opt.test.statistic)
                rpos = rangex(2, lvls, 1)

                values1    = bd.vi[i].res.ind_pvalue
                temp_table = strofreal(values1, "%6.4f")
                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                for(k=1; k<=vars; k++)
                {
                  cur_table[rpos,cpos] = rowshape(temp_table[.,k], lvls)
                  rpos = rpos :+ 2 :+ groups
                }
            }

          /* Adding to Complete Table */

            if(terms == 1 & vars == 1)
            {
              cur_table = cur_table[rangex(2, groups, 1)',.]
              cur_index = J(groups, 1, 1)
            }

            res_table = res_table \ cur_table
            res_index = res_index \ J(vars, 1, cur_index)
        }

      /* Formatting Statistics & Variable Names */

        /* Column Lengths */

          col_lengths = colmax(J(1, cols, 6) \ udstrlen(res_table))
          if(col_lengths[1] > 80) col_lengths[1] = 80

        /* Removing Separators */

          if(!bd.opt.display.separator & length(rpos = selectindex(res_index :!= 0)) > 0)
          {
            res_table = res_table[rpos,.]
            res_index = res_index[rpos,.]
          }

        /* Statistics */

          rows = rows(res_table)
          cpos = range(2, cols, 1)

          res_table[|1,2\rows,cols|] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[cpos]) :+ ":" :+ res_table[|1,2\rows,cols|] :+ "} "

        /* Variable Names */

          if(length(rpos = selectindex(res_index :== 1)) > 0) res_table[rpos,1] = "{res}{lalign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"
          if(length(rpos = selectindex(res_index :== 2)) > 0) res_table[rpos,1] = "{res}{ralign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"

      /* Adding Header */

        /* Table Numbers */

          values1 = J(1, cols, 0)

          if(p_values > 0)
          {
            cpos = rangex(1 + stats, p_values^bd.opt.test.statistic, 2)
            values1[cpos] = values1[cpos] :+ 1
          }

          table_nums = tablenums(col_lengths :+ values1 :+ 2)

        /* Header 1 - Statistic Names */

          cur_table    = "", bd.si.label, J(1, p_values, (J(1, bd.opt.test.statistic, "Stat"), "P-Val"))
          cur_table    = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths) :+ ":" :+ abbrevx(cur_table, col_lengths) :+ "} "
          cur_table[1] = "{res}{space " + strofreal(col_lengths[1] + 1) + "}{c |}"

          res_table = cur_table \ J(1, cols, "") \ res_table
          res_index = .         \ 0              \ res_index

        /* Header 2 - Group Names */

          if(p_values > 0)
          {
            cur_table = "{space " :+ strofreal(col_lengths[1..(stats+1)] :+ 2) :+ "}"
            if(p_overall)    cur_table = cur_table, J(1, 1 + bd.opt.test.statistic, "Overall")
            if(p_individual) cur_table = cur_table, rowshape(J(1, 1 + bd.opt.test.statistic, "vs " :+ strofreal(bd.oi.levels)'), 1)

            bind_cols = J(1, 1 + stats, .), rowshape(J(1, 1 + bd.opt.test.statistic, rangex(1, p_values, 1)), 1)

            res_table = bindcols(cur_table, col_lengths, bind_cols, table_nums, bd.opt.display.align) \ res_table
            res_index = .                                                                             \ res_index
          }

      /* Adding Separators */

        /* Getting Break Positions */

          values2 = table_nums[2..cols], table_nums[cols]
          values2 = selectindex(values2 :!= table_nums)
          values1 = selectindex(values1)

          cpos = values1[selectindex(!inlist(values1, values2))]

        /* Horizontal Separator */

          rows = rows(res_table)

          hsep    =      "{hline " :+ strofreal(col_lengths    :+ 2) :+ "}"
          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c +}"

          if(length(rpos = selectindex(res_index :== 0)) > 0) res_table[rpos,.] = J(length(rpos), 1, hsep)

          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c BT}"

          if(res_index[rows] == 0) res_table[rows,.] = hsep
          else                     res_table         = res_table \ hsep

        /* Vertical Separator */

          if(length(cpos) > 0)
          {
            vsep = substr(res_table[.,1], strrpos(res_table[.,1], "{"))
            res_table[.,cpos] = res_table[.,cpos] :+ vsep
          }

      /* Printing */

        len = max(table_nums)

        for(i=1; i<=len; i++)
        {
          if(len > 1)
          {
            if(i > 1) printf("\n")
            printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
          }

          cpos = selectindex(table_nums :== i)
          display(addcols((res_table[.,1], res_table[.,cpos])))
        }
    }

  /* function : printWide() */

    void function printWide(struct bradmean scalar bd)
    {
      `Integer'   stats, terms, vars, lvls, groups, len, rows, cols, comblen
      `Boolean'   p_overall, p_individual, p_stars, p_scripts, p_values
      `Boolean'   h_stats, h_groupn, h_groups, h_pstats, h_pgroups
      `StringVec' blank, formats, sym, hsep, vsep
      `StringMat' res_table, cur_table, temp_table
      `RealVec'   res_index, cur_index
      `RealMat'   values1, values2, bind_cols
      `Pos'       rpos, cpos
      `RealVec'   col_lengths, table_nums
      `Integer'   rc, i, j

      /* Getting Information */

        stats  = length(bd.si.name)
        terms  = length(bd.vi)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        p_stars      = bd.opt.test.overall    * anyof(bd.si.stars, 1)   * (length(bd.opt.test.stars) > 0)
        p_scripts    = bd.opt.test.individual * anyof(bd.si.scripts, 1) * (bd.opt.test.scripts != .)
        p_overall    = bd.opt.test.overall    * (p_stars   == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual * (p_scripts == 0 | bd.opt.test.force)
        p_values     = p_overall + (p_individual ? (factorial(lvls)/(2 * factorial(lvls - 2))) : 0)

        h_stats   = stats == 1 ? bd.opt.display.statnames : 1
        h_groupn  = bd.opt.over.group_n
        h_groups  = 1
        h_pstats  = p_values > 0 ? (!bd.opt.test.statistic ? bd.opt.display.statnames : 1) : 0
        h_pgroups = p_values > 0

        cols = 1 + (groups * stats) + (p_values * (1 + bd.opt.test.statistic))

        if(p_individual)
        {
          values1 = vec(J(lvls, 1, rangex(1, lvls, 1)')), J(lvls, 1, rangex(1, lvls, 1))
          rpos    = selectindex(values1[.,1] :< values1[.,2])
        }

      /* Getting Results Table */

        res_table = J(0, cols, "")
        res_index = J(0, 1, .)

        for(i=1; i<=terms; i++)
        {
          /* Initializing Current Table */

            rows = vars = length(bd.vi[i].answers)

            cur_table = J(rows, cols, "")

            formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym     = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

          /* Labels */

            cur_table[.,1] = bd.vi[i].answers'
            cur_index      = J(rows, 1, 1 + (terms > 1 & vars > 1))

          /* Statistics */

            for(j=1; j<=stats; j++)
            {
              cpos = rangex(1 + j, groups, stats)

              /* Statistic */

                if(bd.si.name[j] != "ci")
                {
                  values1    = getResults(bd.vi[i].res, bd.si.name[j]) :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  temp_table = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }
                else
                {
                  values1    = getResults(bd.vi[i].res, "lci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  values2    = getResults(bd.vi[i].res, "uci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  temp_table = bd.si.notation[1,j] :+ strofreal(values1, formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  len = length(bd.opt.test.stars)
                  for(k=len; k; k--) temp_table = temp_table :+ ((bd.vi[i].res.ovr_pvalue :< bd.opt.test.stars[k]) :* uchar(735))
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  for(k=vars; k; k--) temp_table[|1,k\lvls,k|] = temp_table[|1,k\lvls,k|] :+ addcols((rowshape(bd.vi[i].res.ind_pvalue[.,k], lvls) :< bd.opt.test.scripts) :* bd.opt.test.letters)
                }

              /* Adding to Current Table */

                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                cur_table[.,cpos] = temp_table'
            }

            cpos = 1 + (stats * groups)

          /* P-Values - Overall */

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  cpos++

                  values1    = bd.vi[i].res.ovr_statistic
                  temp_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") temp_table = (!bd.opt.weight.survey ? "Chi2 = " : "F = ") :+ temp_table
                  else if(bd.opt.test.t_overall)                      temp_table = "t = " :+ temp_table
                  else if(bd.opt.test.f_overall)                      temp_table = "F = " :+ temp_table

                  temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                  cur_table[.,cpos] = temp_table'
                }

              /* P-Values */

                cpos++

                values1    = bd.vi[i].res.ovr_pvalue
                temp_table = strofreal(values1, "%6.4f")
                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                cur_table[.,cpos] = temp_table'
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  cpos = rangex(++cpos, p_values - p_overall, 2)

                  values1    = bd.vi[i].res.ind_statistic[rpos,.]
                  temp_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.t_individual)      temp_table = "t = " :+ temp_table
                  else if(bd.opt.test.f_individual) temp_table = "F = " :+ temp_table

                  temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                  cur_table[.,cpos] = temp_table'
                }

              /* P-Values */

                cpos = rangex(++cpos[1], p_values - p_overall, 1 + bd.opt.test.statistic)

                values1    = bd.vi[i].res.ind_pvalue[rpos,.]
                temp_table = strofreal(values1, "%6.4f")
                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                cur_table[.,cpos] = temp_table'
            }

          /* Adding to Complete Table */

            if(terms > 1 & vars > 1)
            {
              cur_table = (bd.vi[i].question, J(1, cols - 1, "")) \ cur_table
              cur_index = 1                                       \ cur_index
            }

            if(bd.opt.display.separator)
            {
              rows = terms > 1 & vars > 1
              if(i < terms) if(length(bd.vi[i+1].answers) > 1) rows = 1

              if(rows)
              {
                cur_table = cur_table \ J(1, cols, "")
                cur_index = cur_index \ 0
              }
            }

            res_table = res_table \ cur_table
            res_index = res_index \ cur_index
        }

      /* Formatting Statistics & Variable Names */

        /* Column Lengths */

          col_lengths = colmax(J(1, cols, 6) \ udstrlen(res_table))
          if(col_lengths[1] > 80) col_lengths[1] = 80

        /* Statistics */

          rows = rows(res_table)
          cpos = range(2, cols, 1)

          res_table[|1,2\rows,cols|] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[cpos]) :+ ":" :+ res_table[|1,2\rows,cols|] :+ "} "

        /* Variable Names */

          if(length(rpos = selectindex(res_index :== 1)) > 0) res_table[rpos,1] = "{res}{lalign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"
          if(length(rpos = selectindex(res_index :== 2)) > 0) res_table[rpos,1] = "{res}{ralign " :+ strofreal(col_lengths[1]) :+ ":" :+ substr(res_table[rpos,1], 1, col_lengths[1]) :+ "} {c |}"

      /* Adding Header */

        /* Initializing Header */

          rows = max(((h_stats + h_groupn + h_groups), (h_pstats + h_pgroups)))

          cur_table = J(rows, 1, "{space " :+ strofreal(col_lengths :+ 2) :+ "}")
          bind_cols = J(rows, cols, .)

        /* Filling Header */

          for(i=rows; i; i--)
          {
            /* Statistics */

              cpos = rangex(2, stats * groups, 1)

              if(h_stats)
              {
                cur_table[i,cpos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[cpos]) :+ ":" :+ abbrevx(J(1, groups, bd.si.label), col_lengths[cpos]) :+ "} "

                h_stats = 0
              }
              else if(h_groupn)
              {
                temp_table = ("(n = " :+ strofreal(bd.oi.freqs, "%32.0fc")' :+ ")") \ J(bd.opt.over.total, 1, ("(n = " + strofreal(sum(bd.oi.freqs), "%32.0fc") + ")"))

                cur_table[i,cpos] = rowshape(J(1, stats, temp_table), 1)
                bind_cols[i,cpos] = rowshape(J(1, stats, rangex(1, groups, 1)), 1)

                h_groupn = 0
              }
              else if(h_groups)
              {
                temp_table = bd.oi.labels' \ J(bd.opt.over.total, 1, "Total")

                cur_table[i,cpos] = rowshape(J(1, stats, temp_table), 1)
                bind_cols[i,cpos] = rowshape(J(1, stats, rangex(1, groups, 1)), 1)

                h_groups = 0
              }

            /* P-Values */

              cpos = range(2 + (stats * groups), cols, 1)

              if(h_pstats)
              {
                cur_table[i,cpos] = " {" :+ bd.opt.display.align :+ " " :+ strofreal(col_lengths[cpos]) :+ ":" :+ abbrevx(J(1, p_values, (J(1, bd.opt.test.statistic, "Stat"), "P-Val")), col_lengths[cpos]) :+ "} "

                h_pstats = 0
              }
              else if(h_pgroups)
              {
                temp_table = J(1, 0, "")

                if(p_overall) temp_table = temp_table, "Overall"

                if(p_individual)
                {
                  values1 = vec(J(lvls, 1, rangex(1, lvls, 1)')), J(lvls, 1, rangex(1, lvls, 1))
                  values1 = values1[selectindex(values1[.,1] :< values1[.,2]),.]

                  temp_table = temp_table, (strofreal(bd.oi.levels[values1[.,1]]) :+ "v" :+ strofreal(bd.oi.levels[values1[.,2]]))
                }

                cur_table[i,cpos] = vec(J(1 + bd.opt.test.statistic, 1, temp_table))'

                values2           = max(bind_cols[i,.]) :+ rangex(1, p_values, 1)
                bind_cols[i,cpos] = rowshape(J(1, 1 + bd.opt.test.statistic, values2), 1)

                h_pgroups = 0
              }
          }

        /* Table Numbers */

          values1 = J(1, cols, 0)

          cpos = rangex(1 + stats, groups - 1, stats)'
          if(p_values > 0) cpos = cpos, rangex(1 + (stats * groups), p_values^bd.opt.test.statistic, 2)'

          values1[cpos] = values1[cpos] :+ 1

          table_nums = tablenums(col_lengths :+ values1 :+ 2)

        /* Adding to Table */

          res_table = bindcols(cur_table, col_lengths, bind_cols, table_nums, bd.opt.display.align) \ J(1, cols, "") \ res_table
          res_index = J(rows, 1, .)                                                                 \ 0              \ res_index

      /* Adding Separators */

        /* Getting Break Positions */

          values2 = table_nums[2..cols], table_nums[cols]
          values2 = selectindex(values2 :!= table_nums)
          values1 = selectindex(values1)

          cpos = values1[selectindex(!inlist(values1, values2))]

        /* Horizontal Separator */

          rows = rows(res_table)

          hsep    =      "{hline " :+ strofreal(col_lengths    :+ 2) :+ "}"
          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c +}"

          if(length(rpos = selectindex(res_index :== 0)) > 0) res_table[rpos,.] = J(length(rpos), 1, hsep)

          hsep[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c BT}"

          if(res_index[rows] == 0) res_table[rows,.] = hsep
          else                     res_table         = res_table \ hsep

        /* Vertical Separator */

          if(length(cpos) > 0)
          {
            vsep = substr(res_table[.,1], strrpos(res_table[.,1], "{"))
            res_table[.,cpos] = res_table[.,cpos] :+ vsep
          }

      /* Printing */

        len = max(table_nums)

        for(i=1; i<=len; i++)
        {
          if(len > 1)
          {
            if(i > 1) printf("\n")
            printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
          }

          cpos = selectindex(table_nums :== i)
          display(addcols((res_table[.,1], res_table[.,cpos])))
        }
    }

/*======================================================================*/
/*   Mata Functions - Excel                                             */
/*======================================================================*/

  /* function : createExcelSingle() */

    void function createExcelSingle(struct bradmean scalar bd)
    {
      `RealVec' row, col
      `Pos'     pos
      `Integer' rc

      if(!bd.opt.excel.output) return

      /* Initializing Object */

        class xl scalar B

        B = xl()

      /* Loading Book & Setting Worksheet */

        if(fileexists(bd.opt.excel.file_path))
        {
          if(bd.opt.excel.bookreplace)
          {
            rc = unlink(bd.opt.excel.file_path)

            B.create_book(bd.opt.excel.file_path, bd.opt.excel.sheet, substr(pathsuffix(bd.opt.excel.file_path), 2))
          }
          else
          {
            B.load_book(bd.opt.excel.file_path)

            if(bd.opt.excel.sheet == "")
            {
              bd.opt.excel.sheet = B.get_sheets()[1]
            }
            else
            {
              if(anyof(B.get_sheets(), bd.opt.excel.sheet)) B.set_sheet(bd.opt.excel.sheet)
              else                                          B.add_sheet(bd.opt.excel.sheet)
            }

            if(bd.opt.excel.sheetreplace) B.clear_sheet(bd.opt.excel.sheet)
          }
        }
        else
        {
          if(bd.opt.excel.sheet == "") bd.opt.excel.sheet = "Sheet1"

          B.create_book(bd.opt.excel.file_path, bd.opt.excel.sheet, substr(pathsuffix(bd.opt.excel.file_path), 2))
        }

        B.set_mode("open")
        B.set_missing(".")

      /* Getting Initial Position */

        if(bd.opt.excel.bookreplace | bd.opt.excel.sheetreplace)
        {
          row = 1
        }
        else
        {
          row = 1, 50
          col = 1, 6

          while(row[1] >= 1)
          {
            pos = selectindex(rowmax(B.get_cell_type(row, col) :!= "blank"))

            if(length(pos) == 0)
            {
              row = row[1]
              break
            }
            else
            {
              pos = pos[length(pos)] + row[1]
              pos = pos, pos + 10

              if(max(B.get_cell_type(pos, col) :!= "blank") == 0)
              {
                row = pos[1] + 1
                break
              }
            }

            row = row :+ 50
          }
        }

      /* Creating Table */

        if(length(bd.oi.levels) == 0) excelLongNoOver(bd, B, row)
        else if(!bd.opt.display.wide) excelLongOver(bd, B, row)
        else                          excelWide(bd, B, row)

      B.close_book()
    }

  /* function : createExcelMultiple() */

    void function createExcelMultiple(struct bradmean vector bd)
    {
      `RealVec' row, col
      `Pos'     pos
      `Integer' len
      `Integer' rc

      if(!bd[1].opt.excel.output) return

      /* Initializing Object */

        class xl scalar B

        B = xl()

        len = length(bd)

      /* Loading Book */

        if(fileexists(bd[1].opt.excel.file_path))
        {
          if(bd[1].opt.excel.bookreplace)
          {
            rc = unlink(bd[1].opt.excel.file_path)

            B.create_book(bd[1].opt.excel.file_path, bd[1].opt.excel.sheet, substr(pathsuffix(bd[1].opt.excel.file_path), 2))
          }
          else
          {
            B.load_book(bd[1].opt.excel.file_path)
          }
        }
        else
        {
          if(bd[1].opt.excel.sheet == "") bd[1].opt.excel.sheet = "Sheet1"

          B.create_book(bd[1].opt.excel.file_path, bd[1].opt.excel.sheet, substr(pathsuffix(bd[1].opt.excel.file_path), 2))
        }

        B.set_mode("open")
        B.set_missing(".")

      /* Exporting Data */

        for(i=1; i<=len; i++)
        {

        /* Setting Worksheet */

          if(bd[i].opt.excel.sheet == "") bd[i].opt.excel.sheet = B.get_sheets()[1]

          if(anyof(B.get_sheets(), bd[i].opt.excel.sheet)) B.set_sheet(bd[i].opt.excel.sheet)
          else                                             B.add_sheet(bd[i].opt.excel.sheet)

          if(bd[i].opt.excel.sheetreplace) B.clear_sheet(bd[i].opt.excel.sheet)

        /* Getting Initial Position */

          if(bd[1].opt.excel.bookreplace | bd[i].opt.excel.sheetreplace)
          {
            row = 1
          }
          else
          {
            row = 1, 50
            col = 1, 6

            while(row[1] >= 1)
            {
              pos = selectindex(rowmax(B.get_cell_type(row, col) :!= "blank"))

              if(length(pos) == 0)
              {
                row = row[1]
                break
              }
              else
              {
                pos = pos[length(pos)] + row[1]
                pos = pos, pos + 10

                if(max(B.get_cell_type(pos, col) :!= "blank") == 0)
                {
                  row = pos[1] + 1
                  break
                }
              }

              row = row :+ 50
            }
          }

        /* Creating Table */

          if(length(bd[i].oi.levels) == 0) excelLongNoOver(bd[i], B, row)
          else if(!bd[i].opt.display.wide) excelLongOver(bd[i], B, row)
          else                             excelWide(bd[i], B, row)

        }

      B.close_book()
    }

  /* function : excelLongNoOver() */

    void function excelLongNoOver(struct bradmean scalar bd,
                                  class  xl       scalar B,
                                  `Integer'              row)
    {
      `Integer'   stats, terms, vars, len
      `Integer'   font, font_bold, fmt_title, fmt_header, fmt_question, fmt_answer
      `StringVec' txt_stats_c, txt_stats_i
      `RealVec'   fmt_stats_c, fmt_stats_i
      `String'    title
      `StringVec' str_formats, sym
      `StringMat' cur_table
      `RealMat'   values1, values2
      `Pos'       rpos, cpos
      `Pos'       rbound, cbound
      `Integer'   i, j

      /* Getting Information */

        stats = length(bd.si.name)
        terms = length(bd.vi)

        rpos = row

      /* Creating Excel Formats */

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

          txt_stats_c = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundc :> 0) :* ".") :+ (bd.si.roundc :* "0")
          txt_stats_i = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundi :> 0) :* ".") :+ (bd.si.roundi :* "0") :+ (bd.si.percent :* bd.si.symbol :* "%")

          txt_stats_c = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_c :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))
          txt_stats_i = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_i :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))

        /* Statistics - Format ID */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            if(fmt_stats_c[i] != . & fmt_stats_i[i] != .) continue

            /* Continuous */

              len = length(cpos = selectindex(txt_stats_c[i] :== txt_stats_c))

              if(min(fmt_stats_c[cpos]) == .)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
                B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])

                fmt_stats_c[cpos] = J(1, len, fmt_stats_c[i])
              }
              else
              {
                fmt_stats_c[cpos] = J(1, len, min(fmt_stats_c[cpos]))
              }

            /* Binary (Continuous Fill) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_c[1..i]))

              if(len > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[cpos[1]]

                len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

                if(len > 1) fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])

                continue
              }

            /* Binary (New) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

              if(min(fmt_stats_i[cpos]) == .)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
                B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])

                fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])
              }
              else
              {
                fmt_stats_i[cpos] = J(1, len, min(fmt_stats_i[cpos]))
              }
          }

      /* Title */

        title = bd.opt.display.title

        if(title != "")
        {
          B.put_string(rpos, 1, title)
          B.set_fmtid(rpos, 1, fmt_title)

          rpos++
        }

      /* Header */

        cpos   = 2, 1 + stats
        cbound = 1, 1 + stats

        B.put_string(rpos, 2, bd.si.label)
        B.set_fmtid(rpos, cpos, fmt_header)

        B.set_top_border(rpos, cbound, "medium")
        B.set_bottom_border(rpos, cbound, "medium")

        rbound = rpos++

      /* Variables & Statistics */

        for(i=1; i<=terms; i++)
        {
          /* Getting Information */

            vars = length(bd.vi[i].answers)

            str_formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym         = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

          /* Series Name */

            if(terms > 1 & vars > 1)
            {
              B.put_string(rpos, 1, bd.vi[i].question)
              B.set_fmtid(rpos, 1, fmt_question)

              rpos++
            }

          /* Variables Names */

            rpos = rpos, rpos + vars - 1

            B.put_string(rpos[1], 1, bd.vi[i].answers')

            if(terms > 1 & vars > 1) B.set_fmtid(rpos, 1, fmt_answer)
            else                     B.set_fmtid(rpos, 1, fmt_question)

          /* Statistics */

            for(j=1; j<=stats; j++)
            {
              cpos = 1 + j

              if(bd.si.name[j] != "ci")
              {
                values1 = getResults(bd.vi[i].res, bd.si.name[j])' :* (100^(bd.vi[i].binary & bd.si.percent[j] & !bd.si.symbol[j]))

                B.put_number(rpos[1], cpos, values1)
              }
              else
              {
                values1 = getResults(bd.vi[i].res, "lci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                values2 = getResults(bd.vi[i].res, "uci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))

                cur_table = bd.si.notation[1,j] :+ strofreal(values1, str_formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, str_formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                cur_table = ((values1 :!= .) :* cur_table) :+ ((values1 :== .) :* ".")

                B.put_string(rpos[1], cpos, cur_table)
              }

              if(bd.vi[i].binary) B.set_fmtid(rpos, cpos, fmt_stats_i[j])
              else                B.set_fmtid(rpos, cpos, fmt_stats_c[j])
            }

          /* Border */

            if(terms > 1 & i < terms)
            {
              if(vars > 1 | length(bd.vi[i+1].answers) > 1) B.set_bottom_border(rpos[2], cbound, "thin")
            }

          rpos = rpos[2] + 1
        }

        rbound = rbound, rpos - 1

      /* Formatting - General */

        B.set_left_border(rbound, 1, "medium")
        B.set_right_border(rbound, 1, "medium")
        B.set_right_border(rbound, cbound[2], "medium")
        B.set_bottom_border(rbound[2], cbound, "medium")
    }

  /* function : excelLongOver() */

    void function excelLongOver(struct bradmean scalar bd,
                                class  xl       scalar B,
                                `Integer'              row)
    {
      `Integer'   stats, terms, lvls, groups, vars, single, len
      `Boolean'   p_overall, p_individual, p_stars, p_scripts, p_values
      `RealVec'   haspost
      `Integer'   font, font_bold, fmt_title, fmt_whitespace, fmt_header, fmt_question, fmt_answer, fmt_pval, fmt_pstat
      `StringVec' txt_stats_c, txt_stats_i
      `RealVec'   fmt_stats_c, fmt_stats_i
      `String'    title
      `StringVec' str_formats, sym
      `DataMat'   cur_table, temp_table
      `StringVec' labels
      `RealMat'   values1, values2
      `Pos'       rpos, cpos, tpos
      `Pos'       rbound, cbound
      `Integer'   i, j, k

      /* Getting Information */

        stats  = length(bd.si.name)
        terms  = length(bd.vi)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total
        single = terms == 1 & length(bd.vi[1].answers) == 1

        p_stars      = bd.opt.test.overall    * anyof(bd.si.stars, 1)   * (length(bd.opt.test.stars) > 0)
        p_scripts    = bd.opt.test.individual * anyof(bd.si.scripts, 1) * (bd.opt.test.scripts != .)
        p_overall    = bd.opt.test.overall    * (p_stars   == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual * (p_scripts == 0 | bd.opt.test.force)
        p_values     = p_overall + (p_individual * lvls)

        haspost = (p_stars :* bd.si.stars) :| (p_scripts :* bd.si.scripts)

        rpos   = row
        cbound = 1, 1 + stats + (p_values * (1 + bd.opt.test.statistic))

      /* Creating Excel Formats */

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

          if(single)
          {
            fmt_answer = fmt_question
          }
          else
          {
            fmt_answer = B.add_fmtid()
            B.fmtid_set_fontid(fmt_answer, font_bold)
            B.fmtid_set_horizontal_align(fmt_answer, "right")
            B.fmtid_set_fill_pattern(fmt_answer, "solid", bd.opt.excel.color[2])
          }

        /* P-Values */

          if(p_values > 0)
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

        /* Statistics - Numeric Format */

          txt_stats_c = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundc :> 0) :* ".") :+ (bd.si.roundc :* "0")
          txt_stats_i = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundi :> 0) :* ".") :+ (bd.si.roundi :* "0") :+ (bd.si.percent :* bd.si.symbol :* "%")

          txt_stats_c = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_c :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))
          txt_stats_i = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_i :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))

        /* Statistics - Format ID */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            if(fmt_stats_c[i] != . & fmt_stats_i[i] != .) continue

            /* Continuous */

              len = length(cpos = selectindex(txt_stats_c[i] :== txt_stats_c))

              if(min(fmt_stats_c[cpos]) == .)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
                B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])

                fmt_stats_c[cpos] = J(1, len, fmt_stats_c[i])
              }
              else
              {
                fmt_stats_c[cpos] = J(1, len, min(fmt_stats_c[cpos]))
              }

            /* Binary (Continuous Fill) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_c[1..i]))

              if(len > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[cpos[1]]

                len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

                if(len > 1) fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])

                continue
              }

            /* Binary (New) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

              if(min(fmt_stats_i[cpos]) == .)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
                B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])

                fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])
              }
              else
              {
                fmt_stats_i[cpos] = J(1, len, min(fmt_stats_i[cpos]))
              }
          }

      /* Title */

        title = bd.opt.display.title

        if(title != "")
        {
          B.put_string(rpos, 1, title)
          B.set_fmtid(rpos, 1, fmt_title)

          rpos++
        }

      /* Header */

        rbound = rpos
        rpos   = rpos, rpos

        /* Group Names */

          if(p_values > 0)
          {
            cur_table = J(1, p_overall * (1 + bd.opt.test.statistic), "Overall"), rowshape(J(1, p_individual * (1 + bd.opt.test.statistic), "vs " :+ strofreal(bd.oi.levels)'), 1)

            B.put_string(rpos[1], 2 + stats, cur_table)

            if(bd.opt.test.statistic)
            {
              cpos = (2 + stats), (3 + stats)

              for(i=1; i<=p_values; i++)
              {
                B.set_sheet_merge(bd.opt.excel.sheet, rpos, cpos)
                cpos = cpos :+ 2
              }
            }

            rpos = rpos :+ 1
          }

        /* Statistic Names */

          cur_table = bd.si.label, J(1, p_values, (J(1, bd.opt.test.statistic, "Stat"), "P-Val"))

          B.put_string(rpos[1], 2, cur_table)

        /* Formatting */

          rpos = rbound[1], rpos[1]
          cpos = 2, cbound[2]

          B.set_fmtid(rpos, cpos, fmt_header)
          B.set_fmtid(rpos, (1,1), fmt_whitespace)

          B.set_top_border((rpos[1],rpos[1]), cbound, "medium")
          B.set_bottom_border((rpos[2],rpos[2]), cbound, "medium")

      /* Variables & Statistics */

        labels = bd.oi.labels' \ J(bd.opt.over.total, 1, "Total")

        for(i=1; i<=terms; i++)
        {
          /* Getting Information */

            vars = length(bd.vi[i].answers)

            str_formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym         = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

            rpos = rpos[2] + 1, rpos[2] + (!single * vars) + (vars * groups)

          /* Variable Names */

            if(single == 1)                cur_table = labels
            else if(terms == 1)            cur_table = vec(bd.vi[i].answers \ J(1, vars, labels))
            else if(bd.vi[i].type == "xi") cur_table = vec((bd.vi[i].varlist :+ " == " :+ strofreal(bd.vi[i].levels)) \ J(1, vars, labels))
            else                           cur_table = vec(bd.vi[i].varlist \ J(1, vars, labels))

            B.put_string(rpos[1], 1, cur_table)
            B.set_fmtid(rpos, 1, fmt_answer)

          /* Statistics */

            cpos = 2

            for(j=1; j<=stats; j++)
            {
              /* Statistic */

                if(bd.si.name[j] != "ci")
                {
                  values1 = getResults(bd.vi[i].res, bd.si.name[j]) :* (100^(bd.vi[i].binary & bd.si.percent[j] & (haspost[j] | !bd.si.symbol[j])))

                  if(haspost[j]) cur_table = bd.si.notation[1,j] :+ strofreal(values1, str_formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }
                else
                {
                  values1 = getResults(bd.vi[i].res, "lci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  values2 = getResults(bd.vi[i].res, "uci") :* (100^(bd.vi[i].binary & bd.si.percent[j]))

                  cur_table = bd.si.notation[1,j] :+ strofreal(values1, str_formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, str_formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  len = length(bd.opt.test.stars)
                  for(k=len; k; k--) cur_table = cur_table :+ ((bd.vi[i].res.ovr_pvalue :< bd.opt.test.stars[k]) :* uchar(735))
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  for(k=vars; k; k--) cur_table[|1,k\lvls,k|] = cur_table[|1,k\lvls,k|] :+ addcols((rowshape(bd.vi[i].res.ind_pvalue[.,k], lvls) :< bd.opt.test.scripts) :* bd.opt.test.letters)
                }

              /* Adding to Excel */

                if(!haspost[j] & bd.si.name[j] != "ci")
                {
                  B.put_number(rpos[1], cpos, vec(J(!single, vars, .) \ values1))
                }
                else
                {
                  cur_table = ((values1 :!= .) :* cur_table) :+ ((values1 :== .) :* ".")
                  B.put_string(rpos[1], cpos, vec(J(!single, vars, "") \ cur_table))
                }

                if(bd.vi[i].binary) B.set_fmtid(rpos, (cpos,cpos), fmt_stats_i[j])
                else                B.set_fmtid(rpos, (cpos,cpos), fmt_stats_c[j])

              cpos++
            }

          /* P-Values */

            if(p_values > 0)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  cur_table = J(vars * (groups + 1), p_values, "")

                  tpos = 1

                  /* Overall */

                    if(p_overall)
                    {
                      values1    = J(groups, 1, bd.vi[i].res.ovr_statistic)
                      temp_table = strofreal(values1, "%32.2f")

                      if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") temp_table = (!bd.opt.weight.survey ? "Chi2 = " : "F = ") :+ temp_table
                      else if(bd.opt.test.t_overall)                      temp_table = "t = " :+ temp_table
                      else if(bd.opt.test.f_overall)                      temp_table = "F = " :+ temp_table

                      temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                      cur_table[.,tpos] = vec(J(1, vars, "") \ temp_table)

                      tpos++
                    }

                  /* Individual */

                    if(p_individual)
                    {
                      values1 = J(0, lvls, .)

                      for(j=1; j<=vars; j++) values1 = values1 \ J(1, lvls, .) \ rowshape(bd.vi[i].res.ind_statistic[.,j], lvls) \ J(bd.opt.over.total, lvls, .)

                      temp_table = strofreal(values1, "%32.2f")

                      if(bd.opt.test.t_individual)      temp_table = "t = " :+ temp_table
                      else if(bd.opt.test.f_individual) temp_table = "F = " :+ temp_table

                      temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

                      tpos = rangex(tpos, lvls, 1)

                      cur_table[.,tpos] = temp_table
                    }

                  /* Adding to Excel */

                    if(single) cur_table = cur_table[2..(groups + 1),.]

                    tpos = 2 + stats

                    for(j=1; j<=p_values; j++)
                    {
                      B.put_string(rpos[1], tpos, cur_table[.,j])

                      tpos = tpos + 2
                    }
                }

              /* P-Values */

                cur_table = J(vars * (groups + 1), p_values, .)

                tpos = 1

                /* Overall */

                  if(p_overall)
                  {
                    values1 = J(groups, 1, bd.vi[i].res.ovr_pvalue)

                    cur_table[.,tpos] = vec(J(1, vars, .) \ values1)

                    tpos++
                  }

                /* Individual */

                  if(p_individual)
                  {
                    values1 = J(0, lvls, .)

                    for(j=1; j<=vars; j++) values1 = values1 \ J(1, lvls, .) \ rowshape(bd.vi[i].res.ind_pvalue[.,j], lvls) \ J(bd.opt.over.total, lvls, .)

                    tpos = rangex(tpos, lvls, 1)

                    cur_table[.,tpos] = values1
                  }

                /* Adding to Excel */

                  if(single) cur_table = cur_table[2..(groups + 1),.]

                  tpos = 2 + stats + bd.opt.test.statistic

                  for(j=1; j<=p_values; j++)
                  {
                    B.put_number(rpos[1], tpos, cur_table[.,j])

                    tpos = tpos + 1 + bd.opt.test.statistic
                  }
            }
        }

        rbound  = rbound, rpos[2]
        rpos    = rbound
        rpos[1] = rpos[1] + 1 + (p_values > 0)

      /* Formatting - P-Values */

        if(p_values > 0)
        {
          cpos = 1 + stats, 1 + stats

          B.set_right_border(rbound, cpos, "thin")

          cpos = cpos :+ 1

          for(i=1; i<=p_values; i++)
          {
            if(bd.opt.test.statistic)
            {
              B.set_fmtid(rpos, cpos, fmt_pstat)
              cpos = cpos :+ 1
            }

            B.set_fmtid(rpos, cpos, fmt_pval)

            if(i < p_values & bd.opt.test.statistic) B.set_right_border(rbound, cpos, "thin")

            cpos = cpos :+ 1
          }
        }

      /* Formatting - By Variable */

        if(!single)
        {
          cur_table = J(1, cbound[2] - cbound[1], "")

          for(i=rpos[1]; i<=rpos[2]; i++)
          {
            B.set_fmtid(i, 1, fmt_question)

            B.put_string(i, 2, cur_table)

            i = i + groups

            B.set_bottom_border((i,i), cbound, "thin")
          }
        }

      /* Formatting - General */

        B.set_left_border(rbound, 1, "medium")
        B.set_right_border(rbound, 1, "medium")
        B.set_right_border(rbound, cbound[2], "medium")
        B.set_bottom_border(rbound[2], cbound, "medium")

      /* Footer - Stars */

        rpos = rbound[2] + 1

        if(p_stars & bd.opt.test.footer)
        {
          cur_table = (range(1, length(bd.opt.test.stars), 1) :* uchar(735)) :+ "p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'

          B.put_string(rpos, 1, cur_table)

          rpos = rpos, rpos + length(bd.opt.test.stars) - 1

          B.set_fmtid(rpos, (1,1), fmt_title)

          rpos = rpos[2] + 1
        }

      /* Footer - Scripts */

        if(p_scripts & bd.opt.test.footer)
        {
          cur_table = bd.opt.test.letters' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          B.put_string(rpos, 1, cur_table)

          rpos = rpos, rpos + lvls - 1

          B.set_fmtid(rpos, (1,1), fmt_title)
        }
    }

  /* function : excelWide() */

    void function excelWide(struct bradmean scalar bd,
                            class  xl       scalar B,
                            `Integer'              row)
    {
      `Integer'   stats, terms, lvls, groups, vars, len
      `Boolean'   p_overall, p_individual, p_stars, p_scripts, p_values
      `Boolean'   h_stats, h_groupn, h_groups, h_pstats, h_pgroups, h_row
      `RealVec'   haspost
      `Integer'   font, font_bold, fmt_title, fmt_whitespace, fmt_header, fmt_question, fmt_answer, fmt_pval, fmt_pstat
      `StringVec' txt_stats_c, txt_stats_i
      `RealVec'   fmt_stats_c, fmt_stats_i
      `String'    title
      `StringVec' str_formats, sym
      `DataMat'   cur_table, temp_table
      `StringVec' labels
      `RealMat'   values1, values2
      `Pos'       rpos, cpos, tpos
      `Pos'       rbound, cbound
      `Integer'   i, j, k

      /* Getting Information */

        stats  = length(bd.si.name)
        terms  = length(bd.vi)
        groups = (lvls = length(bd.oi.levels)) + bd.opt.over.total

        p_stars      = bd.opt.test.overall    * anyof(bd.si.stars, 1)   * (length(bd.opt.test.stars) > 0)
        p_scripts    = bd.opt.test.individual * anyof(bd.si.scripts, 1) * (bd.opt.test.scripts != .)
        p_overall    = bd.opt.test.overall    * (p_stars   == 0 | bd.opt.test.force)
        p_individual = bd.opt.test.individual * (p_scripts == 0 | bd.opt.test.force)
        p_values     = p_overall + (p_individual ? (factorial(lvls)/(2 * factorial(lvls - 2))) : 0)

        haspost = (p_stars :* bd.si.stars) :| (p_scripts :* bd.si.scripts)

        h_stats   = stats == 1 ? bd.opt.display.statnames : 1
        h_groupn  = bd.opt.over.group_n
        h_groups  = 1
        h_pstats  = p_values > 0 ? (!bd.opt.test.statistic ? bd.opt.display.statnames : 1) : 0
        h_pgroups = p_values > 0

        rpos   = row
        cbound = 1, 1 + (stats * groups) + (p_values * (1 + bd.opt.test.statistic))

      /* Creating Excel Formats */

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

        /* P-Values */

          if(p_values > 0)
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

        /* Statistics - Numeric Format */

          txt_stats_c = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundc :> 0) :* ".") :+ (bd.si.roundc :* "0")
          txt_stats_i = (bd.si.comma :* "#,##") :+ "0" :+ ((bd.si.roundi :> 0) :* ".") :+ (bd.si.roundi :* "0") :+ (bd.si.percent :* bd.si.symbol :* "%")

          txt_stats_c = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_c :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))
          txt_stats_i = ((bd.si.notation[1,.] :!= "") :* (char(34) :+ bd.si.notation[1,.] :+ char(34))) :+ txt_stats_i :+ ((bd.si.notation[2,.] :!= "") :* (char(34) :+ bd.si.notation[2,.] :+ char(34)))

        /* Statistics - Format ID */

          fmt_stats_c = fmt_stats_i = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            if(fmt_stats_c[i] != . & fmt_stats_i[i] != .) continue

            /* Continuous */

              len = length(cpos = selectindex(txt_stats_c[i] :== txt_stats_c))

              if(min(fmt_stats_c[cpos]) == .)
              {
                fmt_stats_c[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_c[i], font)
                B.fmtid_set_vertical_align(fmt_stats_c[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_c[i], "center")
                B.fmtid_set_number_format(fmt_stats_c[i], txt_stats_c[i])

                fmt_stats_c[cpos] = J(1, len, fmt_stats_c[i])
              }
              else
              {
                fmt_stats_c[cpos] = J(1, len, min(fmt_stats_c[cpos]))
              }

            /* Binary (Continuous Fill) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_c[1..i]))

              if(len > 0)
              {
                fmt_stats_i[i] = fmt_stats_c[cpos[1]]

                len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

                if(len > 1) fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])

                continue
              }

            /* Binary (New) */

              len = length(cpos = selectindex(txt_stats_i[i] :== txt_stats_i))

              if(min(fmt_stats_i[cpos]) == .)
              {
                fmt_stats_i[i] = B.add_fmtid()
                B.fmtid_set_fontid(fmt_stats_i[i], font)
                B.fmtid_set_vertical_align(fmt_stats_i[i], "center")
                B.fmtid_set_horizontal_align(fmt_stats_i[i], "center")
                B.fmtid_set_number_format(fmt_stats_i[i], txt_stats_i[i])

                fmt_stats_i[cpos] = J(1, len, fmt_stats_i[i])
              }
              else
              {
                fmt_stats_i[cpos] = J(1, len, min(fmt_stats_i[cpos]))
              }
          }

      /* Title */

        title = bd.opt.display.title

        if(title != "")
        {
          B.put_string(rpos, 1, title)
          B.set_fmtid(rpos, 1, fmt_title)

          rpos++
        }

      /* Header */

        rbound = rpos
        h_row  = max(((h_stats + h_groupn + h_groups), (h_pstats + h_pgroups)))

        /* Creating & Placing */

          for(i=h_row; i; i--)
          {
            rpos = rbound + i - 1

            /* Statistics */

              cpos = 2

              if(h_stats)
              {
                B.put_string(rpos, cpos, J(1, groups, bd.si.label))

                h_stats = 0
              }
              else if(h_groupn)
              {
                cur_table = ("(n = " :+ strofreal(bd.oi.freqs, "%32.0fc")' :+ ")") \ J(bd.opt.over.total, 1, ("(n = " + strofreal(sum(bd.oi.freqs), "%32.0fc") + ")"))

                B.put_string(rpos, cpos, rowshape(J(1, stats, cur_table), 1))

                if(stats > 1)
                {
                  for(j=1; j<=groups; j++)
                  {
                    cpos = 2 + (j - 1) * stats
                    cpos = cpos, cpos + stats - 1

                    B.set_sheet_merge(bd.opt.excel.sheet, (rpos,rpos), cpos)
                  }
                }

                h_groupn = 0
              }
              else if(h_groups)
              {
                cur_table = bd.oi.labels' \ J(bd.opt.over.total, 1, "Total")

                B.put_string(rpos, cpos, rowshape(J(1, stats, cur_table), 1))

                if(stats > 1)
                {
                  for(j=1; j<=groups; j++)
                  {
                    cpos = 2 + (j - 1) * stats
                    cpos = cpos, cpos + stats - 1

                    B.set_sheet_merge(bd.opt.excel.sheet, (rpos,rpos), cpos)
                  }
                }

                h_groups = 0
              }

            /* P-Values */

              cpos = 2 + (stats * groups)

              if(h_pstats)
              {
                cur_table = J(1, bd.opt.test.statistic, "Stat"), "P-Val"

                B.put_string(rpos, cpos, rowshape(J(p_values, 1, cur_table), 1))

                h_pstats = 0
              }
              else if(h_pgroups)
              {
                cur_table = J(0, 1, "")

                if(p_overall) cur_table = cur_table \ "Overall"

                if(p_individual)
                {
                  values2 = vec(J(lvls, 1, rangex(1, lvls, 1)')), J(lvls, 1, rangex(1, lvls, 1))
                  values2 = values2[selectindex(values2[.,1] :< values2[.,2]),.]

                  cur_table = cur_table \ (strofreal(bd.oi.levels[values2[.,1]]) :+ "v" :+ strofreal(bd.oi.levels[values2[.,2]]))'
                }

                B.put_string(rpos, cpos, rowshape(J(1, 1 + bd.opt.test.statistic, cur_table), 1))

                if(bd.opt.test.statistic)
                {
                  for(j=1; j<=p_values; j++)
                  {
                    B.set_sheet_merge(bd.opt.excel.sheet, (rpos,rpos), (cpos,cpos+1))
                    cpos = cpos + 2
                  }
                }

                h_pgroups = 0
              }
          }

        /* Formatting */

          rpos = rbound, rbound + h_row - 1

          B.set_fmtid(rpos, (2,cbound[2]), fmt_header)
          B.set_fmtid(rpos, (1,1), fmt_whitespace)

          B.set_bottom_border((rpos[2],rpos[2]), cbound, "medium")
          B.set_top_border((rpos[1],rpos[1]), cbound, "medium")

      /* Variables & Statistics */

        rpos = rbound + h_row

        for(i=1; i<=terms; i++)
        {
          /* Getting Information */

            vars = length(bd.vi[i].answers)

            str_formats = bd.vi[i].binary ? ("%32." :+ strofreal(bd.si.roundi) :+ "f" :+ (bd.si.comma :* "c")) : ("%32." :+ strofreal(bd.si.roundc) :+ "f" :+ (bd.si.comma :* "c"))
            sym         = bd.vi[i].binary :* bd.si.percent :* bd.si.symbol :* "%"

          /* Series Name */

            if(terms > 1 & vars > 1)
            {
              B.put_string(rpos, 1, bd.vi[i].question)
              B.set_fmtid(rpos, 1, fmt_question)

              rpos++
            }

          /* Variables Names */

            rpos = rpos, rpos + vars - 1

            B.put_string(rpos[1], 1, bd.vi[i].answers')

            if(terms > 1 & vars > 1) B.set_fmtid(rpos, 1, fmt_answer)
            else                     B.set_fmtid(rpos, 1, fmt_question)

          /* Statistics */

            for(j=1; j<=stats; j++)
            {
              cpos = 1 + j

              /* Statistic */

                if(bd.si.name[j] != "ci")
                {
                  values1 = getResults(bd.vi[i].res, bd.si.name[j])' :* (100^(bd.vi[i].binary & bd.si.percent[j] & (haspost[j] | !bd.si.symbol[j])))

                  if(haspost[j]) cur_table = bd.si.notation[1,j] :+ strofreal(values1, str_formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }
                else
                {
                  values1 = getResults(bd.vi[i].res, "lci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))
                  values2 = getResults(bd.vi[i].res, "uci")' :* (100^(bd.vi[i].binary & bd.si.percent[j]))

                  cur_table = bd.si.notation[1,j] :+ strofreal(values1, str_formats[j]) :+ sym[j] :+ bd.si.ci_separator :+ strofreal(values2, str_formats[j]) :+ sym[j] :+ bd.si.notation[2,j]
                }

              /* P-Values - Stars */

                if(p_stars & bd.si.stars[j])
                {
                  len = length(bd.opt.test.stars)

                  for(k=len; k; k--) cur_table = cur_table :+ ((bd.vi[i].res.ovr_pvalue' :< bd.opt.test.stars[k]) :* uchar(735))
                }

              /* P-Values - Scripts */

                if(p_scripts & bd.si.scripts[j])
                {
                  for(k=vars; k; k--) cur_table[|k,1\k,lvls|] = cur_table[|k,1\k,lvls|] :+ addcols((rowshape(bd.vi[i].res.ind_pvalue[.,k], lvls) :< bd.opt.test.scripts) :* bd.opt.test.letters)'
                }

              /* Adding to Excel */

                if(haspost[j] | bd.si.name[j] == "ci") cur_table = ((values1 :!= .) :* cur_table) :+ ((values1 :== .) :* ".")

                for(k=1; k<=groups; k++)
                {
                  if(!haspost[j] & bd.si.name[j] != "ci") B.put_number(rpos[1], cpos, values1[.,k])
                  else                                    B.put_string(rpos[1], cpos, cur_table[.,k])

                  if(bd.vi[i].binary) B.set_fmtid(rpos, (cpos,cpos), fmt_stats_i[j])
                  else                B.set_fmtid(rpos, (cpos,cpos), fmt_stats_c[j])

                  cpos = cpos + stats
                }
            }

            cpos = 2 + (stats * groups)

          /* P-Values - Overall */

            if(p_overall)
            {
              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  values1   = bd.vi[i].res.ovr_statistic'
                  cur_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.chi_overall & bd.vi[i].type == "xi") cur_table = (!bd.opt.weight.survey ? "Chi2 = " : "F = ") :+ cur_table
                  else if(bd.opt.test.t_overall)                      cur_table = "t = " :+ cur_table
                  else if(bd.opt.test.f_overall)                      cur_table = "F = " :+ cur_table

                  cur_table = ((values1 :!= .) :* cur_table) :+ ((values1 :== .) :* ".")

                  B.put_string(rpos[1], cpos, cur_table)
                  B.set_fmtid(rpos, (cpos,cpos), fmt_pstat)

                  cpos++
                }

              /* P-Value */

                values1   = bd.vi[i].res.ovr_pvalue'

                B.put_number(rpos[1], cpos, values1)
                B.set_fmtid(rpos, (cpos,cpos), fmt_pval)

                cpos++
            }

          /* P-Values - Individual */

            if(p_individual)
            {
              values1 = vec(J(lvls, 1, rangex(1, lvls, 1)')), J(lvls, 1, rangex(1, lvls, 1))

              len = length(tpos = selectindex(values1[.,1] :< values1[.,2]))

              /* Statistic */

                if(bd.opt.test.statistic)
                {
                  values1   = bd.vi[i].res.ind_statistic[tpos,.]'
                  cur_table = strofreal(values1, "%32.2f")

                  if(bd.opt.test.t_individual)      cur_table = "t = " :+ cur_table
                  else if(bd.opt.test.f_individual) cur_table = "F = " :+ cur_table

                  cur_table = ((values1 :!= .) :* cur_table) :+ ((values1 :== .) :* ".")

                  for(j=1; j<=len; j++)
                  {
                    B.put_string(rpos[1], cpos, cur_table[.,j])
                    B.set_fmtid(rpos, (cpos,cpos), fmt_pstat)
                    cpos = cpos + 2
                  }

                  cpos = 1 + (stats * groups) + (p_overall * 2) + 2
                }

              /* P-Values */

                values1 = bd.vi[i].res.ind_pvalue[tpos,.]'

                for(j=1; j<=len; j++)
                {
                  B.put_number(rpos[1], cpos, values1[.,j])
                  B.set_fmtid(rpos, (cpos,cpos), fmt_pval)
                  cpos = cpos + 1 + bd.opt.test.statistic
                }
            }

          /* Border */

            if(terms > 1 & i < terms)
            {
              if(vars > 1 | length(bd.vi[i+1].answers) > 1) B.set_bottom_border(rpos[2], cbound, "thin")
            }

          rpos = rpos[2] + 1
        }

        rbound = rbound, rpos - 1

      /* Formatting - Groups */

        if(stats > 1)
        {
          cpos = 1 + stats, 1 + stats

          for(i=1; i<=groups; i++)
          {
            B.set_right_border(rbound, cpos, "thin")
            cpos = cpos :+ stats
          }
        }
        else if(p_values > 0)
        {
          cpos = 1 + groups, 1 + groups

          B.set_right_border(rbound, cpos, "thin")
        }

      /* Formatting - P-Values */

        if(p_values > 0 & bd.opt.test.statistic)
        {
          cpos = 1 + (stats * groups), 1 + (stats * groups)

          for(i=1; i<=p_values; i++)
          {
            B.set_right_border(rbound, cpos, "thin")

            cpos = cpos :+ 2
          }
        }

      /* Formatting - General */

        B.set_left_border(rbound, 1, "medium")
        B.set_right_border(rbound, 1, "medium")
        B.set_right_border(rbound, cbound[2], "medium")
        B.set_bottom_border(rbound[2], cbound, "medium")

      /* Footer - Stars */

        rpos = rbound[2] + 1

        if(p_stars & bd.opt.test.footer)
        {
          cur_table = (range(1, length(bd.opt.test.stars), 1) :* uchar(735)) :+ "p(overall) < 0" :+ strofreal(revorder(bd.opt.test.stars))'

          B.put_string(rpos, 1, cur_table)

          rpos = rpos, rpos + length(bd.opt.test.stars) - 1

          B.set_fmtid(rpos, (1,1), fmt_title)

          rpos = rpos[2] + 1
        }

      /* Footer - Scripts */

        if(p_scripts & bd.opt.test.footer)
        {
          cur_table = bd.opt.test.letters' :+ " sig. diff. from " :+ char(34) :+ bd.oi.labels' :+ char(34) :+ " (p < 0" :+ strofreal(bd.opt.test.scripts) :+ ")"

          B.put_string(rpos, 1, cur_table)

          rpos = rpos, rpos + lvls - 1

          B.set_fmtid(rpos, (1,1), fmt_title)
        }
    }

/*======================================================================*/
/*   Mata Functions - Collect                                           */
/*======================================================================*/

  /* function : combBradmean() */

    struct bradmean vector combBradmean(struct bradmean bd_new, | struct bradmean bd_old)
    {
      if(args() == 1) return(bd_new)
      else            return(bd_old \ bd_new)
    }

end
