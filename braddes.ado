version 15.1
set matastrict on
#delimit;
include bradsuite.mata, adopath;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      braddes.ado                                          **
**   Purpose:      Customizable describe                                **
**   Programmers:  Brian Bradfield                                      **
**   Version:      0.2.0                                                **
**   Date:         03/25/2021                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions - braddes                                          */
/*======================================================================*/

  program define braddes, nclass sortpreserve byable(recall);
  syntax [varlist],
    [
      DATA
      FIELDs(string)
      SORT(string)
      SEPBY(string)
    ];

  *----------------------------------------------------------*
  *   01. Initializing Braddes                               *
  *----------------------------------------------------------*;

    if(c(N) == 0 | c(k) == 1)
    {;
      mata: exit(error(2000));
    };

    mata: mata set matastrict on;
    mata: bd = braddes();
    mata: initDataInfo(bd.dsi);
    mata: initFieldInfo(bd.fi);

  *----------------------------------------------------------*
  *   02. Generating Results                                 *
  *----------------------------------------------------------*;

    mata: generateResults(bd);
    mata: printer(bd);

  *----------------------------------------------------------*
  *   03. Cleaning Up                                        *
  *----------------------------------------------------------*;

    mata: mata drop bd;

  end;

#delimit cr

mata:

/*======================================================================*/
/*   Mata Structures                                                    */
/*======================================================================*/

  /* struct : braddes */

    struct braddes
    {
      /* Sub-Structures */
      struct dataInfo  scalar dsi
      struct fieldInfo scalar fi

      /* Data Matrices */
      `RealMat'   res_num
      `StringMat' res_str
    }

  /* struct : dataInfo */

    struct dataInfo
    {
      /* Command Information */
      `Boolean' output

      /* File Information */
      `String'  label, file_path, date
      `Real'    size

      /* Data Information */
      `Real'    obs, vars
    }

  /* struct : fieldInfo */

    struct fieldInfo
    {
      /* Index */
      `RealVec'   index

      /* Name */
      `StringVec' name, slab, llab

      /* Sorting & Separating */
      `RealVec'   sort_field, sepby
    }

/*======================================================================*/
/*   Mata Functions - Initialization                                    */
/*======================================================================*/

  /* function : initDataInfo() */

    void function initDataInfo(struct dataInfo scalar dsi)
    {
      `Integer' rc

      /* Command Information */

        dsi.output = st_local("data") != ""

      /* File Information */

        rc = _stata("local datalabel : data label")

        dsi.label     = st_local("datalabel")
        dsi.file_path = c("filename")
        dsi.date      = strtrim(c("filedate"))
        dsi.size      = ((c("N") * c("width")) + (4 * c("N"))) / 1024

      /* Data Information */

        dsi.obs  = c("N")
        dsi.vars = c("k") - 1
    }

  /* function : initFieldInfo() */

    void function initFieldInfo(struct fieldInfo scalar fi)
    {
      `String' input_string
      `Tokens' tokens1, tokens2
      `Pos'    pos

      /* Setting Defaults */

        fi.index = rangex(1, 3, 1)'

        fi.name = ("varname", "varlab"        , "vallab"     , "type", "format", "letter", "term", "question", "answer", "numeric", "minum"      , "mipct"      , "nvnum"       , "nvpct"       , "ansnum"      , "anspct"      )
        fi.slab = ("Name"   , "VarLab"        , "ValLab"     , "Type", "Format", "Lett." , "Term", "Question", "Answer", "Num"    , "Miss#"      , "Miss%"      , "NoVal#"      , "NoVal%"      , "Ans#"        , "Ans%"        )
        fi.llab = ("Name"   , "Variable Label", "Value Label", "Type", "Format", "Letter", "Term", "Question", "Answer", "Numeric", "Missing (#)", "Missing (%)", "No Value (#)", "No Value (%)", "Answered (#)", "Answered (%)")

        fi.sort_field = J(0, 1, .)
        fi.sepby      = J(0, 1, .)

      /* Parsing 'Sort' */

        if((input_string = strlower(st_local("sort"))) != "")
        {
          tokens1 = tokens(input_string)
          tokens2 = substr(tokens1, 1, 1)
          tokens1 = subinstr(subinstr(tokens1, "-", ""), "+", "")

          pos     = sort(poslist(tokens1, uniqrows(tokens1')), 1)
          tokens1 = tokens1[pos]
          tokens2 = tokens2[pos]
          pos     = selectindex(inlist(tokens1, fi.name))

          if(length(pos) > 0)
          {
            tokens1 = tokens1[pos]
            tokens2 = tokens2[pos]

            fi.sort_field = poslist(fi.name, tokens1)
            pos           = selectindex(tokens2 :== "-")

            if(length(pos) > 0) fi.sort_field[pos] = fi.sort_field[pos] :* -1
          }
        }

      /* Parsing 'SepBy' */

        if((input_string = strlower(st_local("sepby"))) != "")
        {
          tokens1 = tokens(input_string)
          pos     = poslist(fi.name, tokens1)

          if(length(pos) > 0) fi.sepby = pos
        }

      /* Parsing 'Fields' */

        if((input_string = strlower(st_local("fields"))) != "")
        {
          input_string = subinword(input_string, "all", invtokens(fi.name))
          input_string = subinword(input_string, "varname", "")
          tokens1      = tokens(input_string)
          pos          = poslist(fi.name, tokens1)

          if(length(pos) > 0) fi.index = 1, pos
        }

        fi.name = fi.name[fi.index]
        fi.slab = fi.slab[fi.index]
        fi.llab = fi.llab[fi.index]
    }

/*======================================================================*/
/*   Mata Functions - Generating Results                                */
/*======================================================================*/

  /* function : generateResults() */

    void function generateResults(struct braddes scalar bd)
    {
      `Tokens'  tokens
      `Pos'     pos1, pos2
      `Integer' rc, len, i

      /* Initializing Matrices */

        tokens = tokens(st_local("varlist"))'
        len    = length(tokens)

        bd.res_num = J(len, 7, .)
        bd.res_str = J(len, 16, "")

      /* Getting Var Info - Name, VarLab, ValLab, Type, Format, & Numeric */

        bd.res_str[.,1] = tokens

        for(i=len; i; i--)
        {
          bd.res_str[i,2] = st_varlabel(bd.res_str[i,1])
          bd.res_str[i,3] = st_varvaluelabel(bd.res_str[i,1])
          bd.res_str[i,4] = st_vartype(bd.res_str[i,1])
          bd.res_str[i,5] = st_varformat(bd.res_str[i,1])
          bd.res_num[i,1] = st_isnumvar(bd.res_str[i,1])
        }

      /* Getting Var Info - Letter, Term, Question, & Answer */

        bd.res_str[.,6] = substr(bd.res_str[.,1], 1, 1)
        bd.res_str[.,7] = insidepar(bd.res_str[.,1], "", "_")
        bd.res_str[.,8] = insidepar(bd.res_str[.,2], "]", "")
        bd.res_str[.,9] = insidepar(bd.res_str[.,2], "[", "]")

      /* Filling Out Question Stubs */

        if(anyof(bd.fi.index, 8) | anyof(abs(bd.fi.sort_field), 8) | anyof(bd.fi.sepby, 8))
        {
          pos1 = selectindex((udstrlen(bd.res_str[.,2]) :== 80) :& (bd.res_str[.,9] :!= ""))

          if(length(pos1) > 0)
          {
            tokens = uniqrows(bd.res_str[pos1,8])
            len    = length(tokens)

            for(i=len; i; i--)
            {
              pos1 = selectindex((strpos(bd.res_str[.,8], tokens[i]) :== 1) :& (bd.res_str[.,9] :!= ""))
              pos2 = selectindex(udstrlen(bd.res_str[pos1,8]) :== max(udstrlen(bd.res_str[pos1,8])))
              pos1 = pos1[pos2[1]]
              pos2 = selectindex(bd.res_str[.,8] :== tokens[i])

              bd.res_str[pos2,8] = J(length(pos2), 1, bd.res_str[pos1,8])
            }
          }
        }

      /* Getting Var Info - Missing */

        if(anyof(strpos(bd.fi.name, "mi"), 1) | anyof(strpos(bd.fi.name, "ans"), 1))
        {
          len = rows(bd.res_num)

          for(i=len; i; i--)
          {
            rc = _stata("count if missing(" + bd.res_str[i,1] + ")", 1)
            bd.res_num[i,2] = st_numscalar("r(N)")
          }

          bd.res_num[.,3] = bd.res_num[.,2] :/ bd.dsi.obs
        }

      /* Getting Var Info - No Values */

        if(anyof(strpos(bd.fi.name, "nv"), 1) | anyof(strpos(bd.fi.name, "ans"), 1))
        {
          /* Numeric */

            pos1 = selectindex(bd.res_num[.,1])

            if((len = length(pos1)) > 0)
            {
              for(i=len; i; i--)
              {
                rc = _stata("count if " + bd.res_str[pos1[i],1] + " == .", 1)
                bd.res_num[pos1[i],4] = st_numscalar("r(N)")
              }
            }

          /* Strings */

            pos1 = selectindex(!bd.res_num[.,1])

            if((len = length(pos1)) > 0)
            {
              if(anyof(strpos(bd.fi.name, "mi"), 1))
              {
                bd.res_num[pos1,4] = bd.res_num[pos1,2]
              }
              else
              {
                for(i=len; i; i--)
                {
                  rc = _stata("count if missing(" + bd.res_str[pos1[i],1] + ")", 1)
                  bd.res_num[pos1[i],4] = st_numscalar("r(N)")
                }
              }
            }

          bd.res_num[.,5] = bd.res_num[.,4] :/ bd.dsi.obs
        }

      /* Getting Var Info - Answered */

        if(anyof(strpos(bd.fi.name, "ans"), 1))
        {
          bd.res_num[.,6] = bd.dsi.obs :- bd.res_num[.,2]
          bd.res_num[.,7] = bd.res_num[.,6] :/ (bd.dsi.obs :- (bd.res_num[.,2] :- bd.res_num[.,4]))
        }

      /* Sorting */

        if(length(bd.fi.sort_field) > 0)
        {
          bd.res_str[.,(10..16)] = strofreal(bd.res_num, "%032.16f")
          bd.res_str             = bd.res_str, strofreal(rangex(1, rows(bd.res_str), 1), "%016.0f")

          pos1 = order(bd.res_str, (bd.fi.sort_field, 17))

          bd.res_num = bd.res_num[pos1,.]
          bd.res_str = bd.res_str[pos1,.]
        }

      /* Adding Numbers to Strings */

        pos1 = selectindex(bd.res_num[.,1])
        if((len = length(pos1)) > 0) bd.res_str[pos1,10] = J(len, 1, "Num")

        pos1 = selectindex(!bd.res_num[.,1])
        if((len = length(pos1)) > 0) bd.res_str[pos1,10] = J(len, 1, "Str")

        bd.res_str[.,(11,13,15)] = strofreal(bd.res_num[.,(2,4,6)], "%12.0gc")
        bd.res_str[.,(12,14,16)] = strofreal(bd.res_num[.,(3,5,7)] :* 100, "%5.2f") :+ "%"

      /* Trimming Table */

        bd.res_str = strtrim(bd.res_str)
    }

/*======================================================================*/
/*   Mata Functions - Printer                                           */
/*======================================================================*/

  /* function : printer() */

    void function printer(struct braddes scalar bd)
    {
      `DataMat' res_table, temp_table
      `DataMat' values1, values2
      `RealVec' col_lengths, table_nums
      `Pos'     pos
      `Integer' len, i

      /* Data Info */

        if(bd.dsi.output)
        {
          printf("\n{res}{title:Title}: " + bd.dsi.label)
          printf("\n{res}{title:File}:  " + bd.dsi.file_path)
          printf("\n{res}{title:Date}:  " + bd.dsi.date)
          printf("\n{res}{title:Obs}:   " + strofreal(bd.dsi.obs, "%32.0fc"))
          printf("\n{res}{title:Vars}:  " + strofreal(bd.dsi.vars, "%32.0fc"))

          if(bd.dsi.size > (1024 * 1024)) printf("\n{res}{title:Size}:  " + strofreal(bd.dsi.size/1024/1024, "%32.2g") + " GB")
          else if(bd.dsi.size > 1024)     printf("\n{res}{title:Size}:  " + strofreal(bd.dsi.size/1024     , "%32.2g") + " MB")
          else                            printf("\n{res}{title:Size}:  " + strofreal(bd.dsi.size          , "%32.0g") + " KB")

          printf("\n")
        }

      /* Initializing Results Table */

        res_table   = bd.res_str[.,bd.fi.index]
        col_lengths = colmax(udstrlen(res_table))
        pos         = selectindex(col_lengths :< 5)

        if((len = length(pos)) > 0) col_lengths[pos] = J(1, len, 5)

      /* Adding Header */

        res_table = bd.fi.llab \ res_table
        pos       = selectindex(col_lengths :< udstrlen(bd.fi.llab))

        if(length(pos) > 0) res_table[1,pos] = bd.fi.slab[pos]

        col_lengths = colmax(udstrlen(res_table))

        res_table      = " {lalign " :+ strofreal(col_lengths) :+ ":" :+ res_table :+ "} "
        res_table[.,1] = "{res}" :+ res_table[.,1]

      /* Getting Table Numbers */

        table_nums = tablenums(col_lengths :+ 3)

      /* Adding Vertical Separators */

        if(length(table_nums) > 1)
        {
          values1    = table_nums[2..length(table_nums)],0
          values2    = table_nums :== values1
          values2[1] = 1

          res_table = res_table :+ (values2 :* "{c |}")
        }
        else
        {
          values2 = 0
        }

      /* Adding Horizontal Separators */

        temp_table = ("{hline " :+ strofreal(col_lengths :+ 2) :+ "}") :+ (values2 :* "{c +}")

        if((len = length(bd.fi.sepby)) > 0)
        {
          values1 = bd.res_str[.,bd.fi.sepby]
          values2 = J(1, len, "") \ values1[1..(rows(values1)-1),.]

          pos = selectindex(!rowmin(values1 :== values2))
          pos = pos[selectindex((pos :!= rows(bd.res_str)) :& (pos :!= 1))]

          if((len = length(pos)) > 0)
          {
            pos = pos :+ 1

            for(i=len; i; i--) res_table = insertrows(res_table, temp_table, pos[i])
          }
        }

        res_table = insertrows(res_table, temp_table, 2)
        res_table = res_table \ subinstr(temp_table, "{c +}", "{c BT}")

      /* Printing */

        printf("\n")

        len = max(table_nums)

        for(i=1; i<=len; i++)
        {
          if(len > 1)
          {
            if(i > 1) printf("\n")
            printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
          }

          pos = selectindex(table_nums :== i)
          display(addcols((res_table[.,1], res_table[.,pos])))
        }
    }

end
