version 15.1
#delimit;
include bradsuite.mata, adopath;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradout.ado                                          **
**   Purpose:      Outputs multiple estimation results in a clean table **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.0.1                                                **
**   Date:         02/07/2022                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions - bradout                                          */
/*======================================================================*/

  program define bradout, nclass;
  syntax [anything],
    [
      /* Stats */
      STATs(string)
      STARs(numlist max=3 >0 <1 sort)
      MSTATs(string)
      MINFO(string)

      /* Covariates */
      ORDER(string)
      DROP(string)
      KEEP(string)
      NOBASEs
      NOOMITs
      NOREFs

      /* Labelling */
      COLLABels(string asis)
      MLABels(string asis)
      NOVARLABels
      REFCATs(string asis)
      CATFILL(string)
      REFFILL(string)
      OMITFILL(string)

      /* Display */
      TITLE(string)
      HSEParator
      VSEParator
      WIDE
      BIND

      /* Output */
      EXCEL(string)

      /* System */
      CLEAR
    ];

  *----------------------------------------------------------*
  *   01. Initializing Bradout                               *
  *----------------------------------------------------------*;

    cap mata: st_local("len", strofreal(length(bs)));

    if("`len'" == "")
    {;
      mata: errprintf("{error:no bradsto estimates found}\n");
      mata: exit(301);
    };

    mata: mata set matastrict on;
    mata: bo = bradout();
    mata: initModelInfo(bo.mi);
    mata: initModelStatInfo(bo.msi);
    mata: initStatInfo(bo.si);
    mata: initOptions(bs, bo.opt, bo.si);
    mata: initCovars(bs[bo.opt.models], bo, bo.cv);
    mata: initExcel(bo.ex);

  *----------------------------------------------------------*
  *   02. Creating Output                                    *
  *----------------------------------------------------------*;

    if("`wide'" == "")
    {;
      mata: printLong(bs[bo.opt.models], bo);
    };
    else
    {;
      mata: printWide(bs[bo.opt.models], bo);
    };

    mata: createExcel(bs[bo.opt.models], bo);

  *----------------------------------------------------------*
  *   03. Cleaning Up                                        *
  *----------------------------------------------------------*;

    mata: mata drop bo;

    if("`clear'" != "")
    {;
      cap mata: mata drop bs;
    };


  end;


#delimit cr

mata:

/*======================================================================*/
/*   Mata Structures - bradout                                          */
/*======================================================================*/

  /* struct : bradout */

    struct bradout
    {
      struct modelInfo     scalar mi
      struct modelStatInfo scalar msi
      struct statInfo      scalar si
      struct options       scalar opt
      struct covars        scalar cv
      struct excel         scalar ex
    }

  /* struct : modelInfo */

    struct modelInfo
    {
      /* Index */
      `RealVec'   index

      /* Name */
      `StringVec' name, slab, llab
    }

  /* struct : modelStatInfo */

    struct modelStatInfo
    {
      /* Index */
      `RealVec'   index

      /* Name */
      `StringVec' name, slab, llab

      /* Format */
      `RealVec'   round, comma
    }

  /* struct : statInfo */

    struct statInfo
    {
      /* Index */
      `RealVec'   index

      /* Name */
      `StringVec' name, label

      /* Format - Rounding */
      `RealVec'   round

      /* Format - Notation */
      `StringMat' notation

      /* Format - P-Values */
      `RealVec'   stars

      /* CI Specific */
      `Boolean'   ci_combined
      `String'    ci_separator
    }

  /* struct : options */

    struct options
    {
      /* Models */
      `RealVec'   models

      /* Stats */
      `RealVec'   stars

      /* Covariates */
      `StringVec' covar_order, covar_drop, covar_keep
      `Boolean'   base, omit, reference

      /* Labelling */
      `StringVec' collabels
      `StringVec' mlabels
      `Boolean'   varlabels
      `StringMat' refcats

      /* Fills for Empty */
      `String'    catfill, reffill, omitfill

      /* Display */
      `String'    title
      `Boolean'   hsep, vsep
      `Boolean'   wide, bind
    }

  /* struct : covars */

    struct covars
    {
      /* Index */
      `RealMat'   index

      /* Name */
      `StringVec' name, label, vartype

      /* Cell Type */
      `StringMat' celltype
    }

  /* struct : excel */

    struct excel
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

/*======================================================================*/
/*   Mata Functions - Initialization                                    */
/*======================================================================*/

  /* function : initModelInfo() */

    void function initModelInfo(struct modelInfo scalar mi)
    {
      `String' input_string
      `Tokens' tokens

      /* Setting Defaults */

        mi.index = J(0, 1, .)
        mi.name  = ("mgroup"      \ "depvar"        \ "cmd"     \ "model" \ "family" \ "link" \ "weight")
        mi.slab  = ("M Group"     \ "Dep Var"       \ "Command" \ "Model" \ "Family" \ "Link" \ "Weight")
        mi.llab  = ("Model Group" \ "Dependent Var" \ "Command" \ "Model" \ "Family" \ "Link" \ "Weight")

      /* Parsing Stats */

        if((input_string = st_local("minfo")) != "")
        {
          input_string = subinword(input_string, "all", invtokens(mi.name'))
          tokens       = tokens(strlower(input_string))'
          mi.index     = poslist(mi.name, tokens)
        }

      /* Selecting Stats */

        mi.name = mi.name[mi.index]
        mi.slab = mi.slab[mi.index]
        mi.llab = mi.llab[mi.index]
    }

  /* function : initModelStatInfo() */

    void function initModelStatInfo(struct modelStatInfo scalar msi)
    {
      `String' input_string
      `Tokens' tokens

      /* Setting Defaults */

        msi.index = J(0, 1, .)

        /* Observations */

          msi.name  = ("n" \ "n_sub"      \ "n_yes"   \ "n_pop"        \ "n_subpop"        \ "n_psu" \ "n_strata" \ "n_strata_omit"  \ "n_clust"  \ "n_cds"       \ "n_cdf"       \ "n_cd"           \ "n_g"    \ "g_min"          \ "g_avg"          \ "g_max"          \ "tbar"              \ "n_drop"      \ "n_group_drop"  )
          msi.slab  = ("N" \ "N_Sub"      \ "N_Yes"   \ "PopEst"       \ "SubEst"          \ "PSUs"  \ "Strata"   \ "StOmit"         \ "Clust"    \ "N_CDS"       \ "N_CDF"       \ "N_CD"           \ "Groups" \ "G_Min"          \ "G_Avg"          \ "G_Max"          \ "Tbar"              \ "N_Drop"      \ "G_Drop"        )
          msi.llab  = ("N" \ "N (Subpop)" \ "N (Yes)" \ "Pop Estimate" \ "Subpop Estimate" \ "PSUs"  \ "Strata"   \ "Omitted Strata" \ "Clusters" \ "N (Success)" \ "N (Failure)" \ "N (Determined)" \ "Groups" \ "Min Group Size" \ "Avg Group Size" \ "Max Group Size" \ "H Mean Group Size" \ "N (Dropped)" \ "Groups Dropped")
          msi.round = (0   \ 0            \ 0         \ 0              \ 0                 \ 0       \ 0          \ 0                \ 0          \ 0             \ 0             \ 0                \ 0        \ 0                \ 2                \ 0                \ 2                   \ 0             \ 0               )
          msi.comma = (1   \ 1            \ 1         \ 1              \ 1                 \ 1       \ 1          \ 1                \ 1          \ 1             \ 1             \ 1                \ 1        \ 1                \ 1                \ 1                \ 1                   \ 1             \ 1               )

        /* Error */

          msi.name  = msi.name  \ ("mss"      \ "rss"         \ "tss"      \ "df_m"       \ "df_r"          \ "df_pear"      \ "df_a"          \ "df_b"             \ "rmse" \ "rank" \ "rank0"                \ "rho" \ "sigma_u"       \ "sigma" \ "sigma_e"         \ "deviance" \ "dispers"             \ "corr"       )
          msi.slab  = msi.slab  \ ("MSS"      \ "RSS"         \ "TSS"      \ "DF_m"       \ "DF_r"          \ "DF_p"         \ "DF_a"          \ "DF_b"             \ "RMSE" \ "Rank" \ "Rank0"                \ "Rho" \ "SigmaU"        \ "Sigma" \ "SigmaE"          \ "Dev"      \ "Disp"                \ "Corr"       )
          msi.llab  = msi.llab  \ ("Model SS" \ "Residual SS" \ "Total SS" \ "DF (Model)" \ "DF (Residual)" \ "DF (Pearson)" \ "DF (Absorbed)" \ "DF (F Numerator)" \ "RMSE" \ "Rank" \ "Rank (Constant-Only)" \ "Rho" \ "Panel Std Dev" \ "Sigma" \ "Epsilon Std Dev" \ "Deviance" \ "Deviance Dispersion" \ "Correlation")
          msi.round = msi.round \ (2          \ 2             \ 2          \ 0            \ 0               \ 0              \ 0               \ 0                  \ 4      \ 0      \ 0                      \ 4     \ 4               \ 4       \ 4                 \ 4          \ 4                     \ 4            )
          msi.comma = msi.comma \ (0          \ 0             \ 0          \ 1            \ 1               \ 1              \ 1               \ 1                  \ 0      \ 1      \ 1                      \ 0     \ 0               \ 0       \ 0                 \ 0          \ 0                     \ 0            )

        /* Goodness of Fit */

          msi.name  = msi.name  \ ("r2" \ "r2_a"        \ "r2_p"      \ "r2_w"      \ "r2_o"       \ "r2_b"       \ "ll"                     \ "ll_0"                  \ "ll_c"                  \ "aic" \ "bic" \ "thta_min"    \ "thta_5"          \ "thta_50"          \ "thta_95"          \ "thta_max"   )
          msi.slab  = msi.slab  \ ("R2" \ "Adj_R2"      \ "Pse_R2"    \ "Wit_R2"    \ "Ovr_R2"     \ "Btw_R2"     \ "LL(m)"                  \ "LL(0)"                 \ "LL(c)"                 \ "AIC" \ "BIC" \ "ThtMin"      \ "Tht5"            \ "Tht50"            \ "Tht95"            \ "ThtMax"     )
          msi.llab  = msi.llab  \ ("R2" \ "Adjusted R2" \ "Pseudo R2" \ "Within R2" \ "Overall R2" \ "Between R2" \ "Log Likelihood (Model)" \ "Log Likelihood (Null)" \ "Log Likelihood (Comp)" \ "AIC" \ "BIC" \ "Theta (Min)" \ "Theta (5th Pct)" \ "Theta (50th Pct)" \ "Theta (95th Pct)" \ "Theta (Max)")
          msi.round = msi.round \ (4    \ 4             \ 4           \ 4           \ 4            \ 4            \ 4                        \ 4                       \ 4                       \ 2     \ 2     \ 4             \ 4                 \ 4                  \ 4                  \ 4            )
          msi.comma = msi.comma \ (0    \ 0             \ 0           \ 0           \ 0            \ 0            \ 0                        \ 0                       \ 0                       \ 0     \ 0     \ 0             \ 0                 \ 0                  \ 0                  \ 0            )

        /* Significance */

          msi.name  = msi.name  \ ("f" \ "f_f" \ "p" \ "p_f" \ "chi2" \ "chi2_c"      \ "chi2_dev"        \ "chi2_dis"              \ "tol"              \ "dif"               )
          msi.slab  = msi.slab  \ ("F" \ "F_f" \ "p" \ "p_f" \ "Chi2" \ "Chi2_C"      \ "ChiDev"          \ "ChiDis"                \ "TgtTol"           \ "AchTol"            )
          msi.llab  = msi.llab  \ ("F" \ "F_f" \ "p" \ "p_f" \ "Chi2" \ "Chi2 (Comp)" \ "Chi2 (Deviance)" \ "Chi2 (Dev Dispersion)" \ "Target Tolerance" \ "Achieved Tolerance")
          msi.round = msi.round \ (2   \ 2     \ 4   \ 4     \ 2      \ 2             \ 2                 \ 2                       \ 4                  \ 4                   )
          msi.comma = msi.comma \ (0   \ 0     \ 0   \ 0     \ 0      \ 0             \ 0                 \ 0                       \ 0                  \ 0                   )

        /* Model Information */

          msi.name  = msi.name  \ ("k"          \ "k_aux"          \ "k_eq"      \ "k_eq_model"        \ "k_dv"           \ "n_quad"            \ "phi"             \ "stages"          \ "ic"        )
          msi.slab  = msi.slab  \ ("Pars"       \ "ParAux"         \ "Eqs"       \ "EqsMod"            \ "DVs"            \ "QuadPts"           \ "Scale"           \ "Stages"          \ "Iters"     )
          msi.llab  = msi.llab  \ ("Parameters" \ "Aux Parameters" \ "Equations" \ "Equations (Model)" \ "Dependent Vars" \ "Quadrature Points" \ "Scale Parameter" \ "Sampling Stages" \ "Iterations")
          msi.round = msi.round \ (0            \ 0                \ 0           \ 0                   \ 0                \ 0                   \ 4                 \ 0                 \ 0           )
          msi.comma = msi.comma \ (1            \ 1                \ 1           \ 1                   \ 1                \ 1                   \ 0                 \ 1                 \ 1           )

      /* Parsing Stats */

        if((input_string = st_local("mstats")) != "")
        {
          input_string = subinword(input_string, "all", invtokens(msi.name'))
          tokens       = tokens(strlower(input_string))'
          tokens       = subinword(tokens, "nyes" , "n_yes")
          tokens       = subinword(tokens, "ll_0" , "ll0")
          tokens       = subinword(tokens, "chi"  , "chi2")
          msi.index    = poslist(msi.name, tokens)
        }

      /* Selecting Stats */

        msi.name  = msi.name[msi.index]
        msi.slab  = msi.slab[msi.index]
        msi.llab  = msi.llab[msi.index]
        msi.round = msi.round[msi.index]
        msi.comma = msi.comma[msi.index]
    }

  /* function : initStatInfo() */

    void function initStatInfo(struct statInfo scalar si)
    {
      `String'  input_string
      `Tokens'  tokens, subtokens, statlist
      `Pos'     pos1, pos2
      `Integer' len
      `DataVec' values
      `Integer' i

      /* Setting Defaults */

        si.index  = ( 1  ,  2        ,  3       ,  4       ,  5             )
        si.name   = ("b" , "se"      , "tz"     , "pvalue" , "ci"           )
        si.label  = ("b" , "Std Err" , "t/z"    , "P-Val"  , "Conf Interval")
        si.round  = ( 7  ,  7        ,  2       ,  3       ,  7             )

        si.notation = J(2, 5, "")
        si.stars    = J(1, 5, 0 )

        si.ci_combined  = 0
        si.ci_separator = ","

      /* Parsing Stats */

        if((input_string = strlower(st_local("stats"))) != "")
        {
          /* Finding Stats in List */

            tokens   = tokenbind(input_string)
            statlist = insidepar(tokens, "", "(")
            statlist = subinword(statlist, "p", "pvalue")
            statlist = subinword(statlist, "pval", "pvalue")
            statlist = subinword(statlist, "t", "tz")
            statlist = subinword(statlist, "z", "tz")

            if((len = length(pos1 = selectindex(inlist(statlist, si.name)))) == 0) return

            tokens   = tokens[pos1]
            statlist = statlist[pos1]

          /* Selecting Stats */

            pos1 = poslist(si.name, statlist)

            si.index    = si.index[pos1]
            si.name     = si.name[pos1]
            si.label    = si.label[pos1]
            si.round    = si.round[pos1]
            si.notation = si.notation[.,pos1]
            si.stars    = si.stars[pos1]

          /* Parsing Formats */

            for(i=len; i; i--)
            {
              if((input_string = insidepar(tokens[i], "(", ")")) == "") continue

              subtokens = tokenbind(input_string)

              /* Round */

                if(length(pos2 = selectindex(strpos(subtokens, "round") :== 1)) > 0)
                {
                  values = strtoreal(insidepar(subtokens[pos2[1]], "(", ")"))

                  if(values >= 0 & values <= 7) si.round[i] = values
                }

              /* Notation */

                if(length(pos2 = selectindex(strpos(subtokens, "not") :== 1)) > 0)
                {
                  values = insidepar(subtokens[pos2[1]], "(", ")")

                  if(strpos(values, "par") == 1)      { si.notation[1,i] = "("; si.notation[2,i] = ")"; }
                  else if(strpos(values, "bra") == 1) { si.notation[1,i] = "["; si.notation[2,i] = "]"; }
                }

              /* Stars */

                if(anyof(strpos(subtokens, "star"), 1)) si.stars[i] = 1

              /* CI Specific */

                if(si.name[i] == "ci")
                {
                  if(anyof(strpos(subtokens, "comb"), 1)) si.ci_combined = 1

                  if(length(pos2 = selectindex(strpos(subtokens, "sep") :== 1)) > 0)
                  {
                    values = subtokens[pos2[1]]

                    if(anyof((" ",",","-"), values)) si.ci_separator = values
                  }

                  if(si.ci_combined & si.notation[1,i] == "")
                  {
                    si.notation[1,i] = "["
                    si.notation[2,i] = "]"
                  }
                }
            }
        }

      /* Splitting CI */

        if(anyof(si.name, "ci") & !si.ci_combined)
        {
          pos1 = selectindex(si.name :== "ci")
          pos2 = sort((range(1, length(si.name), 1) \ pos1), 1)

          si.index    = si.index[pos2]
          si.name     = si.name[pos2]
          si.label    = si.label[pos2]
          si.round    = si.round[pos2]
          si.notation = si.notation[.,pos2]
          si.stars    = si.stars[pos2]

          si.name[pos1]  = "lci"
          si.label[pos1] = "Lower CI"
          si.stars[pos1] = 0

          pos1++

          si.index[pos1] = 6
          si.name[pos1]  = "uci"
          si.label[pos1] = "Upper CI"
        }
    }

  /* function : initOptions() */

    void function initOptions(`Bradsto'          matrix bs,
                              struct options  scalar opt,
                              struct statInfo scalar si)
    {
      `String'  input_string
      `Tokens'  tokens, subtokens, statlist
      `Pos'     pos
      `Integer' len

      /* Setting Defaults */

        /* Models */

          opt.models = range(1, length(bs), 1)

        /* Stats */

          opt.stars = (0.05, 0.01, 0.001)

        /* Covariates */

          opt.base      = 1
          opt.omit      = 1
          opt.reference = 1

        /* Labelling */

          opt.varlabels = 1
          opt.refcats   = J(0, 2, "")

        /* Fill for Empty */

          opt.catfill  = "."
          opt.reffill  = "Ref"
          opt.omitfill = "."

        /* Display */

          opt.title = ""
          opt.hsep  = 0
          opt.vsep  = 0
          opt.wide  = 0
          opt.bind  = 0

      /* Parsing Options */

        /* Models */

          if((input_string = st_local("anything")) != "")
          {
            len       = length(bs)
            tokens    = tokens(input_string)
            subtokens = J(1, len, "")

            for(i=len; i; i--) subtokens[i] = bs[i].name

            pos = J(1, len, 0)
            len = length(tokens)

            for(i=len; i; i--) pos = pos :+ strmatch(subtokens, tokens[i])

            if(length(pos = selectindex(pos :> 0)) > 0) opt.models = pos
          }

        /* Stats */

          if((input_string = st_local("stars")) != "") opt.stars = revorder(strtoreal(tokens(input_string)))

        /* Covariates */

          if((input_string = st_local("order"))   != "") opt.covar_order = tokens(input_string)
          if((input_string = st_local("drop"))    != "") opt.covar_drop  = tokens(input_string)
          if((input_string = st_local("keep"))    != "") opt.covar_keep  = tokens(input_string)
          if((input_string = st_local("nobases")) != "") opt.base        = 0
          if((input_string = st_local("noomits")) != "") opt.omit        = 0
          if((input_string = st_local("norefs"))  != "") opt.reference   = 0

        /* Labelling */

          if((input_string = st_local("collabels")) != "")
          {
            tokens = tokens(input_string)

            if(length(tokens) > (len = length(si.label))) tokens = tokens[1..len]

            len = length(tokens)

            for(i=len; i; i--) si.label[i] = tokens[i]
          }

          if((input_string = st_local("mlabels"))     != "") opt.mlabels    = tokens(input_string)
          if((input_string = st_local("novarlabels")) != "") opt.varlabels  = 0

          if((input_string = st_local("refcats")) != "")
          {
            len = length(tokens = tokens(input_string))

            if(mod(len, 2) != 0) tokens = tokens, ""

            opt.refcats = colshape(tokens, 2)
          }

        /* Fill for Empty */

          if((input_string = st_local("catfill"))   != "") opt.catfill  = input_string
          if((input_string = st_local("reffill"))   != "") opt.reffill  = input_string
          if((input_string = st_local("omitfill"))  != "") opt.omitfill = input_string

        /* Display */

          if((input_string = st_local("title"))      != "") opt.title = input_string
          if((input_string = st_local("hseparator")) != "") opt.hsep  = 1
          if((input_string = st_local("vseparator")) != "") opt.vsep  = 1
          if((input_string = st_local("wide"))       != "") opt.wide  = 1
          if((input_string = st_local("bind"))       != "") opt.bind  = 1

          opt.title = getTitle(bs[opt.models], opt)
    }

  /* function : getTitle() */

    `String' getTitle(`Bradsto'         matrix bs,
                      struct options scalar opt)
    {
      `String'    title
      `StringVec' depvars
      `Integer'   len

      title   = opt.title
      depvars = J(1, 0, "")

      if(strlower(title) == "none") return("")
      else if(title != "")          return(title)

      len = length(bs)

      for(i=1; i<=len; i++) if(!anyof(depvars, bs[i].minfo[2])) depvars = depvars, bs[i].minfo[2]

      if(length(depvars) == 1) return(depvars + " - " + st_varlabel(depvars))
      else                     return(invtokens(depvars, ", "))

      return("")
    }

  /* function : initCovars() */

    void function initCovars(`Bradsto'        matrix bs,
                             `Bradout'        scalar bo,
                             struct covars scalar cv)
    {
      `Integer' cvars, mstats, infos, rows // Row Information
      `Integer' models, stats, cols        // Col Information
      `Pos'     rpos1, rpos2               // Row Positions
      `Pos'     cpos1, cpos2               // Col Positions
      `DataMat' cv_levels, cv_labels       // Covariate Information
      `DataMat' temp_table, temp_values    // Temporary Tables
      `Integer' len, i                     // Loops

      /* General Information */

        mstats = length(bo.msi.index)
        infos  = length(bo.mi.index)

        models = length(bs)
        stats  = length(bo.si.name)
        cols   = models * stats

      /* Dropping Empty Model Information */

        if(infos > 0 & !bo.opt.omit)
        {
          temp_values = J(infos, models, "")

          for(i=models; i; i--) temp_values[.,i] = bs[i].minfo[bo.mi.index]

          rpos1 = selectindex(rowsum(temp_values :== "") :!= models)
          infos = length(rpos1)

          if(infos == 0) rpos1 = J(0, 1, .)

          bo.mi.index = bo.mi.index[rpos1]
          bo.mi.name  = bo.mi.name[rpos1]
          bo.mi.slab  = bo.mi.slab[rpos1]
          bo.mi.llab  = bo.mi.llab[rpos1]
        }

      /* Dropping Empty Model Statistics */

        if(mstats > 0 & !bo.opt.omit)
        {
          temp_values = J(mstats, models, .)

          for(i=models; i; i--) temp_values[.,i] = bs[i].mstats[bo.msi.index]

          rpos1  = selectindex(rowsum(temp_values :== .) :!= models)
          mstats = length(rpos1)

          if(mstats == 0) rpos1 = J(0, 1, .)

          bo.msi.index = bo.msi.index[rpos1]
          bo.msi.name  = bo.msi.name[rpos1]
          bo.msi.slab  = bo.msi.slab[rpos1]
          bo.msi.llab  = bo.msi.llab[rpos1]
          bo.msi.round = bo.msi.round[rpos1]
          bo.msi.comma = bo.msi.comma[rpos1]
        }

      /* Initial Covariate Order */

        cv.name = J(0, 1, "")

        for(i=1; i<=models; i++)
        {
          rpos1 = selectindex(!inlist(bs[i].covar_levels, cv.name))
          if(length(rpos1) > 0) cv.name = cv.name \ bs[i].covar_levels[rpos1]
        }

      /* Specified Reorders */

        if((len = length(bo.opt.covar_order)) > 0)
        {
          for(i=len; i; i--)
          {
            rpos1   = strmatch(cv.name, bo.opt.covar_order[i])
            rpos1   = selectindex(rpos1) \ selectindex(!rpos1)
            cv.name = cv.name[rpos1]
          }
        }

        rpos1   = selectindex(cv.name :!= "_cons") \ selectindex(cv.name :== "_cons")
        cv.name = cv.name[rpos1]
        cvars   = length(cv.name)

      /* Covariate Index & Types */

        cv.index    = J(cvars, models, .)
        cv.celltype = J(cvars, cols  , "")

        if(cvars > 0)
        {
          for(i=models; i; i--)
          {
            rpos1 = selectindex(inlist(cv.name, bs[i].covar_levels))
            rpos2 = poslist(bs[i].covar_levels, cv.name[rpos1])
            cpos1 = rangex(1 + (stats * (i - 1)), stats, 1)

            cv.index[rpos1,i]        = rpos2
            cv.celltype[rpos1,cpos1] = J(1, stats, bs[i].covar_types[rpos2])
          }
        }

      /* Dropping Covariates */

        rpos1 = J(cvars, 1, 1)

        /* Omitted */

          if(!bo.opt.omit)
          {
            rpos2 = selectindex((rowsum(cv.celltype :== "o") :+ rowsum(cv.celltype :== "")) :== cols)

            if(length(rpos2) > 0) rpos1[rpos2] = J(length(rpos2), 1, 0)
          }

        /* Base */

          if(!bo.opt.base)
          {
            rpos2 = selectindex((rowsum(cv.celltype :== "b") :+ rowsum(cv.celltype :== "")) :== cols)

            if(length(rpos2) > 0) rpos1[rpos2] = J(length(rpos2), 1, 0)
          }

        /* Specified - Drop */

          if((len = length(bo.opt.covar_drop)) > 0)
          {
            for(i=len; i; i--)
            {
              rpos2 = selectindex(strmatch(cv.name, bo.opt.covar_drop[i]))

              if(length(rpos2) > 0) rpos1[rpos2] = J(length(rpos2), 1, 0)
            }
          }

        /* Specified - Keep */

          if((len = length(bo.opt.covar_keep)) > 0)
          {
            rpos1 = J(cvars, 1, 0)

            for(i=len; i; i--)
            {
              rpos2 = selectindex(strmatch(cv.name, bo.opt.covar_keep[i]))

              if(length(rpos2) > 0) rpos1[rpos2] = J(length(rpos2), 1, 1)
            }
          }

        rpos1       = selectindex(rpos1)
        cvars       = length(rpos1)
        cv.name     = cv.name[rpos1]
        cv.index    = cv.index[rpos1,.]
        cv.celltype = cv.celltype[rpos1,.]

        if(!cvars & !mstats & !infos)
        {
          _error("No covariates, model statistics, or model information selected")
          exit()
        }

      /* Variable Labels, Series Names, & Reference Categories */

        if(cvars > 0)
        {
          /* Variable Labels & Series Names */

            cv_levels = J(cvars, 1, "c"), cv.name, J(1, 3, subinstr(cv.name, "#", " # "))

            if(!allof(cv.name, "_cons"))
            {
              cv_labels      = uniqrows(tokens(invtokens(subinstr(cv.name, "#", " ")'))')
              cv_labels      = cv_labels[selectindex(cv_labels :!= "_cons")]
              cv_labels      = cv_labels, insidepar(cv_labels, ".", ""), insidepar(cv_labels, ".", ""), insidepar(cv_labels, "", ".")
              cv_labels[.,4] = cv_labels[.,4] :* (cv_labels[.,3] :!= cv_labels[.,4])

              if(bo.opt.varlabels)
              {
                cv_labels = cv_labels, (_st_varindex(cv_labels[.,2]')' :!= .) :* cv_labels[.,2]

                /* Categorical Variables */

                  if(length((rpos1 = selectindex(cv_labels[.,4] :!= ""))) > 0)
                  {
                    temp_table = uniqrows(cv_labels[rpos1,3])
                    len        = length(temp_table)

                    for(i=len; i; i--)
                    {
                      rpos2 = rpos1[selectindex(cv_labels[rpos1,3] :== temp_table[i])]

                      if(st_varvaluelabel(temp_table[i]) != "") cv_labels[rpos2,4] = st_vlmap(st_varvaluelabel(temp_table[i]), strtoreal(cv_labels[rpos2,4]))
                      else                                      cv_labels[rpos2,4] = cv_labels[rpos2,1]
                    }
                  }

                /* Continuous Variables */

                  rpos1 = selectindex((cv_labels[.,1] :== cv_labels[.,3]) :& (cv_labels[.,5] :!= ""))

                  if((len = length(rpos1)) > 0)
                  {
                    for(i=len; i; i--) cv_labels[rpos1[i],4] = st_varlabel(cv_labels[rpos1[i],3])
                  }

                /* Series Labels */

                  cv_labels[.,2] = ("n." :* (cv_labels[.,1] :!= cv_labels[.,2])) :+ cv_labels[.,2]
                  temp_table     = uniqrows(cv_labels[selectindex(cv_labels[.,5] :!= ""),3])
                  len            = length(temp_table)

                  for(i=len; i; i--)
                  {
                    rpos1              = selectindex(cv_labels[.,3] :== temp_table[i])
                    cv_labels[rpos1,3] = J(length(rpos1), 1, st_varlabel(temp_table[i]))
                  }

                /* Adding in Missing */

                  if(length((rpos1 = selectindex(cv_labels[.,3] :== ""))) > 0)
                  {
                    cv_labels[rpos1,3] = subinstr(cv_labels[rpos1,2], "n.", "")
                  }

                  if(length((rpos1 = selectindex(cv_labels[.,4] :== ""))) > 0)
                  {
                    cv_labels[rpos1,4] = cv_labels[rpos1,1]
                  }

                /* Filling in Labels */

                  len = rows(cv_labels)

                  for(i=len; i; i--)
                  {
                    cv_levels[.,3] = subinword(cv_levels[.,3], cv_labels[i,1], cv_labels[i,2])
                    cv_levels[.,4] = subinword(cv_levels[.,4], cv_labels[i,1], cv_labels[i,3])
                    cv_levels[.,5] = subinword(cv_levels[.,5], cv_labels[i,1], cv_labels[i,4])
                  }
              }
              else
              {
                cv_labels[.,2] = ("n." :* (cv_labels[.,1] :!= cv_labels[.,2])) :+ cv_labels[.,2]

                len = rows(cv_labels)

                for(i=len; i; i--)
                {
                  cv_levels[.,3] = subinword(cv_levels[.,3], cv_labels[i,1], cv_labels[i,2])
                  cv_levels[.,4] = subinword(cv_levels[.,4], cv_labels[i,1], cv_labels[i,3])
                }
              }
            }

          /* Adding Reference Categories */

            if(bo.opt.reference & !bo.opt.wide)
            {
              /* Manual Input */

                if((len = rows(bo.opt.refcats)) > 0)
                {
                  for(i=len; i; i--)
                  {
                    if(length(rpos1 = selectindex(strmatch(cv_levels[.,2], bo.opt.refcats[i,1]))) > 0)
                    {
                      rpos1 = rpos1[selectindex(rpos1 :== rangex(rpos1[1], length(rpos1), 1))]

                      cv_levels[rpos1,1] = J(length(rpos1), 1, "i")

                      temp_table = "r", J(1, 2, bo.opt.refcats[i,1]), J(1, 2, bo.opt.refcats[i,2])
                      cv_levels  = insertrows(cv_levels, temp_table, rpos1[1])
                    }
                  }
                }

              /* Automatic Categories */

                rpos1 = selectindex(cv_levels[.,1] :== "c")

                if(length(rpos1) > 0)
                {
                  cv_labels = uniqrows(cv_levels[rpos1,3])
                  len       = length(cv_labels)

                  for(i=len; i; i--)
                  {
                    rpos2 = selectindex(cv_levels[.,3] :== cv_labels[i])

                    if(length(rpos2) > 1)
                    {
                      rpos2 = rpos2[selectindex(rpos2 :== rangex(rpos2[1], length(rpos2), 1))]

                      cv_levels[rpos2,1] = J(length(rpos2), 1, "i")
                      rpos2              = rpos2[1]

                      temp_table = "r", J(1, 2, cv_levels[rpos2,3]), J(1, 2, cv_levels[rpos2,4])
                      cv_levels  = insertrows(cv_levels, temp_table, rpos2)
                    }
                  }
                }

              /* Adding to Covariate Information */

                rows  = rows(cv_levels)
                rpos1 = selectindex(cv_levels[.,1] :!= "r")
                rpos2 = selectindex(cv_levels[.,1] :== "r")

                if(length(rpos2) > 0)
                {
                  /* Covariate Types */

                    temp_table = cv.celltype

                    cv.celltype          = J(rows, cols, "")
                    cv.celltype[rpos1,.] = temp_table
                    cv.celltype[rpos2,.] = J(length(rpos2), cols, "r")

                    temp_values = cv.celltype[(2..rows),.] \ cv.celltype[1,.]
                    temp_values = (cv.celltype :!= "r") :| (temp_values :!= "")

                    cv.celltype = cv.celltype :* temp_values

                  /* Covariate Index */

                    temp_values = cv.index

                    cv.index = J(rows, models, .)

                    cv.index[rpos1,.] = temp_values
                }
            }

          /* Adding to Name, Label, & Variable Type */

            cv.name    = cv_levels[.,2]
            cv.label   = cv_levels[.,5]
            cv.vartype = cv_levels[.,1]
        }
        else
        {
          stats = 1
          cols  = models

          cv.label    = J(0, 1   , "")
          cv.vartype  = J(0, 1   , "")
          cv.celltype = J(0, cols, "")
        }

      /* Adding Model Statistics & Information - Long */

        if(!bo.opt.wide & (mstats > 0 | infos > 0))
        {
          /* Index, Name, & Var Type */

            cv.index   = cv.index   \ J(mstats, models, .   )  \ J(infos, models, .   )
            cv.vartype = cv.vartype \ J(mstats,      1, "ms")  \ J(infos, 1     , "mi")
            cv.name    = cv.name    \ bo.msi.name              \ bo.mi.name

          /* Labels */

            if(bo.opt.varlabels)
            {
              len = max((udstrlen(cv.label) \ 8))

              if(mstats > 0)
              {
                temp_table = udstrlen(bo.msi.llab) :> len
                temp_table = (temp_table :* bo.msi.slab) :+ (!temp_table :* bo.msi.llab)
                cv.label   = cv.label \ temp_table
              }

              if(infos > 0)
              {
                temp_table = udstrlen(bo.mi.llab) :> len
                temp_table = (temp_table :* bo.mi.slab) :+ (!temp_table :* bo.mi.llab)
                cv.label   = cv.label \ temp_table
              }
            }
            else
            {
              cv.label = cv.label \ bo.msi.name \ bo.mi.name
            }

          /* Cell Type */

            cpos1 = rangex(1, models, stats)

            temp_table          = J(mstats + infos, cols, "")
            temp_table[.,cpos1] = J(mstats + infos, models, "v")

            cv.celltype = cv.celltype \ temp_table
        }

      /* Adding Model Statistics & Information - Wide */

        if(bo.opt.wide)
        {
          /* Reshaping Cell Types */

            cpos1      = rangex(1, stats, 1)
            temp_table = J(models, cvars * stats, "")

            for(i=models; i; i--)
            {
              cpos2 = cpos1 :+ ((i - 1) * stats)

              temp_table[i,.] = rowshape(cv.celltype[.,cpos2], 1)
            }

            cv.celltype = temp_table

          /* Index, Name, Var Type, & Cell Type */

            cv.index    = J(models, infos, .   ), J(models, mstats, .   ), cv.index'
            cv.vartype  = J(1     , infos, "mi"), J(1     , mstats, "ms"), cv.vartype'
            cv.celltype = J(models, infos, "v" ), J(models, mstats, "v" ), cv.celltype
            cv.name     = bo.mi.name'           , bo.msi.name'           , cv.name'

          /* Labels */

            if(bo.opt.varlabels) cv.label = (bo.mi.llab \ bo.msi.llab \ cv.label)'
            else                 cv.label = (bo.mi.name \ bo.msi.name \ cv.label)'
        }
    }

  /* function : initExcel() */

    void function initExcel(struct excel scalar ex)
    {
      `Tokens' tokens
      `String' input_string, word, path1, path2
      `Real'   value
      `Pos'    pos

      ex.output = (input_string = st_local("excel")) != ""

      if(!ex.output) return

      /* Setting Default Options */

        /* Command Information */
        ex.bookreplace  = 0
        ex.sheetreplace = 0

        /* File Information */
        ex.file_path = pathjoin(c("pwd"), "bradmean_output.xlsx")

        /* Style Information */
        ex.color     = ("228 223 236", "238 236 225")
        ex.font_face = "Calibri"
        ex.font_size = 11

      /* Getting Tokens */

        tokens = tokenbind(input_string)

      /* Mode */

        if(anyof(strpos(tokens, "rep"), 1))      ex.bookreplace  = 1
        if(anyof(strpos(tokens, "sheetrep"), 1)) ex.sheetreplace = 1
        if(anyof(strpos(tokens, "mod"), 1))      ex.bookreplace  = ex.sheetreplace = 0

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

          ex.file_path = pathjoin(path1, path2)
        }

      /* Sheet */

        if(length(pos = selectindex(strpos(tokens, "sheet") :== 1)) > 0)
        {
          ex.sheet = insidepar(tokens[pos[1]], "(", ")")
        }

      /* Colors */

        if(length(pos = selectindex(strpos(tokens, "color") :== 1)) > 0)
        {
          word = insidepar(tokens[pos[1]], "(", ")")

          if(word == "bradfield")            ex.color = ("228 223 236", "238 236 225")
          else if(word == "material_red")    ex.color = ("255 235 238", "235 255 252")
          else if(word == "material_purple") ex.color = ("243 229 245", "231 245 229")
          else if(word == "material_indigo") ex.color = ("232 234 246", "246 244 232")
          else if(word == "material_blue")   ex.color = ("227 242 253", "253 238 227")
          else if(word == "material_green")  ex.color = ("232 245 233", "245 232 244")
          else if(word == "material_orange") ex.color = ("255 243 224", "224 236 255")
          else if(word == "monochrome")      ex.color = ("255 255 255", "255 255 255")
          else if(word == "rti")             ex.color = ("204 220 233", "233 217 204")
        }

      /* Font Face */

        if(length(pos = selectindex(strpos(tokens, "font") :== 1)) > 0)
        {
          word = insidepar(tokens[pos[1]], "(", ")")

          if(word == "arial")          ex.font_face = "Arial"
          else if(word == "calibri")   ex.font_face = "Calibri"
          else if(word == "garamond")  ex.font_face = "Garamond"
          else if(word == "helvetica") ex.font_face = "Helvetica"
          else if(word == "tnr")       ex.font_face = "Times New Roman"
          else if(word == "verdana")   ex.font_face = "Verdana"
        }

      /* Font Size */

        if(length(pos = selectindex(strpos(tokens, "size") :== 1)) > 0)
        {
          value = strtoreal(insidepar(tokens[pos[1]], "(", ")"))

          if(value >= 9 & value <= 12) ex.font_size = value
        }
    }

/*======================================================================*/
/*   Mata Functions - Printer                                           */
/*======================================================================*/

  /* function : printLong */

    void function printLong(`Bradsto' matrix bs,
                            `Bradout' scalar bo)
    {
      `Integer'   cvars, mstats, infos, rows         // Row Information
      `Integer'   models, stats, cols                // Col Information
      `Pos'       pos                                // General Positions
      `Pos'       rpos1, rpos2                       // Row Positions
      `Pos'       cpos1, cpos2                       // Col Positions
      `DataMat'   res_table, temp_table, fmts        // Output Tables
      `DataMat'   values1, values2                   // Value Tables
      `RealVec'   col_lengths, bind_cols, table_nums // Table Information
      `Integer'   len, i, j                          // Loops

      /* General Information */

        cvars  = length(selectindex((bo.cv.vartype :== "i") :| (bo.cv.vartype :== "c")))
        mstats = length(bo.msi.index)
        infos  = length(bo.mi.index)
        rows   = length(bo.cv.name)

        models = length(bs)
        stats  = (cvars > 0) ? length(bo.si.name) : 1
        cols   = models * stats

      /* Placing Values in Results Tables - Covariates */

        res_table = J(rows, cols, "")

        if(cvars > 0)
        {
          fmts = "%32." :+ strofreal(bo.si.round) :+ "f"
          pos  = selectindex((bo.cv.vartype :== "i") :| (bo.cv.vartype :== "c"))

          for(i=models; i; i--)
          {
            /* Getting Positions */

              rpos1 = selectindex(bo.cv.index[.,i] :!= .)

              if(length(rpos1) == 0) continue

              rpos2 = bo.cv.index[rpos1,i]

              cpos1 = rangex(1 + (stats * (i - 1)), stats, 1)
              cpos2 = bo.si.index

            /* Getting Values */

              temp_table = strofreal((values1 = bs[i].results[rpos2,cpos2]), fmts)

              cpos2 = selectindex(bo.si.name :== "ci")

              if(length(cpos2) > 0)
              {
                values2             = strofreal(bs[i].results[rpos2,6], fmts[cpos2])
                temp_table[.,cpos2] = temp_table[.,cpos2] :+ bo.si.ci_separator :+ values2
              }

            /* Adding Notation & Stars */

              temp_table = bo.si.notation[1,.] :+ temp_table :+ bo.si.notation[2,.]

              if(anyof(bo.si.stars, 1))
              {
                len     = length(bo.opt.stars)
                values2 = bs[i].results[rpos2,4]
                cpos2   = selectindex(bo.si.stars)

                for(j=len; j; j--) temp_table[.,cpos2] = temp_table[.,cpos2] :+ ((values2 :< bo.opt.stars[j]) :* uchar(735))
              }

              temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

            /* Placing in Results Table */

              res_table[rpos1,cpos1] = temp_table
          }
        }

      /* Placing Values in Results Tables - Model Stats */

        if(mstats > 0)
        {
          fmts  = "%32." :+ strofreal(bo.msi.round) :+ "f"
          rpos1 = selectindex(bo.cv.vartype :== "ms")
          rpos2 = bo.msi.index

          for(i=models; i; i--)
          {
            cpos1 = 1 + (stats * (i-1))

            res_table[rpos1,cpos1] = strofreal(bs[i].mstats[rpos2], fmts)
          }
        }

      /* Placing Values in Results Tables - Model Info */

        if(infos > 0)
        {
          rpos1 = selectindex(bo.cv.vartype :== "mi")
          rpos2 = bo.mi.index

          for(i=models; i; i--)
          {
            cpos1 = 1 + (stats * (i-1))

            res_table[rpos1,cpos1] = bs[i].minfo[rpos2]
          }
        }

      /* Editing Values for Category, Reference, and Omitted */

        values1   = (bo.cv.celltype :== "r")
        res_table = (values1 :* bo.opt.catfill) :+ (!values1 :* res_table)

        values1   = (bo.cv.celltype :== "b")
        res_table = (values1 :* bo.opt.reffill) :+ (!values1 :* res_table)

        values1   = (bo.cv.celltype :== "o") :| (bo.cv.celltype :== "")
        res_table = (values1 :* bo.opt.omitfill) :+ (!values1 :* res_table)

      /* Adding Stat Labels */

        col_lengths = colmax(udstrlen(res_table))
        cpos1       = selectindex(col_lengths :< 5)

        if((len = length(cpos1)) > 0) col_lengths[cpos1] = J(1, len, 5)

        if(cvars > 0) res_table = abbrevx(J(1, models, bo.si.label), col_lengths) \ res_table

        res_table = " {center " :+ strofreal(col_lengths) :+ ":" :+ res_table :+ "} "

      /* Adding Variable Labels */

        temp_table = bo.cv.label

        len   = max(udstrlen(temp_table)) + 1
        rpos1 = selectindex(bo.cv.vartype :!= "i")
        rpos2 = selectindex(bo.cv.vartype :== "i")

        if(length(rpos1) > 0) temp_table[rpos1] = "{res}{lalign " :+ strofreal(len) :+ ":" :+ substr(temp_table[rpos1], 1, len) :+ "} {c |}"
        if(length(rpos2) > 0) temp_table[rpos2] = "{res}{ralign " :+ strofreal(len) :+ ":" :+ substr(temp_table[rpos2], 1, len) :+ "} {c |}"

        if(cvars > 0) temp_table = ("{res}{space " :+ strofreal(len) :+ "} {c |}") \ temp_table

        res_table   = temp_table, res_table
        col_lengths = len, col_lengths

      /* Adding Model Labels */

        temp_table = J(models, 1, "")

        for(i=models; i; i--) temp_table[i] = bs[i].mlabel

        if((len = length(bo.opt.mlabels)) > 0)
        {
          len = (len > models) ? models : len

          for(i=len; i; i--) temp_table[i] = bo.opt.mlabels[i]
        }

        temp_table = "{res}{space " :+ strofreal(col_lengths[1] :+ 1) :+ "}", rowshape(J(1, stats, temp_table), 1)

      /* Getting Table Numbers & Binding Model Labels */

        bind_cols  = ., rowshape(J(1, stats, rangex(1, models, 1)), 1)
        values1    = J(1, cols(res_table), 0)

        if(bo.opt.vsep)
        {
          rpos1          = rangex(1 + stats, models - 1, stats)'
          values1[rpos1] = values1[rpos1] :+ 1
        }

        if(bo.opt.bind)             table_nums = tablenums(col_lengths :+ values1 :+ 2, bind_cols)
        if(length(table_nums) == 0) table_nums = tablenums(col_lengths :+ values1 :+ 2)

        res_table = bindcols(temp_table, col_lengths, bind_cols, table_nums, "center") \ res_table

      /* Placing Horizontal Separators */

        temp_table    =      "{hline " :+ strofreal(col_lengths    :+ 2) :+ "}"
        temp_table[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c +}"

        if(bo.opt.hsep)
        {
          len     = rows(res_table)
          values1 = bo.cv.vartype
          values2 = "" \ values1[1..(rows(values1)-1)]

          rpos1 = selectindex((values1 :== "r") :| ((values1 :== "c") :& (values2 :== "i"))) :+ 1 :+ (cvars > 0)
          rpos1 = rpos1 \ (2 + (cvars > 0))

          if(infos  > 0) rpos1 = rpos1 \ (len - infos + 1)
          if(mstats > 0) rpos1 = rpos1 \ (len - infos - mstats + 1)

          rpos1 = uniqrows(rpos1)
          len   = length(rpos1)

          for(i=len; i; i--) res_table = insertrows(res_table, temp_table, rpos1[i])
        }
        else
        {
          res_table = insertrows(res_table, temp_table, 2 + (cvars > 0))
        }

        res_table = res_table \ subinstr(temp_table, "+", "BT")

      /* Placing Vertical Separators */

        len = max(table_nums)

        if(bo.opt.vsep & models > 1)
        {
          temp_table = substr(res_table[.,1], strrpos(res_table[.,1], "{"))

          for(i=len; i; i--)
          {
            rpos1 = bind_cols[selectindex(table_nums :== i)]'
            rpos2 = poslist(rpos1, uniqrows(rpos1))

            if(length(rpos2) < 2) continue
            else                  rpos2 = rpos2[2..length(rpos2)]

            rpos1 = selectindex(table_nums :== i) :- 1
            rpos2 = rpos1[rpos2]

            res_table[.,rpos2] = res_table[.,rpos2] :+ temp_table
          }
        }

      /* Printing */

        /* Title */

          if(bo.opt.title != "") printf("\n{title:" + subinstr(subinstr(bo.opt.title, "%", "%%"), "\", "\\") + "}\n")

        /* Table */

          printf("\n")

          for(i=1; i<=len; i++)
          {
            if(len > 1)
            {
              if(i > 1) printf("\n")
              printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
            }

            rpos1 = selectindex(table_nums :== i)
            display(addcols((res_table[.,1], res_table[.,rpos1])))
          }

        /* Footer */

          if(anyof(bo.si.stars, 1))
          {
            temp_table = range(1, length(bo.opt.stars), 1) :* uchar(735)
            temp_table = "{lalign " :+ strofreal(max(udstrlen(temp_table))) :+ ":" :+ temp_table :+ "} p < 0" :+ strofreal(bo.opt.stars)'

            display(temp_table)
          }
    }

  /* function : printWide */

    void function printWide(`Bradsto' matrix bs,
                            `Bradout' scalar bo)
    {
      `Integer'   models, rows                       // Row Information
      `Integer'   infos, mstats, cvars, stats, cols  // Col Information
      `Pos'       pos                                // General Positions
      `Pos'       rpos1, rpos2                       // Row Positions
      `Pos'       cpos1, cpos2                       // Col Positions
      `DataMat'   res_table, temp_table, fmts        // Output Tables
      `DataMat'   values1, values2                   // Value Tables
      `RealVec'   col_lengths, bind_cols, table_nums // Table Information
      `Integer'   len, i, j                          // Loops

      /* General Information */

        models = length(bs)
        rows   = models

        infos  = length(bo.mi.index)
        mstats = length(bo.msi.index)
        cvars  = length(selectindex((bo.cv.vartype :== "i") :| (bo.cv.vartype :== "c")))
        stats  = (cvars > 0) ? length(bo.si.name) : 0
        cols   = mstats + infos + (cvars * stats)

      /* Placing Values in Results Tables - Model Info */

        res_table = J(rows, cols, "")

        if(infos > 0)
        {
          cpos1 = selectindex(bo.cv.vartype :== "mi")
          rpos1 = bo.mi.index

          for(i=models; i; i--) res_table[i,cpos1] = bs[i].minfo[rpos1]'
        }

      /* Placing Values in Results Tables - Model Stats */

        if(mstats > 0)
        {
          fmts  = "%32." :+ strofreal(bo.msi.round) :+ "f"
          cpos1 = selectindex(bo.cv.vartype :== "ms")
          rpos1 = bo.msi.index

          for(i=models; i; i--) res_table[i,cpos1] = strofreal(bs[i].mstats[rpos1], fmts)'
        }

      /* Placing Values in Results Tables - Covariates */

        if(cvars > 0)
        {
          fmts = "%32." :+ strofreal(bo.si.round) :+ "f"
          pos  = rangex(0, stats, 1)'

          for(i=models; i; i--)
          {
            /* Getting Positions */

              cpos1 = selectindex(bo.cv.index[i,.] :!= .)
              cpos2 = bo.si.index

              if(length(cpos1) == 0) continue

              rpos1 = bo.cv.index[i,cpos1]

            /* Getting Values */

              temp_table = strofreal((values1 = bs[i].results[rpos1,cpos2]), fmts)

              cpos2 = selectindex(bo.si.name :== "ci")

              if(length(cpos2) > 0)
              {
                values2             = strofreal(bs[i].results[rpos1,6], fmts[cpos2])
                temp_table[.,cpos2] = temp_table[.,cpos2] :+ bo.si.ci_separator :+ values2
              }

            /* Adding Notation & Stars */

              temp_table = bo.si.notation[1,.] :+ temp_table :+ bo.si.notation[2,.]

              if(anyof(bo.si.stars, 1))
              {
                len     = length(bo.opt.stars)
                values2 = bs[i].results[rpos1,4]
                cpos2   = selectindex(bo.si.stars)

                for(j=len; j; j--) temp_table[.,cpos2] = temp_table[.,cpos2] :+ ((values2 :< bo.opt.stars[j]) :* uchar(735))
              }

              temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* ".")

            /* Placing in Results Table */

              len   = length(cpos1)
              cpos2 = ((cpos1 :- mstats :- infos :- 1) :* (stats :- 1))
              cpos2 = J(1, stats, cpos1') :+ J(1, stats, cpos2') :+ J(len, 1, pos)
              cpos2 = rowshape(cpos2, 1)

              res_table[i,cpos2] = rowshape(temp_table, 1)
          }
        }

      /* Editing Values for Category, Reference, and Omitted */

        values1   = (bo.cv.celltype :== "r")
        res_table = (values1 :* bo.opt.catfill) :+ (!values1 :* res_table)

        values1   = (bo.cv.celltype :== "b")
        res_table = (values1 :* bo.opt.reffill) :+ (!values1 :* res_table)

        values1   = (bo.cv.celltype :== "o") :| (bo.cv.celltype :== "")
        res_table = (values1 :* bo.opt.omitfill) :+ (!values1 :* res_table)

      /* Adding Stat Labels */

        col_lengths = colmax(udstrlen(res_table))
        cpos1       = selectindex(col_lengths :< 5)

        if((len = length(cpos1)) > 0) col_lengths[cpos1] = J(1, len, 5)

        temp_table = J(1, 0, "")

        /* Model Information */

          if(infos > 0)
          {
            cpos1 = rangex(1, infos, 1)
            cpos1 = col_lengths[cpos1] :< udstrlen(bo.mi.llab)'

            values1 = J(1, infos, "")

            if(length((cpos2 = selectindex(cpos1)))  > 0) values1[cpos2] = bo.mi.slab[cpos2]'
            if(length((cpos2 = selectindex(!cpos1))) > 0) values1[cpos2] = bo.mi.llab[cpos2]'

            temp_table = temp_table, values1
          }

        /* Model Stats */

          if(mstats > 0)
          {
            cpos1 = rangex(1 + infos, mstats, 1)
            cpos1 = col_lengths[cpos1] :< udstrlen(bo.msi.llab)'

            values1 = J(1, mstats, "")

            if(length((cpos2 = selectindex(cpos1)))  > 0) values1[cpos2] = bo.msi.slab[cpos2]'
            if(length((cpos2 = selectindex(!cpos1))) > 0) values1[cpos2] = bo.msi.llab[cpos2]'

            temp_table = temp_table, values1
          }

        /* Stats */

          temp_table = temp_table, J(1, cvars, bo.si.label)

        /* Adding to Table */

          temp_table = abbrevx(temp_table, col_lengths)
          res_table  = temp_table \ res_table
          res_table  = " {center " :+ strofreal(col_lengths) :+ ":" :+ res_table :+ "} "

      /* Adding Model Labels */

        temp_table = J(models, 1, "")

        for(i=models; i; i--) temp_table[i] = bs[i].mlabel

        if((len = length(bo.opt.mlabels)) > 0)
        {
          len = (len > models) ? models : len

          for(i=len; i; i--) temp_table[i] = bo.opt.mlabels[i]
        }

        len         = max(udstrlen(temp_table)) + 1
        temp_table  = ("{res}{lalign " :+ strofreal(len) :+ ":" :+ substr(temp_table, 1, len) :+ "} {c |}")
        temp_table  = ("{res}{space "  :+ strofreal(len) :+ "} {c |}") \ temp_table
        res_table   = temp_table, res_table
        col_lengths = len, col_lengths

      /* Adding Covariate Labels */

        temp_table = res_table[1,1], J(1, infos, "Model Information"), J(1, mstats, "Model Statistics")

        if(cvars > 0)
        {
          cpos1      = rangex(1 + infos + mstats, cvars, 1)
          temp_table = temp_table, rowshape(J(1, stats, bo.cv.label[cpos1]'), 1)
        }

      /* Getting Table Numbers & Binding Model Labels */

        bind_cols = ., J(1, infos, 1), J(1, mstats, 1 + (infos > 0)), (rowshape(J(1, stats, rangex(1, cvars, 1)), 1) :+ mstats :+ infos)
        values1   = J(1, cols(res_table), 0)

        if(bo.opt.vsep)
        {
          cpos1 = J(1, 0, .), J(1, infos > 0, infos + 1), J(1, mstats > 0, mstats + infos + 1)

          if(cvars > 0) cpos1 = cpos1, rangex(1 + infos + mstats + stats, cvars - 1, stats)'

          values1[cpos1] = values1[cpos1] :+ 1
        }

        if(bo.opt.bind)             table_nums = tablenums(col_lengths :+ values1 :+ 2, bind_cols)
        if(length(table_nums) == 0) table_nums = tablenums(col_lengths :+ values1 :+ 2)

        res_table = bindcols(temp_table, col_lengths, bind_cols, table_nums, "center") \ res_table

      /* Placing Horizontal Separators */

        temp_table    =      "{hline " :+ strofreal(col_lengths    :+ 2) :+ "}"
        temp_table[1] = "{res}{hline " :+ strofreal(col_lengths[1] :+ 1) :+ "}{c +}"

        res_table = insertrows(res_table, temp_table, 3) \ subinstr(temp_table, "+", "BT")

      /* Placing Vertical Separators */

        len = max(table_nums)

        if(bo.opt.vsep)
        {
          temp_table = substr(res_table[.,1], strrpos(res_table[.,1], "{"))

          for(i=len; i; i--)
          {
            cpos1 = bind_cols[selectindex(table_nums :== i)]'
            cpos2 = poslist(cpos1, uniqrows(cpos1))

            if(length(cpos2) < 2) continue
            else                  cpos2 = cpos2[2..length(cpos2)]

            cpos1 = selectindex(table_nums :== i) :- 1
            cpos2 = cpos1[cpos2]

            res_table[.,cpos2] = res_table[.,cpos2] :+ temp_table
          }
        }

      /* Printing */

        /* Title */

          if(bo.opt.title != "") printf("\n{title:" + subinstr(subinstr(bo.opt.title, "%", "%%"), "\", "\\") + "}\n")

        /* Table */

          printf("\n")

          for(i=1; i<=len; i++)
          {
            if(len > 1)
            {
              if(i > 1) printf("\n")
              printf("(" :+ strofreal(i) :+ "/" :+ strofreal(len) :+ ")\n")
            }

            rpos1 = selectindex(table_nums :== i)
            display(addcols((res_table[.,1], res_table[.,rpos1])))
          }

        /* Footer */

          if(anyof(bo.si.stars, 1))
          {
            temp_table = range(1, length(bo.opt.stars), 1) :* uchar(735)
            temp_table = "{lalign " :+ strofreal(max(udstrlen(temp_table))) :+ ":" :+ temp_table :+ "} p < 0" :+ strofreal(bo.opt.stars)'

            display(temp_table)
          }
    }

/*======================================================================*/
/*   Mata Functions - Excel                                             */
/*======================================================================*/

  /* function : createExcel() */

    void function createExcel(`Bradsto' matrix bs,
                              `Bradout' scalar bo)
    {
      `Boolean' newsheet
      `RealVec' row, col
      `Pos'     pos

      if(!bo.ex.output) return

      /* Initializing Object */

        class xl scalar B

        B = xl()

      /* Loading Book & Setting Worksheet */

        newsheet = bo.ex.bookreplace | bo.ex.sheetreplace

        if(fileexists(bo.ex.file_path))
        {
          /* Loading Book */

            if(bo.ex.bookreplace) B.clear_book(bo.ex.file_path)

            B.load_book(bo.ex.file_path)

          /* Setting Sheet */

            if(bo.ex.sheet == "")
            {
              B.set_sheet(bo.ex.sheet = B.get_sheets()[1])
            }
            else
            {
              if(anyof(B.get_sheets(), bo.ex.sheet))
              {
                B.set_sheet(bo.ex.sheet)
              }
              else
              {
                B.add_sheet(bo.ex.sheet)
                newsheet = 1
              }

              if(bo.ex.bookreplace & bo.ex.sheet != "Sheet1") B.delete_sheet("Sheet1")
            }

          /* Clearing Sheet */

            if(bo.ex.sheetreplace) B.clear_sheet(B.query("sheetname"))
        }
        else
        {
          if(bo.ex.sheet == "") bo.ex.sheet = "Sheet1"

          B.create_book(bo.ex.file_path, bo.ex.sheet, substr(pathsuffix(bo.ex.file_path), 2))

          newsheet = 1
        }

        B.set_mode("open")
        B.set_missing(".")

      /* Getting Initial Position */

        if(bo.ex.bookreplace | bo.ex.sheetreplace)
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

        if(!bo.opt.wide) excelLong(bs, bo, B, row)
        else             excelWide(bs, bo, B, row)

      B.close_book()
    }

  /* function : excelLong */

    void function excelLong(`Bradsto' matrix bs,
                            `Bradout' scalar bo,
                            `Excel'   scalar B,
                            `Integer'        row)
    {
      `Integer'   cvars, mstats, infos, rows         // Information - Row
      `Integer'   models, stats, cols                // Information - Col
      `Pos'       rbound, cbound                     // Table Bounds
      `DataMat'   fonts, table_fmts                  // Table Formats - General
      `DataMat'   stat_fmts, mstat_fmts, info_fmts   // Table Formats - Stats
      `Pos'       rpos1, rpos2                       // Positions - Row
      `Pos'       cpos1, cpos2                       // Positions - Col
      `DataMat'   res_num, res_txt, temp_table, fmts // Output Tables
      `DataMat'   values1, values2                   // Value Tables
      `Integer'   len, i, j                          // Loops

      /* General Information */

        cvars  = length(selectindex((bo.cv.vartype :!= "mi") :& (bo.cv.vartype :!= "ms")))
        mstats = length(bo.msi.index)
        infos  = length(bo.mi.index)
        rows   = length(bo.cv.name)

        models = length(bs)
        stats  = (cvars > 0) ? length(bo.si.name) : 1
        cols   = models * stats
        cbound = 1, 1 + cols

      /* Creating Excel Formats */

        /* Fonts */

          // Regular, Bold
          fonts = J(2, 1, .)

          /* Regular */

            fonts[1] = B.add_fontid()
            B.fontid_set_font(fonts[1], bo.ex.font_face, bo.ex.font_size)

          /* Bold */

            fonts[2] = B.add_fontid()
            B.fontid_set_font(fonts[2], bo.ex.font_face, bo.ex.font_size)
            B.fontid_set_font_bold(fonts[2], "on")

        /* Table Formats */

          // Title, Whitespace, Header, Question, Answer, Legend
          table_fmts = J(6, 1, .)

          /* Title */

            table_fmts[1] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[1], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[1], "left")

          /* Whitespace */

            table_fmts[2] = B.add_fmtid()
            B.fmtid_set_fill_pattern(table_fmts[2], "solid", "white")

          /* Header */

            table_fmts[3] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[3], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[3], "center")
            B.fmtid_set_fill_pattern(table_fmts[3], "solid", bo.ex.color[1])

          /* Question */

            table_fmts[4] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[4], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[4], "left")
            B.fmtid_set_fill_pattern(table_fmts[4], "solid", bo.ex.color[2])

          /* Answer */

            table_fmts[5] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[5], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[5], "right")
            B.fmtid_set_fill_pattern(table_fmts[5], "solid", bo.ex.color[2])

          /* Legend */

            table_fmts[6] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[6], fonts[1])
            B.fmtid_set_horizontal_align(table_fmts[6], "left")

        /* Stat Formats */

          if(cvars > 0)
          {
            values1 = "0" :+ ((bo.si.round :> 0) :* ".") :+ (bo.si.round :* "0")
            values1 = ((bo.si.notation[1,.] :!= "") :* (char(34) :+ bo.si.notation[1,.] :+ char(34))) :+ values1
            values1 = values1 :+ ((bo.si.notation[2,.] :!= "") :* (char(34) :+ bo.si.notation[2,.] :+ char(34)))

            stat_fmts = J(1, stats, .)

            for(i=1; i<=stats; i++)
            {
              if(stat_fmts[i] != .) continue

              cpos1 = selectindex(values1[i] :== values1)
              cpos2 = min(stat_fmts[cpos1])
              len   = length(cpos1)

              if(cpos2 == .)
              {
                stat_fmts[i] = B.add_fmtid()
                B.fmtid_set_fontid(stat_fmts[i], fonts[1])
                B.fmtid_set_vertical_align(stat_fmts[i], "center")
                B.fmtid_set_horizontal_align(stat_fmts[i], "center")
                B.fmtid_set_number_format(stat_fmts[i], values1[i])

                stat_fmts[cpos1] = J(1, len, stat_fmts[i])
              }
              else
              {
                stat_fmts[cpos1] = J(1, len, cpos2)
              }
            }

            stat_fmts = J(1, models, stat_fmts)
          }

        /* Model Stat Formats */

          if(mstats > 0)
          {
            values1 = (bo.msi.comma :* "#,##") :+ "0" :+ ((bo.msi.round :> 0) :* ".") :+ (bo.msi.round :* "0")

            mstat_fmts = J(1, mstats, .)

            for(i=1; i<=mstats; i++)
            {
              if(mstat_fmts[i] != .) continue

              cpos1 = selectindex(values1[i] :== values1)
              cpos2 = min(mstat_fmts[cpos1])
              len   = length(cpos1)

              if(cpos2 == .)
              {
                mstat_fmts[i] = B.add_fmtid()
                B.fmtid_set_fontid(mstat_fmts[i], fonts[1])
                B.fmtid_set_vertical_align(mstat_fmts[i], "center")
                B.fmtid_set_horizontal_align(mstat_fmts[i], "center")
                B.fmtid_set_number_format(mstat_fmts[i], values1[i])

                mstat_fmts[cpos1] = J(1, len, mstat_fmts[i])
              }
              else
              {
                mstat_fmts[cpos1] = J(1, len, cpos2)
              }
            }
          }

        /* Model Info Formats */

          if(infos > 0)
          {
            info_fmts = B.add_fmtid()
            B.fmtid_set_fontid(info_fmts, fonts[1])
            B.fmtid_set_vertical_align(info_fmts, "center")
            B.fmtid_set_horizontal_align(info_fmts, "center")
          }

      /* Placing - Title */

        rpos1 = row

        if(bo.opt.title != "")
        {
          B.put_string(rpos1, 1, bo.opt.title)
          B.set_fmtid(rpos1, 1, table_fmts[1])

          rpos1++
        }

        rbound = rpos1, rpos1 + rows + 1

      /* Placing - Header */

        /* Model Labels */

          temp_table = J(models, 1, "")

          for(i=models; i; i--) temp_table[i] = bs[i].mlabel

          if((len = length(bo.opt.mlabels)) > 0)
          {
            len = (len > models) ? models : len

            for(i=len; i; i--) temp_table[i] = bo.opt.mlabels[i]
          }

          temp_table = rowshape(J(1, stats, temp_table), 1)

          B.put_string(rpos1, 2, temp_table)

          if(stats > 1)
          {
            for(i=1; i<=models; i++)
            {
              cpos1 = 2 + ((i - 1) * stats)
              cpos1 = cpos1, (cpos1 + stats - 1)

              B.set_sheet_merge(bo.ex.sheet, (rpos1,rpos1), cpos1)
            }
          }

          rpos1++

        /* Stat Labels */

          temp_table = J(1, models, bo.si.label)

          B.put_string(rpos1, 2, temp_table)

        /* Formatting Header */

          rpos1 = (rpos1 - 1), rpos1
          cpos1 = 2, cbound[2]

          B.set_fmtid(rpos1, (1,1), table_fmts[2])
          B.set_fmtid(rpos1, cpos1, table_fmts[3])

          rpos1 = rpos1[1], rpos1[1]

          B.set_top_border(rpos1, cbound, "medium")

          rpos1 = rpos1 :+ 1

          B.set_bottom_border(rpos1, cbound, "medium")

          rpos1 = rpos1[1] + 1

      /* Placing - Variables */

        /* Variable Labels */

          temp_table = bo.cv.label

          if(infos > 0)
          {
            rpos2 = selectindex(bo.cv.vartype :== "mi")

            temp_table[rpos2] = bo.mi.llab
          }

          if(mstats > 0)
          {
            rpos2 = selectindex(bo.cv.vartype :== "ms")

            temp_table[rpos2] = bo.msi.llab
          }

          B.put_string(rpos1, 1, temp_table)

        /* Formatting Variables */

          rpos1 = rpos1, (rpos1 + rows - 1)

          B.set_fmtid(rpos1, (1,1), table_fmts[4])

          rpos2 = selectindex(bo.cv.vartype :== "i")

          if((len = length(rpos2)) > 0)
          {
            rpos2 = rpos2 :+ (rpos1[1] - 1)

            for(i=len; i; i--) B.set_fmtid(rpos2[i], 1, table_fmts[5])
          }

          B.set_left_border(rbound, (1,1), "medium")
          B.set_right_border(rbound, (1,1), "medium")

      /* Placing - Stats */

        B.set_missing(bo.opt.omitfill)

        if(cvars > 0)
        {
          /* Initializing Tables */

            fmts = "%32." :+ strofreal(bo.si.round) :+ "f"

            res_num = J(cvars, cols, .)
            res_txt = J(cvars, cols, bo.opt.omitfill)

          /* Getting Values */

            for(i=models; i; i--)
            {
              /* Getting Positions */

                rpos1 = selectindex(bo.cv.index[.,i] :!= .)

                if(length(rpos1) == 0) continue

                rpos2 = bo.cv.index[rpos1,i]

                cpos1 = rangex(1 + (stats * (i - 1)), stats, 1)
                cpos2 = bo.si.index

              /* Getting Values */

                temp_table = strofreal((values1 = bs[i].results[rpos2,cpos2]), fmts)

                cpos2 = selectindex(bo.si.name :== "ci")

                if(length(cpos2) > 0)
                {
                  values2             = strofreal(bs[i].results[rpos2,6], fmts[cpos2])
                  temp_table[.,cpos2] = temp_table[.,cpos2] :+ bo.si.ci_separator :+ values2
                }

              /* Adding Notation & Stars */

                temp_table = bo.si.notation[1,.] :+ temp_table :+ bo.si.notation[2,.]

                if(anyof(bo.si.stars, 1))
                {
                  len     = length(bo.opt.stars)
                  values2 = bs[i].results[rpos2,4]
                  cpos2   = selectindex(bo.si.stars)

                  for(j=len; j; j--) temp_table[.,cpos2] = temp_table[.,cpos2] :+ ((values2 :< bo.opt.stars[j]) :* uchar(735))
                }

                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* bo.opt.omitfill)

              /* Placing in Results Table */

                res_txt[rpos1,cpos1] = temp_table
                res_num[rpos1,cpos1] = values1
            }

          /* Placing Values - Numeric */

            B.set_missing(bo.opt.omitfill)

            rpos1 = rbound[1] + 2
            cpos1 = 2

            B.put_number(rpos1, cpos1, res_num)

          /* Placing Values - String */

            temp_table = J(1, models, (bo.si.name :== "ci")) :| (colmax(ustrpos(res_txt, uchar(735))) :> 0)

            cpos1 = rangex(2, cols, 1)
            rpos1 = (rbound[1] + 2), rbound[2]

            for(i=cols; i; i--)
            {
              cpos2 = cpos1[i], cpos1[i]

              B.set_fmtid(rpos1, cpos2, stat_fmts[i])

              if(temp_table[i]) B.put_string(rpos1[1], cpos2[1], res_txt[.,i])
            }

          /* Filling in Category & Reference */

            rpos1 = rbound[1] + 1

            temp_table = J(1, stats, bo.opt.catfill) \ J(1, stats, bo.opt.reffill)

            for(i=models; i; i--)
            {
              cpos1 = 1 + (stats * (i - 1))
              cpos2 = cpos1 + 1

              /* Category */

                rpos2 = selectindex(bo.cv.celltype[.,cpos1] :== "r")

                if((len = length(rpos2)) > 0)
                {
                  for(j=len; j; j--) B.put_string(rpos1 + rpos2[j], cpos2, temp_table[1,.])
                }

              /* Reference */

                rpos2 = selectindex(bo.cv.celltype[.,cpos1] :== "b")

                if((len = length(rpos2)) > 0)
                {
                  for(j=len; j; j--) B.put_string(rpos1 + rpos2[j], cpos2, temp_table[2,.])
                }
            }

          /* Setting Borders - Internal */

            values1 = bo.cv.vartype[1..cvars]
            values2 = "" \ values1[1..(cvars-1)]

            rpos1 = selectindex((values1 :== "r") :| ((values1 :== "c") :& (values2 :== "i"))) :+ rbound[1]

            if((len = length(rpos1)) > 0)
            {
              for(i=len; i; i--) B.set_bottom_border(J(1, 2, rpos1[i]), cbound, "thin")
            }

          /* Setting Borders - Bottom */

            rpos1 = J(1, 2, (rbound[1] + cvars + 1))

            B.set_bottom_border(rpos1, cbound, "thin")
        }

      /* Placing - Model Stats */

        if(mstats > 0)
        {
          /* Placing Values */

            cpos1   = bo.msi.index
            res_num = J(mstats, cols, .)

            for(i=models; i; i--)
            {
              cpos2 = 1 + (stats * (i - 1))

              res_num[.,cpos2] = bs[i].mstats[cpos1]
            }

            rpos1 = rbound[1] + 2 + cvars

            B.put_number(rpos1, 2, res_num)

          /* Formatting Values */

            cpos2 = 2, cbound[2]

            for(i=mstats; i; i--)
            {
              rpos2 = J(1, 2, (rpos1 + i - 1))

              B.set_fmtid(rpos2, cpos2, mstat_fmts[i])
            }

          /* Setting Border */

            rpos1 = J(1, 2, (rbound[2] - infos))

            B.set_bottom_border(rpos1, cbound, "thin")
        }

      /* Placing - Model Info */

        if(infos > 0)
        {
          /* Placing Values */

            cpos1   = bo.mi.index
            res_txt = J(infos, cols, bo.opt.omitfill)

            for(i=models; i; i--)
            {
              cpos2 = 1 + (stats * (i - 1))

              res_txt[.,cpos2] = bs[i].minfo[cpos1]
            }

            rpos1 = rbound[1] + 2 + cvars + mstats

            B.put_string(rpos1, 2, res_txt)

          /* Formatting Values */

            rpos2 = rpos1, rbound[2]
            cpos2 = 2, cbound[2]

            B.set_fmtid(rpos2, cpos2, info_fmts)
        }

      /* Setting Vertical Borders */

        if((len = models - 1) > 0)
        {
          cpos1 = rangex(1 + stats, len, stats)

          for(i=len; i; i--) B.set_right_border(rbound, J(1, 2, cpos1[i]), "thin")
        }

        B.set_right_border(rbound, J(1, 2, cbound[2]), "medium")

      /* Setting Bottom Border */

        B.set_bottom_border(J(1, 2, rbound[2]), cbound, "medium")

      /* Placing - Footer */

        if(anyof(bo.si.stars, 1))
        {
          rpos1 = rbound[2] + 1
          len   = length(bo.opt.stars)

          values1 = rangex(1, len, 1)
          values2 = ((len + 1) :- values1)

          temp_table = (values1 :* uchar(735)) :+ (values2 :* " ")
          temp_table = temp_table :+ "p(overall) < 0" :+ strofreal(bo.opt.stars)'

          B.put_string(rpos1, 1, temp_table)

          rpos1 = rpos1, (rpos1 + len - 1)

          B.set_fmtid(rpos1, (1,1), table_fmts[6])
        }
    }

  /* function : excelWide */

    void function excelWide(`Bradsto' matrix bs,
                            `Bradout' scalar bo,
                            `Excel'   scalar B,
                            `Integer'        row)
    {
      `Integer'   models, rows                       // Information - Row
      `Integer'   infos, mstats, cvars, stats, cols  // Information - Col
      `Pos'       rbound, cbound                     // Table Bounds
      `DataMat'   fonts, table_fmts                  // Table Formats - General
      `DataMat'   stat_fmts, mstat_fmts, info_fmts   // Table Formats - Stats
      `Pos'       rpos1, rpos2                       // Positions - Row
      `Pos'       cpos1, cpos2                       // Positions - Col
      `DataMat'   res_num, res_txt, temp_table, fmts // Output Tables
      `DataMat'   values1, values2                   // Value Tables
      `Integer'   len, i, j                          // Loops

      /* General Information */

        models = length(bs)
        rows   = models

        infos  = length(bo.mi.index)
        mstats = length(bo.msi.index)
        cvars  = length(selectindex((bo.cv.vartype :!= "mi") :& (bo.cv.vartype :!= "ms")))
        stats  = (cvars > 0) ? length(bo.si.name) : 1
        cols   = infos + mstats + (cvars * stats)
        cbound = 1, 1 + cols

      /* Creating Excel Formats */

        /* Fonts */

          // Regular, Bold
          fonts = J(2, 1, .)

          /* Regular */

            fonts[1] = B.add_fontid()
            B.fontid_set_font(fonts[1], bo.ex.font_face, bo.ex.font_size)

          /* Bold */

            fonts[2] = B.add_fontid()
            B.fontid_set_font(fonts[2], bo.ex.font_face, bo.ex.font_size)
            B.fontid_set_font_bold(fonts[2], "on")

        /* Table Formats */

          // Title, Whitespace, Header, Model Label, Legend
          table_fmts = J(5, 1, .)

          /* Title */

            table_fmts[1] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[1], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[1], "left")

          /* Whitespace */

            table_fmts[2] = B.add_fmtid()
            B.fmtid_set_fill_pattern(table_fmts[2], "solid", "white")

          /* Header */

            table_fmts[3] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[3], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[3], "center")
            B.fmtid_set_fill_pattern(table_fmts[3], "solid", bo.ex.color[1])

          /* Model Label */

            table_fmts[4] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[4], fonts[2])
            B.fmtid_set_horizontal_align(table_fmts[4], "left")
            B.fmtid_set_fill_pattern(table_fmts[4], "solid", bo.ex.color[2])

          /* Legend */

            table_fmts[5] = B.add_fmtid()
            B.fmtid_set_fontid(table_fmts[5], fonts[1])
            B.fmtid_set_horizontal_align(table_fmts[5], "left")

        /* Model Info Formats */

          if(infos > 0)
          {
            info_fmts = B.add_fmtid()
            B.fmtid_set_fontid(info_fmts, fonts[1])
            B.fmtid_set_vertical_align(info_fmts, "center")
            B.fmtid_set_horizontal_align(info_fmts, "center")
          }

        /* Model Stat Formats */

          if(mstats > 0)
          {
            values1 = (bo.msi.comma :* "#,##") :+ "0" :+ ((bo.msi.round :> 0) :* ".") :+ (bo.msi.round :* "0")

            mstat_fmts = J(1, mstats, .)

            for(i=1; i<=mstats; i++)
            {
              if(mstat_fmts[i] != .) continue

              cpos1 = selectindex(values1[i] :== values1)
              cpos2 = min(mstat_fmts[cpos1])
              len   = length(cpos1)

              if(cpos2 == .)
              {
                mstat_fmts[i] = B.add_fmtid()
                B.fmtid_set_fontid(mstat_fmts[i], fonts[1])
                B.fmtid_set_vertical_align(mstat_fmts[i], "center")
                B.fmtid_set_horizontal_align(mstat_fmts[i], "center")
                B.fmtid_set_number_format(mstat_fmts[i], values1[i])

                mstat_fmts[cpos1] = J(1, len, mstat_fmts[i])
              }
              else
              {
                mstat_fmts[cpos1] = J(1, len, cpos2)
              }
            }
          }

        /* Stat Formats */

          values1 = "0" :+ ((bo.si.round :> 0) :* ".") :+ (bo.si.round :* "0")
          values1 = ((bo.si.notation[1,.] :!= "") :* (char(34) :+ bo.si.notation[1,.] :+ char(34))) :+ values1
          values1 = values1 :+ ((bo.si.notation[2,.] :!= "") :* (char(34) :+ bo.si.notation[2,.] :+ char(34)))

          stat_fmts = J(1, stats, .)

          for(i=1; i<=stats; i++)
          {
            if(stat_fmts[i] != .) continue

            cpos1 = selectindex(values1[i] :== values1)
            cpos2 = min(stat_fmts[cpos1])
            len   = length(cpos1)

            if(cpos2 == .)
            {
              stat_fmts[i] = B.add_fmtid()
              B.fmtid_set_fontid(stat_fmts[i], fonts[1])
              B.fmtid_set_vertical_align(stat_fmts[i], "center")
              B.fmtid_set_horizontal_align(stat_fmts[i], "center")
              B.fmtid_set_number_format(stat_fmts[i], values1[i])

              stat_fmts[cpos1] = J(1, len, stat_fmts[i])
            }
            else
            {
              stat_fmts[cpos1] = J(1, len, cpos2)
            }
          }

          stat_fmts = J(1, cvars, stat_fmts)

      /* Placing - Title */

        rpos1 = row

        if(bo.opt.title != "")
        {
          B.put_string(rpos1, 1, bo.opt.title)
          B.set_fmtid(rpos1, 1, table_fmts[1])

          rpos1++
        }

        rbound = rpos1, rpos1 + rows + 1

      /* Placing - Header */

        cpos1 = 2

        /* Model Info */

          if(infos > 0)
          {
            rpos2      = selectindex(bo.cv.vartype :== "mi")
            temp_table = J(1, infos, "Model Information") \ bo.cv.label[rpos2]

            B.put_string(rpos1, cpos1, temp_table)

            cpos1 = cpos1, (cpos1 + infos - 1)

            B.set_sheet_merge(bo.ex.sheet, (rpos1,rpos1), cpos1)

            cpos1 = cpos1[1] + infos
          }

        /* Model Stats */

          if(mstats > 0)
          {
            rpos2      = selectindex(bo.cv.vartype :== "ms")
            temp_table = J(1, mstats, "Model Statistics") \ bo.cv.label[rpos2]

            B.put_string(rpos1, cpos1, temp_table)

            cpos1 = cpos1, (cpos1 + mstats - 1)

            B.set_sheet_merge(bo.ex.sheet, (rpos1,rpos1), cpos1)

            cpos1 = cpos1[1] + mstats
          }

        /* Covariates */

          if(cvars > 0)
          {
            rpos2      = selectindex((bo.cv.vartype :!= "mi") :& (bo.cv.vartype :!= "ms"))
            temp_table = rowshape(J(1, stats, bo.cv.label[rpos2]'), 1) \ J(1, cvars, bo.si.label)

            B.put_string(rpos1, cpos1, temp_table)

            for(i=1; i<=cvars; i++)
            {
              cpos1 = cpos1, (cpos1 + stats - 1)

              B.set_sheet_merge(bo.ex.sheet, (rpos1,rpos1), cpos1)

              cpos1 = cpos1[2] + 1
            }
          }

        /* Formatting */

          rpos1 = rpos1, (rpos1 + 1)
          cpos1 = 2, cbound[2]

          B.set_fmtid(rpos1, (1,1), table_fmts[2])
          B.set_fmtid(rpos1, cpos1, table_fmts[3])

          B.set_top_border((rpos1[1],rpos1[1]), cbound, "medium")
          B.set_bottom_border((rpos1[2],rpos1[2]), cbound, "medium")

          rpos1 = rpos1[2] + 1

      /* Placing - Model Labels */

        /* Placing Values */

          temp_table = J(models, 1, "")

          for(i=models; i; i--) temp_table[i] = bs[i].mlabel

          B.put_string(rpos1, 1, temp_table)

        /* Formatting */

          rpos1 = rpos1, (rpos1 + models - 1)

          B.set_fmtid(rpos1, (1,1), table_fmts[4])

          B.set_left_border(rbound, (1,1), "medium")
          B.set_right_border(rbound, (1,1), "medium")

          rpos1 = rpos1[1]

      /* Placing - Model Info */

        B.set_missing(bo.opt.omitfill)

        if(infos > 0)
        {
          /* Placing Values */

            cpos1   = 2
            cpos2   = bo.mi.index
            res_txt = J(models, infos, bo.opt.omitfill)

            for(i=models; i; i--) res_txt[i,.] = bs[i].minfo[cpos2]'

            B.put_string(rpos1, cpos1, res_txt)

          /* Formatting Values */

            rpos2 = rpos1, (rpos1 + models - 1)
            cpos2 = cpos1, (cpos1 + infos - 1)

            B.set_fmtid(rpos2, cpos2, info_fmts)

            B.set_right_border(rbound, (cpos2[2],cpos2[2]), "thin")
        }

      /* Placing - Model Stats */

        if(mstats > 0)
        {
          /* Placing Values */

            cpos1   = 2 + infos
            cpos2   = bo.msi.index
            res_num = J(models, mstats, .)

            for(i=models; i; i--) res_num[i,.] = bs[i].mstats[cpos2]'

            B.put_number(rpos1, cpos1, res_num)

          /* Formatting Values */

            rpos2 = rpos1, (rpos1 + models - 1)
            cpos2 = (cpos1, cpos1) :- 1

            for(i=mstats; i; i--) B.set_fmtid(rpos2, (cpos2 :+ i), mstat_fmts[i])

            cpos2 = cpos2 :+ mstats

            B.set_right_border(rbound, cpos2, "thin")
        }

      /* Placing - Stats */

        if(cvars > 0)
        {
          /* Initializing Tables */

            fmts = "%32." :+ strofreal(bo.si.round) :+ "f"

            res_num = J(models, (cvars * stats), .)
            res_txt = J(models, (cvars * stats), bo.opt.omitfill)

          /* Getting Values */

            for(i=models; i; i--)
            {
              /* Getting Positions */

                cpos1 = selectindex(bo.cv.index[i,.] :!= .)
                cpos2 = bo.si.index

                if(length(cpos1) == 0) continue

                rpos1 = bo.cv.index[i,cpos1]

              /* Getting Values */

                temp_table = strofreal((values1 = bs[i].results[rpos1,cpos2]), fmts)

                cpos2 = selectindex(bo.si.name :== "ci")

                if(length(cpos2) > 0)
                {
                  values2             = strofreal(bs[i].results[rpos1,6], fmts[cpos2])
                  temp_table[.,cpos2] = temp_table[.,cpos2] :+ bo.si.ci_separator :+ values2
                }

              /* Adding Notation & Stars */

                temp_table = bo.si.notation[1,.] :+ temp_table :+ bo.si.notation[2,.]

                if(anyof(bo.si.stars, 1))
                {
                  len     = length(bo.opt.stars)
                  values2 = bs[i].results[rpos1,4]
                  cpos2   = selectindex(bo.si.stars)

                  for(j=len; j; j--) temp_table[.,cpos2] = temp_table[.,cpos2] :+ ((values2 :< bo.opt.stars[j]) :* uchar(735))
                }

                temp_table = ((values1 :!= .) :* temp_table) :+ ((values1 :== .) :* bo.opt.omitfill)

              /* Placing in Results Table */

                cpos1 = (cpos1 :- mstats :- infos)'
                cpos1 = cpos1 :+ ((cpos1 :- 1) :* (stats - 1))
                cpos1 = J(1, stats, cpos1) :+ J(rows(cpos1), 1, rangex(0, stats, 1)')
                cpos1 = rowshape(cpos1, 1)

                res_txt[i,cpos1] = rowshape(temp_table, 1)
                res_num[i,cpos1] = rowshape(values1, 1)
            }

          /* Placing Values - Numeric */

            rpos1 = rbound[1] + 2
            cpos1 = 2 + infos + mstats

            B.put_number(rpos1, cpos1, res_num)

          /* Placing Values - String */

            temp_table = J(1, cvars, (bo.si.name :== "ci")) :| (colmax(ustrpos(res_txt, uchar(735))) :> 0)

            len   = cvars * stats
            rpos1 = rpos1, (rpos1 + models - 1)
            cpos1 = rangex(cpos1, len, 1)

            for(i=len; i; i--)
            {
              cpos2 = J(1, 2, cpos1[i])

              B.set_fmtid(rpos1, cpos2, stat_fmts[i])

              if(temp_table[i]) B.put_string(rpos1[1], cpos2[1], res_txt[.,i])
            }

          /* Filling in Reference */

            rpos1 = rangex(rpos1[1], models, 1)
            cpos1 = rangex(1 + infos + mstats, cvars, stats)'

            for(i=models; i; i--)
            {
              cpos2 = cpos1[selectindex(bo.cv.celltype[i,cpos1] :== "b")] :+ 1

              if((len = length(cpos2)) > 0)
              {
                for(j=len; j; j--) B.put_string(rpos1[i], cpos2[j], J(1, stats, bo.opt.reffill))
              }
            }

          /* Setting Borders */

            cpos1 = cpos1 :+ stats

            for(i=cvars; i; i--) B.set_right_border(rbound, (cpos1[i],cpos1[i]), "thin")
        }

      /* Setting Overall Borders */

        B.set_right_border(rbound, (cbound[2],cbound[2]), "medium")
        B.set_bottom_border((rbound[2],rbound[2]), cbound, "medium")

      /* Placing - Footer */

        if(anyof(bo.si.stars, 1))
        {
          rpos1 = rbound[2] + 1
          len   = length(bo.opt.stars)

          values1 = rangex(1, len, 1)
          values2 = ((len + 1) :- values1)

          temp_table = (values1 :* uchar(735)) :+ (values2 :* " ")
          temp_table = temp_table :+ "p(overall) < 0" :+ strofreal(bo.opt.stars)'

          B.put_string(rpos1, 1, temp_table)

          rpos1 = rpos1, (rpos1 + len - 1)

          B.set_fmtid(rpos1, (1,1), table_fmts[5])
        }
    }

end
