version 15.1
set matastrict on

/*======================================================================*/
/*======================================================================*/
/*                                                                      */
/*   Program:      bradsuite.mata                                       */
/*   Purpose:      General functions & aliases for bradsuite programs   */
/*   Programmers:  Brian Bradfield                                      */
/*   Version:      0.1.0                                                */
/*   Date:         03/26/2021                                           */
/*                                                                      */
/*======================================================================*/
/*======================================================================*/

/*======================================================================*/
/*   Mata Aliases                                                       */
/*======================================================================*/

    // General - Numeric
    local Real      real scalar
    local RealVec   real vector
    local RealMat   real matrix

    // General - String
    local String    string scalar
    local StringVec string vector
    local StringMat string matrix

    // General - Transmorphic
    local Data    transmorphic scalar
    local DataVec transmorphic vector
    local DataMat transmorphic matrix

    // Type - Numbers
    local Boolean real scalar
    local Integer real scalar

    // Type - Positions
    local Pos real vector

    // Type - Tokens
    local Tokens string vector

    // Structures
    local Bradsto struct bradsto
    local Bradout struct bradout

    // Classes
    local Excel class xl

mata:

/*======================================================================*/
/*   Functions - List Searches                                          */
/*======================================================================*/

  /* function : anylist() */

    `Boolean' anylist(`DataMat' haystack,
                      `DataMat' needle)
    {
      `Integer' rows, cols

      rows = rows(needle)
      cols = cols(needle)

      for(i=rows; i; i--) for(j=cols; j; j--) if(anyof(haystack, needle[i,j])) return(1)

      return(0)
    }

  /* function : inlist() */

    `RealMat' inlist(`DataMat' haystack,
                     `DataMat' needle)
    {
      `RealMat' values
      `Integer' rows, cols
      `Integer' i, j

      rows   = rows(haystack)
      cols   = cols(haystack)
      values = J(rows, cols, 0)

      for(i=rows; i; i--) for(j=cols; j; j--) values[i,j] = anyof(needle, haystack[i,j])

      return(values)
    }

  /* function : poslist() */

    `RealVec' poslist(`DataVec'  haystack,
                      `DataVec'  needle)
    {
      `RealVec' pos, output
      `Integer' len, i

      output = J(rows(needle), cols(needle), .)
      len    = length(needle)

      for(i=len; i; i--)
      {
        if(length(pos = selectindex(haystack :== needle[i])) > 0)
        {
          output[i] = pos[1]
        }
      }

      return(output[selectindex(output :!= .)])
    }

/*======================================================================*/
/*   Functions - Extended Functions                                     */
/*======================================================================*/

  /* function : abbrevx() */

    `StringVec' abbrevx(`StringVec' istrings,
                        `RealVec'   ilens)
    {
      `StringVec' ostrings
      `Pos'       pos, pos1, pos2

      ostrings = istrings

      pos1 = udstrlen(istrings) :> ilens
      pos2 = ((ilens :>= 5) :& (strpos(istrings, ".") :== 0)) :| ((ilens :>= 8) :& (strpos(istrings, ".") :!= 0))
      pos2 = (ilens :<= 32) :& pos2

      if(length(pos = selectindex(pos1 :&  pos2)) > 0) ostrings[pos] = abbrev(ostrings[pos], ilens[pos])
      if(length(pos = selectindex(pos1 :& !pos2)) > 0) ostrings[pos] = substr(ostrings[pos], 1, ilens[pos])

      return(ostrings)
    }

  /* function : rangex() */

    `RealVec' rangex(`Real' start,
                     `Real' steps,
                     `Real' interval)
    {
      return(range(start, start + ((steps - 1) * interval), interval))
    }

/*======================================================================*/
/*   Functions - String Functions                                       */
/*======================================================================*/

  /* function : gentokens() */

    `Tokens' gentokens(`String' istring)
    {
      `Tokens' tokens

      t = tokeninit(" ", "", (`""""', `"`""'"', "()"), 1)
      tokenset(t, istring)
      tokens = tokengetall(t)

      return(tokens)
    }

  /* function : insidepar() */

    `StringVec' insidepar(`StringVec' istring,
                          `String'    istart,
                          `String'    iend)
    {
      `Integer' rows, cols
      `Pos'     spos, epos, pos

      rows = rows(istring)
      cols = cols(istring)

      if(istart == "")
      {
        spos = J(rows, cols, 1)
        epos = strpos(istring, iend)

        if(len = length(pos = selectindex(epos :== 0)))
        {
          if(rows == 1) epos[pos] = J(1, len, .)
          else          epos[pos] = J(len, 1, .)
        }
      }
      else if(iend == "")
      {
        spos = strpos(istring, istart) :+ 1
        epos = J(rows, cols, .)
      }
      else
      {
        spos = strpos(istring, istart) :+ 1
        epos = strrpos(istring, iend)
      }

      return(substr(istring, spos, epos :- spos))
    }

  /* function : tokenbind() */

    `Tokens' tokenbind(`String' istring)
    {
      `Tokens'  tokens
      `RealVec' index
      `Pos'     pos

      tokens = gentokens(istring)

      if(length(pos = selectindex(index = strpos(tokens, "("))) > 0)
      {
        tokens[pos :- 1] = tokens[pos :- 1] :+ tokens[pos]
        tokens = tokens[selectindex(!index)]
      }

      return(tokens)
    }

/*======================================================================*/
/*   Functions - Table Functions                                        */
/*======================================================================*/

  /* function : addcols() */

    `StringVec' addcols(`StringMat' istring)
    {
      `StringVec' values
      `Integer'   rows, cols
      `Integer'   i

      rows = rows(istring)
      cols = cols(istring)

      if(cols == 1) return(istring)
      if(rows == 1) return(invtokens(istring, sep))

      values = istring[.,1]
      for(i=2; i<=cols; i++) values = values :+ istring[.,i]

      return(values)
    }

  /* function : bindcols() */

    `StringMat' bindcols(`StringMat' istrings,
                         `RealMat'   ilens,
                         `RealMat'   bcols,
                         `RealVec'   tnums,
                         `String'    align)
    {
      `StringMat' ostrings
      `Integer'   rows, cols
      `RealVec'   breaks, values
      `Integer'   max, len, comblen
      `Pos'       pos
      `Integer'   i, j

      rows = rows(istrings)
      cols = cols(istrings)

      ostrings = J(rows, cols, "")

      breaks    = tnums[1], tnums[1..(cols-1)]
      breaks    = breaks :!= tnums
      breaks[1] = 0

      for(i=rows; i; i--)
      {
        len = length(pos = selectindex(bcols[i,.] :== .))

        if(len > 0) ostrings[i,pos] = istrings[i,pos]

        if(len == cols) continue

        values = bcols[i,.] :+ runningsum(breaks)
        max    = max(values)

        for(j=max; j; j--)
        {
          len = length(pos = selectindex(values :== j))

          if(len == 0) continue

          if(len == 1)
          {
            ostrings[i,pos] = " {" + align + " " + strofreal(ilens[pos]) + ":" + abbrevx(istrings[i,pos], ilens[pos]) + "} "
          }
          else
          {
            comblen = sum(ilens[pos]) + (2 * (len - 1))
            ostrings[i,pos[1]] = " {center " + strofreal(comblen) + ":" + abbrevx(istrings[i,pos[1]], comblen) + "} "
          }
        }
      }

      ostrings[.,1] = J(rows, 1, "{res}{space " + strofreal(ilens[1] + 1) + "}{c |}")

      return(ostrings)
    }

  /* function : insertrows() */

    `DataMat' insertrows(`DataMat' imatrix,
                         `DataMat' irows,
                         `Integer' pos)
    {
      return((pos > 1 ? (imatrix[(1..(pos-1)),.] \ irows) : irows) \ imatrix[(pos..rows(imatrix)),.])
    }

  /* function : tablenums() */

    `RealVec' tablenums(`RealVec' ilens,
                      | `RealVec' ibind)
    {
      `RealVec' table_lens, table_nums
      `Pos'     pos1, pos2, pos3
      `Integer' linesize
      `Integer' i

      linesize = c("linesize")
      linesize = linesize < 120 ? 120 : linesize
      linesize = linesize > 250 ? 250 : linesize

      table_lens    = runningsum(ilens)
      table_nums    = J(1, length(ilens), .)
      table_nums[1] = 0
      pos1          = selectindex(table_nums :== .)

      i = 1

      if(args() == 1)
      {
        do
        {
          table_lens       = ilens[1] :+ runningsum(ilens[pos1])
          pos1             = pos1[selectindex(table_lens :< linesize)]
          table_nums[pos1] = J(1, length(pos1), i)

          i++
        } while(length(pos1 = selectindex(table_nums :== .)) > 0)
      }
      else
      {
        do
        {
          table_lens = ilens[1] :+ runningsum(ilens[pos1])

          pos2 = selectindex(table_lens :< linesize)
          pos3 = selectindex(table_lens :>= linesize)
          pos1 = pos1[pos2[selectindex(!inlist(ibind[pos1[pos2]], ibind[pos1[pos3]]))]]

          if(length(pos1) == 0) return(J(0,1,.))

          table_nums[pos1] = J(1, length(pos1), i)

          i++
        } while(length(pos1 = selectindex(table_nums :== .)) > 0)
      }

      return(table_nums)
    }

/*======================================================================*/
/*   Functions - Other Functions                                        */
/*======================================================================*/

  /* function : checkerr() */

    void function checkerr(`Integer' errcode)
    {
      if(errcode == 0) return

      if(errcode == 101)
      {
        errprintf("{error:weights not allowed with the svy prefix}\n")
        errprintf("{error:the {bf:svy} prefix assumes survey weights were already specified using {bf:svyset}}\n")
        exit(101)
      }

      if(errcode == 102)
      {
        errprintf("{error:no numeric variables specified}\n")
        exit(102)
      }

      if(errcode == 119)
      {
        errprintf("{error:data not set up for svy, use {helpb svyset}}\n")
        exit(119)
      }

      if(errcode == 198)
      {
        errprintf("{error:option vce() of mean is not allowed with the svy prefix}\n")
        exit(198)
      }

      if(errcode == 908)
      {
        errprintf("{error:matsize too small}\n")
        exit(908)
      }

      exit(error(errcode))
    }

/*======================================================================*/
/*   Structures - bradsto                                               */
/*======================================================================*/

  /* struct : bradsto */

    struct bradsto
    {
      /* General Model Information */
      `String'    name
      `String'    mlabel
      `StringVec' minfo

      /* Covariate Details */
      `StringVec' covar_names  // names of covariates
      `StringVec' covar_levels // levels of covariates
      `StringVec' covar_types  // types of covariates (Base, Omitted, Variable)

      /* Results */
      `RealVec' mstats
      `RealMat' results
    }

end
