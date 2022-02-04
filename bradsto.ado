version 15.1
#delimit;
include bradsuite.mata, adopath;

**======================================================================**
**======================================================================**
**                                                                      **
**   Program:      bradsto.ado                                          **
**   Purpose:      Stores estimates for use in bradout                  **
**   Programmers:  Brian Bradfield                                      **
**   Version:      1.0.0                                                **
**   Date:         02/03/2022                                           **
**                                                                      **
**======================================================================**
**======================================================================**;

/*======================================================================*/
/*   Stata Functions - bradsto                                          */
/*======================================================================*/

  program define bradsto, nclass sortpreserve;
  args;

  *----------------------------------------------------------*
  *   01. Command - Clear                                    *
  *----------------------------------------------------------*;

    local complete = 0;

    if(`"`0'"' == "clear")
    {;
      cap mata: mata drop bs;
      local complete = 1;
    };

  *----------------------------------------------------------*
  *   02. Command - Directory                                *
  *----------------------------------------------------------*;

  *----------------------------------------------------------*
  *   03. Command - Drop                                     *
  *----------------------------------------------------------*;

  *----------------------------------------------------------*
  *   04. Command - Store                                    *
  *----------------------------------------------------------*;

    if(`complete' == 0)
    {;
      mata: _parse_colon("hascolon", "rhscmd");

      cap mata: length(bs);

      if(_rc != 0)
      {;
        mata: bs1 = initBradsto(0);
        mata: bs  = combBradsto(bs1);
      };
      else
      {;
        mata: bs1 = initBradsto(length(bs));
        mata: bs  = combBradsto(bs1, bs);
      };

      mata: mata drop bs1;
    };

  end;

#delimit cr

mata:

/*======================================================================*/
/*   Mata Functions - Initializing Bradsto                              */
/*======================================================================*/

  /* function : initBradsto() */

    `Bradsto' initBradsto(`Integer' models)
    {
      `Bradsto'   bs_new
      `String'    command
      `StringMat' options
      `Integer'   n, n_pop, n_strata, n_clust, n_psu, n_cds, n_cdf
      `Integer'   df_m, mss, df_r, rss, rmse, r2, r2_a, r2_p, ll, ll_0, aic, bic
      `Integer'   f, chi2, p, k, ic, rank
      `RealMat'   mat_results
      `Pos'       pos
      `Integer'   len, rc, i

      /* Initializing */

        bs_new        = bradsto()
        bs_new.minfo  = J(7, 1, "")
        bs_new.mstats = J(72, 1, .)

      /* Parsing Options */

        command = st_local("rhscmd")
        options = subinstr(st_local("0"), char(34), "")

        if(strlen(options) > 0)
        {
          options = tokenbind(options)'
          options = insidepar(options, "", "("), insidepar(options, "(", ")")
          len     = rows(options)

          for(i=len; i; i--)
          {
            if(options[i,1] == "mlabel") bs_new.mlabel   = options[i,2]
            if(options[i,1] == "prefix") bs_new.name     = options[i,2]
            if(options[i,1] == "mgroup") bs_new.minfo[1] = options[i,2]
          }
        }

        if(bs_new.name == "") bs_new.name = "est"

        bs_new.name = bs_new.name + strofreal(models + 1)

        if(bs_new.mlabel == "") bs_new.mlabel = bs_new.name

      /* Running Command */

        rc = _stata(command)

        if(rc != 0) exit(error(rc))

      /* General Model Information */

        bs_new.minfo[2] = st_global("e(depvar)")
        bs_new.minfo[3] = st_global("e(cmd)")
        bs_new.minfo[4] = st_global("e(model)")
        bs_new.minfo[5] = st_global("e(family)")
        bs_new.minfo[6] = st_global("e(link)")
        bs_new.minfo[7] = st_global("e(weight)")

      /* Covariate Details */

        stata("local covars : colnames r(table)", 1)

        bs_new.covar_names  = tokens(st_local("covars"))'
        bs_new.covar_levels = subinstr(bs_new.covar_names , "b.", ".")
        bs_new.covar_levels = subinstr(bs_new.covar_levels, "o.", ".")
        bs_new.covar_levels = subinstr(bs_new.covar_levels, "c.", "")
        bs_new.covar_types  = J(length(bs_new.covar_names), 1, "v")

        pos = selectindex(strpos(bs_new.covar_names, "#") :!= 0)
        if((len = length(pos)) > 0)
        {
          for(i=len; i; i--)
          {
            if(allof(strpos(tokens(subinstr(bs_new.covar_names[pos[i]], "#", " ")), "b.") :!= 0, 1))
            {
              bs_new.covar_types[pos[i]] = "b"
            }
          }
        }

        pos = selectindex((strpos(bs_new.covar_names, "b.") :!= 0) :& (strpos(bs_new.covar_names, "#") :== 0))
        if((len = length(pos)) > 0)
        {
          bs_new.covar_types[pos] = J(len, 1, "b")
        }

        pos = selectindex(strpos(bs_new.covar_names, "o.") :!= 0)
        if((len = length(pos)) > 0)
        {
          bs_new.covar_types[pos] = J(len, 1, "o")
        }

      /* Scalars - Model Statistics */

        n             = length(st_numscalar("e(N)"))             == 0 ? 0 : st_numscalar("e(N)")
        n_sub         = length(st_numscalar("e(N_sub)"))         == 0 ? . : st_numscalar("e(N_sub)")
        n_yes         = .
        n_pop         = length(st_numscalar("e(N_pop)"))         == 0 ? . : st_numscalar("e(N_pop)")
        n_subpop      = length(st_numscalar("e(N_subpop)"))      == 0 ? . : st_numscalar("e(N_subpop)")
        n_psu         = length(st_numscalar("e(N_psu)"))         == 0 ? . : st_numscalar("e(N_psu)")
        n_strata      = length(st_numscalar("e(N_strata)"))      == 0 ? . : st_numscalar("e(N_strata)")
        n_strata_omit = length(st_numscalar("e(N_strata_omit)")) == 0 ? . : st_numscalar("e(N_strata_omit)")
        n_clust       = length(st_numscalar("e(N_clust)"))       == 0 ? . : st_numscalar("e(N_clust)")
        n_cds         = length(st_numscalar("e(N_cds)"))         == 0 ? 0 : st_numscalar("e(N_cds)")
        n_cdf         = length(st_numscalar("e(N_cdf)"))         == 0 ? 0 : st_numscalar("e(N_cdf)")
        n_g           = length(st_numscalar("e(N_g)"))           == 0 ? 0 : st_numscalar("e(N_g)")
        g_min         = length(st_numscalar("e(g_min)"))         == 0 ? 0 : st_numscalar("e(g_min)")
        g_avg         = length(st_numscalar("e(g_avg)"))         == 0 ? 0 : st_numscalar("e(g_avg)")
        g_max         = length(st_numscalar("e(g_max)"))         == 0 ? 0 : st_numscalar("e(g_max)")
        tbar          = length(st_numscalar("e(Tbar)"))          == 0 ? 0 : st_numscalar("e(Tbar)")
        n_drop        = length(st_numscalar("e(N_drop)"))        == 0 ? 0 : st_numscalar("e(N_drop)")
        n_group_drop  = length(st_numscalar("e(N_group_drop)"))  == 0 ? 0 : st_numscalar("e(N_group_drop)")
        mss           = length(st_numscalar("e(mss)"))           == 0 ? . : st_numscalar("e(mss)")
        rss           = length(st_numscalar("e(rss)"))           == 0 ? . : st_numscalar("e(rss)")
        tss           = length(st_numscalar("e(tss)"))           == 0 ? . : st_numscalar("e(tss)")
        df_m          = length(st_numscalar("e(df_m)"))          == 0 ? . : st_numscalar("e(df_m)")
        df_r          = length(st_numscalar("e(df_r)"))          == 0 ? . : st_numscalar("e(df_r)")
        df_pear       = length(st_numscalar("e(df_pear)"))       == 0 ? . : st_numscalar("e(df_pear)")
        df_a          = length(st_numscalar("e(df_a)"))          == 0 ? . : st_numscalar("e(df_a)")
        df_b          = length(st_numscalar("e(df_b)"))          == 0 ? . : st_numscalar("e(df_b)")
        rmse          = length(st_numscalar("e(rmse)"))          == 0 ? . : st_numscalar("e(rmse)")
        rank          = length(st_numscalar("e(rank)"))          == 0 ? . : st_numscalar("e(rank)")
        rank0         = length(st_numscalar("e(rank0)"))         == 0 ? . : st_numscalar("e(rank0)")
        rho           = length(st_numscalar("e(rho)"))           == 0 ? . : st_numscalar("e(rho)")
        sigma_u       = length(st_numscalar("e(sigma_u)"))       == 0 ? . : st_numscalar("e(sigma_u)")
        sigma         = length(st_numscalar("e(sigma)"))         == 0 ? . : st_numscalar("e(sigma)")
        sigma_e       = length(st_numscalar("e(sigma_e)"))       == 0 ? . : st_numscalar("e(sigma_e)")
        deviance      = length(st_numscalar("e(deviance)"))      == 0 ? . : st_numscalar("e(deviance)")
        dispers       = length(st_numscalar("e(dispers)"))       == 0 ? . : st_numscalar("e(dispers)")
        ecorr         = length(st_numscalar("e(corr)"))          == 0 ? . : st_numscalar("e(corr)")
        r2            = length(st_numscalar("e(r2)"))            == 0 ? . : st_numscalar("e(r2)")
        r2_a          = length(st_numscalar("e(r2_a)"))          == 0 ? . : st_numscalar("e(r2_a)")
        r2_p          = length(st_numscalar("e(r2_p)"))          == 0 ? . : st_numscalar("e(r2_p)")
        r2_w          = length(st_numscalar("e(r2_w)"))          == 0 ? . : st_numscalar("e(r2_w)")
        r2_o          = length(st_numscalar("e(r2_o)"))          == 0 ? . : st_numscalar("e(r2_o)")
        r2_b          = length(st_numscalar("e(r2_b)"))          == 0 ? . : st_numscalar("e(r2_b)")
        ll            = length(st_numscalar("e(ll)"))            == 0 ? . : st_numscalar("e(ll)")
        ll_0          = length(st_numscalar("e(ll_0)"))          == 0 ? . : st_numscalar("e(ll_0)")
        ll_c          = length(st_numscalar("e(ll_c)"))          == 0 ? . : st_numscalar("e(ll_c)")
        aic           = .
        bic           = .
        thta_min      = length(st_numscalar("e(thta_min)"))      == 0 ? . : st_numscalar("e(thta_min)")
        thta_5        = length(st_numscalar("e(thta_5)"))        == 0 ? . : st_numscalar("e(thta_5)")
        thta_50       = length(st_numscalar("e(thta_50)"))       == 0 ? . : st_numscalar("e(thta_50)")
        thta_95       = length(st_numscalar("e(thta_95)"))       == 0 ? . : st_numscalar("e(thta_95)")
        thta_max      = length(st_numscalar("e(thta_max)"))      == 0 ? . : st_numscalar("e(thta_max)")
        f             = length(st_numscalar("e(F)"))             == 0 ? . : st_numscalar("e(F)")
        f_f           = length(st_numscalar("e(F_f)"))           == 0 ? . : st_numscalar("e(F_f)")
        p             = length(st_numscalar("e(p)"))             == 0 ? . : st_numscalar("e(p)")
        p_f           = length(st_numscalar("e(p_f)"))           == 0 ? . : st_numscalar("e(p_f)")
        chi2          = length(st_numscalar("e(chi2)"))          == 0 ? . : st_numscalar("e(chi2)")
        chi2_c        = length(st_numscalar("e(chi2_c)"))        == 0 ? . : st_numscalar("e(chi2_c)")
        chi2_dev      = length(st_numscalar("e(chi2_dev)"))      == 0 ? . : st_numscalar("e(chi2_dev)")
        chi2_dis      = length(st_numscalar("e(chi2_dis)"))      == 0 ? . : st_numscalar("e(chi2_dis)")
        tol           = length(st_numscalar("e(tol)"))           == 0 ? . : st_numscalar("e(tol)")
        dif           = length(st_numscalar("e(dif)"))           == 0 ? . : st_numscalar("e(dif)")
        k             = length(st_numscalar("e(k)"))             == 0 ? . : st_numscalar("e(k)")
        k_aux         = length(st_numscalar("e(k_aux)"))         == 0 ? . : st_numscalar("e(k_aux)")
        k_eq          = length(st_numscalar("e(k_eq)"))          == 0 ? . : st_numscalar("e(k_eq)")
        k_eq_model    = length(st_numscalar("e(k_eq_model)"))    == 0 ? . : st_numscalar("e(k_eq_model)")
        k_dv          = length(st_numscalar("e(k_dv)"))          == 0 ? . : st_numscalar("e(k_dv)")
        n_quad        = length(st_numscalar("e(n_quad)"))        == 0 ? . : st_numscalar("e(n_quad)")
        phi           = length(st_numscalar("e(phi)"))           == 0 ? . : st_numscalar("e(phi)")
        stages        = length(st_numscalar("e(stages)"))        == 0 ? . : st_numscalar("e(stages)")
        ic            = length(st_numscalar("e(ic)"))            == 0 ? . : st_numscalar("e(ic)")

        if(p == .) p = Ftail(df_m, df_r, f)

      /* Results - Table */

        bs_new.results = st_matrix("r(table)")'

      /* Scalars - Observations (Postestimation) */

        rc = _stata("assert missing(" + bs_new.minfo[2] + ") | inlist(" + bs_new.minfo[2] + ",0,1)", 1)

        if(rc == 0)
        {
          rc    = _stata("count if " + bs_new.minfo[2] + " == 1 & e(sample)", 1)
          n_yes = st_numscalar("r(N)")
        }

      /* Scalars - Goodness of Fit (Postestimation) */

        rc = _stata("estat ic", 1)

        if(rc == 0)
        {
          mat_results = st_matrix("r(S)")

          if(ll_0 == .) ll_0 = mat_results[2]

          aic = mat_results[5]
          bic = mat_results[6]
        }

      /* Scalars - Placing in Matrix */

        bs_new.mstats[1]  = n             // number of observations
        bs_new.mstats[2]  = n_sub         // number of subpopulation observations
        bs_new.mstats[3]  = n_yes         // number of 1s in a 0/1 variable
        bs_new.mstats[4]  = n_pop         // estimate of population size
        bs_new.mstats[5]  = n_subpop      // estimate of population size
        bs_new.mstats[6]  = n_psu         // number of primary sampling units
        bs_new.mstats[7]  = n_strata      // number of strata
        bs_new.mstats[8]  = n_strata_omit // number of strata omitted
        bs_new.mstats[9]  = n_clust       // number of clusters
        bs_new.mstats[10] = n_cds         // number of completely determined successes
        bs_new.mstats[11] = n_cdf         // number of completely determined failures
        bs_new.mstats[12] = n_cds + n_cdf // number of completely determined observations
        bs_new.mstats[13] = n_g           // number of groups
        bs_new.mstats[14] = g_min         // smallest group size
        bs_new.mstats[15] = g_avg         // average group size
        bs_new.mstats[16] = g_max         // largest group size
        bs_new.mstats[17] = tbar          // harmonic mean of group sizes
        bs_new.mstats[18] = n_drop        // number of observations dropped because of all positive or all negative outcomes
        bs_new.mstats[19] = n_group_drop  // number of groups dropped because of all positive or all negative outcomes

        bs_new.mstats[20] = mss           // model sum of squares
        bs_new.mstats[21] = rss           // residual sum of squares
        bs_new.mstats[22] = tss           // total sum of squares
        bs_new.mstats[23] = df_m          // model degrees of freedom
        bs_new.mstats[24] = df_r          // residual degrees of freedom
        bs_new.mstats[25] = df_pear       // degrees of freedom for Pearson chi-squared
        bs_new.mstats[26] = df_a          // degrees of freedom for absorbed effect
        bs_new.mstats[27] = df_b          // numerator degrees of freedom for F statistic
        bs_new.mstats[28] = rmse          // root mean squared error
        bs_new.mstats[29] = rank          // rank of e(V)
        bs_new.mstats[30] = rank0         // rank of e(V) for constant-only model
        bs_new.mstats[31] = rho           // rho
        bs_new.mstats[32] = sigma_u       // panel-level standard deviation
        bs_new.mstats[33] = sigma         // ancillary parameter (gamma, lnormal)
        bs_new.mstats[34] = sigma_e       // standard deviation of epsilon_it
        bs_new.mstats[35] = deviance      // deviance
        bs_new.mstats[36] = dispers       // deviance dispersion
        bs_new.mstats[37] = ecorr         // corr(u_i, Xb)

        bs_new.mstats[38] = r2            // R-squared
        bs_new.mstats[39] = r2_a          // adjusted R-squared
        bs_new.mstats[40] = r2_p          // pseudo R-squared
        bs_new.mstats[41] = r2_w          // R-squared within model
        bs_new.mstats[42] = r2_o          // R-squared overall model
        bs_new.mstats[43] = r2_b          // R-squared between model
        bs_new.mstats[44] = ll            // log likelihood
        bs_new.mstats[45] = ll_0          // log likelihood, constant-only model
        bs_new.mstats[46] = ll_c          // log likelihood, comparison model
        bs_new.mstats[47] = aic           // Akaike information criterion
        bs_new.mstats[48] = bic           // Bayesian information criterion
        bs_new.mstats[49] = thta_min      // minimum theta
        bs_new.mstats[50] = thta_5        // theta, 5th percentile
        bs_new.mstats[51] = thta_50       // theta, 50th percentile
        bs_new.mstats[52] = thta_95       // theta, 95th percentile
        bs_new.mstats[53] = thta_max      // maximum theta

        bs_new.mstats[54] = f             // F statistic
        bs_new.mstats[55] = f_f           // F statistic for test of u_i=0
        bs_new.mstats[56] = p             // p-value for model test
        bs_new.mstats[57] = p_f           // p-value for test of u_i=0
        bs_new.mstats[58] = chi2          // chi-squared
        bs_new.mstats[59] = chi2_c        // chi-squared for comparison test
        bs_new.mstats[60] = chi2_dev      // chi-squared test of deviance
        bs_new.mstats[61] = chi2_dis      // chi-squared test of deviance dispersion
        bs_new.mstats[62] = tol           // target tolerance
        bs_new.mstats[63] = dif           // achieved tolerance

        bs_new.mstats[64] = k             // number of parameters
        bs_new.mstats[65] = k_aux         // number of auxiliary parameters
        bs_new.mstats[66] = k_eq          // number of equations in e(b)
        bs_new.mstats[67] = k_eq_model    // number of equations in overall model test
        bs_new.mstats[68] = k_dv          // number of dependent variables
        bs_new.mstats[69] = n_quad        // number of quadrature points
        bs_new.mstats[70] = phi           // scale parameter
        bs_new.mstats[71] = stages        // number of sampling stages
        bs_new.mstats[72] = ic            // number of iterations

      /* Model Information - Svy Weight */

        if(strpos(st_global("e(prefix)"), "svy") != 0)
        {
          rc = _stata("svyset", 1)
          bs_new.minfo[7] = st_global("r(wvar)")
        }

      return(bs_new)
    }

  /* function : combBradsto() */

    `Bradsto' combBradsto(`Bradsto' bs_new, | `Bradsto' bs_old)
    {
      if(args() == 1) return(bs_new)
      else            return(bs_old \ bs_new)
    }

end
