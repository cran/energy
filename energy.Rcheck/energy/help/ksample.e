ksample.e               package:energy               R Documentation

_E-_s_t_a_t_i_s_t_i_c (_E_n_e_r_g_y _S_t_a_t_i_s_t_i_c) _f_o_r _M_u_l_t_i_v_a_r_i_a_t_e _k-_s_a_m_p_l_e _T_e_s_t _o_f _E_q_u_a_l _D_i_s_t_r_i_b_u_t_i_o_n_s

_D_e_s_c_r_i_p_t_i_o_n:

     Returns the E-statistic (energy statistic) for the multivariate
     k-sample test of equal distributions.

_U_s_a_g_e:

      ksample.e(x, sizes, distance = FALSE, ix = 1:sum(sizes), 
                incomplete = FALSE, N = 100)

_A_r_g_u_m_e_n_t_s:

       x: data matrix of pooled sample

   sizes: vector of sample sizes

distance: logical: if TRUE, x is a distance matrix

      ix: a permutation of the row indices of x 

incomplete: logical: if TRUE, compute incomplete E-statistics

       N: incomplete sample size

_D_e_t_a_i_l_s:

     The k-sample multivariate E-statistic for testing equal
     distributions is returned. The statistic is computed from the
     original pooled samples, stacked in  matrix 'x' where each row is
     a multivariate observation, or from the distance  matrix 'x' of
     the original data. The first 'sizes[1]' rows of 'x' are the first
     sample, the next 'sizes[2]' rows of 'x' are the second sample,
     etc.

     The two-sample E-statistic proposed by Szekely and Rizzo (2003) is
     the e-distance e(S_i,S_j), defined for two samples S_i, S_j of
     size n_i, n_j by

       e(S_i, S_j) = (n_i n_j)(n_i+n_j)[2M_(ij)-M_(ii)-M_(jj)],

     where

     M_{ij} = 1/(n_i n_j) sum[1:n_i, 1:n_j] ||X_(ip) - X_(jq)||,

     || || denotes Euclidean norm, and X_(ip) denotes the p-th
     observation in the i-th sample.   The k-sample   E-statistic is
     defined by summing the pairwise e-distances over  all k(k-1)/2
     pairs  of samples:

                      _E_ = sum[i<j] e(S_i,S_j).

     Large values of _E_ are significant.

     If 'incomplete==TRUE', an incomplete E-statistic (which is an
     incomplete V-statistic) is computed. That is, at most 'N'
     observations from each sample are used,  by sampling without
     replacement as needed.

_V_a_l_u_e:

     The value of the multisample E-statistic corresponding to the
     permutation 'ix' is returned.

_N_o_t_e:

     This function computes the E-statistic only.  For the test
     decision, a nonparametric bootstrap test (approximate permutation
     test) is provided by the function 'eqdist.etest'.

_A_u_t_h_o_r(_s):

     Maria Rizzo rizzo@math.ohiou.edu

_R_e_f_e_r_e_n_c_e_s:

     Szekely, G. J. and Rizzo, M. L. (2003) Testing for Equal
     Distributions in High Dimension, submitted.

     Szekely, G. J. (2000) E-statistics: Energy of  Statistical
     Samples, preprint.

_S_e_e _A_l_s_o:

     'eqdist.etest'

_E_x_a_m_p_l_e_s:

     ## compute 3-sample E-statistic for 4-dimensional iris data
      data(iris)
      ksample.e(iris[,1:4], c(50,50,50))

     ## compute univariate two-sample incomplete E-statistic
      x1 <- rnorm(200)
      x2 <- rnorm(300, .5)
      x <- c(x1, x2)
      ksample.e(x, c(200, 300), incomplete=TRUE, N=100)
      

