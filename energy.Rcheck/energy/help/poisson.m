poisson.m               package:energy               R Documentation

_M_e_a_n _D_i_s_t_a_n_c_e _S_t_a_t_i_s_t_i_c _f_o_r _T_e_s_t_i_n_g _P_o_i_s_s_o_n _D_i_s_t_r_i_b_u_t_i_o_n

_D_e_s_c_r_i_p_t_i_o_n:

     Returns the mean distance statistic for a goodness-of-fit test of
     Poisson distribution with unknown parameter.

_U_s_a_g_e:

     poisson.m(x)

_A_r_g_u_m_e_n_t_s:

       x: vector of nonnegative integers, the sample data 

_D_e_t_a_i_l_s:

     The mean distance test of Poissonity was proposed and implemented
     by Szekely and Rizzo (2004). The test is based on the result that
     the sequence of expected values E|X-j|, j=0,1,2,... characterizes
     the distribution of the random variable X. As an application of
     this characterization one can get an estimator hat F(j) of the
     CDF. The test statistic is a Cramer-von Mises type of distance,
     with M-estimates replacing the usual EDF estimates of the CDF: 

 M_n = n sum [j>=0] (hat F(j) - F(j; hat lambda))^2 f(j; hat lambda).

     See Szekely and Rizzo (2004) for the computing formula.

_V_a_l_u_e:

     The value of the M-statistic for testing Poisson distribution is
     returned.

_A_u_t_h_o_r(_s):

     Maria Rizzo rizzo@math.ohiou.edu

_R_e_f_e_r_e_n_c_e_s:

     Szekely, G. J. and Rizzo, M. L. (2004) Mean Distance Test of
     Poisson Distribution, _Statistics and Probability Letters_, to
     appear.

_S_e_e _A_l_s_o:

     'poisson.mtest'

_E_x_a_m_p_l_e_s:

      x <- rpois(20, 1)
     poisson.m(x)
      

