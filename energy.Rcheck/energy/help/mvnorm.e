mvnorm.e               package:energy               R Documentation

_E-_s_t_a_t_i_s_t_i_c (_E_n_e_r_g_y _S_t_a_t_i_s_t_i_c) _f_o_r _T_e_s_t_i_n_g _M_u_l_t_i_v_a_r_i_a_t_e _N_o_r_m_a_l_i_t_y

_D_e_s_c_r_i_p_t_i_o_n:

     Computes the E-statistic (energy statistic) for testing
     multivariate  or univariate normality when parameters are
     estimated.

_U_s_a_g_e:

     mvnorm.e(x)

_A_r_g_u_m_e_n_t_s:

       x: matrix or vector of sample data

_D_e_t_a_i_l_s:

     If 'x' is a matrix, each row is a multivariate observation. The
     data will be standardized to zero mean and identity covariance
     matrix using the sample mean vector and sample covariance matrix.
     If 'x' is a vector, the univariate statistic 'normal.e(x)' is
     returned.  If the data contains missing values or the sample
     covariance matrix is  singular, NA is returned.

     The E-test of multivariate normality was proposed and implemented
     by Szekely and Rizzo (2004). The test statistic for  d-variate
     normality is given by

 E = n((2/n) sum[1:n] E||y_i-Z|| - E||Z-Z'|| - (1/n^2) sum[1:n,1:n] ||y_i-y_j||),

     where y_1,...,y_n is the standardized sample,  Z, Z' are iid
     standard d-variate normal, and || || denotes Euclidean norm.

_V_a_l_u_e:

     The value of the E-statistic for multivariate (univariate)
     normality is returned.

_A_u_t_h_o_r(_s):

     Maria Rizzo rizzo@math.ohiou.edu

_R_e_f_e_r_e_n_c_e_s:

     Szekely, G. J. and Rizzo, M. L. (2004) A New Test for 
     Multivariate Normality, _Journal of Multivariate Analysis_, to
     appear.

     Rizzo, M. L. (2002). A New Rotation Invariant Goodness-of-Fit
     Test, Ph.D. dissertation, Bowling Green State University.

     Szekely, G. J. (1989) Potential and Kinetic Energy in Statistics, 
     Lecture Notes, Budapest Institute of Technology (Technical
     University).

_S_e_e _A_l_s_o:

     'normal.e'

_E_x_a_m_p_l_e_s:

      
      ## compute multivariate normality test statistic for iris Setosa data
      data(iris)
      mvnorm.e(iris[1:50, 1:4])
      

