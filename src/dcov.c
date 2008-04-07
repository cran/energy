/*
   dcov.c: distance correlation and covariance statistics
   and dCov test for multivariate independence

   Szekely, G.J., Rizzo, M.L., and Bakirov, N.K. (2007)
   "Measuring and testing dependence by correlation of distances"
   Annals of Statistics, Vol. 35 No. 6, pp. 2769-2794.

   Software: Maria Rizzo     mrizzo at bgsu.edu
             URL: personal.bgsu.edu/~mrizzo

   Notes:
   1. The distance covariance dCov is not the test
   statistic. The test statistic is the V-statistic
   n*dCov^2 or nV^2. We use dCov^2 in the test
   and return the estimates dCov, dCor, dVarX, dVarY
   in dCOVtest.
   2. dCOVtest is much faster than dCovTest
   The two methods of computing dCov^2 are algebraically
   equivalent. dCovTest is not used in the dcov package
   but kept here for validation and historical reasons.
   Also note that the returned objects are different
   types.
*/

#include <R.h>
#include <Rmath.h>

void   dCOVtest(double *x, double *y, int *byrow, int *dims,
                double *index, double *reps, double *DCOV,
                double *pval);
void   dCovTest(double *x, double *y, int *byrow, int *dims,
                double *index, double *reps, double *Dstat,
                double *pval);

void   dCOV(double *x, double *y, int *byrow, int *dims,
            double *index, int *idx, double *DCOV);
void   Akl(double **akl, double **A, int *n);

/* functions in utilities.c */
extern double **alloc_matrix(int r, int c);
extern int    **alloc_int_matrix(int r, int c);
extern void   free_matrix(double **matrix, int r, int c);
extern void   free_int_matrix(int **matrix, int r, int c);
extern void   permute(int *J, int n);
extern void   roworder(double *x, int *byrow, int r, int c);
extern void   Euclidean_distance(double *x, double **Dx, int n, int d);
extern void   index_distance(double *x, double **Dx, int n, int d, double index);


void dCOVtest(double *x, double *y, int *byrow, int *dims,
              double *index, double *reps,
              double *DCOV, double *pval) {
    /*  computes dCov(x,y), dCor(x,y), dVar(x), dVar(y)
        V-statistic is n*dCov^2 where n*dCov^2 --> Q
        dims[0] = n (sample size)
        dims[1] = p (dimension of X)
        dims[2] = q (dimension of Y)
        dims[3] = R (number of replicates)
        index : exponent for distance
        DCOV  : vector [dCov, dCor, dVar(x), dVar(y)]
     */
    int    i, j, k, n, n2, p, q, r, J, K, M, N, R;
    int*   perm;
    double **Dx, **Dy, **A, **B;
    double dcov, V;

    n = dims[0];
    p = dims[1];
    q = dims[2];
    R = dims[3];
    N = n * n;

    if (*byrow == FALSE) {
        /* avoid this step: use as.double(t(x)) in R */
        roworder(x, byrow, n, p);
        *byrow = FALSE;  /* false for y */
        roworder(y, byrow, n, q);
    }

    Dx = alloc_matrix(n, n);
    index_distance(x, Dx, n, p, *index);
    A = alloc_matrix(n, n);
    Akl(Dx, A, dims);
    free_matrix(Dx, n, n);
    Dy = alloc_matrix(n, n);
    index_distance(y, Dy, n, q, *index);
    B = alloc_matrix(n, n);
    Akl(Dy, B, dims);
    free_matrix(Dy, n, n);

    n2 = ((double) n) * n;
    /* compute dCov(x,y), dVar(x), dVar(y) */
    for (k=0; k<4; k++)
        DCOV[k] = 0.0;
    for (k=0; k<n; k++)
        for (j=0; j<n; j++) {
            DCOV[0] += A[k][j]*B[k][j];
            DCOV[2] += A[k][j]*A[k][j];
            DCOV[3] += B[k][j]*B[k][j];
        }

    for (k=0; k<4; k++) {
        DCOV[k] /= n2;
        DCOV[k] = sqrt(DCOV[k]);
    }
    /* compute dCor(x, y) */
    V = DCOV[2]*DCOV[3];
    if (V > DBL_EPSILON)
        DCOV[1] = DCOV[0] / sqrt(V);
        else DCOV[1] = 0.0;

    /* compute the replicates */
    if (R > 0) {
        perm = Calloc(n, int);
        M = 0;
        for (i=0; i<n; i++) perm[i] = i;
        for (r=0; r<R; r++) {
           permute(perm, n);
           dcov = 0.0;
           for (k=0; k<n; k++) {
               K = perm[k];
               for (j=0; j<n; j++) {
                   J = perm[j];
                   dcov += A[k][j]*B[K][J];
               }
           }
           dcov /= n2;
           dcov = sqrt(dcov);
           reps[r] = dcov;
           if (dcov >= DCOV[0]) M++;
        }
        *pval = (double) M / (double) (R);
        Free(perm);
    }

    free_matrix(A, n, n);
    free_matrix(B, n, n);
    return;
}

void dCOV(double *x, double *y, int *byrow, int *dims,
              double *index, int *idx, double *DCOV) {
    /*  computes dCov(x,y), dCor(x,y), dVar(x), dVar(y)
        V-statistic is n*dCov^2 where n*dCov^2 --> Q
        dims[0] = n (sample size)
        dims[1] = p (dimension of X)
        dims[2] = q (dimension of Y)
        index : exponent for distance
        idx   : index vector, a permutation of sample indices
        DCOV  : vector [dCov, dCor, dVar(x), dVar(y)]
     */
    int    j, k, n, n2, p, q, N;
    double **Dx, **Dy, **A, **B;
    double V;

    n = dims[0];
    p = dims[1];
    q = dims[2];
    N = n * n;

    if (*byrow == FALSE) {
        /* avoid this step: use as.double(t(x)) in R */
        roworder(x, byrow, n, p);
        *byrow = FALSE;  /* false for y */
        roworder(y, byrow, n, q);
    }

    Dx = alloc_matrix(n, n);
    Dy = alloc_matrix(n, n);
    A = alloc_matrix(n, n);
    B = alloc_matrix(n, n);

    index_distance(x, Dx, n, p, *index);
    index_distance(y, Dy, n, q, *index);
    Akl(Dx, A, dims);
    Akl(Dy, B, dims);
    n2 = ((double) n) * n;
    /* compute dCov(x,y), dVar(x), dVar(y) */
    for (k=0; k<4; k++)
        DCOV[k] = 0.0;
    for (k=0; k<n; k++)
        for (j=0; j<n; j++) {
            DCOV[0] += A[k][j]*B[k][j];
            DCOV[2] += A[k][j]*A[k][j];
            DCOV[3] += B[k][j]*B[k][j];
        }

    for (k=0; k<4; k++) {
        DCOV[k] /= n2;
        DCOV[k] = sqrt(DCOV[k]);
    }
    /* compute dCor(x, y) */
    V = DCOV[2]*DCOV[3];
    if (V > DBL_EPSILON)
        DCOV[1] = DCOV[0] / sqrt(V);
        else DCOV[1] = 0.0;

    free_matrix(Dx, n, n);
    free_matrix(Dy, n, n);
    free_matrix(A, n, n);
    free_matrix(B, n, n);
    return;
}

void Akl(double **akl, double **A, int *n) {
    /* -computes the A_{kl} or B_{kl} distances from the
        distance matrix (a_{kl}) or (b_{kl}) for dCov, dCor, dVar
        dCov = mean(Akl*Bkl), dVar(X) = mean(Akl^2), etc.
    */
    int j, k, N;
    double *akbar;
    double abar;

    N = *n;
    akbar = Calloc(N, double);
    abar = 0.0;
    for (k=0; k<N; k++) {
        akbar[k] = 0.0;
        for (j=0; j<N; j++) {
            akbar[k] += akl[k][j];
        }
        abar += akbar[k];
        akbar[k] /= (double) N;
    }
    abar /= (double) (N*N);

    for (k=0; k<N; k++)
        for (j=k; j<N; j++) {
            A[k][j] = akl[k][j] - akbar[k] - akbar[j] + abar;
            A[j][k] = A[k][j];
        }
    Free(akbar);
}

void dCovTest(double *x, double *y, int *byrow, int *dims,
              double *index, double *reps,
              double *Dstat, double *pval) {
    /*  this provides an alternate, algebraically
        equivalent (but much slower) method for computing
        dCov^2 and the dCov test of independence

        approx permutation E test for multiv. indep. of X in R^p and Y in R^q
        statistic is dCov^2 where n*dCov^2 --> Q
        dims[0] = n (sample size)
        dims[1] = p (dimension of X)
        dims[2] = q (dimension of Y)
        dims[3] = B (number of replicates, dimension of reps)
        index : exponent for distance
        Dstat : the statistic dCov^2 (V_n^2) and S1, S2, S3
     */
    int    b, i, j, k, n, p , q, B, I, J, M, N;
    int    *perm;
    double Cx, Cy, Cxy, C3, S1, S2, S3, n2, n3;
    double **Dx, **Dy;

    n = dims[0];
    p = dims[1];
    q = dims[2];
    B = dims[3];
    N = n * n;

    if (*byrow == FALSE) {
        /* avoid this step: use as.double(t(x)) in R */
        roworder(x, byrow, n, p);
        *byrow = FALSE;  /* false for y */
        roworder(y, byrow, n, q);
    }

    Dx = alloc_matrix(n, n);
    Dy = alloc_matrix(n, n);

    index_distance(x, Dx, n, p, *index);
    index_distance(y, Dy, n, q, *index);

    Cx = Cy = Cxy = C3 = 0.0;
    n2 = ((double) n) * n;
    n3 = n2 * n;

    /* compute observed test statistic */
    for (i=0; i<n; i++) {
        for (j=0; j<n; j++) {
            Cx += (Dx[i][j]);
            Cy += (Dy[i][j]);
            Cxy += (Dx[i][j] * Dy[i][j]);
            for (k=0; k<n; k++)
                C3 += (Dx[k][i] * Dy[k][j]);
        }
    }
    Cx /= n2;
    Cy /= n2;
    S1 = Cxy / n2;
    S2 = Cx * Cy;
    S3 = C3 / n3;
    *Dstat = (S1 + S2 - 2*S3);
    Dstat[1] = S1;
    Dstat[2] = S2;
    Dstat[3] = S3;

    /* compute the replicates, S2 does not change
       permute the indices of the second sample only
    */
    if (B > 0) {
        perm = Calloc(n, int);
        M = 0;
        for (i=0; i<n; i++)
            perm[i] = i;
        for (b = 0; b < B; b++) {
            permute(perm, n);
            C3 = 0.0;
            Cxy = 0.0;
            for (i=0; i<n; i++) {
                I = perm[i];
                for (j=0; j<n; j++) {
                    J = perm[j];
                    Cxy += (Dx[i][j] * Dy[I][J]);
                    for (k=0; k<n; k++)
                        C3 += (Dx[k][i] * Dy[I][J]);
                }
            }
            S1 = Cxy / n2;
            S3 = C3 / n3;
            reps[b] = (S1 + S2 - 2*S3);
            if (reps[b] >= (*Dstat)) M++;
        }
        *pval = (double) M / (double) (B);
        Free(perm);
    }
    /* test statistic (the V-statistic) is nV_n^2 = n*Dstat[0]
       a normalized version is n*Dstat[0]/Dstat[2]
    */

    free_matrix(Dx, n, n);
    free_matrix(Dy, n, n);
    return;
}
