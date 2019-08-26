#include <R.h>

/* a trick to keep up with the parameters */
static double parms[5];
#define rI parms[0]
#define rG parms[1]
#define rM parms[2]
#define AE parms[3]
#define K  parms[4]

/* initializers */
void initparms(void (* odeparms)(int *, double *)) {
    int N=5;
    odeparms(&N, parms);
}

/* names for states and derivatives */
#define P y[0]
#define C y[1]
#define dP ydot[0]
#define dC ydot[1]

void derivs(int *neq, double *t, double *y, 
            double *ydot, double *yout, int *ip){
    if (ip[0] < 1) error("nout should be at least 1");
    dP = rG*P*(1-P/K) - rI*P*C;
    dC = rI*P*C*AE - rM*C;
    yout[0] = P + C;
}

