# 2017/03/06
# def journal__20170306__compute_uncertainty_in_parallel_eflux():
from sympy import *             #
import sympy as sp
# qpar depends on hpar, vpar, vprp, ppar, pprp
# define all variables as sympy symbols
hpar, vpar, vprp, ppar, pprp, pprppar = symbols(
    'hpar vpar vprp ppar pprp pprppar')
shpar_hpar, shpar_vpar, shpar_vprp, shpar_ppar, shpar_pprp, shpar_pprppar = symbols(
    'shpar_hpar shpar_vpar shpar_vprp shpar_ppar shpar_pprp shpar_pprppar')
svpar_hpar, svpar_vpar, svpar_vprp, svpar_ppar, svpar_pprp, svpar_pprppar = symbols(
    'svpar_hpar svpar_vpar svpar_vprp svpar_ppar svpar_pprp svpar_pprppar')
svprp_hpar, svprp_vpar, svprp_vprp, svprp_ppar, svprp_pprp, svprp_pprppar = symbols(
    'svprp_hpar svprp_vpar svprp_vprp svprp_ppar svprp_pprp svprp_pprppar')
sppar_hpar, sppar_vpar, sppar_vprp, sppar_ppar, sppar_pprp, sppar_pprppar = symbols(
    'sppar_hpar sppar_vpar sppar_vprp sppar_ppar sppar_pprp sppar_pprppar')
spprp_hpar, spprp_vpar, spprp_vprp, spprp_ppar, spprp_pprp, spprp_pprppar = symbols(
    'spprp_hpar spprp_vpar spprp_vprp spprp_ppar spprp_pprp spprp_pprppar')
spprppar_hpar, spprppar_vpar, spprppar_vprp, spprppar_ppar, spprppar_pprp, spprppar_pprppar = symbols(
    'spprppar_hpar spprppar_vpar spprppar_vprp spprppar_ppar spprppar_pprp spprppar_pprppar')
vlist = [hpar, vpar, vprp, ppar, pprp, pprppar]
sigshpar = [shpar_hpar, shpar_vpar, shpar_vprp,
            shpar_ppar, shpar_pprp, shpar_pprppar]
sigsvpar = [svpar_hpar, svpar_vpar, svpar_vprp,
            svpar_ppar, svpar_pprp, svpar_pprppar]
sigsvprp = [svprp_hpar, svprp_vpar, svprp_vprp,
            svprp_ppar, svprp_pprp, svprp_pprppar]
sigsppar = [sppar_hpar, sppar_vpar, sppar_vprp,
            sppar_ppar, sppar_pprp, sppar_pprppar]
sigspprp = [spprp_hpar, spprp_vpar, spprp_vprp,
            spprp_ppar, spprp_pprp, spprp_pprppar]
sigspprppar = [spprppar_hpar, spprppar_vpar, spprppar_vprp,
               spprppar_ppar, spprppar_pprp, spprppar_pprppar]
sigsmat = [sigshpar, sigsvpar, sigsvprp, sigsppar, sigspprp, sigspprppar]

qpar = hpar + 2 * vprp * pprppar + vpar * ppar + 0.5 * \
    vpar * (2 * pprp + ppar)  # parallel energy flux

pdiffs = [diff(qpar, var) for var in vlist]
# pdiffssq = [pdiffs * diff(qpar, var) for var in vlist]
pdiffssq = []
pIter = iter(pdiffs)
for i in pIter:
    tmplist = [i * diff(qpar, var) for var in vlist]
    pdiffssq.append(tmplist)

# each member of the list 'exprlines' represents dQ_||/dx_i *
# SUM(dQ_||/dx_j for x_j in {hpar, vpar, vprp, ppar, pprp, pparprp})
#
# In other words, if we sum every element of exprlines, we get back
# SUM(dQ_||/dx_i * dQ_||/dx_j*sigma__x_i__x_j for x_i,x_j in {hpar, vpar,
# vprp, ppar, pprp, pparprp})
count = 0
pIter = iter(pdiffssq)
exprlines = []
for pdiffprodrow in pIter:
    tmpline = sum([thisprod * thissigma for (thisprod, thissigma)
                   in zip(pdiffprodrow, sigsmat[count])])
    exprlines.append(tmpline)
    count += 1

totexpr = simplify(sum(exprlines))

# Print the product dQ_||/dx_i * dQ_||/dx_j
count = 0
for i in pdiffssq:
    print('')
    print('**{!s}**'.format(vlist[count]))
    count2 = 0
    for j in i:
        print('{!s:30}: {!s}'.format(
            '[{:2},{:2}] d{}*d{}'.format(count, count2, vlist[count], vlist[count2]), j))
        count2 += 1
    count += 1
lines = [a + b + c + d + e + f for (a, b, c, d, e, f) in zip(
    pdiffssq[0], pdiffssq[1], pdiffssq[2], pdiffssq[3], pdiffssq[4], pdiffssq[5])]

# Here's hpar defined in terms of qpar, with pprppar = 0
# Oh, but right: it won't show up because anytime we take a derivate wrt
# to hpar, we get back 1.
qpar2 = symbols('qpar2')
hpar2 = qpar2 - 3 / 2 * vpar * ppar - vpar * pprp

# Substitutions--kill diag term, and plug in qpar 2 (since SDT doesn't
# calculate hpar)
killperppar = ((pprppar, 0),
               (spprppar_pprppar, 0),
               (spprppar_hpar, 0),
               (spprppar_vpar, 0),
               (spprppar_vprp, 0),
               (spprppar_ppar, 0),
               (spprppar_pprp, 0),
               (shpar_pprppar, 0),
               (svpar_pprppar, 0),
               (svprp_pprppar, 0),
               (sppar_pprppar, 0),
               (spprp_pprppar, 0),
               (hpar, hpar2),
               (svpar_hpar, shpar_vpar),
               (svprp_hpar, shpar_vprp),
               (svprp_vpar, svpar_vprp),
               (sppar_hpar, shpar_ppar),
               (sppar_vpar, svpar_ppar),
               (sppar_vprp, svprp_ppar),
               (spprp_hpar, shpar_pprp),
               (spprp_vpar, svpar_pprp),
               (spprp_vprp, svprp_pprp),
               (spprp_ppar, sppar_pprp))

print('')
print('En final:')
print(totexpr.subs(killperppar))
print(simplify(totexpr.subs(killperppar)))
