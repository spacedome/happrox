# happrox

Some approximation theory routines in Haskell.

## Chebyshev
The `Approx.Cheb` module contains some basic Chebyshev routines (see Chebfun, ApproxFun.jl).
While not entirely full featured, it is sufficient to solve some linear ODEs.

The following solves `d^2x/dx u = - sin (pi * x)` with a sixth order Chebyshev polynomial approximation.

```haskell
dl = DL (chebDf2 6) DirichletBC DirichletBC
u = dl <\\> Function (\x -> - sin (pi * x))
```
