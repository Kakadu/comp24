Algorithm W is the best known algorithm for implementing Hindley-Milner type inference.
It's simple but has the property of intermingling two separate processes: constraint generation and solving.

Our implementation is based on Heeren B. J., Hage J., Swierstra S. D. (2002) **Generalizing Hindley-Milner type inference algorithms**.
In this approach constraints on types are first collected by bottom-up traversal ([constraints generation](IGen/)), and then solved independently ([constraints solving](ISolve.ml)).
