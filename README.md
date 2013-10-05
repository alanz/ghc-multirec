ghc-multirec
============

MultiRec instance for GHC AST

The exe provides an example based on
<https://github.com/kosmikus/zipper/blob/master/examples/ASTEditor.hs>

Notes:

* As of 2013-10-05, the multirec-alt-driver package from hackage
  does not install, it needs a bit of TLC. Will supply patch later.

* As a side effect this package provides `Show` instances (and missing
  `Outputable` instances) for the GHC AST
