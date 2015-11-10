# CTLModelChecker
A basic implementation of a CTL model checker in Haskell

##Usage

The parser requires parsec to be installed (run `cabal install parsec`)

To use, load up `test.hs` in GHCi and run `satF "?{} (![] c)"` for computing the satisfiability of the formula ∃◇∀□c in the given transition system. To re/define a transition system, you will need to define a `Graph` (list of pairs of nodes), a list of nodes which are the initial states and a labeling function which returns a list of labels for each state in the graph.
