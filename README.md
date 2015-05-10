L5-Haskell
==========

A Compiler for the L5 Language written in Haskell

More info to come later.

Todo:

 - [x] Allow support for multiple backends for L1 (linux/darwin)
 - [x] Finish L4
 - [ ] Finish L5
 - [ ] Add Haddock everywhere
 - [x] Refactor 'Reader' code to use Trifecta or something cleaner.
 - [ ] Consider using Trifecta to do the other language parsing
 - [ ] Add more built in data types (String, Char, etc)
 - [ ] Add L6 - A Language to allow user defined data types
 - [ ] Refactor label and temporary var code because it's all over the place and nasty
 - [ ] Consider using Data.Map.Strict (see notes here: http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html)


 replace set.union with <>, set.unions with mconcat