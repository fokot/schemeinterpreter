<a href='http://www.recurse.com' title='Made with love at the Recurse Center'><img src='https://cloud.githubusercontent.com/assets/2883345/11322972/9e553260-910b-11e5-8de9-a5bf00c352ef.png' height='59px'/></a>

# Scheme interpreter from [tutorial](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)

[![Build Status](https://travis-ci.org/fokot/schemeinterpreter.svg?branch=master)](https://travis-ci.org/fokot/schemeinterpreter)


I added test-cases, implemented reader's exercises and polished code a little bit

Compatibility can be tested with [online Scheme interpreter](https://repl.it/languages/Scheme)

### [Stack](http://haskellstack.org) - the build system
#### Project was created by
* `cabal init` - creates initial cabal config file
* `stack init` - creates new stack project, stack.yaml etc.

#### Selfexplanatory
* `stack build`
* `stack exec schemeinterpreter` - this runs scheme repl :-)
* `stack test`
* `stack repl` - opens haskell repl
