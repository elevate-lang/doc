| Strategy | Scala URL | Haskell URL | works |
|----------|-----------|-------------|-------|
| `RISE` AST | - | | |
| `RewriteResult` Type | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/RewriteResult.scala | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L34 | |
|----------|-----------|-------------|-------|
| `id` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L26 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L37 | |
| `fail` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L28 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L40 | |
| `seq` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L33 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L55 | |
| `lChoice` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L37 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L58 | |
| `try` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L43 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L61 | |
| `repeat` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L46 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L63 | |
|----------|-----------|-------------|-------|
| `body` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L13 | | |
| `function` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L41 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L75 | |
| `argument` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L49 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L82 | |
|----------|-----------|-------------|-------|
| `topDown` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L42 | | |
| `bottomUp` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L66 | | |
| `allTopDown` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L46 | | |
| `allBottomUp` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L54 | | |
| `tryAll` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L50 | | |
|----------|-----------|-------------|-------|
| `normalize` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L55 | | |
| `etaReduction` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/package.scala#L42 | | |
| `etaAbstraction` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/package.scala#L47 | | |
| `not` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L19 | | |
| `contains` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L38 | | |
| `isEqualTo` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L36 | | |
|----------|-----------|-------------|-------|
| `addId` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L120 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L93 | |
| `transposeMove` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/movement.scala#L50 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L99 | |
| `idToTranspose` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L132 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L111 | |
| `splitJoin` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L29 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L122 | |
| `mapFusion` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L41 | https://github.com/elevate-lang/doc/blob/fe90d1f4a11773a8d9d263f3f9a61afa1e13a819/src/mini-elevate/src/TestStrategies.hs#L133 | |
| `mapFission` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L102 | | |
| `fuseReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L76 | | |
| `fissionReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L90 | | | 
