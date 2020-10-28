| Strategy | Scala URL | Haskell URL | works |
|----------|-----------|-------------|-------|
| `RISE` AST | - | | |
| `RewriteResult` Type | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/RewriteResult.scala | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L39 | |
|----------|-----------|-------------|-------|
| `id` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L26 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L42 | |
| `fail` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L28 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L45 | |
| `seq` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L33 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L60 | |
| `lChoice` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L37 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L63 | |
| `try` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L43 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L66 | |
| `repeat` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L46 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L68 | |
|----------|-----------|-------------|-------|
| `body` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L13 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L76 | |
| `function` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L41 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L83 | |
| `argument` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L49 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L90 | |
|----------|-----------|-------------|-------|
| `topDown` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L42 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L97 | |
| `bottomUp` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L66 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L99 | |
| `allTopDown` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L46 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L101 | |
| `allBottomUp` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L54 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L103 | |
| `tryAll` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala#L50 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L105 | |
|----------|-----------|-------------|-------|
| `normalize` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L55 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L107 | |
| `etaReduction` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/package.scala#L42 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L124 | |
| `etaAbstraction` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/package.scala#L47 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L138 | |
| `not` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L19 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L109 | |
| `contains` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L38 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L122 | |
| `isEqualTo` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/predicate.scala#L36 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L115 | |
|----------|-----------|-------------|-------|
| `addId` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L120 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L145 | |
| `transposeMove` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/movement.scala#L50 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L147 | |
| `idToTranspose` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L132 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L154 | |
| `splitJoin` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L29 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L161 | |
| `mapFusion` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L41 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L168 | |
| `mapFission` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L102 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L181 | |
| `fuseReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L76 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L197 | |
| `fissionReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L90 | https://github.com/elevate-lang/doc/blob/master/src/mini-elevate/src/TestStrategies.hs#L211 | | 
