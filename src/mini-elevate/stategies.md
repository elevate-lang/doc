| Strategy | Scala URL | Haskell URL | works |
|----------|-----------|-------------|-------|
| `RISE` AST | - | | |
| `RewriteResult` Type | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/RewriteResult.scala | | |
|----------|-----------|-------------|-------|
| `id` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L26 | | |
| `fail` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L28 | | |
| `seq` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L33 | | |
| `lChoice` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L37 | | |
| `try` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L43 | | |
| `repeat` | https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala#L46 | | |
|----------|-----------|-------------|-------|
| `body` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L13 | | |
| `function` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L41 | | |
| `argument` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala#L49 | | |
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
| `addId` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L120 | | |
| `transposeMove` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/movement.scala#L50 | | |
| `idToTranspose` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L132 | | |
| `splitJoin` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L29 | | |
| `mapFusion` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L41 | | |
| `mapFission` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L102 | | |
| `fuseReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L76 | | |
| `fissionReduceMap` | https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L90 | | | 
