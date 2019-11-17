# statip 0.2.3 (2019-11-17)

## Bug fixes

* Fix bug in `prune()`. 

* Now `hellinger()` returns the Hellinger distance, not the square of it (Issue #1, thanks to benjones13).

## Features

* Use `model.frame()` to improve the way formulas are processed in `picor()`. 

* Now `mfv()` is powered by a function `tableNA()` and returns a factor with the same levels as `x`, when `x` is itself a factor. 

* In `mfv()` the argument `na.rm` is soft-deprecated (so still accepted for now, with a message thrown); the default argument is now called `na_rm`. 

* Add various statistical functions: `cv()` (coefficient of variation), `midhinge()`, `midrange()`, `trimean()`. 

* Add `histo()` to compute a histogram and `find_breaks()` to give breakpoints just like `MASS::truehist()` does.


# statip 0.2.0

## Features

* Add function `picor()` for univariate piecewise-constant regression. 

* Add function `bandwidth()`. 

* Add function `predict.kmeans()` (with dependency to package `clue`).

* Add `...` argument in `mfv()` and `mfv1()` (useful for the subsequent call of these functions within package 'modeest'). 

## Performance

* Function `if_else()` from package 'dplyr' is no longer used since it 
currently inconsistently transforms a matrix into a vector; the base `ifelse()` 
is used instead. 


# statip 0.1.5

* `mfv()` was not working with factors.


# statip 0.1.4

* Update authors list. 

* Add functions `mfv()` and `mfv1()`, which compute the mode (most frequent value) 
found in a vector of discrete values. 

* Add small updates before CRAN submission. 
