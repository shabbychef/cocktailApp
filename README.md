

# cocktailApp

[![Build Status](https://travis-ci.org/shabbychef/cocktailApp.png)](https://travis-ci.org/shabbychef/cocktailApp)
[![codecov.io](http://codecov.io/github/shabbychef/cocktailApp/coverage.svg?branch=master)](http://codecov.io/github/shabbychef/cocktailApp?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/cocktailApp)](https://cran.r-project.org/package=cocktailApp)
[![Downloads](http://cranlogs.r-pkg.org/badges/cocktailApp?color=green)](http://www.r-pkg.org/pkg/cocktailApp)
[![Total](http://cranlogs.r-pkg.org/badges/grand-total/cocktailApp?color=green)](http://www.r-pkg.org/pkg/cocktailApp)



A Shiny app to discover cocktails.

-- Steven E. Pav, shabbychef@gmail.com

## Installation

This package can be installed from 
[github](https://www.github.com/shabbychef/cocktailApp "cocktailApp")
via devtools:


```r
if (require(devtools)) {
    # latest greatest
    install_github(repo = "cocktailApp", username = "shabbychef", 
        ref = "master")
}
```

# Basic Usage

The app can be run in a few ways: 

1. You can download the github repo and run the `app.R` in the main directory,
	 either via `shiny::runApp()` or by moving this directory to a location that
	 Shiny Server serves.
1. You can install the package and then use the `cocktailApp()` function.


## Data

The underlying data to power the shiny app is also available from this package.
It is called, simply, `cocktails`. This data frame has rows for each
ingredient, with amounts, and units, and is joined to information about the
cocktail, which is identified by name, an upstream ID, URL, rating, number of
votes, and more.



```r
library(cocktailApp)
library(dplyr)
library(knitr)
utils::data("cocktails", package = "cocktailApp")
cocktails %>% arrange(desc(rating)) %>% head(n = 10) %>% 
    select(cocktail, ingredient, amt, unit, rating) %>% 
    knitr::kable()
```



|cocktail             |ingredient                       |   amt|unit    | rating|
|:--------------------|:--------------------------------|-----:|:-------|------:|
|Jersey Sour          |Berneroy Fine Calvados           |  2.00|fl oz   |      5|
|Jersey Sour          |Freshly squeezed lemon juice     |  1.00|fl oz   |      5|
|Jersey Sour          |Sugar syrup (2 sugar to 1 water) |  0.50|fl oz   |      5|
|Jersey Sour          |Pasteurised egg white            |  0.50|fl oz   |      5|
|Jersey Sour          |Lemon zest twist                 |  1.00|garnish |      5|
|Julep (Generic Name) |Mint leaves                      | 12.00|fresh   |      5|
|Julep (Generic Name) |Brandy, whisk(e)y, gin, rum etc. |  2.50|fl oz   |      5|
|Julep (Generic Name) |Sugar syrup (2 sugar to 1 water) |  0.75|fl oz   |      5|
|Julep (Generic Name) |Angostura Aromatic Bitters       |  3.00|dash    |      5|
|Julep (Generic Name) |Mint sprig                       |  1.00|garnish |      5|


