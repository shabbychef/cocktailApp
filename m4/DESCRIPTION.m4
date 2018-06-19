dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", 
    role=c("aut","cre"),
    email="shabbychef@gmail.com",
    comment = c(ORCID = "0000-0002-4197-6195")))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Shiny App to Discover Cocktails
BugReports: https://github.com/shabbychef/PKG_NAME()/issues
Description: A Shiny app to discover cocktails. The
    app allows one to search for cocktails by ingredient,
    filter on rating, and number of ingredients. The
    package also contains data with the ingredients of
    nearly 16 thousand cocktails scraped from the web.
Depends: 
    R (>= 3.0.2),
    shiny
Imports:
    shinythemes,
    dplyr,
    tidyr,
    tibble,
    ggplot2,
    magrittr,
    ggtern,
    forcats,
dnl readr,
dnl    urltools,
dnl    stringr
    DT
URL: https://github.com/shabbychef/PKG_NAME()
dnl VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4:et
