# Copyright 2018 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav

# This file is part of cocktailApp.
#
# cocktailApp is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# cocktailApp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with cocktailApp.  If not, see <http://www.gnu.org/licenses/>.

# env var:
# nb: 
# see also:
# todo:
# changelog: 
#
# Created: 2018.07.01
# Copyright: Steven E. Pav, 2018-2018
# Author: Steven E. Pav
# Comments: Steven E. Pav

# helpers#FOLDUP
set.char.seed <- function(str) {
	set.seed(as.integer(charToRaw(str)))
}
#UNFOLD

library(dplyr)
context("data as expected")# FOLDUP

utils::data("cocktails", package="cocktailApp")
test_that("data_size",{#FOLDUP
	indat <- cocktails
	expect_gt(nrow(cocktails),10e4)
	expect_gt(ncol(cocktails),11)
	expect_true(all(c('amt','unit','ingredient','cocktail','url','short_ingredient') %in% colnames(cocktails)))
})#UNFOLD

# UNFOLD

context("code runs at all")#FOLDUP
utils::data("cocktails", package="cocktailApp")

test_that("shiny bits",{#FOLDUP
	indat <- head(cocktails,500)

	expect_error(recipe_df <- .add_id(indat),NA)
	expect_error(cocktail_df <- .distill_info(recipe_df),NA)

	both <- list(recipe=recipe_df %>% dplyr::select(-cocktail,-rating,-votes,-url),cocktail=cocktail_df)

	expect_error(both2 <- .filter_ingredients(both,name_regex='sazerac',must_have_ing=c('Bourbon','Averna'),
															 must_not_have_ing=c(),
															 logical_sense='OR'),NA)

	skip_on_cran()
	skip_on_travis()
	two_ing <- c('Bourbon','Averna')
	expect_error(both3 <- .filter_num_ingredients(both2,must_have_ing=two_ing,min_rating=2,max_ingr=10,max_other_ingr=5),NA)
	expect_error(both4 <- .filter_tstat(both3,min_t=2,t_zero=1),NA)
	expect_error(both5 <- .filter_src(both4,from_sources=c('diffordsguide')),NA)
	expect_error(both6 <- .add_description(both5),NA)

	expect_error(tbl <- .drinks_table(both6),NA)
	expect_error(merged <- .merge_both(both6),NA)
})#UNFOLD
test_that('plot stuff',{# FOLDUP
	indat <- head(cocktails,500)
	expect_error(both <- .gen_both(indat),NA)
	expect_error(both_alt <- .gen_both(),NA)

	expect_error(both2 <- .filter_ingredients(both,name_regex='sazerac',must_have_ing=c('Bourbon','Averna'),
															 must_not_have_ing=c(),
															 logical_sense='OR'),NA)

	skip_on_cran()
	skip_on_travis()
	two_ing <- c('Bourbon','Averna')
	expect_error(both3 <- .filter_num_ingredients(both2,must_have_ing=two_ing,min_rating=2,max_ingr=10,max_other_ingr=5),NA)
	expect_error(ptern <- .prepare_ternary(both3,two_ing=two_ing),NA)
	expect_error(.make_ggtern_plot(ptern,two_ing),NA)

	expect_error(both4 <- .filter_tstat(both3,min_t=2,t_zero=1),NA)
	expect_error(both5 <- .filter_src(both4,from_sources=c('diffordsguide')),NA)
	expect_error(both6 <- .add_description(both5),NA)

	expect_error(merged <- .merge_both(both6),NA)

	expect_error(ph <- .make_bar_plot(merged),NA)
})# UNFOLD
test_that('correlation and coingredient',{# FOLDUP
	indat <- head(cocktails,500)

	#skip_on_cran()
	#skip_on_travis()
	expect_error(recipe_df <- .add_id(indat),NA)

	expect_error(rhov1 <- .coingredients(recipe_df),NA)
	expect_error(rhov2 <- .ingredient_rho(recipe_df),NA)
})# UNFOLD
test_that("call the app?",{#FOLDUP
	expect_error(blah <- cocktailApp(),NA)
})#UNFOLD
# 2FIX: check the effects of NA
#UNFOLD


#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
