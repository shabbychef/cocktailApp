#! /usr/bin/r
#
# Copyright 2019-2019 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav 
#
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
#
# Created: 2019.06.30
# Copyright: Steven E. Pav, 2019
# Author: Steven E. Pav <shabbychef@gmail.com>
# Comments: Steven E. Pav

suppressMessages({
	library(readr)
	library(dplyr)
	library(usethis)
})

cocktails <- readr::read_csv('cocktails.csv',
														 col_types=cols(amt = col_double(),
																	unit = col_character(),
																	ingredient = col_character(),
																	cocktail = col_character(),
																	rating = col_double(),
																	upstream_id = col_double(),
																	url = col_character(),
																	votes = col_character(),
																	added = col_character(),
																	src = col_character(),
																	short_ingredient = col_character(),
																	proportion = col_double()
																	))


cat('cocktails is ',dim(cocktails),'on',nrow(distinct(cocktails,url)),'distinct cocktails\n')

usethis::use_data(cocktails,overwrite=TRUE,internal=FALSE)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
