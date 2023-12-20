# Copyright 2018-2018 Steven E. Pav. All Rights Reserved.
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

# Created: 2018-06-15
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav
# Comments: Steven E. Pav

# no longer @importFrom Ternary TernaryPlot TernaryPoints TernaryText

#' Shiny app to discover cocktails.
#' 
#' @section Legal Mumbo Jumbo:
#'
#' cocktailApp is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU Lesser General Public License for more details.
#'
#' @template etc
#'
#' @import shiny
#' @importFrom dplyr mutate arrange select filter rename left_join right_join coalesce distinct summarize everything ungroup first sample_n one_of n
#' @importFrom ggplot2 ggplot labs coord_flip aes geom_col geom_point geom_text guide_legend
#' @importFrom shinythemes shinytheme
#' @importFrom magrittr %>%
#' @importFrom forcats fct_rev
#' @importFrom utils data
#' @importFrom tibble tribble 
#' @importFrom tidyr spread
#' @importFrom stats setNames
#'
#' @importFrom graphics legend
#' @importFrom grDevices rgb
#'
#'
#' @name cocktailApp-package
#' @rdname cocktailApp-package
#' @docType package
#' @title Shiny app to discover cocktails.
#' @keywords package
#' @note
#' 
#' This package is maintained as a hobby. 
#'
NULL

#' @title News for package 'cocktailApp':
#'
#' @description 
#'
#' News for package \sQuote{cocktailApp}
#'
#' \newcommand{\CRANpkg}{\href{https://cran.r-project.org/package=#1}{\pkg{#1}}}
#' \newcommand{\cocktailApp}{\CRANpkg{cocktailApp}}
#'
#' @section \cocktailApp{} Version 0.2.3 (2023-07-18) :
#' \itemize{
#' \item fix bug where you could not search for Gin or Vodka.
#' \item package had been archived because of dependency on ggtern.
#' \item convert citEntry to bibentry
#' }
#'
#' @section \cocktailApp{} Version 0.2.2 (2021-04-01) :
#' \itemize{
#' \item merge many short ingredients.
#' \item fix some units in webtender data.
#' \item search by ingredient regex.
#' }
#'
#' @section \cocktailApp{} Version 0.2.1 (2019-07-01) :
#' \itemize{
#' \item CRAN fix as tests were hanging.
#' \item replace \code{Ternary} package with \code{ggtern}.
#' }
#'
#' @section \cocktailApp{} Version 0.2.0 (2018-08-19) :
#' \itemize{
#' \item adding another source.
#' \item adding \dQuote{Hobson's Choice} button.
#' \item removing dependency on \code{ggtern} package, replacing with \code{Ternary}.
#' }
#'
#' @section \cocktailApp{} Initial Version 0.1.0 (2018-07-05) :
#' \itemize{
#' \item first CRAN release.
#' }
#'
#' @name cocktailApp-NEWS
#' @rdname NEWS
NULL

#' @title Cocktails Data
#' @description Ingredients of over 26 thousand cocktails, scraped from the web.
#' @format A \code{data.frame} object with around 117,000 rows and 12 columns. The
#' data were scraped from four websites: Difford's guide, Webtender, and 
#' Kindred Cocktails, all scraped in late 2017; and Drinks Mixer, scraped in
#' mid 2018.
#'
#' The columns are defined as follows:
#' \describe{
#'  \item{\code{amt}}{The numeric amount of the ingredient.}
#'  \item{\code{unit}}{The unit corresponding to the amount. 
#'  The most common entry is \code{fl oz}, which is the unit for \sQuote{main}
#'  ingredients.
#'  The second most common entry is \code{garnish}. These two units
#'  account for over 95 percent of the rows of the data.}
#'  \item{\code{ingredient}}{The name of the ingredient. These may have odd
#'  qualifiers, or brand specifications. Some of these qualifications are
#'  stripped out in the \code{short_ingredient} field.}
#'  \item{\code{cocktail}}{The name of the cocktail.}
#'  \item{\code{rating}}{The rating assigned to the cocktail in the upstream database. For some
#'  sources, the ratings have been rescaled. Ratings are on a scale of 0 to 5.}
#'  \item{\code{upstream_id}}{An ID code from the upstream source.}
#'  \item{\code{url}}{The upstream URL.}
#'  \item{\code{votes}}{The number of votes in the rating, from the upstream
#'  database. Not always available.}
#'  \item{\code{added}}{The date the cocktail was added to the upstream database. Not always available.}
#'  \item{\code{src}}{The source of the cocktail, as listed in the upstream database. Usually not available.}
#'  \item{\code{short_ingredient}}{A shortened form of the ingredient, stripping away some of the qualifiers. 
#'  This is subject to change in future releases of this package, when a better term extraction solution is found.}
#'  \item{\code{proportion}}{For ingredients where the \code{unit} is \code{fl oz}, 
#'   this is the proportion of the given cocktail that consists of the given ingredient. For a given
#'   cocktail, the proportions should sum to one.}
#' }
#' @source Difford's Guide, \url{https://www.diffordsguide.com/},
#' Webtender, \url{https://www.webtender.com},
#' Kindred Cocktails, \url{https://kindredcocktails.com},
#' Drinks Mixer, \url{http://www.drinksmixer.com}.
#' @note 
#' The data were scraped from several websites, which falls in a legal gray area.
#' While, in general, raw factual data can not be copyright, there is a difference between the law and a lawsuit. 
#' The package author in no way claims any copyright on this data.
#' @usage data(cocktails)
#' @examples
#' data(cocktails)
#' str(cocktails)
#'
#' require(dplyr)
#' cocktails %>%
#' 	filter(short_ingredient %in% c('Averna','Bourbon')) %>%
#' 	group_by(cocktail,url) %>%
#' 		mutate(isok=n() > 1) %>%
#' 	ungroup() %>%
#' 	filter(isok) %>%
#' 	arrange(desc(rating),cocktail) %>%
#' 	select(cocktail,ingredient,amt,unit,rating) %>%
#' 	head(n=8)
#'
"cocktails"

globalVariables(c('cocktails','votes','rating','cocktail','proportion','url','short_ingredient','unit',
									'cocktail_id','coamount','amt',
									'deno','deno2','rhoval',
									'sum_cova','n','rat','tot_ingr','tot_has_ingr','tot_am','ncocktails',
									'tstat','page_src','tst',
									'has_or_must','has_and_must','has_not_must',
									'matches_name','ingr_class','description',
									'isfl',
									'match_re_ing',
									'has_ing','bad_ing',
									'ck_ok',
									'pct_amt',
									'tot_amt','has_both','Other',
									'ingredient','coingredient','cova','wts'))


# Define UI for ...
my_ui <- function(page_title='Drink Schnauzer') {  # nocov start
	utils::data("cocktails", package="cocktailApp")
	indat <- cocktails

	# let's order ingredients by number of times they
	# appear, then alphabetical. seems about right.

	normo <- indat %>%
		dplyr::group_by(short_ingredient) %>%
		dplyr::summarize(tot_am=sum(proportion,na.rm=TRUE)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(ingr_class=cut(tot_am,breaks=c(-1,0,10,100,Inf),
													labels=c('garnish','uncommon-spirit','common-spirit','base-spirit'))) %>%
		dplyr::arrange(tot_am,short_ingredient) %>%
		dplyr::mutate(ingr_class=forcats::fct_rev(ingr_class)) %>%
		dplyr::arrange(ingr_class,short_ingredient)

	#ingr <- unique(indat$ingredient)
	#ingr <- normo$ingredient
	ingr <- (split(normo$short_ingredient,normo$ingr_class)) 
	#drnk <- unique(indat$cocktail)

	sources <- indat %>%
		dplyr::select(url) %>%
		dplyr::mutate(url=gsub('^http://(www.)?(.+).com/.+$','\\2',url)) %>%
		dplyr::distinct(url)
	all_source <- unique(sources$url)

	if (!is.null(page_title)) {
		tp_bits <- titlePanel(page_title)
	} else {
		tp_bits <- tagList()
	}

# Define UI for ...
	shinyUI(
		fluidPage(theme=shinytheme("spacelab"),#FOLDUP
			# for this, see: http://stackoverflow.com/a/22886762/164611
			# Application title
			tags$head(
						# load accounting js
						#tags$script(src='js/accounting.js'),
						tags$script(src='test.js'),
						# points for style:
						tags$style(".table .alignRight {color: black; text-align:right;}"),
						tags$link(rel="stylesheet", type="text/css", href="style.css")
			),
			tp_bits,
			# tags$img(id = "logoimg", src = "logo.png", width = "200px"),
			sidebarLayout(#FOLDUP
				position="left",
			sidebarPanel(#FOLDUP
				width=2,
				selectInput("must_have_ing","Must Have:",choices=ingr,selected=c(),multiple=TRUE),
				selectInput("logical_sense","Join by:",choices=c('OR','AND'),selected='OR',multiple=FALSE),
				selectInput("must_not_have_ing","Must Not Have:",choices=ingr,selected=c(),multiple=TRUE),
				selectInput("from_sources","Sources:",choices=all_source,selected=all_source[grepl('diffords|kindred',all_source)],multiple=TRUE),
				textInput("name_regex","Name Regex:",value='',placeholder='^sazerac'),
				textInput("ing_regex","Ingredient Regex:",value='',placeholder='^[Cc]hartreus'),
				helpText('Select for random cocktails:'),
				checkboxInput("hobsons","Hobson's Choice!",value=FALSE),
				hr(),
				sliderInput("max_ingr","Maximum Ingredients:",sep='',min=1,max=20,value=6),
				sliderInput("max_other_ingr","Maximum Unlisted Ingredients:",sep='',min=1,max=20,value=6),
				sliderInput("min_rating","Minimum Rating",min=1,max=5,value=3.5,step=0.5),
				sliderInput("min_tstat","Minimum T-Stat",min=1,max=100,value=2,step=0.25),
				sliderInput("t_zero","T-Stat Zero",min=1,max=5,value=2.5,step=0.25),
				hr(),
				helpText('data scraped from the web'),
				bookmarkButton('bookmark',title='bookmark page'),
				hr()
				),#UNFOLD
		mainPanel(#FOLDUP
			width=9,
			tabsetPanel(
				tabPanel('drinks',#FOLDUP
								 helpText('Select rows from this table to see the recipe below',
													'and also in the plot tab.'),
						DT::dataTableOutput('drinks_table'),
						hr(),
						helpText('Ingredients Table:'),
						tableOutput('ingredients_table')
						),#UNFOLD
				tabPanel('ternary',#FOLDUP
						helpText('A ternary plot based on the first two Must Have ingredients selected.'),
						plotOutput('selected_ingredients_tern_plot',height='100%',width='100%')
						),#UNFOLD
				tabPanel('plots',#FOLDUP
						helpText('A bar plot of ingredients in the selected cocktails.',
										 'If nothing appears here, select rows of the table in the "drinks" tab to populate.'),
						plotOutput('selected_ingredients_bar_plot')
						),#UNFOLD
				tabPanel('other',#FOLDUP
						helpText('This is not well tested, but here one should find a table of common co-ingredients.',
										 'If you have selected ingredients in the "Must Have" input, other ingredients which',
										 'commonly co-occur should appear in this table.'),
						DT::dataTableOutput('suggestions_table')
						)
					)  # tabSetPanel#UNFOLD
				)  # mainPanel#UNFOLD
			) # sidebarLayout#UNFOLD
		)  # fluidPage#UNFOLD
	)  # shinyUI
	
}  # nocov end


.applylink <- function(title,url) {
	as.character(a(title,href=url,target="_blank"))
}
applylink <- function(title,url) {
	as.character(mapply(.applylink,title,url))
}

# add a unique ID to recipe data based on url and cocktail name
.add_id <- function(recipe_df) {
	# fake a distinct id
	subs <- recipe_df %>%
		dplyr::distinct(cocktail,url) %>%
		tibble::rowid_to_column(var='cocktail_id')
	recipe_df %>%
		dplyr::left_join(subs,by=c('cocktail','url'))
}

# creates information about cocktails from the recipe data frame
.distill_info <- function(recipe_df) {
	cocktail_df <- recipe_df %>%
		dplyr::mutate(isfl=(unit=='fl oz')) %>%
		dplyr::group_by(cocktail_id) %>%
			dplyr::summarize(cocktail=dplyr::first(cocktail),
											 rating=dplyr::first(rating),
											 votes=dplyr::first(votes),
											 url=dplyr::first(url),
											 tot_ingr=sum(isfl)) %>%
		dplyr::ungroup() %>%
		mutate(votes=as.numeric(votes)) %>%
		dplyr::mutate(page_src=gsub('^http://(www.)?(.+).com/.+$','\\2',url))
}

.gen_both <- function(raw_dat=NULL) {
	if (missing(raw_dat) || is.null(raw_dat)) {
		utils::data("cocktails", package="cocktailApp")
		raw_dat <- cocktails
	}
	# basically normalize the data: recipes and cocktails, 
	# and keep them in a list
	# fake a distinct id
	recipe_df <- raw_dat %>%
		.add_id()
	cocktail_df <- recipe_df %>%
		.distill_info()
	list(recipe=recipe_df %>% dplyr::select(-cocktail,-rating,-votes,-url),cocktail=cocktail_df)
}

.filter_ingredients <- function(both,name_regex,must_have_ing,must_not_have_ing,
																ing_regex='',logical_sense=c('AND','OR'),extra_ids=NULL) {
	logical_sense <- match.arg(logical_sense)

	if (nzchar(name_regex)) {
		match_name <- both$cocktail %>%
			dplyr::distinct(cocktail,cocktail_id) %>%
			dplyr::filter(grepl(pattern=name_regex,x=cocktail,ignore.case=TRUE,perl=TRUE,fixed=FALSE)) %>%
			dplyr::distinct(cocktail_id) %>%
			dplyr::mutate(matches_name=nzchar(name_regex))
	} else {
		# empty
		match_name <- tibble::tribble(~cocktail_id,~matches_name)
	}
	if (!is.null(extra_ids)) {
		more_match <- both$cocktail %>%
			dplyr::filter(cocktail_id %in% extra_ids) %>%
			dplyr::select(cocktail_id) %>%
			dplyr::mutate(matches_name=TRUE) 
		match_name <- match_name %>% rbind(more_match)
	}

	# for the moment, these operations are terribly slow in dplyr 0.8.2
	# cf https://github.com/tidyverse/dplyr/issues/4458
	# when we upgrade to 0.8.3, it is worth timing the alternatives
	# on this.
	ok_by_ing <- both$recipe %>%
		dplyr::mutate(bad_ing=(short_ingredient %in% must_not_have_ing)) 

	# search regex by ingredient
	if (nzchar(ing_regex)) {
		ok_by_ing <- ok_by_ing %>%
			dplyr::mutate(match_re_ing=grepl(pattern=ing_regex,x=short_ingredient,ignore.case=TRUE,perl=TRUE,fixed=FALSE)) 
	} else {
		ok_by_ing <- ok_by_ing %>%
			dplyr::mutate(match_re_ing=FALSE)
	}
	if ((length(must_have_ing) > 0) || nzchar(ing_regex)) {
		if ((logical_sense=='AND') && (length(must_have_ing) > 1)) {   # n.b. AND with one term is the same as OR.
			ok_by_ing <- ok_by_ing %>%
				dplyr::group_by(cocktail_id) %>%
					dplyr::summarize(ck_ok = (all(must_have_ing %in% short_ingredient) | any(match_re_ing)) & !any(bad_ing)) %>%
				dplyr::ungroup()
		} else {
			ok_by_ing <- ok_by_ing %>%
				dplyr::mutate(has_ing=(short_ingredient %in% must_have_ing)) %>%
				dplyr::group_by(cocktail_id) %>%
					dplyr::summarize(ck_ok=any(has_ing | match_re_ing) & !any(bad_ing)) %>%
				dplyr::ungroup() 
		}
	} else {
		# empty
		ok_by_ing <- tibble::tribble(~cocktail_id,~ck_ok)
	}

  new_recipe <- match_name %>%
		dplyr::left_join(both$recipe,by='cocktail_id') %>%
		dplyr::select(-matches_name) %>%
		rbind(ok_by_ing %>%
					dplyr::filter(ck_ok) %>%
					dplyr::select(-ck_ok) %>%
					dplyr::left_join(both$recipe,by='cocktail_id')) 

	new_cocktail <- both$cocktail %>%
		dplyr::right_join(new_recipe %>% dplyr::distinct(cocktail_id),by='cocktail_id')

	list(recipe=new_recipe,cocktail=new_cocktail)
}

.filter_num_ingredients <- function(both,must_have_ing,min_rating,max_ingr,max_other_ingr) {
	tot_has <- both$recipe %>%
		dplyr::group_by(cocktail_id) %>%
		dplyr::summarize(tot_has_ingr=sum(short_ingredient %in% must_have_ing)) %>%
		dplyr::ungroup() 

	new_cocktail <- both$cocktail %>%
		dplyr::filter(rating >= min_rating,
									tot_ingr <= max_ingr) %>%
		dplyr::left_join(tot_has,by='cocktail_id') %>%
		dplyr::filter(tot_ingr <= max_other_ingr + tot_has_ingr) %>%
		dplyr::select(-tot_has_ingr)

	new_recipe <- both$recipe %>%
		dplyr::right_join(new_cocktail %>% select(cocktail_id),by='cocktail_id')

	list(recipe=new_recipe,cocktail=new_cocktail)
}

.filter_tstat <- function(both,min_t=2,t_zero=2.5,miss_votes=20,t_digits=2) {
	new_cocktail <- both$cocktail %>%
		dplyr::mutate(tstat=signif((rating - t_zero) * sqrt(coalesce(votes,miss_votes)),t_digits)) %>%
		dplyr::filter(tstat >= min_t) 
	new_recipe <- both$recipe %>%
		dplyr::right_join(new_cocktail %>% select(cocktail_id),by='cocktail_id')

	list(recipe=new_recipe,cocktail=new_cocktail)
}
.filter_src <- function(both,from_sources) {
	new_cocktail <- both$cocktail %>%
		dplyr::filter(page_src %in% from_sources) 
	new_recipe <- both$recipe %>%
		dplyr::right_join(new_cocktail %>% select(cocktail_id),by='cocktail_id')
	list(recipe=new_recipe,cocktail=new_cocktail)
}


.add_description <- function(both) {
	descdat <- both$recipe %>%
		dplyr::filter(unit=='fl oz') %>%
		dplyr::arrange(dplyr::desc(amt)) %>%
		dplyr::group_by(cocktail_id) %>%
			dplyr::summarize(description=paste0(paste0(short_ingredient,collapse=', '),'.')) %>%
		dplyr::ungroup() 
	new_cocktail <- both$cocktail %>%
		dplyr::left_join(descdat,by=c('cocktail_id')) 
	list(recipe=both$recipe,cocktail=new_cocktail)
}

# merge and arrange
.merge_both <- function(both) {
	both$recipe %>%
		dplyr::left_join(both$cocktail,by='cocktail_id') %>%
		dplyr::select(cocktail,rating,amt,unit,ingredient,everything()) %>%
		dplyr::arrange(dplyr::desc(rating),cocktail,dplyr::desc(as.numeric(unit=='fl oz')),dplyr::desc(amt))
}

.drinks_table <- function(both) {
	otdat <- both$cocktail %>%
		dplyr::mutate(cocktail=applylink(cocktail,url)) %>%
		dplyr::select(rating,tstat,cocktail,description)
}

# from the recipe_df, compute co-ingredients table.
.coingredients <- function(recipe_df) {
	sub_df <- recipe_df %>%
		dplyr::select(short_ingredient,cocktail_id,rating,proportion) 
	coing <- sub_df %>%
		dplyr::filter(!is.na(proportion)) %>%
		dplyr::mutate(rating=coalesce(rating,1)) %>%
		dplyr::inner_join(sub_df %>%
							 dplyr::rename(coingredient=short_ingredient,coamount=proportion),by=c('cocktail_id','rating'),relationship='many-to-many') %>%
		dplyr::mutate(cova=proportion * coamount) %>%
		dplyr::mutate(wts=rating) %>%
		dplyr::group_by(short_ingredient,coingredient) %>%
			dplyr::summarize(sum_cova=sum(cova*wts,na.rm=TRUE),
								sum_wts=sum(wts,na.rm=TRUE),
								ncocktails=n()) %>%
		dplyr::ungroup() %>%
		dplyr::arrange(dplyr::desc(ncocktails))
}

.ingredient_rho <- function(recipe_df) {
	coing <- .coingredients(recipe_df) 
	diagv <- coing %>%
		dplyr::filter(short_ingredient==coingredient) %>%
		dplyr::mutate(deno=sqrt(sum_cova))

	rhov <- coing %>%
		dplyr::left_join(diagv %>% select(short_ingredient,deno),by='short_ingredient',relationship='many-to-many') %>%
		dplyr::left_join(diagv %>% select(coingredient,deno) %>% rename(deno2=deno),by='coingredient',relationship='many-to-many') %>%
		dplyr::mutate(rhoval=sum_cova / (deno * deno2)) %>%
		dplyr::filter(!is.na(rhoval)) %>% 
		dplyr::select(short_ingredient,coingredient,ncocktails,rhoval) %>%
		dplyr::filter(ncocktails > 2) %>%
		dplyr::arrange(dplyr::desc(rhoval))
}

.prepare_ternary <- function(both,two_ing) {
	stopifnot(length(two_ing) > 1)
	stopifnot(all(c('cocktail_id','cocktail','rating','page_src') %in% colnames(both$cocktail)))
	stopifnot(all(c('cocktail_id','unit','short_ingredient','proportion') %in% colnames(both$recipe)))

	bycols <- both$recipe %>%
		dplyr::filter(unit=='fl oz',
									short_ingredient %in% two_ing) %>%
		dplyr::group_by(cocktail_id,short_ingredient) %>%
			dplyr::summarize(tot_amt=sum(proportion,na.rm=TRUE)) %>%
		dplyr::ungroup() %>%
		dplyr::rename(proportion=tot_amt) %>%
		dplyr::group_by(cocktail_id) %>%
			dplyr::mutate(has_both=length(proportion) > 1,
										Other=1 - sum(proportion)) %>%
		dplyr::ungroup() %>%
		dplyr::filter(has_both) 

	# fun! get CRAN checks to shut up.
	. <- NULL
	retv <- bycols %>%
		tidyr::spread(key=short_ingredient,value=proportion,fill=0) %>%
		setNames(gsub('\\s','_',names(.))) %>%
		dplyr::left_join(both$cocktail %>% select(cocktail_id,cocktail,rating,page_src),by='cocktail_id')
}

.make_bar_plot <- function(both_df) {
	#facet_grid(.~rating) + 
	ph <- both_df %>%
		dplyr::filter(unit=='fl oz') %>%
		dplyr::arrange(dplyr::desc(rating)) %>%
		mutate(pct_amt=100*proportion) %>%
		ggplot(aes(ingredient,pct_amt,fill=cocktail)) + 
		geom_col(position='dodge') + 
		coord_flip() +
		labs(y='amount (%)',
				 x='ingredient',
				 title='selected drinks')
}

#' @importFrom ggtern ggtern Tlab Llab Rlab
.make_ggtern_plot <- function(tern_df,preing) {
	ing <- gsub('\\s','_',preing)
	ph <- tern_df %>%
		ggtern::ggtern(ggplot2::aes_string(x=ing[1],y=ing[2],z='Other',
																			 shape='page_src',label='cocktail',color='rating')) +
		ggplot2::geom_point(aes(size=rating),alpha=0.5) +
		ggtern::Llab(preing[1]) + ggtern::Tlab(preing[2]) + 
		ggplot2::geom_text(hjust='inward',vjust='inward') +
		ggplot2::guides(shape=guide_legend(title='source'))
	ph
}

# testing
#preing <- c('bourbon','benedictine') 
#tern_df <- data.frame(bourbon=runif(50,max=0.5),benedictine=runif(50,max=0.3)) %>% 
	#mutate(Other=1-bourbon-benedictine) %>%
	#mutate(rating=sample(2:5,n(),replace=TRUE)) %>%
	#mutate(cocktail=sample(paste0('xy',letters),n(),replace=TRUE)) %>%
	#mutate(page_src=sample(1:3,n(),replace=TRUE)) 
#

#.make_tern_plot <- function(tern_df,preing) {
	#preing <- c(preing,'Other')
	#TernaryPlot(alab=paste(preing[1],'\u2192'),
							#blab=paste(preing[2],'\u2192'),
							#clab=paste('\u2190',preing[3]),
							#atip=preing[1],btip=preing[2],ctip=preing[3],
							#point='up', lab.cex=0.8, grid.minor.lines = 0,
							#grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white', 
							#axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
							#padding=0.08)
							 ##bg=tern_df$rating / 3,
	#coords <- tern_df %>% select(one_of(preing[1]),one_of(preing[2]),one_of(preing[3]))
	#blue0 <- min(min(tern_df$rating),1)
	#blueness <- (tern_df$rating - blue0) / (5 - blue0)
	#redness <- 1 - blueness
	#fac_src <- factor(tern_df$page_src)
	#pch0 <- 22
	#TernaryPoints(coords,
								#col=rgb(red=redness,green=0,blue=blueness,alpha=0.25),
								#bg=rgb(red=redness,green=0,blue=blueness,alpha=0.25),
								#cex=tern_df$rating / 3,
								#pch=pch0+as.numeric(fac_src))
	#TernaryText(coords,
							#tern_df$cocktail,
							#col=rgb(red=redness,green=0,blue=blueness,alpha=0.65),
							#bg=rgb(red=redness,green=0,blue=blueness,alpha=0.65),
							#cex=1)
	#legend('right',
				 #pt.cex=1.8,
				 #pt.bg=rgb(0, 0, 255, 128, NULL, 255), 
				 #pch=pch0+(1:length(levels(fac_src))),
				 #legend=levels(fac_src),
				 #cex=0.8, bty='n')
#}

# WAT?
#  @param input  the shiny server (reactive) input list.
#  @param output the shiny server (reactive) output list.
#  @param session  a shiny server session object?
# Define server logic # FOLDUP
my_server <- function(input, output, session) { # nocov start
	get_both <- reactive({
		utils::data("cocktails", package="cocktailApp")
		both <- .gen_both(cocktails)
	})
	# like a covariance of ingredients
	get_ing_rho <- reactive({
		both <- get_both()
		# add rating
		recipe_df <- both$recipe %>%
			left_join(both$cocktail,by='cocktail_id')
		rhov <- .ingredient_rho(recipe_df)
	})
	suggested_ingr <- reactive({
		retv <- get_ing_rho() %>%
			rename(ingredient=short_ingredient) %>%
			dplyr::filter(ingredient %in% input$must_have_ing,ingredient != coingredient,ncocktails > 5) %>%
			dplyr::arrange(dplyr::desc(rhoval))
	})

	filter_ingr <- reactive({
		.filter_ingredients(both=get_both(),name_regex=input$name_regex,extra_ids=hobsons_choice$ids,
												must_have_ing=input$must_have_ing,
												must_not_have_ing=input$must_not_have_ing,
												ing_regex=input$ing_regex,
												logical_sense=input$logical_sense)
	})
	filter_num_ingr <- reactive({
		.filter_num_ingredients(both=filter_ingr(),must_have_ing=input$must_have_ing,
														min_rating=input$min_rating,max_ingr=input$max_ingr,
														max_other_ingr=input$max_other_ingr)

	})
	filter_tstat <- reactive({
		.filter_tstat(both=filter_num_ingr(),min_t=input$min_tstat,t_zero=input$t_zero)
	})
	filter_src <- reactive({
		.filter_src(both=filter_tstat(),from_sources=input$from_sources)
	})
	final_both <- reactive({
		.add_description(both=filter_src())
	})
	final_merged <- reactive({
		.merge_both(both=final_both())
	})
	selectable <- reactive({
	})

	# if the user selects any drinks from the table, 
	# take their ingredients
	selected_drinks <- reactive({
		both <- final_both()
		drinks <- both$cocktail 

		selrows <- input$drinks_table_rows_selected
		otdat <- both$recipe %>%
			dplyr::inner_join(drinks[selrows,] %>% 
												dplyr::select(cocktail_id,cocktail,rating),by='cocktail_id')
		otdat
	})

	hobsons_choice <- reactiveValues(ids=NULL)
	
	observeEvent(input$hobsons,{
		if (input$hobsons) {
			both <- .filter_num_ingredients(both=get_both(),must_have_ing=c(),
															min_rating=input$min_rating,max_ingr=input$max_ingr,
															max_other_ingr=input$max_other_ingr) 
			both <- .filter_tstat(both=both,min_t=input$min_tstat,t_zero=input$t_zero)
			both <- .filter_src(both=both,from_sources=input$from_sources)
			eligible <- both$cocktail %>%
				distinct(cocktail_id) %>%
				sample_n(size=5,replace=FALSE)
			hobsons_choice$ids <- eligible$cocktail_id
		} else {
			hobsons_choice$ids <- NULL
		}
	})
	# table of comparables #FOLDUP
	output$drinks_table <- DT::renderDataTable({
		otdat <- .drinks_table(both=final_both())
		# for this javascript shiznit, recall that javascript starts
		# counting at zero!
		#
		# cf 
		# col rendering: http://rstudio.github.io/DT/options.html
		# https://github.com/jcheng5/shiny-jsdemo/blob/master/ui.r
		DT::datatable(otdat,caption='Matching cocktails. Click on a row to populate the ingredients table below.',
									escape=FALSE,rownames=FALSE,
									options=list(order=list(list(1,'desc'),list(0,'desc'),list(2,'asc')),
															 paging=TRUE,
															 pageLength=15))
	},
	server=TRUE)#UNFOLD
	# table of suggestions #FOLDUP
	output$suggestions_table <- DT::renderDataTable({
		selco <- suggested_ingr() 

		# for this javascript shiznit, recall that javascript starts
		# counting at zero!
		#
		# cf 
		# col rendering: http://rstudio.github.io/DT/options.html
		# https://github.com/jcheng5/shiny-jsdemo/blob/master/ui.r
		DT::datatable(selco, caption='Coingredients.',
									escape=FALSE, rownames=FALSE,
									options=list(paging=TRUE,
															 pageLength=20)) %>%
		DT::formatRound(columns=c('rhoval'),digits=2)
	},
	server=TRUE)#UNFOLD

	output$selected_ingredients_bar_plot <- renderPlot({
		ph <- selected_drinks() %>% .make_bar_plot()
		ph
	})
	output$ingredients_table <- renderTable({
		#	may have to select down some more.
		retv <- selected_drinks() %>% dplyr::select(cocktail,amt,unit,ingredient)
	},striped=TRUE,width='100%')

	output$selected_ingredients_tern_plot <- renderPlot({
		shiny::validate(shiny::need(length(input$must_have_ing) > 1,'Must select 2 or more must have ingredients.'))
		preing <- input$must_have_ing[1:2]

		tern_df <- .prepare_ternary(both=final_both(),two_ing=preing)
		shiny::validate(shiny::need(nrow(tern_df) > 0,paste('No cocktails found with both',preing[1],'and',preing[2])))
		ph <- .make_ggtern_plot(tern_df,preing=preing)
		# see https://github.com/rstudio/shiny/issues/915
		print(ph)
		NULL
		#.make_tern_plot(tern_df,preing=preing)
	},height=900,width=1300)
	#'drinks_table_rows_all',
	setBookmarkExclude(c('bookmark',
											 'drinks_table_cell_clicked',
											 'drinks_table_row_last_clicked'))
	observeEvent(input$bookmark,{ session$doBookmark() })
} # nocov end

# UNFOLD

#' @title cocktailApp .
#'
#' @description 
#'
#' A \code{shiny} app to explore cocktails. The app allows you to enter ingredients
#' that a cocktail must have, or ingredients that it must not have. One can
#' filter by number of ingredients, minimum rating, minimum \sQuote{t stat}
#' (computed as the rating minus the T stat zero all multiplied by the square
#' root of the number of ratings). One can also search for cocktail by regex.
#'
#' In the main tab, titled \dQuote{drinks}, one can find a table with the
#' summaries of matching cocktails. Selecting rows of this table will
#' cause the cocktail table below to be populated with more details on each
#' selected cocktail. Selecting rows will also populate the bar chart
#' in the \dQuote{plots} tab.
#'
#' If two or more ingredients are selected, drinks with non-zero quantities
#' of both of these will be shown in a ternary plot in the \dQuote{tern}
#' tab. 
#'
#' In the \dQuote{other} tab is a table with common co-ingredients of the
#' selected ingredients. A co-ingredient is an ingredient that commonly
#' occurs with the selected ingredient, as measured by the number of
#' cocktails, and by \sQuote{rho}, which is like a correlation based
#' on the proportion.
#' 
#' A checkbox labelled, \dQuote{Hobson's Choice} allows you to populate
#' the cocktail table with five random cocktails that meet the numerical
#' filters on number of ingredients, rating, and so on, but which do not
#' meet the ingredient selections. Unselecting and re-selecting the
#' checkbox selects a new set of random cocktails. Note that the random
#' selection is not responsive to changes in the numerical filters.
#'
#' @section Screenshots:
#'
#' The main page looks as follows. In this case the user has selected
#' two ingredients, \sQuote{Benedictine} and \sQuote{Bourbon}. The
#' user has modified some of the numeric filters resulting in only
#' six cocktails in the cocktail table on the right in the main
#' tab.
#'
#' \if{html}{
#' \figure{Screenshot-mainpage.png}{options: width="100\%" alt="Screenshot: landing page of app"}
#' }
#' \if{latex}{
#' \figure{Screenshot-mainpage.png}{options: width=14cm}
#' }
#'
#' In the next screenshot, the user has selected two of
#' the rows of the cocktail table,
#' which causes the ingredients table
#' on the lower right to be populated with the recipes of the
#' selected cocktails. Instead one could click on the linked
#' cocktail names to be taken to the upstream source of the recipe,
#' which is recommended since those pages typically have better
#' instructions.
#'
#' \if{html}{
#' \figure{Screenshot-ingredients.png}{options: width="100\%" alt="Screenshot: landing page of app, with selected cocktails"}
#' }
#' \if{latex}{
#' \figure{Screenshot-ingredients.png}{options: width=14cm}
#' }
#'
#' In the following screenshot, the user has selected two ingredients,
#' \sQuote{Benedictine} and \sQuote{bourbon}, then clicked on the
#' the main table, then selected the \sQuote{plots} tab. This
#' shows a bar plot of the proportions of all ingredients
#' in all the selected cocktails.
#'
#' \if{html}{
#' \figure{Screenshot-barplot.png}{options: width="100\%" alt="Screenshot: bar plot of ingredients"}
#' }
#' \if{latex}{
#' \figure{Screenshot-barplot.png}{options: width=14cm}
#' }
#'
#' In this screenshot, the user has selected two ingredients,
#' \sQuote{Benedictine} and \sQuote{bourbon}, then clicked on the
#' \sQuote{ternary} tab, which shows a ternary plot of the proportions
#' of cocktails with non-zero proportions of the first two selected
#' ingredients. The third dimension of the ternary plot is \sQuote{other}
#' ingredients.
#'
#' \if{html}{
#' \figure{Screenshot-ternary.png}{options: width="100\%" alt="Screenshot: ternary plot of ingredients"}
#' }
#' \if{latex}{
#' \figure{Screenshot-ternary.png}{options: width=14cm}
#' }
#'
#' In this screenshot, the user has checked the \dQuote{Hobson's Choice}
#' box, which adds 5 random cocktails to the cocktail table.
#'
#' \if{html}{
#' \figure{Screenshot-hobsons.png}{options: width="100\%" alt="Screenshot: main page with Hobsons choice"}
#' }
#' \if{latex}{
#' \figure{Screenshot-hobsons.png}{options: width=14cm}
#' }
#'
#' @return Runs the \code{shiny} app.
#'
#' @param page_title  an optional page title for the app. A \code{NULL} value
#' causes no page title to be used.
#' @inheritParams shiny::shinyApp
#' @keywords shiny
#' @template etc
#' @name cocktailApp
#' @rdname cocktailApp
#' @examples 
#' \dontrun{
#' cocktailApp()
#' }
#' @export
cocktailApp <- function(page_title='Drink Schnauzer',enableBookmarking='url') {
	shinyApp(ui=function(request){ my_ui(page_title=page_title) },  # to enable bookmarking, do this rigamarole.
					 server=my_server,
					 enableBookmarking=enableBookmarking)
}
# importFrom DT dataTableOutput renderDataTable datatable 

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
