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
#' @importFrom dplyr mutate arrange select filter rename left_join coalesce distinct summarize everything
#' @importFrom utils data
#' @importFrom ggplot2 ggplot labs coord_flip aes geom_col geom_point geom_text guide_legend
#' @importFrom shinythemes shinytheme
#' @importFrom magrittr %>%
#' @importFrom forcats fct_rev
#' @importFrom tibble tribble 
#' @importFrom tidyr spread
#' @importFrom ggtern ggtern Tlab Llab Rlab
#' @importFrom stats setNames
#'
#' @name cocktailApp
#' @rdname cocktailApp
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
#' @section \cocktailApp{} Initial Version 0.1.0 (2018-06-19) :
#' \itemize{
#' \item first CRAN release.
#' }
#'
#' @name cocktailApp-NEWS
#' @rdname NEWS
NULL

#' @title Cocktails Data
#' @description Ingredients of nearly 16 thousand cocktails, scraped from the web.
#' @format A \code{data.frame} object with 77,301 rows and 12 columns. The
#' data are scraped from three websites: Difford's guide, Webtender, and 
#' Kindred Cocktails, in late 2017.
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
#' @source Difford's Guide, \url{http://www.diffordsguide.com/},
#' Webtender, \url{http://www.webtender.com},
#' Kindred Cocktails, \url{http://kindredcocktails.com}.
#' @note 
#' The data were scraped from several websites, which falls in a legal gray area.
#' While, in general, raw factual data can not be copyright, there is a difference between the law and a lawsuit. 
#' The package author in no way claims any copyright on this data.
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @examples
#' \dontrun{
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
#' 	select(cocktail,ingredient,amt,unit,rating)
#'
#' }
"cocktails"


globalVariables(c('cocktails','votes','rating','cocktail','proportion','normalize_amt','url','short_ingredient','unit',
									'cocktail_id','coamount','amt','norm_amt',
									'deno','deno2','rhoval',
									'sum_cova','n','rat','tot_ingr','tot_has_ingr','tot_am','ncocktails',
									'tstat','page_src','tst',
									'has_or_must','has_and_must','has_not_must',
									'matches_name','ingr_class','description',
									'tot_amt','has_both','Other',
									'ingredient','coingredient','cova','wts'))


# Define UI for ...
my_ui <- function(){
	utils::data("cocktails", package="cocktailApp")
	indat <- cocktails

	# let's order ingredients by number of times they
	# appear, then alphabetical. seems about right.

	normo <- indat %>%
		dplyr::group_by(short_ingredient) %>%
		dplyr::summarize(tot_am=sum(proportion,na.rm=TRUE)) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(ingr_class=cut(tot_am,breaks=c(-1,0,10,100,1000),
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
			titlePanel("Drink Schnauzer"),
			# tags$img(id = "logoimg", src = "logo.png", width = "200px"),
			sidebarLayout(#FOLDUP
				position="left",
			sidebarPanel(#FOLDUP
				width=2,
				h3('Parameters'),
				selectInput("must_have_ing","Must Have:",choices=ingr,selected=c(),multiple=TRUE),
				selectInput("logical_sense","Join by:",choices=c('OR','AND'),selected='OR',multiple=FALSE),
				selectInput("must_not_have_ing","Must Not Have:",choices=ingr,selected=c(),multiple=TRUE),
				selectInput("from_sources","Sources:",choices=all_source,selected=all_source[grepl('diffords|kindred',all_source)],multiple=TRUE),
				textInput("name_regex","Name Regex:",value='',placeholder='^sazerac'),
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
	
}  


.applylink <- function(title,url) {
	as.character(a(title,href=url,target="_blank"))
}
applylink <- function(title,url) {
	as.character(mapply(.applylink,title,url))
}

# Define server logic # FOLDUP
my_server <- function(input, output, session) {
	just_load <- reactive({
		#indat <- readr::read_csv('data/cocktails.csv')
		utils::data("cocktails", package="cocktailApp")
		indat <- cocktails
	})

	get_all <- reactive({
		indat <- just_load() %>%
			dplyr::mutate(votes=as.numeric(votes)) %>%
			dplyr::mutate(tstat=signif((rating - input$t_zero) * sqrt(coalesce(votes,20)),digits=2)) %>%
			dplyr::mutate(page_src=gsub('^http://(www.)?(.+).com/.+$','\\2',url)) 

		# fake a distinct id
		subs <- indat %>%
			distinct(cocktail,url) %>%
			tibble::rowid_to_column(var='cocktail_id')
		indat <- indat %>%
			dplyr::left_join(subs,by=c('cocktail','url'))
		indat
	})
	get_normalized <- reactive({
		indat <- get_all()
		normo <- indat %>%
			rename(normalize_amt=proportion)
	})

	get_coingredients <- reactive({
		normo <- get_normalized()
		coing <- normo %>% 
			dplyr::filter(!is.na(normalize_amt)) %>%
			dplyr::select(short_ingredient,cocktail_id,rating,normalize_amt) %>%
			dplyr::rename(ingredient=short_ingredient) %>%
			dplyr::mutate(rating=coalesce(rating,1)) %>%
			dplyr::inner_join(normo %>% 
								 dplyr::select(short_ingredient,cocktail_id,rating,normalize_amt) %>%
								 dplyr::rename(ingredient=short_ingredient) %>%
								 dplyr::rename(coingredient=ingredient,coamount=normalize_amt),by=c('cocktail_id','rating')) %>%
			dplyr::mutate(cova=normalize_amt * coamount) %>%
			dplyr::mutate(wts=rating) %>%
			dplyr::group_by(ingredient,coingredient) %>%
				dplyr::summarize(sum_cova=sum(cova*wts,na.rm=TRUE),
									sum_wts=sum(wts,na.rm=TRUE),
									ncocktails=n()) %>%
			dplyr::ungroup() %>%
			dplyr::arrange(dplyr::desc(ncocktails))
	})
	# like a covariance of ingredients
	get_ing_rho <- reactive({
		coing <- get_coingredients()
		
		diagv <- coing %>%
			dplyr::filter(ingredient==coingredient) %>%
			dplyr::mutate(deno=sqrt(sum_cova))

		rhov <- coing %>%
			dplyr::left_join(diagv %>% select(ingredient,deno),by='ingredient') %>%
			dplyr::left_join(diagv %>% select(coingredient,deno) %>% rename(deno2=deno),by='coingredient') %>%
			dplyr::mutate(rhoval=sum_cova / (deno * deno2)) %>%
			dplyr::filter(!is.na(rhoval)) %>% 
			dplyr::select(ingredient,coingredient,ncocktails,rhoval) %>%
			dplyr::filter(ncocktails > 2) %>%
			dplyr::arrange(dplyr::desc(rhoval))
	})

	suggested_ingr <- reactive({
		rhov <- get_ing_rho()
		rhov %>%
			dplyr::filter(ingredient %in% input$must_have_ing) %>%
			dplyr::filter(ingredient != coingredient) %>%
			dplyr::filter(ncocktails > 5) %>%
			dplyr::arrange(dplyr::desc(rhoval))
	})

	filter_ingr <- reactive({
		normo <- get_normalized()
		if (nzchar(input$name_regex)) {
			match_name <- normo %>%
				dplyr::distinct(cocktail,cocktail_id) %>%
				dplyr::filter(grepl(pattern=input$name_regex,x=cocktail,ignore.case=TRUE,
										 perl=TRUE,fixed=FALSE)) %>%
				dplyr::distinct(cocktail_id) %>%
				dplyr::mutate(matches_name=nzchar(input$name_regex))
		} else {
			# empty
			match_name <- tibble::tribble(~cocktail_id,~matches_name)
		}

		otdat <- normo %>%
			dplyr::group_by(cocktail_id) %>%
				dplyr::mutate(has_or_must=any(short_ingredient %in% input$must_have_ing),
							 has_and_must=all(input$must_have_ing %in% short_ingredient),
							 has_not_must=any(short_ingredient %in% input$must_not_have_ing)) %>%
			dplyr::ungroup() %>%
			dplyr::left_join(match_name,by='cocktail_id') %>%
			dplyr::mutate(matches_name=coalesce(matches_name,FALSE)) %>%
			dplyr::filter( (!has_not_must & ((input$logical_sense=='AND') | has_or_must) & ((input$logical_sense=='OR') | has_and_must)) |
						 matches_name) %>%
			dplyr::select(-has_and_must,-has_not_must,-has_or_must,-matches_name)
		otdat
	})


	filter_num_ingr <- reactive({
		indat <- filter_ingr()
		rdat <- indat %>%
			dplyr::group_by(cocktail_id) %>%
				dplyr::summarize(rat=dplyr::first(rating),
									tst=dplyr::first(tstat),
									tot_ingr=sum(grepl('fl oz',unit)),
									tot_has_ingr=sum(short_ingredient %in% input$must_have_ing)) %>%
			dplyr::ungroup() %>%
			dplyr::filter(rat >= input$min_rating,
						 tst >= input$min_tstat,
						 tot_ingr <= input$max_ingr,
						 tot_ingr <= input$max_other_ingr + tot_has_ingr) %>%
			distinct(cocktail_id)

		otdat <- indat %>%
			dplyr::inner_join(rdat %>% dplyr::select(cocktail_id),by=c('cocktail_id'))
		otdat
	})
	filter_src <- reactive({
		otdat <- filter_num_ingr() %>%
			dplyr::filter(page_src %in% input$from_sources) %>% 
			dplyr::select(cocktail,rating,amt,unit,ingredient,everything()) %>%
			dplyr::arrange(dplyr::desc(rating),cocktail,dplyr::desc(as.numeric(grepl('fl oz',unit))),dplyr::desc(amt))
	})
	
	filtered_cocktails <- reactive({
		otdat <- filter_src() %>%
			dplyr::select(-page_src) 

		descdat <- otdat %>%
			dplyr::filter(grepl('fl oz',unit)) %>%
			dplyr::arrange(dplyr::desc(amt)) %>%
			dplyr::group_by(cocktail_id) %>%
				summarize(description=paste0(paste0(short_ingredient,collapse=', '),'.')) %>%
			dplyr::ungroup() 

		drinks <- otdat %>%
			dplyr::distinct(cocktail_id,cocktail,url,rating,tstat) %>%
			dplyr::left_join(descdat,by=c('cocktail_id')) 

		ingredients <- otdat %>%
			dplyr::select(cocktail_id,cocktail,amt,unit,ingredient)

		list(drinks=drinks,ingredients=ingredients)
	})

	selected_drinks <- reactive({
		selco <- filtered_cocktails()
		drinks <- selco$drinks
		selrows <- input$drinks_table_rows_selected
		otdat <- selco$ingredients %>%
			dplyr::inner_join(drinks[selrows,] %>% 
								 dplyr::select(cocktail_id,rating),by='cocktail_id')
		otdat
	})

	# table of comparables #FOLDUP
	output$drinks_table <- DT::renderDataTable({
		selco <- filtered_cocktails()
		otdat <- selco$drinks %>%
			dplyr::mutate(cocktail=applylink(cocktail,url)) %>%
			select(rating,tstat,cocktail,description)

		# for this javascript shiznit, recall that javascript starts
		# counting at zero!
		#
		# cf 
		# col rendering: http://rstudio.github.io/DT/options.html
		# https://github.com/jcheng5/shiny-jsdemo/blob/master/ui.r
		DT::datatable(otdat,
									caption='Matching cocktails. Click on a row to populate the ingredients table below.',
									escape=FALSE,
									rownames=FALSE,
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
		DT::datatable(selco,
									caption='Coingredients.',
									escape=FALSE,
									rownames=FALSE,
									options=list(paging=TRUE,
															 pageLength=20)) %>%
		DT::formatRound(columns=c('rhoval'),digits=2)
	},
	server=TRUE)#UNFOLD

	output$selected_ingredients_bar_plot <- renderPlot({
		#flist <- filtered_cocktails()
		#plotdat <- flist$ingredients %>%
			#dplyr::filter(grepl('fl oz',unit)) %>%
			#dplyr::group_by(cocktail) %>% 
				#dplyr::mutate(norm_amt=amt / sum(amt,na.rm=TRUE)) %>%
			#dplyr::ungroup() 
			##arrange(desc(rating))
		plotdat <- selected_drinks() %>%
			dplyr::filter(grepl('fl oz',unit)) %>%
			dplyr::group_by(cocktail_id) %>% 
				dplyr::mutate(norm_amt=amt / sum(amt,na.rm=TRUE)) %>%
			dplyr::ungroup() %>%
			dplyr::arrange(dplyr::desc(rating))

			#facet_grid(.~rating) + 
		ph <- plotdat %>%
			dplyr::mutate(pct_amt=100*norm_amt) %>%
			ggplot(aes(ingredient,norm_amt,fill=cocktail)) + 
			geom_col(position='dodge') + 
			coord_flip() +
			labs(y='amount (%)',
					 x='ingredient',
					 title='selected drinks')
		ph
	})
	output$ingredients_table <- renderTable({
		otdat <- selected_drinks() %>%
			select(-cocktail_id,-rating)
	},striped=TRUE,width='100%')

	output$selected_ingredients_tern_plot <- renderPlot({
		shiny::validate(shiny::need(length(input$must_have_ing) > 1,'Must select 2 or more must have ingredients.'))
		preing <- input$must_have_ing[1:2]

		otdat <- filter_src()

		blah <- otdat %>%
			dplyr::filter(unit=='fl oz') %>%
			dplyr::filter(short_ingredient %in% preing) %>%
			dplyr::select(short_ingredient,page_src,cocktail,rating,cocktail_id,normalize_amt) %>%
			dplyr::group_by(cocktail_id,cocktail,page_src,rating,short_ingredient) %>%
				dplyr::summarize(tot_amt=sum(normalize_amt,na.rm=TRUE)) %>%
			dplyr::ungroup() %>%
			dplyr::rename(normalize_amt=tot_amt) %>%
			dplyr::group_by(cocktail_id) %>%
				dplyr::mutate(has_both=length(normalize_amt) > 1,
											Other=1 - sum(normalize_amt)) %>%
			dplyr::ungroup() %>%
			dplyr::filter(has_both) 

		# fun! get CRAN checks to shut up.
		. <- NULL
		shiny::validate(shiny::need(nrow(blah) > 0,'No cocktails have those two ingredients. Try again.'))
		blah <- blah %>%
			tidyr::spread(key=short_ingredient,value=normalize_amt,fill=0) %>%
			setNames(gsub('\\s','_',names(.)))

		ing <- gsub('\\s','_',preing)

		ph <- blah %>%
				ggtern::ggtern(ggplot2::aes_string(x=ing[1],y=ing[2],z='Other',
																					 shape='page_src',label='cocktail',color='rating')) +
				ggplot2::geom_point(aes(size=rating),alpha=0.5) +
				ggtern::Llab(preing[1]) + ggtern::Tlab(preing[2]) + 
				ggplot2::geom_text(hjust='inward',vjust='inward') +
				ggplot2::guides(shape=guide_legend(title='source'))
		# see https://github.com/rstudio/shiny/issues/915
		print(ph)
		NULL
	},height=900,width=1300)


	setBookmarkExclude(c('bookmark'))
	observeEvent(input$bookmark,{
								 session$doBookmark()
	})
}

# UNFOLD

#' @title cocktailApp .
#'
#' @description 
#'
#' A shiny app to explore cocktails. The app allows you to enter ingredients
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
#' @usage
#'
#' cocktailApp()
#'
#' @return Runs the shiny app.
#'
#' @keywords shiny
#' @template etc
#'
#' @examples 
#' \dontrun{
#' cocktailApp()
#' }
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @export
cocktailApp <- function() {
	shinyApp(ui=my_ui(), server=my_server)
}
# importFrom DT dataTableOutput renderDataTable datatable 

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
