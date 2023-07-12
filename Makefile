######################
# 
# Created: 2018-06-15
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav
######################

############### FLAGS ###############

VMAJOR 						 = 0
VMINOR 						 = 2
VPATCH  					 = 3
#VDEV 							 = .0001
#VDEV 							 = .001
VDEV 							 = 
PKG_NAME 					:= cocktailApp

RPKG_USES_RCPP 		:= 0

include ./rpkg_make/Makefile

PKG_DEPS 					+= data/cocktails.rda

data-raw/cocktails.csv : ../drinksy/drinks.csv
	cp $< $@

# overload
data/%.rda : data-raw/%.csv
	r -l usethis,devtools,readr -e '$* <- readr::read_csv("$<",guess_max=100000);usethis::use_data($*,overwrite=TRUE,internal=FALSE)'

.PHONY : cocktail_data

cocktail_data : data/cocktails.rda  ## copy over data from ../drinksy

submodule_help : ## help with git submodules
	echo "git submodule update --init --recursive"

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
