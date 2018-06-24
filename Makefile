######################
# 
# Created: 2018-06-15
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav
######################

############### FLAGS ###############

VMAJOR 						 = 0
VMINOR 						 = 1
VPATCH  					 = 0
#VDEV 							 = .0004
VDEV 							 = 
PKG_NAME 					:= cocktailApp

RPKG_USES_RCPP 		:= 0

include ./rpkg_make/Makefile

PKG_DEPS 					+= data/cocktails.rda

data-raw/cocktails.csv : ../drinksy/drinks.csv
	cp $< $@

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=129:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:tags=.tags;:syn=make:ft=make:ai:si:cin:nu:fo=croqt:cino=p0t0c5(0:
