# Copyright 2018-2018 Steven E. Pav. All Rights Reserved.
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
# Created: 2018-07-01
# Copyright: Steven E. Pav, 2018-2018
# Author: Steven E. Pav
# Comments: Steven E. Pav

# because Hadley says it should be like this.
# see https://github.com/hadley/devtools/wiki/Testing

library(testthat)
library(cocktailApp)

# once you have a directory of tests, uncomment this.
test_check("cocktailApp")

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
