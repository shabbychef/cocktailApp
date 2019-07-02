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

context("app-function")
# This file is for testing the applications in the apps/ directory.

library(shinytest)

test_that("cocktailApp() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

	# WTF is the apps directory?

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp("apps/the_app/", compareImages = FALSE))
})

#for vim modeline: (do not edit)
# vim:ts=2:sw=2:tw=79:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r:ai:si:cin:nu:fo=croql:cino=p0t0c5(0:
