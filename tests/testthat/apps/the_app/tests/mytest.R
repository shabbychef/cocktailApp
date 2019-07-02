app <- ShinyDriver$new("../", seed = 1234)
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(`must_have_ing-selectized` = "bou")
app$setInputs(`must_have_ing-selectized` = "bourbon")
app$setInputs(must_have_ing = "Bourbon")
app$setInputs(`must_have_ing-selectized` = "")
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(must_have_ing = character(0))
app$snapshot()
app$setInputs(`must_have_ing-selectized` = "C")
app$setInputs(`must_have_ing-selectized` = "Ch")
app$setInputs(`must_have_ing-selectized` = "Cha")
app$setInputs(`must_have_ing-selectized` = "Chartr")
app$setInputs(must_have_ing = "Chartreuse")
app$setInputs(`must_have_ing-selectized` = "/gree")
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(`must_have_ing-selectized` = "gree")
app$setInputs(`must_have_ing-selectized` = "green")
app$setInputs(`must_have_ing-selectized` = "green ch")
app$setInputs(`must_have_ing-selectized` = "green cha")
app$setInputs(`must_have_ing-selectized` = "green char")
app$setInputs(`must_have_ing-selectized` = "green chart")
app$setInputs(must_have_ing = c("Chartreuse", "Green Or Yellow Chartreuse"))
app$setInputs(`must_have_ing-selectized` = "Green")
app$setInputs(`must_have_ing-selectized` = "Green Char")
app$setInputs(must_have_ing = c("Chartreuse", "Green Or Yellow Chartreuse", "Green Chartreuse"))
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(max_ingr = 4)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(max_ingr = 3)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(max_other_ingr = 4)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(max_other_ingr = 3)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(min_rating = 4)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(min_rating = 4.5)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(must_have_ing = c("Chartreuse", "Green Chartreuse"))
app$setInputs(must_have_ing = "Green Chartreuse")
app$setInputs(`must_have_ing-selectized` = "gin")
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(must_have_ing = character(0))
app$setInputs(`must_have_ing-selectized` = "gin")
app$setInputs(must_have_ing = "Ginger")
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(must_have_ing = character(0))
app$setInputs(`must_have_ing-selectized` = "g")
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(`must_have_ing-selectized` = "dr")
app$setInputs(must_have_ing = "Dry Gin")
app$setInputs(`must_have_ing-selectized` = "ch")
app$setInputs(`must_have_ing-selectized` = "cha")
app$setInputs(`must_have_ing-selectized` = "char")
app$setInputs(`must_have_ing-selectized` = "chart")
app$setInputs(`must_have_ing-selectized` = "chartre")
app$setInputs(must_have_ing = c("Dry Gin", "Green Chartreuse"))
app$setInputs(max_ingr = 8)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(max_other_ingr = 8)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(min_rating = 3.5)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(min_rating = 3)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$setInputs(min_rating = 2)
app$setInputs(from_sources = c("diffordsguide", "kindredcocktails", "webtender"))
app$setInputs(from_sources = c("diffordsguide", "kindredcocktails", "webtender", "drinksmixer"))
app$snapshot()
app$setInputs(must_have_ing = "Dry Gin")
app$setInputs(must_have_ing = character(0))
app$setInputs(`must_have_ing-selectized` = "")
app$setInputs(`must_have_ing-selectized` = "bour")
app$setInputs(must_have_ing = "Bourbon")
app$setInputs(`must_have_ing-selectized` = "be")
app$setInputs(`must_have_ing-selectized` = "ben")
app$setInputs(must_have_ing = c("Bourbon", "Benedictine"))
app$snapshot()
