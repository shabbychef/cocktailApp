library(shinytest)
app <- ShinyDriver$new("../")
app$snapshotInit("crantest")

app$snapshot()
app$setInputs(must_have_ing = "Bourbon")
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()
app$setInputs(must_have_ing = c("Bourbon", "Averna"))
app$snapshot()
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(logical_sense = "AND")
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()
# Input 'suggestions_table_rows_current' was set, but doesn't have an input binding.
# Input 'suggestions_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()
app$setInputs(from_sources = c("diffordsguide", "kindredcocktails", "webtender"))
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(max_ingr = 4)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(min_rating = 4.5)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(min_rating = 1.5)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(min_tstat = 23.5)
app$setInputs(min_tstat = 10)
app$setInputs(min_tstat = 6)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(max_ingr = 6)
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(logical_sense = "OR")
# Input 'drinks_table_rows_current' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_all' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_rows_selected' was set, but doesn't have an input binding.
# Input 'drinks_table_row_last_clicked' was set, but doesn't have an input binding.
# Input 'drinks_table_cell_clicked' was set, but doesn't have an input binding.
app$snapshot()
