
library("DBI")
# con <- dbConnect(RSQLite::SQLite(), ":memory:")
con <- dbConnect(RSQLite::SQLite(), "temp.sqlite")

dbWriteTable(con, "mtcars", mtcars[1:5, ])
dbReadTable(con, "mtcars")
dbDisconnect(con)
