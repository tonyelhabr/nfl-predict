cmd /c C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nfl-predict/R/_scrape-odds-tr.R"  >> "C:/Users/aelhabr/Documents/projects/nfl-predict/R/_scrape-odds-tr.log" 2>&1
rem Rscript "C:/Users/aelhabr/Documents/projects/nfl-predict/R/scrape-odds-tr.R"
rem R CMD BATCH "C:/Users/aelhabr/Documents/projects/nfl-predict/R/scrape-odds-tr.R"
rem pause

"C:\Program Files\Git\bin\git.exe" add .
"C:\Program Files\Git\bin\git.exe" commit -m 'scheduled commit'
rem "C:\Program Files\Git\bin\git.exe" commit -a -m 'scheduled commit'
"C:\Program Files\Git\bin\git.exe" push -u origin master
rem pause