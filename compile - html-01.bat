@echo off
Rscript -e "rmarkdown::render_site('01.Rmd', encoding='UTF-8')"
pause