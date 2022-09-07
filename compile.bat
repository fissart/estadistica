@echo off
Rscript -e "rmarkdown::render_site(encoding='UTF-8')"
pause