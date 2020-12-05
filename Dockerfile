FROM r-base
RUN R -e "install.packages(c('methods', 'jsonlite', 'tseries'), \
                           dependencies=TRUE, \
                           repos='http://cran.rstudio.com/')" 
COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts
CMD ["Rscript", "myscript.R"]