FROM r-base:4.3.1
WORKDIR /app
RUN apt update && apt install -y locales locales-all fonts-noto ttf-ancient-fonts
RUN R -e "options(repos = \
list(CRAN='https://cloud.r-project.org/'),install.packages.compile.from.source = 'never');\
install.packages(c('shiny','ggplot2','stringr','foreach','seewave'))" && echo "local(options(shiny.port = 8888, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
COPY . .
EXPOSE 8888
CMD ["R","-e","shiny::runApp('.')"]