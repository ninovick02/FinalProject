# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
# install plumber, tidymodels, tidyverse, ggplot2
RUN R -e "install.packages(c('tidymodels', 'plumber', 'tidyverse', 'ggplot2'))"

# copy myAPI.R from the current directory into the container
COPY plumber.R plumber.R
COPY savedtree.RData savedtree.RData
COPY Data/diabetes_binary_health_indicators_BRFSS2015.csv Data/diabetes_binary_health_indicators_BRFSS2015.csv


# open port to traffic
EXPOSE 8000

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
