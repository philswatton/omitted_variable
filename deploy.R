#### Phil Swatton - University of Essex/UCL ####
# Script for deploying shiny app designed to let students visualise the effect of ommitted variable bias

library(rsconnect)

rsconnect::setAccountInfo(name='philswatton',
                          token='AA02538D4AA16CE67C7E38E72DB0A466',
                          secret='<SECRET>')

rsconnect::deployApp('C:\\Users\\User\\Documents\\Projects\\omitted_variable')