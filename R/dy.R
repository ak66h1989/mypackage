

dy<- function(title="new world") {

  d<-dygraph(lungDeaths,main = title)
  htmlwidgets::saveWidget(d, "dy.html", selfcontained = FALSE)
}


#dy<- function(title="new world") {
#  library(dygraphs) #需要先匯入或寫在depends,跟yearmon有關

#  d<-dygraph::dygraph(lungDeaths,main = title)
#  d
#}

