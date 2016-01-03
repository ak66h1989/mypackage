DT<- function(data=iris) {

  DT<-datatable(data)
  htmlwidgets::saveWidget(DT, "DT.html", selfcontained = FALSE)
}

