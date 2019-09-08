

gband <- function(xmin, xmax, fill="grey", alpha=.5, ymin=-Inf, ymax=Inf) {
  annotate("rect",
           fill = fill,
           alpha = alpha, # larger alpha is darker rectangle
           xmin = xmin, xmax = xmax,
           ymin = ymin, ymax = ymax)
}


ma <- function(x, t, align="right") {
  zoo::rollapply(x, t, function(x) mean(x, na.rm = TRUE), fill = NA, 
                 align = align)
}


get_puf_vnames <- function(){
  # get data frame with PUF variables names and labels (vdesc)
  readRDS("./data/puf.vnames.rds")
}
