head(data)

#rem.data <- data[-123,]
  
IDN <- 1:100
IND <- 101:185
PHL <- 186:225
THA <- 226:360
VNM <- 361:415

country <- list(IDN, IND, PHL, THA, VNM)

fieldnames <- row.names(data)

data$field <- fieldnames

datField <- as.data.frame(t(data[, !names(data) %in% "field"]))

names(datField) <- fieldnames

#datField[1:5, 1:5]

datField <- datField[-1:-3, ] # delete row 1 to row 3

row.names(datField) <- NULL

datField <- apply(datField,2, as.numeric)

qgraph(cor(datField), 
       layout = "spring",
       minimum = 0.4,
       groups = country,
       color = c("red", "orange", "yellow", "blue", "pink"))

