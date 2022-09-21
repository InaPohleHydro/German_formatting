

##number formatting with comma as decimal separator#####
comma.decimal <- function(my.number, #the number that should be formatted
                        number.digits = 2){ # number of digits
  gsub("\\.","\\,",format(round(my.number,number.digits),nsmall = number.digits))
}

##axis labels with comma as decimal separator and pre-defined axis labels and label positions#####
axis.comma.decimal <- function(side = 1, #side of the axis
                               my.numbers = c(1:10)/100, #the numbers that should be formatted
                               number.digits = 2, # number of digits
                               cex = 1){ #text size
  my.labels <- comma.decimal(my.numbers,number.digits = number.digits)
  axis(at = my.numbers, labels = my.labels, side = side, cex = cex)
}

##axis labels with comma as decimal separator, axis labels and label positions derived based on lower and upper thresholds as well as number of labels#####
axis.comma.decimal.auto <- function(side = 1, #side of the axis
                                    lower.limit = 0, #lower limit of the axis
                                    upper.limit = 0.25, #upper limit of the axis
                                    length.out = 5, #number of labels
                                    cex = 1){#text size
  digits.lower <- if(lower.limit != 0){ceiling(-(log(abs(lower.limit),10)))} else {0}
  digits.upper <- if(upper.limit != 0){ceiling(-(log(abs(upper.limit),10)))} else {0}
  number.digits <- max(c(digits.lower,digits.upper) + 2)
  my.numbers <- seq(lower.limit,upper.limit,length.out = length.out)
  my.labels <-  comma.decimal(my.numbers,number.digits = number.digits)
  axis(at = my.numbers, labels = my.labels, side = side, cex = cex)
}
