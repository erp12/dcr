###################################################################################################
## Commonly Used Reduce Functions
###################################################################################################

##' Reduce function to calculate sum of a variable for each group
##' @param var the name of the variable to sum over
##' @export
##'
reduceSum <- function(var) dc_code(sprintf("reduceSum(function(d) {return d.%s;})", var))

##' Reduce function to count number of records for each group
##' @export
reduceCount <- function() dc_code("reduceCount()")

##' Reduce function to calculate mean of a variable for each group
##' @param var the name of the variable to calculate mean
##' @param weight variable specifying weight when calculating mean
##' @export
##'
reduceMean <- function(var, weight = NULL) {
  if (is.null(weight)) return(reduceMean_nw(var))
  return(reduceMean_w(var, weight))
}

reduceMean_nw <- function(var) {
  code <- sprintf("reduce(function (p, v) {
                  ++p.count;
                  p.sum += v.%s;
                  p.avg = p.sum / p.count;
                  return p;
                  },
                  function (p, v) {
                  --p.count;
                  p.sum -= v.%s;
                  if (p.count == 0) {
                  p.avg = 0;
                  } else {
                  p.avg = p.sum / p.count;
                  }
                  return p;
                  },
                  function () {
                  return {
                  count: 0,
                  sum: 0
                  };
                  })
                  ", var, var)
  attr(code, "reduceMean") <- var
  code
}

reduceMean_w <- function(var, w) {
  code <- sprintf("reduce(function (p, v) {
                  p.count += v.%s;
                  p.sum += v.%s * v.%s;
                  p.avg = p.sum / p.count;
                  return p;
                  },
                  function (p, v) {
                  p.count -= v.%s;
                  p.sum -= v.%s * v.%s;
                  if (p.count == 0) {
                  p.avg = 0;
                  } else {
                  p.avg = p.sum / p.count;
                  }
                  return p;
                  },
                  function () {
                  return {
                  count: 0,
                  sum: 0
                  };
                  })
                  ", w, w, var, w, w, var)
  attr(code, "reduceMean") <- var
  code
}
