#' The internal parameter permutation function
#'
#' This function is internal and used for creating order independent params
#' @param param.names names of multiplicative parameters to permute (character vector)
#' @param values values of multiplicative parameters to permute (numeric vector)
#' @param n.cores number of cores for doParallel (numeric), default = 2
#' @keywords internal
#' @return a dataframe with columns param.names (character) and avg.xx (numeric)
#'
#' @note Performance may be slow with large numbers of parameters. sytem.time elapsed is 3 params ~ 0.011 | 5 params ~ 0.042 | 7 params ~ 1.741 | 8 params ~ 39.224
#' @import dplyr
#' @export
#' @examples
#' wParamPermute(param.names = c("one", "hundred", "five"), values = c(1, 100, 5))


wParamPermute <- function(param.names, values, n.cores = 2) {
  n_params <- length(param.names)
  if (n_params > 10){ # too much
    stop("Do you really have more than 10 multiplicative factors? This code can handle up to 10.", call. = FALSE)
  }
  permutations <- function(x, prefix = c()) {
    # creates a permutation matrix for one vector
    if (length(x) == 0)
      return(prefix)
    do.call(rbind, sapply(
      1:length(x), FUN = function(idx)
        permutations(x[-idx], c(prefix, x[idx])), simplify = FALSE
    ))
  }
  if (n_params > 4) {
    #parallel computing for large permutations
    if (requireNamespace("doParallel", quietly = TRUE)) {
      library(doParallel)
      registerDoParallel(cores = n.cores)
    }
  } else {
    warning(
      "doParallel may make this function faster for large order permutations if it is installed.",
      call. = FALSE
    )
  }

  pf <- permutations(param.names) # these are the names
  upf <- permutations(values) # these are the associated values
  cpf <- cbind(data.frame(pf), data.frame(upf))
  colnames(cpf) <-
    c(paste0("var",seq(1:n_params)),paste0("X",seq(1:n_params)))

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  xx <- data.frame(cpf) %>%
    mutate(m1 = X1,
           s1 = X1 - 1)
  for (i in 2:n_params) {
    xx <- xx %>%
      mutate_(.dots = setNames(list(varval = paste0("m", i - 1, " * X", i)),
                               paste0("m", i))) %>%
      mutate_(.dots = setNames(list(varval = paste0("m", i, " - m", i - 1)),
                               paste0("s", i)))
  }


  getsvars <- function(x) {
    val <- colnames(cpf)[max.col(cpf == x)]
    svar <- gsub("var","s",colnames(cpf)[max.col(cpf == x)])
    sval <- NA
    for (j in 1:length(svar)) {
      sval[j] = xx[j,svar[j]]
    }
    return(mean(sval))
  }
  avg.xx <- lapply(param.names, function(x)
    getsvars(x))
  g <-
    as.data.frame(cbind(param.names = param.names, avg.xx = avg.xx))

  return(g) # this is the table of permuted parameters

}
