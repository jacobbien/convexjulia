#' Function that executes julia code
#'
#' \code{calljulia} lets you run julia code from R.
#'
#' This function writes the inputs to temporary files (which are deleted unless
#' \code{delete.temp} is set to \code{FALSE}).  Next, a file of julia code is
#' produced (named \code{doit.jl} and saved in the working directory).  Next,
#' this julia code is run by invoking julia from the command line.  Finally, the
#' outputs are saved to file and read back into R.
#'
#' @param julia.code a string of julia code to be run. The names of variables
#'   used in this string should match those in the \code{inputs} argument.
#' @param inputs List containing all variables that need to be passed to julia.
#'   The names of the elements of the list should match the names used in
#'   \code{julia.code}.  All variables must be numeric (e.g. matrices, vectors,
#'   scalars)
#' @param output.names Array of names of all variables that should be returned
#'   from julia.  The names should match those used in julia.code.  All
#'   variables must be numeric (no strings).
#' @param delete.temp Default \code{TRUE}.  Indicates whether the temporary
#'   files that are created should be deleted.
#' @param norun Default \code{FALSE}.  Mostly for debugging purposes.  Returns
#'   the command that would be run in julia without it actually opening julia.
#' @param julia.call How julia can be invoked through the \code{system} command.
#'   Default: \code{"julia"}. In OS X this might be something like
#'   \code{"/Applications/Julia-0.5.app/Contents/Resources/julia/bin/julia"}.
#'   However, this argument does not need to be provided if ~/.profile (for OSX)
#'   has the line export
#'   PATH=/Applications/Julia-0.5.app/Contents/Resources/julia/bin:$PATH
#'   where "0.5" would be replaced with whatever version of Julia you have.
#' @return Returns the following: \describe{
#' \item{\code{outputs}}{List containing all variables that were listed in
#' \code{output.names} with the values they have after \code{julia.code} was
#' executed in Julia} \item{\code{command}}{the string that is run on the
#' command line.}}
#' @seealso \code{\link{callconvex}} \code{\link{callconvex.varyparams}}
#' @examples
#' \dontrun{
#' set.seed(1)
#' x  <- matrix(rnorm(1000), 20, 50)
#' # change this next line based on where julia is located:
#' julia <- "/Applications/Julia-0.3.3.app/Contents/Resources/julia/bin/julia"
#' jl <- calljulia("y = x + a; z = y - 2", inputs=list(x = x, a = 10),
#'    output.names=c("y", "z"), julia.call = julia)
#' range(jl$y - x)
#' }
calljulia <- function(julia.code, inputs = list(), output.names = NULL,
                      delete.temp = TRUE, norun = FALSE,
                      julia.call="julia") {
  infiles <- outfiles <- NULL
  if ("time_elapsed" %in% output.names) stop("'time_elapsed' is an invalid name.")
  before <- paste("# This .jl file was automatically created by the R package",
                  "'convexjulia' on", Sys.Date(), "\n")
  before <- sprintf("%s\n# Read data into Julia:", before)
  if (length(inputs) > 0) {
    for (i in seq(length(inputs))) {
      # write input variable to text file:
      input.name <- names(inputs)[i]
      if (!is.numeric(inputs[[i]]))
        stop(sprintf("All inputs must be numeric! (check %s)", input.name))
      pi <- processinput(input = inputs[[i]], input.name = input.name)
      before <- sprintf("%s\n%s", before, pi$jl.code)
      infiles <- c(infiles, pi$file)
    }
  }
  before <- sprintf("%s\n\n# Julia code to run:\ntic()", before)
  after <- "time_elapsed=toc()\n\n# Write data out from Julia:\n"
  if (all(output.names == "")) output.names <- NULL
  output.names <- c(output.names, "time_elapsed")
  if (!is.null(output.names)) {
    for (out in output.names) {
      # form julia expression to write output variables to file:
      file <- sprintf("temp_out_%s.txt", out)
      after <- sprintf("%swritedlm(\"%s\", %s)\n", after, file, out)
      outfiles <- c(outfiles, file)
    }
  }
  juliafile <- file("doit.jl")
  writeLines(c(before, julia.code, after), juliafile)
  close(juliafile)
  # put together full julia command:
  if (.Platform$OS.type == "unix")
    command <- sprintf("%s doit.jl", julia.call)
  else stop("Only doing unix-based OS for now.")
  if (norun) return(command)
  # execute command to open julia, run code, and exit:
  system(command)

  # read julia output files into R:
  outputs <- list()
  if (!is.null(output.names)) {
    for (i in seq(length(output.names))) {
      outputs[[output.names[[i]]]] <- drop(as.matrix(read.csv(outfiles[i],
                                                              header = FALSE,
                                                              sep = "\t")))
      colnames(outputs[[output.names[[i]]]]) <- NULL
    }
  }
  if (delete.temp) {
    # delete text files that were created:
    files <- paste(c(infiles, outfiles), collapse=" ")
    system(sprintf("rm %s", files))
  }
  outputs[["command"]] <- paste(before, julia.code, after, sep = "")
  outputs
}

processinput <- function(input, input.name) {
  isint <- all(round(input) == input)
  file <- sprintf("temp_in_%s.txt", input.name)
  write.table(input,
              file=file,
              row.names=FALSE,
              col.names=FALSE)
  # form julia expression to read in this file:
  before <- sprintf("print(\"Reading %s into Julia...\\n\")", input.name)
  before <- sprintf("%s\n%s = readdlm(\"%s\")\n", before, input.name, file)
  if (isint) {
    # this input was integer valued in R, so make it a Int in julia:
    before <- sprintf("%s%s = [Int(_tempp_%s) for _tempp_%s in %s]\n", before,
                      input.name, input.name, input.name, input.name)
  }
  # perform the equivalent of "drop" in R:
  before <- sprintf("%sdim = size(%s)\n", before, input.name)
  before <- sprintf("%sif (dim == (1,1))\n  %s = %s[1, 1]\n", before,
                    input.name, input.name, input.name)
  before <- sprintf("%selseif (dim[1] == 1)\n  %s = %s[1, :]\n", before,
                    input.name, input.name, input.name)
  before <- sprintf("%selseif (dim[2] == 1)\n  %s = %s[:, 1]\nend", before,
                    input.name, input.name, input.name)
  list(jl.code = before, file = file)
}

#' Simple R interface to Convex.jl
#'
#' This function lets you perform Disciplined Convex Programming
#' (\url{http://stanford.edu/~boyd/papers/disc_cvx_prog.html}) using the julia
#' package Convex.jl.
#'
#' This function uses \code{\link{calljulia}}.
#'
#' @param opt.vars string of code to declare optimization variables
#' @param pr.def string of code defining a problem called \code{pr}
#' @param const.vars list of non-optimization variables used in expression.
#'   Labels of list elements should be the names of the variables.
#' @param opt.var.names array of names of optimization variables to return.
#' @param pr.solve string of code for solving problem \code{pr}. This can be
#'   to specify the solver used, for example.
#' @param code.before optional string of julia code to run before solving
#'   problem
#' @param code.after optional string of julia code to run after solving
#'   problem
#' @param delete.temp Default \code{TRUE}.  Indicates whether the temporary
#'   files that are created should be deleted.
#' @param norun Default \code{FALSE}.  Mostly for debugging purposes.  Returns
#'   the command that would be run in julia without it actually opening julia.
#' @param julia.call How julia can be invoked through the \code{system} command.
#'   Default: \code{"julia"}. In OS X this might be something like
#'   \code{"/Applications/Julia-0.5.app/Contents/Resources/julia/bin/julia"}.
#'   However, this argument does not need to be provided if ~/.profile (for OSX)
#'   has the line export
#'   PATH=/Applications/Julia-0.5.app/Contents/Resources/julia/bin:$PATH
#'   where "0.5" would be replaced with whatever version of Julia you have.
#' @return Returns \code{optval} and \code{status} from Convex.jl in addition to
#'   optimal values of variables named in \code{opt.var.names}.
#' @references
#' Udell, Madeleine, et al. "Convex optimization in Julia." Proceedings of the
#'  1st First Workshop for High Performance Technical Computing in Dynamic
#'  Languages. IEEE Press, 2014.
#'
#' Convex.jl julia package. \url{https://github.com/cvxgrp/Convex.jl}
#'
#' Grant, Michael, Stephen Boyd, and Yinyu Ye. Disciplined convex programming.
#' Springer US, 2006.
#' @seealso \code{\link{callconvex.varyparams}}
#' @examples
#' \dontrun{
#' set.seed(1)
#' n <- 200; p <- 100
#' x  <- scale(matrix(rnorm(n * p), n, p))
#' beta <- rnorm(p)
#' y <- x %*% beta + 0.1 * rnorm(n)
#' # change this next line based on where julia is located:
#' julia <- "/Applications/Julia-0.3.3.app/Contents/Resources/julia/bin/julia"
#' # solve the lasso:
#' pr.def <- "pr = minimize(sum_squares(y - x * b) + lam * norm(b, 1))"
#' lasso <- callconvex(opt.vars = "b = Variable(p)",
#'                     pr.def = pr.def,
#'                     const.vars = list(x = x, y = y, lam = 10, p = p),
#'                     opt.var.names = "b", julia.call = julia)
#' # solve the lasso in bound form:
#' pr.def2 <- paste("pr = minimize(sum_squares(y - x * b))",
#'                  "pr.constraints += norm(b, 1) <= s", sep = ";")
#' lasso.bnd <- callconvex(opt.vars = "b = Variable(p)",
#'                     pr.def = pr.def2,
#'                     const.vars = list(x = x, y = y, s = 70, p = p),
#'                     opt.var.names = "b", julia.call = julia)
#' }
callconvex <- function(opt.vars, pr.def, const.vars, opt.var.names,
                       pr.solve = "solve!(pr)", code.before = "",
                       code.after = "", delete.temp = TRUE, norun = FALSE,
                       julia.call = "julia") {
  jl.code <- sprintf("using Convex\n%s\n%s\n%s\n%s", code.before, opt.vars,
                     pr.def, pr.solve)
  jl.code <- sprintf("%s\nstatus=string(pr.status)\noptval=pr.optval", jl.code)
  jl.code <- sprintf("%s\n%s", jl.code, code.after)
  sol <- calljulia(jl.code,
                   inputs = const.vars,
                   output.names = c(paste(opt.var.names, ".value", sep=""),
                                    "optval", "status"),
                   delete.temp = delete.temp,
                   norun = norun, julia.call = julia.call)
  for (nam in opt.var.names) {
    sol[[nam]] <- sol[[paste(nam, ".value", sep = "")]]
    sol[[paste(nam, ".value", sep = "")]] <- NULL
  }
  sol$status <- paste(sol$status, collapse="")
  sol
}

#' Simple R interface to Convex.jl to solve sequence of problems
#'
#' Like \code{\link{callconvex}} but allows a sequence of problems to be
#' performed that are identical except that a single parameter is varied. The
#' parameter varied can be scalar or vector valued.
#'
#' This function uses \code{\link{calljulia}}.
#'
#' @param opt.vars string of code to declare optimization variables
#' @param pr.def string of code defining a problem called \code{pr}
#' @param const.vars list of non-optimization variables used in expression.
#'   Labels of list elements should be the names of the variables.
#' @param vary.param list with a single vector or matrix (with a name). E.g.
#'   list(lam=c(0.1, 1, 2)). This is a non-optimization variable used in CVX
#'   expression that you want varied. If a vector, then each element is a
#'   parameter; if a matrix, then each column is a vector-parameter. The problem
#'   will be solved at each such level.
#' @param opt.var.names array of names of optimization variables to return.
#' @param pr.solve string of code for solving problem \code{pr}. This can be
#'   to specify the solver used, for example.
#' @param code.before optional string of julia code to run before solving
#'   problems
#' @param code.after optional string of julia code to run after solving
#'   problems
#' @param delete.temp Default TRUE.  Indicates whether the temporary files that
#'   are created should be deleted.
#' @param norun Default FALSE.  Mostly for debugging purposes.  Returns the
#'   command that would be run in julia without it actually opening julia.
#' @param julia.call How julia can be invoked through the \code{system} command.
#'   Default: "julia". However, even if this is the alias in your default shell,
#'   \code{system} might use a different shell in which "julia" is not
#'   recognized. For example, in OS X this might be something like
#'   "/Applications/Julia-0.3.3.app/Contents/Resources/julia/bin/julia"
#' @return Returns \code{optval} and \code{status} from Convex.jl in addition to
#'   optimal values of variables named in \code{opt.var.names}.  If optimization
#'   variable is a vector, then returns a matrix in which each column
#'   corresponds to a solution; if optimization variable is a matrix, then each
#'   returns a 3-dimensional array whose third mode corresponds to different
#'   solutions.
#' @references
#' Udell, Madeleine, et al. "Convex optimization in Julia." Proceedings of the
#'  1st First Workshop for High Performance Technical Computing in Dynamic
#'  Languages. IEEE Press, 2014.
#'
#' Convex.jl julia package. \url{https://github.com/cvxgrp/Convex.jl}
#'
#' Grant, Michael, Stephen Boyd, and Yinyu Ye. Disciplined convex programming.
#' Springer US, 2006.
#' @seealso \code{\link{callconvex}}
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 200; p <- 50
#' x  <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' y <- x %*% beta + 0.1 * rnorm(n)
#' julia <- "/Applications/Julia-0.3.3.app/Contents/Resources/julia/bin/julia"
#' # vary lambda for the lasso (with an unpenalized intercept)
#' pr.def <- "pr = minimize(sum_squares(y - b0 - x * b) + lam * norm(b, 1))"
#' opt.vars <- "b = Variable(p); b0 = Variable(1)"
#' lasso <- callconvex.varyparams(opt.vars = opt.vars, pr.def = pr.def,
#'                               const.vars = list(x = x, y = y, p = p),
#'                               vary.param = list(lam = seq(9, 2, length=10)),
#'                               opt.var.names = c("b", "b0"),
#'                               julia.call = julia)
#' }
callconvex.varyparams <- function(opt.vars, pr.def, const.vars, vary.param,
                                 opt.var.names, pr.solve = "solve!(pr)",
                                 code.before = "", code.after = "",
                                 norun=FALSE, julia.call = "julia",
                                 delete.temp = TRUE) {
  if (class(vary.param) != "list" || length(vary.param) != 1)
    stop("vary.param must be a list with a single element.")
  stopifnot(class(vary.param[[1]]) %in% c("numeric", "matrix"))
  if (is.null(names(vary.param)))
    stop("names(vary.param) must be nonnull.")
  param.name <- names(vary.param)
  # add vary.param to const.vars:
  const.vars[[sprintf("%s_all", param.name)]] <- vary.param[[1]]
  code <- sprintf("using Convex\n%s\n%s\n", code.before, opt.vars)
  if (is.matrix(vary.param[[1]])) nprob <- ncol(vary.param[[1]])
  else nprob <- length(vary.param[[1]])
  code <- sprintf("%soptval = zeros(%s)\n", code, nprob)
  code <- sprintf("%sstatus = Array(ASCIIString, %s)\n", code, nprob)
  # create for loop that varies parameter
  code <- sprintf("%sfor iter = 1:%s\n", code, nprob)
  if (is.matrix(vary.param[[1]])) {
    # each column of vary.param
    code <- sprintf("%s  %s = %s_all[:, iter]\n  %s\n  %s\n",
                    code, param.name, param.name, pr.def, pr.solve)
  } else {
    # each element of vary.param
    code <- sprintf("%s  %s = %s_all[iter]\n  %s\n  %s\n",
                    code, param.name, param.name, pr.def, pr.solve)
  }
  code <- sprintf("%s  status[iter] = string(pr.status)\n", code)
  code <- sprintf("%s  optval[iter] = pr.optval\n", code)
  code <- sprintf("%s  %s\n", code, code.after)
  for (nam in opt.var.names) {
    file <- sprintf("temp_out_%s_$iter.txt", nam)
    code <- sprintf("%s  writedlm(\"%s\", %s.value)\n", code, file, nam)
  }
  code <- sprintf("%send\n", code)
  sol <- calljulia(code,
                   inputs = const.vars,
                   output.names = c("optval", "status"),
                   norun = norun,
                   julia.call = julia.call,
                   delete.temp = delete.temp)
  # read julia output files into R:
  outfiles <- NULL
  for (nam in opt.var.names) {
    for (iter in seq(nprob)) {
      file <- sprintf("temp_out_%s_%s.txt", nam, iter)
      outfiles <- c(outfiles, file)
      tmp <- drop(as.matrix(read.csv(file, header = FALSE, sep = "\t")))
      colnames(tmp) <- NULL
      if (iter == 1) {
        if (is.matrix(tmp)) sol[[nam]] <- array(NA, c(dim(tmp), nprob))
        else
          sol[[nam]] <- matrix(NA, length(tmp), nprob)
      }
      if (is.matrix(sol[[nam]]))
        sol[[nam]][, iter] <- tmp
      else if (is.array(sol[[nam]]))
        sol[[nam]][, , iter] <- tmp
      else stop(sprintf("%s is of a type not yet handled.", nam))
    }
  }
  if (delete.temp) {
    # delete text files that were created:
    files <- paste(outfiles, collapse=" ")
    system(sprintf("rm %s", files))
  }
  sol
}
