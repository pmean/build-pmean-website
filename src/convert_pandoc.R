function (input, to = NULL, from = NULL, output = NULL, citeproc = FALSE, 
    options = NULL, verbose = FALSE, wd = NULL) 
{
    find_pandoc()
    if (is.null(wd)) {
        wd <- base_dir(input)
    }
    oldwd <- setwd(wd)
    on.exit(setwd(oldwd), add = TRUE)
    args <- c(input)
    if (!is.null(to)) {
        if (to == "html") 
            to <- "html4"
        args <- c(args, "--to", to)
    }
    if (!is.null(from)) 
        args <- c(args, "--from", from)
    if (!is.null(output)) 
        args <- c(args, "--output", output)
    stack_size <- getOption("pandoc.stack.size", default = "512m")
    args <- c(c("+RTS", paste0("-K", stack_size), 
        "-RTS"), args)
    args <- c(args, options)
    if (citeproc) {
        args <- c(args, "--filter", pandoc_citeproc())
        i <- stats::na.omit(match(c("--natbib", "--biblatex"), 
            options))
        if (length(i)) 
            options <- options[-i]
    }
    command <- paste(quoted(pandoc()), paste(quoted(args), collapse = " "))
    if (verbose) 
        cat(command, "\n")
    with_pandoc_safe_environment({
        result <- system(command)
    })
    if (result != 0) 
        stop("pandoc document conversion failed with error ", 
            result, call. = FALSE)
    invisible(NULL)
}
<bytecode: 0x00000143cc74f4f0>
<environment: namespace:rmarkdown>