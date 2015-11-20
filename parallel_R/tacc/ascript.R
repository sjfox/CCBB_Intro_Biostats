#!/usr/bin/env Rscript

args = commandArgs(TRUE)
time = Sys.time()
contents = paste(time, args[1], sep=": ")
cat(contents, "\n", file=args[2], append=TRUE)
