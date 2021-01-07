#!/usr/local/bin/Rscript
system("clear")

pck_ <- "priceR"
pck <- pck_[!(pck_ %in% installed.packages()[,"Package"])]
if(length(pck)){
  cat(paste0("Installing: ", pck, "\n"))
  install.packages(pck, repos = 'https://cran.rstudio.com/')
}

suppressWarnings(suppressMessages(invisible(lapply(pck_, require, character.only = TRUE))))

cat("Enter the first amount: ");
curr_1 <- readLines("stdin",n=1);
curr_1 <- as.numeric(curr_1)

cat("Enter the first currency (ex. USD, EUR, TRY): ");
fx_1 <- readLines("stdin",n=1);
fx_1 <- toupper(fx_1)

cat("Enter the second currency (ex. USD, EUR, TRY): ");
fx_2 <- readLines("stdin",n=1);
fx_2 <- toupper(fx_2)

invisible(capture.output(fx_ <- suppressWarnings(suppressMessages(invisible(exchange_rate_latest(fx_1))))))
invisible(capture.output(rate_ <- suppressWarnings(suppressMessages(invisible(as.numeric(fx_[which(as.character(fx_$currency) == fx_2),2]))))))
system("clear");

cat(paste0(curr_1, " ", toupper(fx_1), " = ",as.numeric(curr_1)*rate_, " ",fx_2),"\n\n\n")
