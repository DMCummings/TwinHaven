getCapitalization <- function( ticker ){
        
        ## load librarys
        library(quantmod)
        library(xtable)
        
        ## download and store data
        inter <- getFinancials(ticker, src = "yahoo", auto.assign = FALSE)
        BS_A <- viewFinancials(inter, "BS", "A")
        BS_Q <- viewFinancials(inter, "BS", "Q")
        IS_A <- viewFinancials(inter, "IS", "A")
        IS_Q <- viewFinancials(inter, "IS", "Q")
        CF_A <- viewFinancials(inter, "CF", "A")
        CF_Q <- viewFinancials(inter, "CF", "Q")
        
        ## obtain current liabilities, total debt, total liabilities, stockholder's equity
        currentliabilities <- BS_Q["Total Current Liabilities",1]
        totaldebt <- BS_Q["Total Debt", 1]
        totalliabilities <- BS_Q["Total Liabilities", 1]
        shareholdersequity <- BS_Q["Total Equity",1]
        capitalization = totalliabilities + shareholdersequity
        
        ## turn it into a data frame
        df <- data.frame(currentliabilities, totaldebt, totalliabilities, shareholdersequity, capitalization)
        colnames(df) <- c("Current Liabilities", "Total Debt",
                          "Total Liabilities", "Shareholder's Equity", "Total Capitalization")
        
        xtab <- xtable(df, caption = "Capitalization in Millions")
        
        print.xtable(xtab, include.rownames = FALSE, caption.placement = "top")
        
}