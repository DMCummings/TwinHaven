getDebtToEquityRatio <- function( ticker ){
        
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
        
        ## obtain current liabilities, total equity, and debt to equity ratio
        currentliabilities <- BS_Q["Total Current Liabilities",1]
        totalequity <- BS_Q["Total Equity",1]
        debttoequityratio <- currentliabilities/totalequity
        
        ## turn it into a data frame
        df <- data.frame(currentliabilities, totalequity, debttoequityratio)
        
        df <- data.frame(t(df), row.names = c("Current Liabilities", "Total Equity", "Debt to Equity Ratio"))
        
        xtable(df)
}