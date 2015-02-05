getCurrentRatio <- function( ticker ){
        
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
        
        ## obtain current assets, current liabilities, and current ratio
        currentassets <- BS_Q["Total Current Assets",1]
        currentliabilities <- BS_Q["Total Current Liabilities",1]
        currentratio <- currentassets/currentliabilities
        
        ## turn it into a data frame
        df <- data.frame(currentassets, currentliabilities, currentratio)
        
        df <- data.frame(t(df), row.names = c("Current Assets", "Current Liabilities", "Current Ratio"))
        
        xtable(df)
}