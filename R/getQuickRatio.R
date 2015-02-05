getQuickRatio <- function( ticker ){
        
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
        
        ## obtain current assets, total inventory, current liabilities, and quick ratio
        currentassets <- BS_Q["Total Current Assets",1]
        totalinventory <- BS_Q["Total Inventory",1]
        currentliabilities <- BS_Q["Total Current Liabilities",1]
        quickratio <- (currentassets - totalinventory) / currentliabilities
        
        ## turn it into a data frame
        df <- data.frame(currentassets, totalinventory, currentliabilities, quickratio)
        
        df <- data.frame(t(df), row.names = c("Current Assets", "Total Inventory", "Current Liabilities", "Quick Ratio"))
        
        xtable(df)
}