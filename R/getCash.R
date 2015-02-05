getCash <- function( ticker ){
        
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
        
        ## obtain cash, capex, and cashflow numbers
        cash <- BS_Q["Cash & Equivalents",1]
        capex <- CF_Q["Capital Expenditures",1]
        opCF <- CF_Q["Cash from Operating Activities",1]
        inCF <- CF_Q["Cash from Investing Activities",1]
        finCF <- CF_Q["Cash from Financing Activities",1]
        changeinCF = sum(opCF, inCF, finCF)
        
        ## turn it into a data frame
        df <- data.frame(cash, capex, opCF, inCF, finCF, changeinCF)
        colnames(df) <- c("Cash", "Capital Expenditures",
                          "CF from Ops", "CF from Investing", "CF from Financing", "Change in CF")
        
        xtab <- xtable(df, caption = "Cash Flow Summary in Millions")
        
        print.xtable(xtab, include.rownames = FALSE, caption.placement = "top")
        
}