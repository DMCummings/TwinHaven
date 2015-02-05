getTotalDebtToLTMEbitda <- function( ticker ){
        
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
        
        
        ## store relevant data for calculating LTM EBITDA and TOTAL DEBT
        totaldebt <- BS_Q["Total Debt",1]
        ebit <- NULL
        
        ## calculate ebit for the past 4 quarters
        for(i in 1:4){
                ebit[i] <- IS_Q["Gross Profit", i] - IS_Q["Selling/General/Admin. Expenses, Total", i]
        }
        
        DandA <- NULL
        adjusted.DandA <- NULL
        
        ## calculate depreciation and amortization for the past 4 quarters
        for(i in 1:4){
                DandA[i] <- CF_Q["Depreciation/Depletion",i] + CF_Q["Amortization",i]
        }
        
        inter.sum <- sum(DandA) / 10
        
        adjusted.DandA = c(inter.sum, inter.sum, inter.sum, inter.sum)
        
        ebitda <- ebit + adjusted.DandA
        
        LTMebitda <- sum(ebitda)
        
        ratio <- totaldebt / LTMebitda
        
        df <- data.frame(totaldebt, LTMebitda, ratio)
        
        colnames(df) <- c("Total Debt", "LTM EBITDA", "Total Debt / LTM EBITDA")
        
        df
        xtab <- xtable(df, caption = "Total Debt and LTM EBITDA in Millions")
        
        print.xtable(xtab, include.rownames = FALSE, caption.placement = "top")
}