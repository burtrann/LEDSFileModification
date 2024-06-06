    library(dplyr)
    library(stringr)

    args <- commandArgs(trailingOnly = TRUE)
    #args <- "C:/tmp/FischerTMProject/Fisher TM March 2024.txt"

    last_slash_pos <- max(gregexpr("/", args)[[1]])
    file_path <- substr(args, 1, last_slash_pos)
    newfile <- paste(file_path,"FTM_New.txt",collapse = " ")
    
    df <- read.csv(args,skip = 1,header = TRUE,sep="|",colClasses = "character")

    df$LINE_ITEM_NUMBER_OF_UNITS = round(as.numeric(df$LINE_ITEM_NUMBER_OF_UNITS),digits=2)
    df$LINE_ITEM_UNIT_COST = as.numeric(df$LINE_ITEM_UNIT_COST)
    df$LINE_ITEM_ADJUSTMENT_AMOUNT = as.numeric(df$LINE_ITEM_ADJUSTMENT_AMOUNT)
    df$LINE_ITEM_ADJUSTMENT_AMOUNT <- 0

    df_sel_cols <- df[c("INVOICE_DATE","INVOICE_NUMBER","CLIENT_ID","LAW_FIRM_MATTER_ID","INVOICE_TOTAL",
                        "BILLING_START_DATE","BILLING_END_DATE","LINE_ITEM_ADJUSTMENT_AMOUNT","LINE_ITEM_TOTAL",
                        "TIMEKEEPER_ID","LAW_FIRM_ID","TIMEKEEPER_NAME","TIMEKEEPER_CLASSIFICATION")]
    df_sel_cols$LINE_ITEM_TOTAL = as.numeric(df_sel_cols$LINE_ITEM_TOTAL)

    line_item_tot <- df_sel_cols %>%
        filter(TIMEKEEPER_ID == "0034") %>%
        group_by(INVOICE_DATE,INVOICE_NUMBER,CLIENT_ID,LAW_FIRM_MATTER_ID,INVOICE_TOTAL,
                 BILLING_START_DATE,BILLING_END_DATE,LINE_ITEM_ADJUSTMENT_AMOUNT,
                 TIMEKEEPER_ID,LAW_FIRM_ID,TIMEKEEPER_NAME,TIMEKEEPER_CLASSIFICATION) %>%
        summarise(sum_line_item_total = sum(LINE_ITEM_TOTAL))
    line_item_tot$LINE_ITEM_ADJUSTMENT_AMOUNT <- 0
    line_item_tot$LINE_ITEM_TOTAL <- ""
    
    db_sel_col_LineItemNumber <- df[c("INVOICE_NUMBER","LINE_ITEM_NUMBER")]
    db_sel_col_LineItemNumber[,2] = as.numeric(db_sel_col_LineItemNumber[,2])
    max_line_number_per_invoice <-  db_sel_col_LineItemNumber %>% group_by(INVOICE_NUMBER) %>% summarize(max_line_number = max(LINE_ITEM_NUMBER))

    output_new_results <- data.frame(matrix(ncol=24,nrow=0))
    colnames(output_new_results) <- c("INVOICE_DATE","INVOICE_NUMBER","CLIENT_ID","LAW_FIRM_MATTER_ID","INVOICE_TOTAL",
      "BILLING_START_DATE","BILLING_END_DATE","INVOICE_DESCRIPTION","LINE_ITEM_NUMBER","EXP.FEE.INV_ADJ_TYPE",
      "LINE_ITEM_NUMBER_OF_UNITS","LINE_ITEM_ADJUSTMENT_AMOUNT","LINE_ITEM_TOTAL","LINE_ITEM_DATE","LINE_ITEM_TASK_CODE",
      "LINE_ITEM_EXPENSE_CODE","LINE_ITEM_ACTIVITY_CODE","TIMEKEEPER_ID","LINE_ITEM_DESCRIPTION","LAW_FIRM_ID",
      "LINE_ITEM_UNIT_COST","TIMEKEEPER_NAME","TIMEKEEPER_CLASSIFICATION","CLIENT_MATTER_ID..")

    for (i in 1:nrow(line_item_tot)){
        INVOICE_DATE <- line_item_tot$INVOICE_DATE[i]
        INVOICE_NUMBER <- line_item_tot$INVOICE_NUMBER[i]
        CLIENT_ID <- line_item_tot$CLIENT_ID[i]
        LAW_FIRM_MATTER_ID <- line_item_tot$LAW_FIRM_MATTER_ID[i]
        INVOICE_TOTAL <- line_item_tot$INVOICE_TOTAL[i]
        BILLING_START_DATE <- line_item_tot$BILLING_START_DATE[i]
        BILLING_END_DATE <- line_item_tot$BILLING_END_DATE[i]
        INVOICE_DESCRIPTION <- ""
        LINE_ITEM_NUMBER <- sprintf("%05d",max_line_number_per_invoice$max_line_number[max_line_number_per_invoice$INVOICE_NUMBER == INVOICE_NUMBER] +1)        
        EXP.FEE.INV_ADJ_TYPE = "IF"
        LINE_ITEM_NUMBER_OF_UNITS = 1.00
        LINE_ITEM_ADJUSTMENT_AMOUNT = 0 
        LINE_ITEM_TOTAL <- ""
        LINE_ITEM_DATE <- ""
        LINE_ITEM_TASK_CODE <- ""
        LINE_ITEM_EXPENSE_CODE <- ""
        LINE_ITEM_ACTIVITY_CODE <- ""
        TIMEKEEPER_ID = ""        
        LINE_ITEM_DESCRIPTION <- "Less Fee Adjustment"
        LAW_FIRM_ID <- line_item_tot$LAW_FIRM_ID[i]
        LINE_ITEM_UNIT_COST <- " "
        TIMEKEEPER_NAME <- ""
        TIMEKEEPER_CLASSIFICATION <- ""
        CLIENT_MATTER_ID = unique(df$CLIENT_MATTER_ID..[df$INVOICE_NUMBER== INVOICE_NUMBER])
        
        output_new_results[nrow(output_new_results)+1,] <- 
            c(INVOICE_DATE,INVOICE_NUMBER,CLIENT_ID,LAW_FIRM_MATTER_ID,INVOICE_TOTAL,
              BILLING_START_DATE,BILLING_END_DATE,INVOICE_DESCRIPTION,LINE_ITEM_NUMBER,EXP.FEE.INV_ADJ_TYPE,
              LINE_ITEM_NUMBER_OF_UNITS,LINE_ITEM_ADJUSTMENT_AMOUNT,LINE_ITEM_TOTAL,LINE_ITEM_DATE,LINE_ITEM_TASK_CODE,
              LINE_ITEM_EXPENSE_CODE,LINE_ITEM_ACTIVITY_CODE,TIMEKEEPER_ID,LINE_ITEM_DESCRIPTION,LAW_FIRM_ID,
              LINE_ITEM_UNIT_COST,TIMEKEEPER_NAME,TIMEKEEPER_CLASSIFICATION,CLIENT_MATTER_ID)
    }
# EXISTING LINES WITH "0034", for all line items with the same invoice
#1. CHANGE LINE_ITEM_UNIT_COST = 875.00
#2. Add .10 to each LINE_ITEM_NUMBER_OF_UNITS (eg. 0.2 to 0.3)
#3. LINE_ITEM_TOTAL = LINE_ITEM_NUMBER_OF_UNITS * 875.00
#4. SumLineItemTotal2 = sum(LINE_ITEM_TOTAL)
    #1
    df[!grepl("no charge", df$LINE_ITEM_DESCRIPTION,ignore.case = TRUE) & df$TIMEKEEPER_ID == "0034", "LINE_ITEM_UNIT_COST"] <- 875.00
    #2
    df$LINE_ITEM_NUMBER_OF_UNITS <- ifelse(
        df$TIMEKEEPER_ID == "0034" & !grepl("no charge", tolower(df$LINE_ITEM_DESCRIPTION), fixed = TRUE),
        df$LINE_ITEM_NUMBER_OF_UNITS + 0.10,
        df$LINE_ITEM_NUMBER_OF_UNITS
    )
    #3    
    df$LINE_ITEM_TOTAL <- round(df$LINE_ITEM_NUMBER_OF_UNITS * df$LINE_ITEM_UNIT_COST,2)
    #4
    df_sel_cols <- df[c("INVOICE_NUMBER","LINE_ITEM_NUMBER","LINE_ITEM_TOTAL","TIMEKEEPER_ID")]
    df_sel_cols[,3] = as.numeric(df_sel_cols[,3]) #Change LINE_ITEM_TOTAL to numeric.
    line_item_tot2 <- df_sel_cols %>%
        filter(TIMEKEEPER_ID == "0034") %>%
        group_by(INVOICE_NUMBER, TIMEKEEPER_ID) %>%
        summarise(sum_line_item_total = sum(LINE_ITEM_TOTAL))

    
    df_merge_total_quantity <- merge(line_item_tot, line_item_tot2, by = "INVOICE_NUMBER", all = TRUE)
    df_merge_total_quantity$LINE_ITEM_ADJUSTMENT_AMOUNT <- df_merge_total_quantity$sum_line_item_total.x - df_merge_total_quantity$sum_line_item_total.y
    df_merge_total_quantity$LINE_ITEM_TOTAL <- df_merge_total_quantity$sum_line_item_total.x - df_merge_total_quantity$sum_line_item_total.y
    
    df_merge_total_quantity$LINE_ITEM_TOTAL = as.numeric(df_merge_total_quantity$LINE_ITEM_TOTAL)
    output_new_results$LINE_ITEM_ADJUSTMENT_AMOUNT = as.numeric(output_new_results$LINE_ITEM_ADJUSTMENT_AMOUNT)

    output_new_results$LINE_ITEM_NUMBER_OF_UNITS = as.numeric(output_new_results$LINE_ITEM_NUMBER_OF_UNITS)
    output_new_results$LINE_ITEM_TOTAL = as.numeric(output_new_results$LINE_ITEM_TOTAL)    

    output_new_results[output_new_results$INVOICE_NUMBER == df_merge_total_quantity$INVOICE_NUMBER,"LINE_ITEM_ADJUSTMENT_AMOUNT"] <-df_merge_total_quantity$LINE_ITEM_ADJUSTMENT_AMOUNT
    output_new_results[output_new_results$INVOICE_NUMBER == df_merge_total_quantity$INVOICE_NUMBER,"LINE_ITEM_TOTAL"] <-df_merge_total_quantity$LINE_ITEM_ADJUSTMENT_AMOUNT
    output_new_results$LINE_ITEM_DATE <- output_new_results$INVOICE_DATE
    
    output_new_results$TIMEKEEPER_CLASSIFICATION[is.na(output_new_results$TIMEKEEPER_CLASSIFICATION)] <- ""
    output_new_results$LINE_ITEM_UNIT_COST[is.na(output_new_results$LINE_ITEM_UNIT_COST)] <- ""
    df$LINE_ITEM_UNIT_COST = as.character(df$LINE_ITEM_UNIT_COST)
    
    colnames(df)[colnames(df) == "CLIENT_MATTER_ID.."] <- "CLIENT_MATTER_ID[]"
    colnames(output_new_results)[colnames(output_new_results) == "CLIENT_MATTER_ID.."] <- "CLIENT_MATTER_ID[]"
    colnames(df)[colnames(df) == "EXP.FEE.INV_ADJ_TYPE"] <- "EXP/FEE/INV_ADJ_TYPE"
    colnames(output_new_results)[colnames(output_new_results) == "EXP.FEE.INV_ADJ_TYPE"] <- "EXP/FEE/INV_ADJ_TYPE"
    
    output_merge_results <- bind_rows(df,output_new_results)

    
    line <- "LEDES1998B[]"
    col_names <- colnames(output_new_results)
    formatted_col_names <- paste(col_names,collapse ="|")
    writeLines(line,newfile)
    #writeLines(formatted_col_names,newfile,append=TRUE)
    cat(formatted_col_names,"\n",file=newfile,append=TRUE)
    write.table(output_merge_results,file=newfile, append=TRUE,quote=FALSE,sep = "|",col.names = FALSE)
    


