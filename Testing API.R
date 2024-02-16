config <- yaml::read_yaml("config.yml")

sf_auth(username = config$salesforce$username,
        password = config$salesforce$password,
        security_token   = config$salesforce$security_token)



# filter records that was created before this month
filter1 <- list(column = "CREATED_DATE",
                operator = "lessThan", 
                value = "THIS_MONTH")

# filter records where the account billing address city is not empty
filter2 <-  list(column = "ACCOUNT.ADDRESS1_CITY",
                 operator = "notEqual", 
                 value = "")

# combine filter1 and filter2 using 'AND' which means that records must meet both filters
results_using_AND <- sf_run_report(my_report_id, 
                                   report_boolean_logic = "1 AND 2",
                                   report_filters = list(filter1, filter2))



my_report_id <- "00OUo0000017dX8MAI"
results <- sf_run_report(my_report_id)
results
