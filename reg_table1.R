library(haven)
library(plm)
library(stargazer)
library(lmtest)

# Load data using haven ----
data <- read_dta("ppd_data_regression_abfm.dta")

# Define dependent variables and control variables as lists ####
## defined as lists for easy iteration ####
dep_var_list <- c(
  "report_discount_100",
  "investmentreturnassumption_100",
  "report_liability_rate",
  "actliabilities_gasb_rate",
  "report_fund_rate_100",
  "actfundedratio_gasb_100"
)

control_list <- c(
  "normcostrate_tot",
  "percentreqcontpaid",
  "avgreturn_5yr_100",
  "bef_act_ratio",
  "uaalamortperiod_gasb",
  "assetsmoothingperiod_gasb",
  "close_cost_code",
  "lntotmembership",
  "socseccovered",
  "planclosed",
  "trs_plans",
  "police_fire_plan",
  "single_plan"
)

# Initialize an empty list to store regression models
results_list <- list()

data_full <- data |> filter(full_sample == 1)

# Loop through each dependent variable and run the regression
for (DV in dep_var_list) {
  # Create the regression formula dynamically
  formula <- as.formula(paste(DV, "~ post_67 +", paste(control_list, collapse = " + ")))
  
  model <- plm(
    formula,
    data = data_full,
    index = c("ppd_id", "fy"),
    model = "random"
  )
  
  # Store the model in the results list
  results_list[[DV]] <- model
}


# Generate the Quarto-ready output using Stargazer
stargazer(
  results_list[[1]],
  results_list[[2]],
  results_list[[3]],
  type = "html",
  # Use "html" or "latex" depending on the format Quarto is using
  title = "Table 1: The change of discount rate, liabilities, and funded ratios post GASB 67",
  align = TRUE,
  dep.var.labels = dep_var_list,
  omit.stat = c("f", "ser"),
  # To exclude certain statistics if needed
  out = "Table_1.html"  # Save the results as an HTML file to be embedded in Quarto
)
