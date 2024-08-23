--Create BSC Finance Table
CREATE OR REPLACE VIEW BSC_FINANCE_TABLE_VIEW AS
  SELECT b."FUNCTION",b."CATEGORY",b."SITE",b."COST_CENTER",b."NAME", c.CATEGORY AS SUPPLY_CATGEGORY, a.EXPTYPE, a.SUB_ACCOUNT, a.SUB_ACCOUNT_DESCRIPTION, a.MONTH, a.SUM_OF_MONTH_BUDGET, a.SUM_OF_MONTH_ACTUAL, a.SUM_OF_YTD_BUDGET, a.SUM_OF_YTD_ACTUAL, a.SUM_OF_ANNUAL_BUDGET,
SUM_OF_REMAINING_BUDGET_YTD  FROM BSC_FINANCE_TABLE a 
LEFT JOIN BSC_COST_CENTER_MAPPING_VIEW b on a.CC = b.COST_CENTER AND a.FUNCTION = b.FUNCTION
LEFT JOIN BSC_FINANCE_SUPPLIER_MAPPING c on a.SUB_ACCOUNT_DESCRIPTION = c.ACCTNAME;


---UPDATED
CREATE OR REPLACE VIEW BSC_FINANCE_TABLE_VIEW AS
SELECT b."FUNCTION",b."CATEGORY",b."SITE",b."COST_CENTER",b."NAME", c.CATEGORY AS SUPPLY_CATGEGORY, a.EXPTYPE, a.SUB_ACCOUNT, a.SUB_ACCOUNT_DESCRIPTION, a.MONTH, a.SUM_OF_MONTH_BUDGET, a.SUM_OF_MONTH_ACTUAL, a.SUM_OF_YTD_BUDGET, a.SUM_OF_YTD_ACTUAL, a.SUM_OF_ANNUAL_BUDGET,
SUM_OF_REMAINING_BUDGET_YTD  FROM BSC_FINANCE_TABLE a 
LEFT JOIN BSC_COST_CENTER_MAPPING_VIEW b on a.CC = b.COST_CENTER --AND a.FUNCTION = b.FUNCTION
LEFT JOIN  BSC_FINANCE_SUPPLIER_MAPPING c on a.SUB_ACCOUNT_DESCRIPTION = c.ACCTNAME
WHERE b.FUNCTION NOT IN ('Corporate', 'Finance');