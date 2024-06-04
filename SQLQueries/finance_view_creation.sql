CREATE VIEW BSC_FINANCE_ACCESS_DATA_VIEW AS
SELECT A.*,
       B.ROLLUP,B.FSDIVISION,B.DIVISION,B.SUBDIV,B.ENTITY,
       B.FUND_TYPE,B.SITE_CODE,B.COST_CENTER_CODE,B.CLOUD_CC,
       C.CATEGORY
FROM BSC_FINANCE_ACCESS_DATA A
    LEFT JOIN BSC_FINANCE_COST_CENTER_MAPPING B
      ON A.CC = B.COST_CENTER
    LEFT JOIN BSC_FINANCE_SUPPLIER_MAPPING C
        ON A.SUB_ACCOUNT_DESCRIPTION=C.ACCTNAME;