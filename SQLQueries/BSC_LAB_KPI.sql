create or replace view OAO_PRODUCTION.BSC_LAB_KPI_METRICS as
SELECT 'Lab' AS SERVICE,
       c.SITE,
       c.MONTHROLLUP AS REPORTING_MONTH,
       TO_CHAR(c.MONTHROLLUP,'Mon YYYY') AS PREMIER_REPORTING_PERIOD,
       CASE
           WHEN c.TEST = 'Troponin' THEN
                'Troponin (<=50 min)'
           ELSE
                'HGB (<=60 min)'
       END AS METRIC_NAME_SUBMITTED,
       (c.TotalReceiveResultInTarget/c.TotalResulted) AS VALUE,
       NULL AS UPDATED_USER,
       SYSDATE AS UPDATED_TIME
FROM (SELECT b.SITE1 AS SITE ,
       b.TEST,
       b.MONTHROLLUP,
       COUNT(*) AS TotalResulted,
       SUM(CASE
             WHEN RECEIVETIME_TATINCLUDE = 1
                 THEN RECEIVERESULTINTARGET
             END) AS TotalReceiveResultInTarget
      FROM (SELECT CASE
                   WHEN a.SITE = 'MSC'
                   THEN 'MSBI'
                   WHEN a.SITE = 'MSUS'
                   THEN 'MSBI'
                   ELSE a.SITE
                END AS SITE1, a.*
            FROM LAB_KPI_PREPROCESSED_DAILY a
            WHERE  SITE!= 'MSSN') b
      WHERE (TEST = 'HGB' AND
             DASHBOARDPRIORITY = 'Stat') OR
             (TEST = 'Troponin' AND
              MASTERSETTING = 'ED' )
        GROUP BY b.SITE1, b.TEST, b.MONTHROLLUP) c
WHERE MONTHROLLUP <=   (SELECT ADD_MONTHS(TRUNC(SYSDATE ,'mm'),-1) As Currnt_Time FROM dual)
UNION
SELECT 'Lab' AS SERVICE,
       c.SITE,
       c.MONTHROLLUP AS REPORTING_MONTH,
       TO_CHAR(c.MONTHROLLUP,'Mon YYYY') AS PREMIER_REPORTING_PERIOD,
       CASE
           WHEN c.TEST = 'Troponin' THEN
                'Troponin (<=50 min)'
           ELSE
                'HGB (<=60 min)'
       END AS METRIC_NAME_SUBMITTED,
       (c.TotalReceiveResultInTarget/c.TotalResulted) AS VALUE,
       NULL AS UPDATED_USER,
       SYSDATE AS UPDATED_TIME
FROM (SELECT 'SYSTEM' AS SITE ,
           b.TEST,
           b.MONTHROLLUP,
           COUNT(*) AS TotalResulted,
           SUM(CASE
                 WHEN RECEIVETIME_TATINCLUDE = 1
                     THEN RECEIVERESULTINTARGET
                 END) AS TotalReceiveResultInTarget
    FROM LAB_KPI_PREPROCESSED_DAILY b
    WHERE (TEST = 'HGB' AND
         DASHBOARDPRIORITY = 'Stat') OR
         (TEST = 'Troponin' AND
          MASTERSETTING = 'ED' ) AND
         SITE != 'MSSN'
    GROUP BY  b.TEST, b.MONTHROLLUP) c
WHERE MONTHROLLUP <=   (SELECT ADD_MONTHS(TRUNC(SYSDATE ,'mm'),-1) As Currnt_Time FROM dual)