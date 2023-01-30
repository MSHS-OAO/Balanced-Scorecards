-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

UPDATE
SUMMARY_REPO
SET
    METRIC_NAME_SUBMITTED = 'Non-Isolation Average TAT'
WHERE 
    METRIC_NAME_SUBMITTED = 'Non-IsolationAverage TAT' AND
    SERVICE = 'Environmental Services';
    
UPDATE
SUMMARY_REPO
SET
    METRIC_NAME_SUBMITTED = 'Prime Time Room Utilization ESTIMATE All Rooms (%) 8:30A-5:00P, M-F'
WHERE
    METRIC_NAME_SUBMITTED = 'Prime Time Room Utilization ESTIMATE All Rooms (%)' AND
    SERVICE = 'Imaging';
