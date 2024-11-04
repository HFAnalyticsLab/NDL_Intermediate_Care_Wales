---------------------------------------
---- Bridgend Cohort Creation
---------------------------------------
-- Step 1: Linkage
	-- 1.1 Link Reablement table to ALF_PE using PERSON_ID_PE = SYSTEM_ID_PE
	-- 1.2 Link to WDSD to get WOB and GNDR_CD
	-- 1.3 Link to geog to get WIMD
	-- 1.4 Link to rural urban to get RUC
	-- 1.5 Link to ADDE to get death date
	-- 1.6 Limit to packages with a start date in 2021/22-2023/24 financial year - added 12/07/2024
	-- 1.7 Remove records where WIMD doesn't relate to reablement start date
-- Step 2: Cleaning
	-- 2.1 Remove records where ALF_STS_CD isn't in 1, 4 or 39
	-- 2.2 Remove records where GNDR_CD not in 1 or 2
	-- 2.3 Remove records with NULL WOB
	-- 2.4 Remove records with NULL LSOA 
	-- 2.5 Remove records not resident in Bridgend on reablement start date
	-- 2.6 Remove records with DEATH_DT before reablement start date
	-- 2.7 Remove records not registered at Welsh GP for full year prior to reablement start date
	-- 2.8 Remove records not registered with a SAIL GP for full year prior to reablement start date
-- Step 3: Prepare analysis variables
	-- 3.1 Add age variable
	-- 3.2 Add count of packages per person


DROP TABLE SAILW1658V.LB_COHORT_BR;

CREATE TABLE SAILW1658V.LB_COHORT_BR (ALF_PE BIGINT,
	ALF_STS_CD INTEGER,
	WOB DATE,
	GNDR_CD INTEGER,
	LSOA2011_CD VARCHAR(10),
	LSOA_DESC VARCHAR(17),
	WIMD_2019_QUINTILE INTEGER,
	RUC11CD CHARACTER(2),
	RUC11 VARCHAR(47),
	DEATH_DT DATE,
	START_DT DATE,
	END_DT DATE,
	REABLEMENT_START_DATE DATE,
	REABLEMENT_END_DATE DATE,
	TYPE_OF_SERVICE_DELIVERED VARCHAR(41),
	SERVICE_PROVIDER VARCHAR(24),
	LOCATION_OF_PATIENT_AT_TIME_OF_REFERRAL VARCHAR(19),
	REASON_FOR_NON_COMPLETION_OF_REABLEMENT_PACKAGE VARCHAR(36),
	RELATIVE_LEVEL_OF_SUPPORT_NEEDS_POST_REABLEMENT VARCHAR(61),
	HOURS_PER_WEEK_FOR_REABLEMENT_CARE DOUBLE
);

INSERT INTO SAILW1658V.LB_COHORT_BR
SELECT *
FROM(
	SELECT DISTINCT A.ALF_PE,
		A.ALF_STS_CD,
		W.WOB,
		W.GNDR_CD,
		G.LSOA2011_CD,
		G.LSOA_DESC,
		G.WIMD_2019_QUINTILE,
		R.RUC11CD,
		R.RUC11,
		D.DEATH_DT, 
		G.START_DATE,
		G.END_DATE, 
		R.REABLEMENT_START_DATE,
		R.REABLEMENT_END_DATE,
		R.TYPE_S_OF_SERVICE_DELIVERED,
		R.SERVICE_PROVIDER_S_ ,
		R.LOCATION_OF_PATIENT_AT_TIME_OF_REFERRAL_E_G_HOME_HOSPITAL_,
		R.REASON_FOR_NON_COMPLETION_OF_REABLEMENT_PACKAGE,
		R.RELATIVE_LEVEL_OF_SUPPORT_NEEDS_POST_REABLEMENT_WELSH_GOV_REPORTING_REQUIREMENT_,
		R.HOURS_PER_WEEK_FOR_REABLEMENT_CARE 
	FROM SAIL1658V.RCBR_RCBR_20240502 R
	-- 1.1 Link Reablement table to ALF_PE using PERSON_ID_PE = SYSTEM_ID_PE
	LEFT JOIN SAIL1658V.RCBR_RCBR_ALF_20240502 A
		ON R.SYSTEM_ID_PE = A.SYSTEM_ID_PE
	-- 1.2 Link to WDSD to get WOB and GNDR_CD
	LEFT JOIN SAIL1658V.WDSD_SINGLE_CLEAN_AR_PERS_20240408 W
		ON A.ALF_PE = W.ALF_PE
	-- 1.3 Link to geog to get WIMD
	LEFT JOIN SAIL1658V.WDSD_SINGLE_CLEAN_GEO_CHAR_LSOA2011_20240408 G 
		ON A.ALF_PE = G.ALF_PE
	-- 1.4 Link to rural urban to get RUC
	LEFT JOIN SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL R 
		ON G.LSOA2011_CD = R.LSOA11CD 
	-- 1.5 Link to ADDE to get death date
	LEFT JOIN SAIL1658V.ADDE_DEATHS_20240401 D
		ON A.ALF_PE = D.ALF_PE)
-- 1.6 Limit to packages with a start date in 2021/22-2023/24 financial year - added 12/07/2024
WHERE REABLEMENT_START_DATE BETWEEN '2021-04-01' AND '2024-03-31';

-- 1.7 Remove records where WIMD doesn't relate to reablement start date
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE WIMD_2019_QUINTILE IS NOT NULL 
	AND (START_DT > REABLEMENT_START_DATE OR END_DT < REABLEMENT_START_DATE);

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR; 

-- 2.1 Remove records where ALF_STS_CD isn't in 1, 4 or 39
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE ALF_STS_CD NOT IN(1, 4, 39); 

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR; 

-- 2.2 Remove records where GNDR_CD not in 1 or 2
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE GNDR_CD IS NULL OR GNDR_CD NOT IN(1,2); 

-- 2.3 Remove records with NULL WOB
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE WOB IS NULL;

-- 2.4 Remove records with NULL LSOA 
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE LSOA2011_CD IS NULL;

-- 2.5 Remove records not resident in Bridgend on reablement start date
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE LSOA_DESC != 'Bridgend';

-- 2.6 Remove records with DEATH_DT before reablement start date
DELETE FROM SAILW1658V.LB_COHORT_BR
WHERE DEATH_DT < REABLEMENT_START_DATE;

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR;

/*
-- 2.7 Remove those not resident in Wales on reablement start date and for one year prior 
-- Add welsh flag column
 ALTER TABLE SAILW1658V.LB_COHORT_BR ADD COLUMN WALES_FLAG INTEGER;
-- Merge WDSD_ADD_WALES table, set WALES_FLAG to 1 for those resident in Wales on their reablement start date and for a year before
 MERGE
INTO
	SAILW1658V.LB_COHORT_BR A
		USING (
	SELECT
		A.*
	FROM
		SAILW1658V.LB_COHORT_BR A
	LEFT JOIN SAIL1658V.WDSD_SINGLE_CLEAN_GEO_WALES_20240408 W ON
		A.ALF_PE = W.ALF_PE
	WHERE
		W.WELSH_ADDRESS = 1
		AND (A.REABLEMENT_START_DATE - 1 YEAR >= W.START_DATE)
		AND (A.REABLEMENT_START_DATE <= W.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	AND A.REABLEMENT_START_DATE = B.REABLEMENT_START_DATE
	AND A.REABLEMENT_END_DATE = B.REABLEMENT_END_DATE
	WHEN MATCHED THEN
UPDATE
SET
	WALES_FLAG = 1;

-- Delete rows where not resident in Wales on their reablement start date and for a year before
 DELETE
FROM
	SAILW1658V.LB_COHORT_BR
WHERE
	WALES_FLAG IS NULL; 
*/

-- 2.8 Remove records not registered with a SAIL GP for full year prior to reablement start date
-- Add GP flag column
 ALTER TABLE SAILW1658V.LB_COHORT_BR ADD COLUMN SAIL_GP_FLAG INTEGER;
-- Merge LB_SAIL_GP_REG_LOOKUP table, set SAIL_GP_FLAG to 1 for those resident in Wales on their first assess date and for a year before
MERGE
INTO
	SAILW1658V.LB_COHORT_BR A
		USING (
	SELECT
		A.*
	FROM
		SAILW1658V.LB_COHORT_BR A
	LEFT JOIN SAILW1658V.LB_SAIL_GP_REG_LOOKUP S ON
		A.ALF_PE = S.ALF_PE
	WHERE
		(A.REABLEMENT_START_DATE - 1 YEAR >= S.START_DATE)
		AND (A.REABLEMENT_START_DATE <= S.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	AND A.REABLEMENT_START_DATE = B.REABLEMENT_START_DATE
	AND A.REABLEMENT_END_DATE = B.REABLEMENT_END_DATE 
	WHEN MATCHED THEN
UPDATE
SET
	SAIL_GP_FLAG = 1; 

-- Delete rows where not registered at a SAIL GP on their reablement start date and for a year before
 DELETE
FROM
	SAILW1658V.LB_COHORT_BR
WHERE
	SAIL_GP_FLAG IS NULL; 

-- Check count
SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR; 

-- 3.1 Add age variable
 ALTER TABLE SAILW1658V.LB_COHORT_BR ADD COLUMN AGE INTEGER;
-- Calculate age on reablement start date
 UPDATE
	SAILW1658V.LB_COHORT_BR
SET
	AGE = YEARS_BETWEEN(REABLEMENT_START_DATE,
	WOB);

-- 3.2 Add count of packages per person
ALTER TABLE SAILW1658V.LB_COHORT_BR ADD COLUMN PACKAGE_NUM INTEGER;
UPDATE
	SAILW1658V.LB_COHORT_BR
SET
	PACKAGE_NUM = ROW_NUMBER() OVER(PARTITION BY ALF_PE ORDER BY ALF_PE, REABLEMENT_START_DATE, REABLEMENT_END_DATE);
	
-- Check count
SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR; 

SELECT COUNT(UNIQUE(ALF_PE))
FROM SAILW1658V.LB_COHORT_BR; 

SELECT COUNT(*)
FROM SAILW1658V.LB_COHORT_BR
WHERE PACKAGE_NUM = 1; 

SELECT COUNT(*)
FROM SAILW1658V.MA_COHORT_BRIDGEND mcb;