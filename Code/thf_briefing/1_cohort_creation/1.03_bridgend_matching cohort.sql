---------------------------------------
---- BRIDGEND matching cohort creation
---------------------------------------
-- Step 1: Linkage
	-- 1.1 Link WDSD to geog to get WIMD
	-- 1.2 Link to rural urban to get RUC
	-- 1.3 Link to ADDE to get death date
	-- 1.4 Remove records where ALF is in Bridgend's reablement cohort (-- to rerun with updated Bridgen's cohort)
-- Step 2: Cleaning
	
	-- 2.1 Remove records where GNDR_CD not in 1 or 2
	-- 2.3 Remove records with NULL WOB
	-- 2.4 Remove records with NULL LSOA 
	-- 2.5 Remove records not resident in BRIDGEND
	-- 2.6 Remove records with DEATH_DT before study start (TBD)


DROP TABLE SAILW1658V.JP_MATCHING_COHORT_BRIDGEND;

CREATE TABLE SAILW1658V.JP_MATCHING_COHORT_BRIDGEND (
	ALF_PE BIGINT,	
	WOB DATE,
	GNDR_CD INTEGER,
	LSOA2011_CD VARCHAR(10),
	LSOA_DESC VARCHAR(17),
	WIMD_2019_QUINTILE INTEGER,
	RUC11CD CHARACTER(2),
	RUC11 VARCHAR(47),
	DEATH_DT DATE,
	START_DT DATE,
	END_DT DATE
);

INSERT INTO SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
SELECT DISTINCT W.ALF_PE,
	W.WOB,
	W.GNDR_CD,
	G.LSOA2011_CD,
	G.LSOA_DESC,
	G.WIMD_2019_QUINTILE,
	R.RUC11CD,
	R.RUC11,
	D.DEATH_DT, 
	G.START_DATE,
	G.END_DATE	
FROM SAIL1658V.WDSD_SINGLE_CLEAN_AR_PERS_20240408 W
-- 1.1 Link to geog to get WIMD
LEFT JOIN SAIL1658V.WDSD_SINGLE_CLEAN_GEO_CHAR_LSOA2011_20240408 G 
	ON W.ALF_PE = G.ALF_PE
-- 1.2 Link to rural urban to get RUC
LEFT JOIN SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL R 
	ON G.LSOA2011_CD = R.LSOA11CD 
-- 1.3 Link to ADDE to get death date
LEFT JOIN SAIL1658V.ADDE_DEATHS_20240401 D
	ON W.ALF_PE = D.ALF_PE; 

-- 1.4 Remove records in LB_COHORT_BRIDGEND (to run with final Bridgend cohort)
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE ALF_PE IN (SELECT DISTINCT ALF_PE FROM SAILW1658V.LB_COHORT_BR); -- Dropped 


-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

-- 2.1 Remove records where GNDR_CD not NULL or in 1 or 2
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE GNDR_CD NOT IN(1,2) OR GNDR_CD IS NULL; -- dropped 

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

-- 2.2 Remove records with NULL WOB
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE WOB IS NULL; -- 

-- 2.3 Remove records with NULL LSOA 
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE LSOA2011_CD IS NULL; -- 

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

-- 2.4 Remove records not resident in BRIDGEND 
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE LSOA_DESC != 'Bridgend' OR LSOA_DESC IS NULL; -- dropped 

-- Check remaining row count
SELECT COUNT(*)
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

-- 2.5 Remove records with DEATH_DT before TBD
DELETE FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND
WHERE DEATH_DT < ''; -- dropped 


SELECT COUNT(*)
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

SELECT COUNT(UNIQUE(ALF_PE))
FROM SAILW1658V.JP_MATCHING_COHORT_BRIDGEND; -- 

