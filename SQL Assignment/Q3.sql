set serveroutput on

--- STEP 2 - CREATING ANALYTICAL VIEW
CREATE OR REPLACE VIEW ANALYTICAL_VIEW AS
SELECT * FROM MINING_DATA_BUILD_V
UNION ALL SELECT * FROM MINING_DATA_APPLY_V;

SELECT COUNT(*) AS NO_OF_ROWS FROM ANALYTICAL_VIEW;

--- STEP 3
--- CREATING TRAINING DATA VIEW
CREATE OR REPLACE VIEW TRAINING_DATA AS
WITH ROW_COUNT AS (SELECT COUNT(*) COUNT FROM ANALYTICAL_VIEW )
SELECT AVIEW.* FROM ANALYTICAL_VIEW AVIEW
WHERE ORA_HASH(CUST_ID, (SELECT COUNT FROM ROW_COUNT)-1, 12345) <= (SELECT COUNT FROM ROW_COUNT)*60/100;

--- CREATING TEST DATA VIEW
CREATE OR REPLACE VIEW TEST_DATA AS
WITH ROW_COUNT AS (SELECT COUNT(*) COUNT FROM ANALYTICAL_VIEW )
SELECT AVIEW.* FROM ANALYTICAL_VIEW AVIEW
WHERE ORA_HASH(CUST_ID, (SELECT COUNT FROM ROW_COUNT)-1, 12345) > (SELECT COUNT FROM ROW_COUNT)*60/100;

SELECT COUNT(*) AS NO_OF_ROWS FROM TRAINING_DATA;
SELECT COUNT(*) AS NO_OF_ROWS FROM TEST_DATA;

--- CREATING DECISION TREE SETTINGS
CREATE TABLE DTREE_SETTINGS
( SETTING_NAME VARCHAR2(30),
 SETTING_VALUE VARCHAR2(4000));
 
--- INSERT DECISION TREE SETTINGS -- AUTOMATIC DATA PREPARATION IS ON
BEGIN
 INSERT INTO DTREE_SETTINGS (SETTING_NAME, SETTING_VALUE)
 VALUES (DBMS_DATA_MINING.ALGO_NAME, DBMS_DATA_MINING.ALGO_DECISION_TREE);
 
 INSERT INTO DTREE_SETTINGS (SETTING_NAME, SETTING_VALUE)
 VALUES (DBMS_DATA_MINING.PREP_AUTO,DBMS_DATA_MINING.PREP_AUTO_ON);
END;
/
 
SELECT * FROM DTREE_SETTINGS;

--- FOR AFFINITY CARD
BEGIN 
    DBMS_DATA_MINING.CREATE_MODEL( 
        model_name          => 'DECISION_TREE_AFFINITY_2', 
        mining_function     => dbms_data_mining.classification,
        data_table_name     => 'TRAINING_DATA', 
        case_id_column_name => 'cust_id', 
        target_column_name  => 'affinity_card',
        settings_table_name => 'DTREE_SETTINGS'); 
END;
/

describe user_mining_model_settings;

SELECT model_name, 
       mining_function, 
       algorithm, 
       ROUND(build_duration,5) AS BUILD_DURATION, 
       model_size 
FROM user_MINING_MODELS;

SELECT setting_name, 
         setting_value, 
         setting_type 
FROM user_mining_model_settings
WHERE model_name in 'DECISION_TREE_AFFINITY_2';
  
SELECT attribute_name, 
       attribute_type, 
       usage_type, 
       target 
FROM  all_mining_model_attributes 
WHERE model_name = 'DECISION_TREE_AFFINITY_2';

---APPLYING MODEL TO TEST DATA SET
CREATE OR REPLACE VIEW DTREE_TEST_RESULTS
AS
SELECT cust_id,
       ROUND(prediction(DECISION_TREE_AFFINITY_2 USING *), 5)  predicted_value,
       ROUND(prediction_probability(DECISION_TREE_AFFINITY_2 USING *), 5) probability
FROM   TEST_DATA;

SELECT * FROM DTREE_TEST_RESULTS;


----------------------NAIVE BAYES ALGORITHM------------------------------------
CREATE TABLE NBAYES_SETTINGS
( SETTING_NAME VARCHAR2(30),
 SETTING_VALUE VARCHAR2(4000));
 

BEGIN
 INSERT INTO NBAYES_SETTINGS (SETTING_NAME, SETTING_VALUE)
 VALUES (DBMS_DATA_MINING.ALGO_NAME, DBMS_DATA_MINING.ALGO_NAIVE_BAYES);
 
 INSERT INTO NBAYES_SETTINGS (SETTING_NAME, SETTING_VALUE)
 VALUES (DBMS_DATA_MINING.PREP_AUTO,DBMS_DATA_MINING.PREP_AUTO_ON);
END;
/

SELECT * FROM NBAYES_SETTINGS;

--- FOR AFFINITY CARD
BEGIN 
    DBMS_DATA_MINING.CREATE_MODEL( 
        model_name          => 'NAIVE_BAYES_AFFINITY', 
        mining_function     => dbms_data_mining.classification,
        data_table_name     => 'TRAINING_DATA', 
        case_id_column_name => 'cust_id', 
        target_column_name  => 'affinity_card',
        settings_table_name => 'NBAYES_SETTINGS'); 
END;
/

describe user_mining_model_settings;

SELECT model_name, 
       mining_function, 
       algorithm, 
       ROUND(build_duration, 5) AS BUILD_DURATION, 
       model_size 
FROM user_MINING_MODELS;

SELECT setting_name, 
         setting_value, 
         setting_type 
FROM user_mining_model_settings
WHERE model_name in 'NAIVE_BAYES_AFFINITY';
  
SELECT attribute_name, 
       attribute_type, 
       usage_type, 
       target 
from  all_mining_model_attributes 
where model_name = 'NAIVE_BAYES_AFFINITY';

---APPLYING MODEL TO TEST DATA SET
CREATE OR REPLACE VIEW NBAYES_TEST_RESULTS
AS
SELECT cust_id,
       ROUND(prediction(NAIVE_BAYES_AFFINITY USING *), 5)  predicted_value,
       ROUND(prediction_probability(NAIVE_BAYES_AFFINITY USING *), 5) probability
FROM   TEST_DATA;

SELECT * FROM NBAYES_TEST_RESULTS;

--- DTREE CONFUSION MATRIX
DECLARE
v_accuracy NUMBER;
BEGIN
DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
accuracy => v_accuracy,
apply_result_table_name => 'dtree_test_results',
target_table_name => 'test_data',
case_id_column_name => 'cust_id',
target_column_name => 'affinity_card',
confusion_matrix_table_name => 'dtree_confusion_matrix',
score_column_name => 'PREDICTED_VALUE',
score_criterion_column_name => 'PROBABILITY',
cost_matrix_table_name => null,
apply_result_schema_name => null,
target_schema_name => null,
cost_matrix_schema_name => null,
score_criterion_type => 'PROBABILITY');
DBMS_OUTPUT.PUT_LINE('**** MODEL ACCURACY ****: ' || ROUND(v_accuracy,4));
END;

select * from dtree_confusion_matrix;

--- NAIVE BAYES Confusion Matrix
DECLARE
v_accuracy NUMBER;
BEGIN
DBMS_DATA_MINING.COMPUTE_CONFUSION_MATRIX (
accuracy => v_accuracy,
apply_result_table_name => 'NBAYES_test_results',
target_table_name => 'test_data',
case_id_column_name => 'cust_id',
target_column_name => 'affinity_card',
confusion_matrix_table_name => 'NBAYES_confusion_matrix',
score_column_name => 'PREDICTED_VALUE',
score_criterion_column_name => 'PROBABILITY',
cost_matrix_table_name => null,
apply_result_schema_name => null,
target_schema_name => null,
cost_matrix_schema_name => null,
score_criterion_type => 'PROBABILITY');
DBMS_OUTPUT.PUT_LINE('**** MODEL ACCURACY ****: ' || ROUND(v_accuracy,4));
END;

select * from NBAYES_confusion_matrix;


--- LABELLED_DATA contains the data from 20 records and an additional attribute that contains the Predicted outcome or value, and Predicted Probability value.
CREATE TABLE LABELLED_DATA AS
SELECT FULL.* FROM
(SELECT AVIEW.*, PROB.DTREE_PREDICTED_VALUE, PROB.DTREE_PROBABILITY, PROB.NBAYES_PREDICTED_VALUE, PROB.NBAYES_PROBABILITY FROM ANALYTICAL_VIEW AVIEW,
(SELECT DTREE.CUST_ID, DTREE.PREDICTED_VALUE AS DTREE_PREDICTED_VALUE, DTREE.PROBABILITY AS DTREE_PROBABILITY, NBAYES.PREDICTED_VALUE AS NBAYES_PREDICTED_VALUE, NBAYES.PROBABILITY AS NBAYES_PROBABILITY
FROM DTREE_TEST_RESULTS DTREE 
JOIN NBAYES_TEST_RESULTS NBAYES 
ON DTREE.CUST_ID = NBAYES.CUST_ID) PROB
WHERE AVIEW.CUST_ID = PROB.CUST_ID
ORDER BY dbms_random.value) FULL
WHERE rownum <= 20;

SELECT * FROM LABELLED_DATA;
