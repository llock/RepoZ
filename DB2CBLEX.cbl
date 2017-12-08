      ******************************************************************
      *   DO NOT REMOVE.  CHAMP LINK CONTROL STATEMENTS.
      ******************************************************************
      * STARTOPT:
      * DB2OEXP: YES
      * DB2OISO: UR
      * ENDOPT:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2CBLEX.
      *    AUTHOR. R. WAGNER
      *
      *    OWNER:
      *
      *    JOB NUMBER(S):
      *
      *REMARKS.
      *
      *
      *
      *  INPUT PARMS:  NONE
      *
      *  OUTPUT PARMS: NONE
      *
      *  INPUT FILES:  NONE
      *
      *  OUTPUT FILES: ALL EMPLOYEE RECORDS
      *
      *  COPY MEMBERS:
      *
      *    TABLES:
      *           NONE
      *    SWITCHES:
      *
      *
      *    EXITS:
      *
      *      NORMAL:
      *             WHEN A END OF TABLE FETCH RETURN CODE IS RECEIVED
      *
      *      ABNORMAL:
      *
      *    RETURN CODES:
      *
      *    SPECIAL LOGIC:  NONE
      *                                                                 
      ******************************************************************
      ***             P R O G R A M  C H A N G E  L O G                *
      ******************************************************************
      *  CHANGED BY:                                  DATE:            *
      *                                                                *
      *  LOUIS - ADDED REPORT HEADER                  2017-08-18       *
      *  LOUIS - TURNED NUM ON                        2017-08-29       *
      *  LOUIS - REMOVED REDUNDANT LOG                2017-08-30       *
      *  LOUIS - ADDED A COPYBOOK FOR REPORT          2017-09-01       *
      *  LOUIS - MOVED DCLGEN TO A COPYBOOK           2017-09-07       *
      *  LOUIS - CHANGED COPYBOOK REPORT FOR SALARY   2017-09-22       *
      *  LOUIS - ADDED A REPORT TITLE                 2017-11-14       *
      *                                                                *
      ******************************************************************
      ***           E N D  P R O G R A M  C H A N G E  L O G           *
      ******************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT REPORT-FILE         ASSIGN TO RPTO0010.

       DATA DIVISION.

       FILE SECTION.


      ******************************************************************
      * FILE:  REPORT-FILE                        DDNAME - GPSO0010    *
      *                                                                *
      ******************************************************************

       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS REPORT-RECORD.

       01  REPORT-RECORD     PIC X(80).

      /
       WORKING-STORAGE SECTION.
       01  START-OF-WORKING-STORAGE    PIC X(40)
           VALUE 'DB2CBLEX START-OF-WORKING-STORAGE'.

       01  C-PROG-MOD.
           05 C-THIS-PGM               PIC X(08) VALUE 'DB2CBLEX'.
      /
       COPY REPORT.
      /
      ***********              ***********
      *      DB2 COMMUNICATION AREA      *
      ***********              ***********

           EXEC SQL INCLUDE SQLCA END-EXEC.
      * DCLGEN FOR EMP TABLE
           EXEC SQL INCLUDE DCLEMP END-EXEC.

      ***********              ***********
      *      DB2 BASIC RETURN CODES      *
      ***********              ***********

       01  DB2-RETURNS.
            05 DB2-OK                PIC S9(04) COMP VALUE 0.
            05 DB2-END-OF-TABLE      PIC S9(04) COMP VALUE 100.
      /
       01 NULL_AREA.
          05 NULL_IND           PIC S9(4) COMP OCCURS 2 TIMES.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************

      ****************************************************************
      *   * CURSOR CALL FOR GPS CONTRACT INTERFACES TABLES          **
      *   *                                        (PRICE ADD ONS)  **
      *   * THIS INCLUDE CONTAINS THE CURSOR CODE FOR RETRIEVING    **
      *   * PRICE ADD ON CONTRACT DATA FROM GPS INTERFACES TABLES   **
      ****************************************************************
             EXEC SQL
                  DECLARE EMP_RECORD  CURSOR FOR

                     SELECT EMPNO,
                            FIRSTNME,
                            LASTNAME,
                            WORKDEPT,
                            SALARY
                     FROM DSN8110.EMP

                   END-EXEC.

      /

       01   W-CURR-DATE-YMD.
              05  W-CURR-YYYY    PIC X(4) VALUE ' '.
              05  DASH1          PIC X(1) VALUE '-'.
              05  W-CURR-MM      PIC X(2) VALUE ' '.
              05  DASH2          PIC X(1) VALUE '-'.
              05  W-CURR-DD      PIC X(2) VALUE ' '.
       01   C-CURR-DATE-YMD  REDEFINES W-CURR-DATE-YMD  PIC X(10).

       01   W-PART-NBR       PIC X(15) VALUE ' '.

       01   CONSTANTS.
            05  C-ABEND-PGM      PIC X(08)  VALUE  'WAASABND'.
            05  C-ABEND-CODE     PIC S9(09) COMP SYNC VALUE +3555.
            05  C-ABEND-TYPE     PIC X(02)  VALUE 'DN'.


      /
      *            **MISC WORK STORAGE**

       01  WS-CURR-JULIAN7             PIC S9(07) COMP-3 VALUE ZEROES.


       01  PROGRAMMING-DISPLAY.
           03 PROG-MESSAGE             PIC X(40).
           03 FILLER                   PIC X(6) VALUE SPACES.
           03 PROG-TOTALS              PIC Z(5)9.
           03 FILLER                   PIC X(28) VALUE SPACES.

       01  SUBSCRIPTS.
           05  W-ADDON-SUB    PIC S9(4) COMP VALUE ZERO.

       01  WS-DOUBLE-WORD              PIC S9(8) COMP SYNC.

       01  C-VARIABLE-NAMES.
           05  C-YES                   PIC X(1) VALUE 'Y'.
           05  C-NO                    PIC X(1) VALUE 'N'.
           05  C-JOB-NAME              PIC X(8) VALUE 'XXXXXXXX'.
           05  C-GOOD-RETURN           PIC S9(08) COMP SYNC VALUE +0.

       01  ACCUMULATORS.
          05  A-RECORDS-READ           PIC S9(8) COMP  VALUE ZERO.
          05  A-RECORDS-WRITTEN        PIC S9(8) COMP  VALUE ZERO.


      /
       LINKAGE SECTION.

       PROCEDURE DIVISION.
      ******************************************************************
      *                                                                *
      *           M A I N  L O O P                                     *
      *                                                                *
      ******************************************************************

           OPEN OUTPUT REPORT-FILE.

           INITIALIZE   REPORT-RECORD
                      W-REPORT-RECORD.

           WRITE REPORT-RECORD  FROM  W-REPORT-TITLE.
           WRITE REPORT-RECORD  FROM  W-REPORT-HEADER1.
           WRITE REPORT-RECORD  FROM  W-REPORT-HEADER2.

           PERFORM P5000-OPEN-EMP-RECORD.

           IF  SQLCODE  =  DB2-OK
               PERFORM  P5020-FETCH-EMP-RECORD
               IF  SQLCODE  =  DB2-OK
                   PERFORM P0100-PROCESS-EMP-RECORD UNTIL
                           SQLCODE  NOT =  DB2-OK
                   PERFORM P5010-CLOSE-EMP-RECORD
               ELSE
                   NEXT SENTENCE
               END-IF
           ELSE
               NEXT SENTENCE
           END-IF.

           DISPLAY 'TOTAL RECORDS READ        '  A-RECORDS-READ.
           DISPLAY 'TOTAL RECORDS WRITTEN     '  A-RECORDS-WRITTEN.


           CLOSE REPORT-FILE.


       EXIT-PROGRAM.
           GOBACK.
      /
       P0100-PROCESS-EMP-RECORD.

      ******************************************************************
      ******************************************************************


           IF  SQLCODE  =  DB2-OK

                   PERFORM P0200-LOAD-EMP-DATA
                   IF  SQLCODE  =  DB2-OK
                       CONTINUE
                   END-IF
                   WRITE REPORT-RECORD  FROM  W-REPORT-RECORD
                   COMPUTE A-RECORDS-WRITTEN = A-RECORDS-WRITTEN + 1
           ELSE
               IF  SQLCODE  =  DB2-END-OF-TABLE
                   NEXT SENTENCE
               ELSE
                   DISPLAY 'P100 PROCESS EMP REC'
                   DISPLAY 'SQLCODE = ', SQLCODE.

           PERFORM P5020-FETCH-EMP-RECORD.
      /
      *****************************************************************
      *                                                               *
      *****************************************************************
       P0200-LOAD-EMP-DATA.

            INITIALIZE  W-REPORT-RECORD.

            MOVE WORKDEPT                       TO  REP-WORK-DEPT.
            MOVE EMPNO                          TO  REP-EMP-NBR.
            MOVE LASTNAME-TEXT(1:LASTNAME-LEN)  TO  REP-LAST-NAME.
            MOVE FIRSTNME-TEXT(1:FIRSTNME-LEN)  TO  REP-FIRST-NAME.
            MOVE SALARY                         TO  REP-SALARY.
      /
      ******************************************************************
      *          O P E N  G P S  C O N T R A C T  C U R S O R          *
      ******************************************************************
       P5000-OPEN-EMP-RECORD.

             EXEC SQL
               OPEN EMP_RECORD
             END-EXEC.

             IF  SQLCODE  =  DB2-OK
                 NEXT SENTENCE
             ELSE
                 DISPLAY 'ERROR IN DB2 CALL TO EMP RECORD'
                 DISPLAY 'SQLCODE =', SQLCODE
                 DISPLAY 'P5000-OPEN-EMP-REC'.
      /
      ******************************************************************
      *        C L O S E  G P S  C O N T R A C T  C U R S O R          *
      ******************************************************************
       P5010-CLOSE-EMP-RECORD.

             EXEC SQL
               CLOSE EMP_RECORD
             END-EXEC.

             IF  SQLCODE  =  DB2-OK
                 NEXT SENTENCE
             ELSE
                 DISPLAY ' R5010-CLOSE-EMP-REC'
                 DISPLAY ' SQLCODE', SQLCODE.
      /
      ******************************************************************
      *          F E T C H  G P S  C O N T R A C T  D A T A            *
      ******************************************************************
       P5020-FETCH-EMP-RECORD.

           INITIALIZE DCLEMP.

           EXEC SQL
              FETCH EMP_RECORD

              INTO  :DCLEMP.EMPNO,
                    :DCLEMP.FIRSTNME,
                    :DCLEMP.LASTNAME,
                    :DCLEMP.WORKDEPT,
                    :DCLEMP.SALARY

              INDICATOR :NULL_IND

           END-EXEC.

              IF  SQLCODE  =  DB2-OK
                  COMPUTE  A-RECORDS-READ  =  A-RECORDS-READ  +  1
              ELSE
                  IF  SQLCODE  =  DB2-END-OF-TABLE
                      INITIALIZE DCLEMP
                  ELSE
                      DISPLAY 'P5020 EMP REC FETCH'
                      DISPLAY 'SQLCODE = ', SQLCODE.
      /


      *                                                                 
