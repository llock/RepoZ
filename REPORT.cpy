      ***             P R O G R A M  C H A N G E  L O G                *
      ******************************************************************
      *  CHANGED BY:                                  DATE:            *
      *                                                                *
      *  LOUIS - CHANGED REP-SALARY FORMAT            2017-09-22       *
      *  LOUIS - ADDED A REPORT TITLE                 2017-11-14       *
      *                                                                *
      ******************************************************************

       01  W-REPORT-RECORD.
           05  REP-WORK-DEPT                    PIC X(3).
           05  SPACER1                          PIC X.
           05  REP-EMP-NBR                      PIC X(06).
           05  SPACER2                          PIC X.
           05  REP-LAST-NAME                    PIC X(15).
           05  SPACER3                          PIC X.
           05  REP-FIRST-NAME                   PIC X(12).
           05  SPACER4                          PIC X.
           05  REP-SALARY                       PIC ZZZZZZ9.99.
           05  SPACER7                          PIC X(30).
      /
       01  W-REPORT-HEADER1.
           05  HD1-WORK-DEPT           PIC X(3) VALUE 'DEP'.
           05  SPACER1                          PIC X VALUE ' '.
           05  HD1-EMP-NBR             PIC X(06) VALUE 'EMPNUM'.
           05  SPACER2                          PIC X VALUE ' '.
           05  HD1-LAST-NAME           PIC X(15) VALUE 'LASTNME'.
           05  SPACER3                          PIC X VALUE ' '.
           05  HD1-FIRST-NAME          PIC X(12) VALUE 'FIRSTNME'.
           05  SPACER4                          PIC X VALUE ' '.
           05  HD1-SALARY              PIC X(9) VALUE 'SALARY'.
           05  SPACER7                          PIC X(31) VALUE ' '.
      /
       01  W-REPORT-HEADER2.
           05  HD2-WORK-DEPT           PIC X(3) VALUE '---'.
           05  SPACER1                          PIC X VALUE ' '.
           05  HD2-EMP-NBR             PIC X(06) VALUE '------'.
           05  SPACER2                          PIC X VALUE ' '.
           05  HD2-LAST-NAME           PIC X(15) VALUE '-------'.
           05  SPACER3                          PIC X VALUE ' '.
           05  HD2-FIRST-NAME          PIC X(12) VALUE '--------'.
           05  SPACER4                          PIC X VALUE ' '.
           05  HD2-SALARY              PIC X(9) VALUE '------'.
           05  SPACER7                          PIC X(31) VALUE ' '.
      /
       01  W-REPORT-TITLE.
           05  SPACER1                 PIC X(5) VALUE '*****'.
           05  REPORT-TITLE   PIC X(19) VALUE ' EMPLOYEE REPORT 1 '.
           05  SPACER2                 PIC X(5) VALUE '*****'.
           05  SPACER3                          PIC X(51) VALUE ' '.