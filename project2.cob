      ******************************************************************
      *Author: David Nguyen
      *Due Date: October 19, 2021
      *Purpose: project2
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. project2.
           AUTHOR. David Nguyen.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'NEWEMP'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD.
       01  INPUT-REC PIC X(106).
       FD  PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRNT-REC PIC X(125).
       WORKING-STORAGE SECTION.
       01  INPUT-DATA.
           03 I-EMPID PIC X(7).
           03 I-LNAME PIC X(15).
           03 I-FNAME PIC X(15).
           03 I-EMPTYPE PIC X(2).
           03 I-TITLE PIC X(17).
           03 I-SSN PIC X(9).
           03 I-EMPTYSPACES1 PIC X(24).
           03 I-DATE PIC X(8).
           03 I-EMPTYSPACES2 PIC X(2).
           03 I-EMPRATE PIC 9(6).
           03 I-EMPSTATUS PIC X(1).
       01  PRNT-DATA1.
           03 L-SSN.
               05 L-FORMATSSN PIC XXXBXXBXXXX.
               05 FILLER PIC X(4).
           03 L-LNAME PIC X(20).
           03 L-FNAME PIC X(20).
           03 L-EMPID PIC X(10).
           03 L-TITLE PIC X(20).
           03 L-EMPTYPE PIC X(10).
           03 L-DATE.
               05 L-FORMATDATE PIC 99/99/9999.
               05 FILLER PIC X(5) VALUES SPACES.
           03 L-EMPRATE.
               05 L-FORMATEEMPRATE PIC 9999B99.
               05 FILLER PIC X(3) VALUES SPACES.
           03 L-EMPSTATUS PIC X(1).
       01  PRNT-HEADING1.
           03 H1-CURR-DATE PIC 99/99/99.
           03 FILLER PIC X(47) VALUES SPACES.
           03 FILLER PIC X(50) VALUES 'THE BEST IS YET TO COME, INC'.
           03 FILLER PIC X(5) VALUES SPACES.
           03 FILLER PIC X(7) VALUE 'PAGE'.
           03 H1-PAGENUM PIC ZZ9 VALUE 1.
       01  PRNT-HEADING2.
           03 FILLER PIC X(54) VALUES SPACES.
           03 FILLER PIC X(50) VALUES 'EMPLOYEE CLASSIFICATION AND PAY'.
           03 FILLER PIC X(21) VALUE SPACES.
       01  PRNT-HEADING3.
           03 FILLER PIC X(15) VALUES 'SSN'.
           03 FILLER PIC X(20) VALUES 'LAST'.
           03 FILLER PIC X(20) VALUES 'FIRST'.
           03 FILLER PIC X(10) VALUES 'EMP ID'.
           03 FILLER PIC X(20) VALUES 'TITLE'.
           03 FILLER PIC X(10) VALUES 'TYPE'.
           03 FILLER PIC X(15) VALUES 'DATE'.
           03 FILLER PIC X(10) VALUES 'RATE'.
           03 FILLER PIC X(5) VALUES 'ST'.
       01  PRNT-FOOTER1.
           03 FILLER PIC X(40) VALUES 
           'NUMBER OF EMPLOYEE RECORDS READ:'.
           03 F1-EMPCOUNTER PIC 9(4) VALUE ZERO.
       01  PRNT-FOOTER2.
           03 FILLER PIC X(40) VALUES 'NUMBER OF HOURLY EMPLOYEES:'.
           03 F2-HEMPCOUNT PIC 9(4) VALUE ZERO.
           03 FILLER PIC X(5).
           03 FILLER PIC X(40) VALUES 'AVERAGE HOURLY RATE:'.
           03 F2-AVGHRATE PIC $999.99.
       01  PRNT-FOOTER3.
           03 FILLER PIC X(40) VALUES 'NUMBER OF SALARIED EMPLOYEES:'.
           03 F3-SEMPCOUNT PIC 9(4) VALUE ZERO.
           03 FILLER PIC X(5).
           03 FILLER PIC X(40) VALUES 'AVERAGE SALARIED RATE:'.
           03 F3-AVGSRATE PIC $9999.99.
       01  PRNT-FOOTER4.
           03 FILLER PIC X(10) VALUES 'TYPE 1:'.
           03 F4-T1 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 2:'.
           03 F4-T2 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 3:'.
           03 F4-T3 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 4:'.
           03 F4-T4 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 5:'.
           03 F4-T5 PIC 9(3).
           03 FILLER PIC X(5).
       01  PRNT-FOOTER5.
           03 FILLER PIC X(10) VALUES 'TYPE 6:'.
           03 F5-T6 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 7:'.
           03 F5-T7 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 8:'.
           03 F5-T8 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 9:'.
           03 F5-T9 PIC 9(3).
           03 FILLER PIC X(5).
           03 FILLER PIC X(10) VALUES 'TYPE 10:'.
           03 F5-T10 PIC 9(3).
           03 FILLER PIC X(5).
       01  MISC.
           03 EOF-I PIC 9 VALUE 0.
           03 PGNUM PIC 999 VALUE 1.
           03 RECORDPAGECOUNTER PIC 99 VALUE 0.
           03 TOTALHRATE PIC 9(12).
           03 TOTALSRATE PIC 9(12).
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
               OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEADER.
           PERFORM 1500-LOOP
               UNTIL EOF-I = 1;
           PERFORM 1450-PRINT-FOOTERHEADER.
           PERFORM 1700-PRINT-FOOTER.
           CLOSE INPUT-FILE
               PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEADER.
           ACCEPT H1-CURR-DATE FROM DATE.
           IF PGNUM = 1
                   WRITE PRNT-REC FROM PRNT-HEADING1
               ELSE
                   MOVE SPACES TO PRNT-REC
                   WRITE PRNT-REC
                       AFTER ADVANCING 1 LINE
                   MOVE SPACES TO PRNT-REC
                   WRITE PRNT-REC
                       AFTER ADVANCING PAGE
                   WRITE PRNT-REC FROM PRNT-HEADING1
                       AFTER ADVANCING 1 LINE
           END-IF.
           WRITE PRNT-REC FROM PRNT-HEADING2
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-HEADING3
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
           MOVE 0 TO RECORDPAGECOUNTER.
           ADD 1 TO PGNUM.
           MOVE PGNUM TO H1-PAGENUM.
       1450-PRINT-FOOTERHEADER.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING PAGE.
           ACCEPT H1-CURR-DATE FROM DATE.
           WRITE PRNT-REC FROM PRNT-HEADING1
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-HEADING2
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
               AFTER ADVANCING 1 LINE.
       1500-LOOP.
           PERFORM 1600-PRINT-RECORDS.
           PERFORM 2000-READ-INPUT.
       1600-PRINT-RECORDS.
           IF RECORDPAGECOUNTER = 10
               PERFORM 1400-PRINT-HEADER
           END-IF.
           MOVE I-SSN TO L-FORMATSSN.
           INSPECT L-FORMATSSN REPLACING ALL ' ' BY '-'.
           MOVE I-LNAME TO L-LNAME.
           MOVE I-FNAME TO L-FNAME.
           MOVE I-EMPID TO L-EMPID.
           MOVE I-TITLE TO L-TITLE.
           MOVE I-EMPTYPE TO L-EMPTYPE.
           IF I-EMPTYPE = '01'
              COMPUTE F4-T1 = F4-T1 + 1
           END-IF.
           IF I-EMPTYPE = '02'
              COMPUTE F4-T2 = F4-T2 + 1
           END-IF.
           IF I-EMPTYPE = '03'
              COMPUTE F4-T3 = F4-T3 + 1
           END-IF.
           IF I-EMPTYPE = '04'
              COMPUTE F4-T4 = F4-T4 + 1
           END-IF.
           IF I-EMPTYPE = '05'
              COMPUTE F4-T5 = F4-T5 + 1
           END-IF.
           IF I-EMPTYPE = '06'
              COMPUTE F5-T6 = F5-T6 + 1
           END-IF.
           IF I-EMPTYPE = '07'
              COMPUTE F5-T7 = F5-T7 + 1
           END-IF.
           IF I-EMPTYPE = '08'
              COMPUTE F5-T8 = F5-T8 + 1
           END-IF.
           IF I-EMPTYPE = '09'
              COMPUTE F5-T9 = F5-T9 + 1
           END-IF.
           IF I-EMPTYPE = '10'
              COMPUTE F5-T10 = F5-T10 + 1
           END-IF.
           MOVE I-DATE TO L-FORMATDATE.
           MOVE I-EMPRATE TO L-FORMATEEMPRATE.
           INSPECT L-FORMATEEMPRATE REPLACING ALL ' ' BY '.'.
           MOVE I-EMPSTATUS TO L-EMPSTATUS.
           IF L-EMPSTATUS = 'H'
               COMPUTE F2-HEMPCOUNT = F2-HEMPCOUNT + 1
               COMPUTE TOTALHRATE = TOTALHRATE + I-EMPRATE
              ELSE
               COMPUTE F3-SEMPCOUNT = F3-SEMPCOUNT + 1
               COMPUTE TOTALSRATE = TOTALSRATE + I-EMPRATE
           END-IF.
           WRITE PRNT-REC FROM PRNT-DATA1
               AFTER ADVANCING 1 LINE.
           COMPUTE RECORDPAGECOUNTER = RECORDPAGECOUNTER + 1.
           COMPUTE F1-EMPCOUNTER = F1-EMPCOUNTER + 1.
       1700-PRINT-FOOTER.
           COMPUTE TOTALHRATE = TOTALHRATE / F2-HEMPCOUNT.
           COMPUTE TOTALSRATE = TOTALSRATE / F3-SEMPCOUNT.
           MOVE TOTALHRATE TO F2-AVGHRATE.
           MOVE TOTALSRATE TO F3-AVGSRATE.
           WRITE PRNT-REC FROM PRNT-FOOTER1
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-FOOTER2
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-FOOTER3
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-FOOTER4
               AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-FOOTER5
               AFTER ADVANCING 1 LINE.
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.
       END PROGRAM project2.
