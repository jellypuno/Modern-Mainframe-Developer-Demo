      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0013
       AUTHOR.        Jelly P.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

             SELECT IN001 ASSIGN TO INDD01
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-IN-STAT.

             SELECT OUT001 ASSIGN TO OUDD01
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL
              FILE STATUS IS WS-OUT-STAT.

      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.

       FD  IN001 RECORDING MODE F
               RECORD CONTAINS 189 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS COVID-REC-FIELDS.
       01  COVID-REC-FIELDS   PIC X(189).
      *
       FD  OUT001 RECORDING MODE F
               RECORD CONTAINS 133 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS OUTPUT-REC.
       01 OUTPUT-REC        PIC X(133).

      *
       WORKING-STORAGE SECTION.

       01  UNSTRING-COVID-RECORDS.
           05  UCR-COUNTRY         PIC X(50).
           05  UCR-COUNTRY-CODE    PIC X(4).
           05  UCR-SLUG            PIC X(50).
           05  UCR-NEW-CNFRM       PIC 9(5).
           05  UCR-TOT-CNFRM       PIC 9(5).
           05  UCR-NEW-DEATH       PIC 9(5).
           05  UCR-TOT-DEATH       PIC 9(5).
           05  UCR-NEW-RECVR       PIC 9(5).
           05  UCR-TOT-RECVR       PIC 9(5).
           05  UCR-TIMESTAMP       PIC X(25).

       01  WS-STORAGE.
           05  WS-COUNTRY          PIC X(50).
           05  WS-CC               PIC X(4).
           05  WS-SLUG             PIC X(50).
           05  WS-TS               PIC X(25).
           05  WS-ASTER            PIC X(80) VALUE ALL '*'.

       COPY CONSTANT.
       COPY OUREC001.

      *------------------
       PROCEDURE DIVISION.
      *------------------

       A0001-MAIN.

            PERFORM B0001-OPEN-FILES THRU B0001-EXIT
            PERFORM C0001-INIT-FILES THRU C0001-EXIT
            PERFORM D0001-READ-FILES THRU D0001-EXIT
            PERFORM E0001-PROC-FILES THRU E0001-EXIT UNTIL WS-EOF-YES
            PERFORM Z0001-CLOS-FILES THRU Z0001-EXIT
            .

       A0001-MAIN-EXIT.
            EXIT.

       B0001-OPEN-FILES.

           OPEN INPUT IN001.

            IF WS-IN-STAT NOT EQUAL ZEROES
               SET WS-MSG-OP-IN TO TRUE
               MOVE WS-IN-STAT TO WS-ERR-CDE
               SET WS-PROC-OPEN TO TRUE
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.

            OPEN OUTPUT OUT001.

            IF WS-OUT-STAT NOT EQUAL ZEROES
               SET WS-MSG-OP-OU TO TRUE
               MOVE WS-OUT-STAT TO WS-ERR-CDE
               SET WS-PROC-OPEN TO TRUE
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.

       B0001-EXIT.
            EXIT.

       C0001-INIT-FILES.

            SET WS-EOF-NO          TO TRUE
            SET WS-INIT-YES        TO TRUE
            INITIALIZE WS-ERROR-HANDLING
            .
       C0001-EXIT.
            EXIT.

       D0001-READ-FILES.

            READ IN001
              AT END SET WS-EOF-YES TO TRUE.

            EVALUATE TRUE

               WHEN WS-IN-STAT EQUAL '10' AND WS-INIT-YES
                    PERFORM Z0001-CLOS-FILES THRU Z0001-EXIT

               WHEN WS-IN-STAT EQUAL '10' AND WS-INIT-NO
                    GO TO D0001-EXIT

               WHEN WS-IN-STAT EQUAL ZEROES
                    SET WS-INIT-NO TO TRUE

               WHEN OTHER
                    SET WS-MSG-RD-IN TO TRUE
                    MOVE WS-IN-STAT TO WS-ERR-CDE
                    SET WS-PROC-READ TO TRUE
                    PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT

               END-EVALUATE
               .
       D0001-EXIT.
            EXIT.

       E0001-PROC-FILES.

            IF COVID-REC-FIELDS(1:1) EQUAL '"'
                UNSTRING COVID-REC-FIELDS DELIMITED BY ','
                   INTO UCR-COUNTRY
                        UCR-COUNTRY-CODE
                        UCR-SLUG
                        UCR-NEW-CNFRM
                        UCR-TOT-CNFRM
                        UCR-NEW-DEATH
                        UCR-TOT-DEATH
                        UCR-NEW-RECVR
                        UCR-TOT-RECVR
                        UCR-TIMESTAMP

            DISPLAY WS-ASTER
            DISPLAY 'DATE: ' UCR-TIMESTAMP(2:10)
            DISPLAY 'TIME: ' UCR-TIMESTAMP(12:8)
            DISPLAY 'COUNTRY: ' UCR-COUNTRY
            DISPLAY 'COUNTRY CODE: ' UCR-COUNTRY-CODE
            DISPLAY 'SLUG: ' UCR-SLUG
            DISPLAY 'NEW CONFIRMED CASES: ' UCR-NEW-CNFRM
            DISPLAY 'TOTAL CONFIRMED CASES: ' UCR-TOT-CNFRM
            DISPLAY 'NEW DEATHS: ' UCR-NEW-DEATH
            DISPLAY 'TOTAL DEATHS: ' UCR-TOT-DEATH
            DISPLAY 'NEW RECOVERIES: ' UCR-NEW-RECVR
            DISPLAY 'TOTAL RECOVERIES: ' UCR-TOT-RECVR
            DISPLAY WS-ASTER

            END-IF.

            PERFORM D0001-READ-FILES THRU D0001-EXIT.

       E0001-EXIT. EXIT.

       P0001-PRINT-REC.

           WRITE OUTPUT-REC FROM WS-PRINT-REPORT.

           IF WS-OUT-STAT NOT EQUAL ZEROES
               SET WS-MSG-WR-OU TO TRUE
               MOVE WS-OUT-STAT TO WS-ERR-CDE
               SET WS-PROC-PRNT TO TRUE
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
           END-IF.

       P0001-EXIT. EXIT.

       Y0001-ERR-HANDLING.

            DISPLAY '********************************'.
            DISPLAY '  ERROR HANDLING REPORT '.
            DISPLAY '********************************'.
            DISPLAY '  ' WS-ERR-MSG.
            DISPLAY '  ' WS-ERR-CDE.
            DISPLAY '  ' WS-ERR-PROC.
            DISPLAY '********************************'.

            PERFORM Z0001-CLOS-FILES THRU Z0001-EXIT.

       Y0001-EXIT.
            EXIT.

       Z0001-CLOS-FILES.

           CLOSE IN001.

            IF WS-IN-STAT NOT EQUAL ZEROES
               SET WS-MSG-CL-IN TO TRUE
               MOVE WS-IN-STAT TO WS-ERR-CDE
               SET WS-PROC-CLOS TO TRUE
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.

            CLOSE OUT001.

            IF WS-OUT-STAT NOT EQUAL ZEROES
               SET WS-MSG-CL-IN TO TRUE
               MOVE WS-IN-STAT TO WS-ERR-CDE
               SET WS-PROC-CLOS TO TRUE
               PERFORM Y0001-ERR-HANDLING THRU Y0001-EXIT
            END-IF.

            STOP RUN.

       Z0001-EXIT.
            EXIT.
