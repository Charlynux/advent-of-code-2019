       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.
          INPUT-OUTPUT SECTION.
             FILE-CONTROL.
             SELECT IMAGE ASSIGN TO 'input'
             ORGANIZATION IS SEQUENTIAL.            
    
       DATA DIVISION.
          FILE SECTION.
          FD IMAGE.
          01 IMAGE-FILE PIC 9(1).
    
          WORKING-STORAGE SECTION.
          01 WS-IMAGE PIC 9(1).
          01 WS-EOF PIC A(1).
          01 WS-SIZE PIC 9(5).
          01 WS-I PIC 9(5).
          01 WS-J PIC 9(5).
          01 WS-Z PIC 9(5).
          01 WS-POINTER PIC 9(5).
          01 WS-OFFSET-J PIC 9(5).
          01 WS-OFFSET-Z PIC 9(5).
          01 WS-PIXEL PIC 9(1).

          01 WS-DIGITS PIC 9(1) OCCURS 20000 TIMES.

       PROCEDURE DIVISION.
         MOVE 0 TO WS-SIZE.
          OPEN INPUT IMAGE.
             PERFORM UNTIL WS-EOF='Y'
             READ IMAGE INTO WS-IMAGE
                AT END MOVE 'Y' TO WS-EOF
                NOT AT END PERFORM STORE
             END-READ
             END-PERFORM.
          CLOSE IMAGE.
        MOVE 0 TO WS-J
        PERFORM UNTIL WS-J = 6
           MOVE 0 TO WS-I
           PERFORM UNTIL WS-I = 25
               MOVE 0 TO WS-Z
               MOVE 2 TO WS-PIXEL
               PERFORM UNTIL WS-PIXEL NOT = 2
                   MULTIPLY 25 BY WS-J GIVING WS-OFFSET-J
                   MULTIPLY 25 BY WS-Z GIVING WS-OFFSET-Z
                   MULTIPLY 6 BY WS-OFFSET-Z
                   MOVE WS-I TO WS-POINTER
                   ADD WS-OFFSET-J TO WS-POINTER
                   ADD WS-OFFSET-Z TO WS-POINTER
                   MOVE WS-DIGITS(WS-POINTER) TO WS-PIXEL
                   ADD 1 TO WS-Z
               END-PERFORM
               IF WS-PIXEL = 0 THEN
                   DISPLAY ' '  WITH NO ADVANCING
                ELSE
                   DISPLAY 'X'  WITH NO ADVANCING
               END-IF
               ADD 1 TO WS-I
           END-PERFORM
            DISPLAY ' '
           ADD 1 TO WS-J
        END-PERFORM
       STOP RUN.

       STORE.
               MOVE WS-IMAGE TO WS-DIGITS(WS-SIZE).
               ADD 1 TO WS-SIZE.
