      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRESA_DOS_BIFES.





       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  DATA-SISTEMA.
         05 ANO                    PIC 9(04)  VALUES ZEROS.
         05 MES                    PIC 9(02)  VALUES ZEROS.
         05 DIA                    PIC 9(02)  VALUES ZEROS.
       01  HORA-SISTEMA.
         05 HORA                   PIC 9(02)  VALUES ZEROS.
         05 MINUTO                 PIC 9(02)  VALUES ZEROS.

        77 LINHA                   PIC 99     VALUES ZEROS.
        77 TEMP-MENU               PIC X.
           88 VALIDAR-TEMP-MENU    VALUES "0" THRU "6".
        77 MENU                    PIC 9      VALUE 9.

        77 TOTAL                   PIC 999V99 VALUES ZEROS.
        77 SAIDA-TOTAL             PIC ZZ9.99.
        77 RESPOSTA                PIC X      VALUES SPACE.
        77 APAGA-LINHA             PIC 99     VALUES ZEROS.

       SCREEN SECTION.
        01 CLS BLANK SCREEN.
        01 CABECALHO.
         05 LINE 1  COL 01 VALUE "        EMPRESA DOS BIFES"
           FOREGROUND-COLOR 2 HIGHLIGHT.
         05 LINE 3  COL 01 VALUE "===================================="
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 4  COL 01 VALUE "N§        MENU              PRE€O   "
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 5  COL 01 VALUE "===================================="
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 7  COL 01 VALUE "1  HAMBURGUER PEQUENO       5,15 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 8  COL 01 VALUE "2  HAMBURGUER MDIO         6,05 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 9  COL 01 VALUE "3  HAMBURGUER GRANDE        7,10 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 10 COL 01 VALUE "4  HAMBURGUER SUPERGRANDE   8,20 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 11 COL 01 VALUE "5  BATATA                   4,50 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 12 COL 01 VALUE "6  SALADA                   5,00 EUR"
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 14 COL 01 VALUE "0  SAIR "
           FOREGROUND-COLOR 6 HIGHLIGHT.
         05 LINE 3  COL 50 VALUE "===================================="
           FOREGROUND-COLOR 3 HIGHLIGHT.
         05 LINE 4  COL 50 VALUE "N§        MENU              PRE€O   "
           FOREGROUND-COLOR 3 HIGHLIGHT.
         05 LINE 5  COL 50 VALUE "===================================="
           FOREGROUND-COLOR 3 HIGHLIGHT.

       PROCEDURE DIVISION.
           DISPLAY CABECALHO.

        CALENDARIO.

           ACCEPT DATA-SISTEMA FROM DATE YYYYMMDD.
           ACCEPT HORA-SISTEMA FROM TIME.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)    AT 0150.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTO)        AT 0178.


        INICIO.
           MOVE 7 TO LINHA.
           MOVE 0 TO TOTAL.

        ACEITAR-MENU.

           MOVE 9 TO MENU.
           PERFORM UNTIL MENU = 0
               ACCEPT TEMP-MENU AT LINE LINHA COL 50
               IF (NOT VALIDAR-TEMP-MENU) THEN
                   DISPLAY "Escolha entre 0 e 6"  AT LINE LINHA COL 84
                   FOREGROUND-COLOR 4 HIGHLIGHT
                   GO TO ACEITAR-MENU
               ELSE
                   DISPLAY " " ERASE EOL AT LINE LINHA COL 84
               MOVE FUNCTION NUMVAL(TEMP-MENU) TO MENU

      *        IF (MENU > 6) THEN
      *             DISPLAY "Escolha entre 0 e 6"  AT LINE LINHA COL 84
      *             GO TO ACEITAR-MENU
      *         ELSE
                   EVALUATE MENU
             WHEN 1
                   DISPLAY "HAMBURGUER PEQUENO       5,15 EUR"
                   AT LINE LINHA COL 50
                   ADD 5.15 TO TOTAL
                   ADD 1    TO LINHA

             WHEN 2
                   DISPLAY "HAMBURGUER MDIO         6,05 EUR"
                   AT LINE LINHA COL 50
                   ADD 6.05 TO TOTAL
                   ADD 1    TO LINHA
             WHEN 3
                   DISPLAY "HAMBURGUER GRANDE        7,10 EUR"
                   AT LINE LINHA COL 50
                   ADD 7.10 TO TOTAL
                   ADD 1    TO LINHA
             WHEN 4
                   DISPLAY "HAMBURGUER SUPERGRANDE   8,20 EUR"
                   AT LINE LINHA COL 50
                   ADD 8.20 TO TOTAL
                   ADD 1 TO LINHA
             WHEN 5
                   DISPLAY "BATATA                   4,50 EUR"
                   AT LINE LINHA COL 50
                   ADD 4.50 TO TOTAL
                   ADD 1 TO LINHA
             WHEN 6
                   DISPLAY "SALADA                   5,00 EUR"
                   AT LINE LINHA COL 50
                   ADD 5.00 TO TOTAL
                   ADD 1 TO LINHA
             WHEN 0
                   GO NOVA-CONTA
           END-EVALUATE
           MOVE TOTAL TO SAIDA-TOTAL
           DISPLAY FUNCTION CONCATENATE ("TOTAL: ",SAIDA-TOTAL," EUR")
           AT 1620 FOREGROUND-COLOR 3 HIGHLIGHT
           END-IF
           END-PERFORM.



         NOVA-CONTA.
           MOVE " " TO RESPOSTA.
           ADD 2 TO LINHA
           PERFORM UNTIL FUNCTION UPPER-CASE(RESPOSTA) = "S" OR
                         FUNCTION UPPER-CASE(RESPOSTA) = "N"
                DISPLAY "NOVA CONTA? (S/N)" AT LINE LINHA COL 50
                FOREGROUND-COLOR 3 HIGHLIGHT
                ACCEPT RESPOSTA AT LINE LINHA COL 68
           END-PERFORM.

           IF (FUNCTION UPPER-CASE(RESPOSTA) = "S") THEN
               PERFORM APAGA-LINHAS
               GO TO INICIO
           ELSE
              PERFORM APAGA-LINHAS
              DISPLAY " PROGRAMA ENCERRADO" AT LINE 07 COL 49
              FOREGROUND-COLOR 5 HIGHLIGHT
              ACCEPT OMITTED AT LINE 08 COL 50
              STOP RUN
           END-IF.


         APAGA-LINHAS.
           MOVE 07 TO APAGA-LINHA.
           PERFORM UNTIL APAGA-LINHA > LINHA
              DISPLAY "                                           "
                      AT LINE APAGA-LINHA COL 50
              ADD 1 TO APAGA-LINHA
           END-PERFORM.
           DISPLAY "       "   AT 1627.


       END PROGRAM EMPRESA_DOS_BIFES.
