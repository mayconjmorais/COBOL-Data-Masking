      ******************************************************************
      * This routine is a challenge suggest by my first manager
      * when I was a internal in a huge finantial institution.
      *
      * The purpose is simple, I'd receive a string (name, document,
      * address) and I need to mask it.
      *
      * This way the info will be safe to be access for other projects.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.                      CUDMASK.
       AUTHOR.                          MAYCON MORAIS.
       DATE-WRITTEN.                    MAY 08, 2010.
       DATE-COMPILED.

      ******************************************************************
	  *================================================================*
      *        E N V I R O N M E N T     D I V I S I O N               *
      *================================================================*

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT E1TC1AR ASSIGN TO "C:\TEMP\MASK.TXT"
           ORGANIZATION LINE SEQUENTIAL
           FILE STATUS IS  WS-STATUS-E1.

           SELECT S1TC1AR ASSIGN TO "C:\TEMP\MASK_OUT.TXT"
           ORGANIZATION LINE SEQUENTIAL
           FILE STATUS IS  WS-STATUS-S1.

      *================================================================*
      *                D A T A    D I V I S I O N                      *
      *================================================================*

       DATA DIVISION.

       FILE SECTION.
       FD E1TC1AR.
          COPY 'REGE1'                 IN copy-lib.

       FD S1TC1AR.
       01 REG-SAI PIC X(100).


       WORKING-STORAGE SECTION.
      *================================================================*
      *                   A R E A   DE   C O P Y                       *
      *================================================================*

      *COPY REGE1.
      *================================================================*
      *      W O R K I N G - S T O R A G E   S E C T I O N             *
      *================================================================*

       01 WS-ARQUIVOS-STATUS.

         05 WS-STATUS-E1                PIC 9(002) VALUE ZEROS.
         05 WS-STATUS-S1                PIC 9(002) VALUE ZEROS.


       01 WS-ALFABETO.
         05 WS-ALFA                     PIC X(027) VALUE 
         'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'.

       01 LETRA                        REDEFINES WS-ALFABETO.
         05 WS-LETRA                   PIC X(001) OCCURS 27.

       01 WS-VARIAVEIS-AUXILIARES.
         05 WS-X                       PIC 9(002) VALUE ZEROES.
         05 WS-Y                       PIC 9(002) VALUE ZEROES.
         05 WS-AUX                     PIC 9(002) VALUE ZEROES.

         05 WS-ULTIMO-CHAR             PIC 9(002) VALUE ZEROS.

         05 WS-TAMANHO                 PIC 9(002) VALUE 20.
         05 WS-VALIDADE                PIC 9(001) VALUE ZEROES.

         05 WS-PRESERVA                PIC 9(002) VALUE ZEROS.

       01 WS-OUT-FILE.
         05 WS-O-MASK.
           10 WS-O-NAME                PIC X(01) OCCURS 30.
         05 WS-O-DOC                   PIC 9(10).
         05 WS-O-ADDRESS               PIC X(50).
         05 WS-O-POST-CODE             PIC X(06).

      ******************************************************************
       01 CT-CONSTANTES.
         05 CT-0                       PIC 9(002) VALUE ZEROS.
         05 CT-10                      PIC 9(002) VALUE 10.

      ******************************************************************
       01 CN-CONTADORES.
         05 CN-CONT                    PIC 9(002) VALUE ZEROS.

      ******************************************************************

       01 SW-SWITCHES.

         05 SW-ARQUIVO                  PIC X(03) VALUE 'N'.
            88 SW-SIM-FIM-ARQUIVO VALUE 'Y'.
            88 SW-NAO-FIM-ARQUIVO VALUE 'N'.

      *================================================================*
      *        P R O C E D U R E     D I V I S I O N                   *
      *================================================================*

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           PERFORM 1000-INICIALIZA.

           PERFORM 2000-PROCESSA
             UNTIL SW-SIM-FIM-ARQUIVO

           PERFORM 3000-FINALIZA.

       0000-PRINCIPAL-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   I N I C I A L I Z A                 *
      *================================================================*

       1000-INICIALIZA.

           INITIALIZE                   E1-REGIS.

           PERFORM 1100-ABRIR-ARQUIVOS.

           PERFORM 1200-LER-REGISTRO.

      *    PERFORM 1210-TRATA-ESPACO.

      *    PERFORM 1300-VALIDA-REGISTRO.

      *    PERFORM 1400-ACHA-ULTIMO-CHAR.
       1000-INICIALIZA-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   A B E R T U R A   E N T R A D A     *
      *================================================================*

       1100-ABRIR-ARQUIVOS.

           OPEN INPUT E1TC1AR

           IF WS-STATUS-E1 EQUAL CT-0 OR
             WS-STATUS-E1 EQUAL CT-10
               CONTINUE
           ELSE
               DISPLAY ' ERRO NA ABERTURA DE ARQUIVO '
               DISPLAY ' ERROR = ' WS-STATUS-E1
               PERFORM 3000-FINALIZA
           END-IF

      *================================================================*
      *        P A R A G R A F O   A B E R T U R A   S A I D A         *
      *================================================================*

           OPEN OUTPUT S1TC1AR

           IF WS-STATUS-S1 EQUAL CT-0 OR
             WS-STATUS-S1 EQUAL CT-10
               CONTINUE
           ELSE
               DISPLAY ' ERRO NA ABERTURA DE ARQUIVO '
               DISPLAY ' ERROR = ' WS-STATUS-S1
               PERFORM 3000-FINALIZA
           END-IF.
       1100-ABRIR-ARQUIVOS-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   L E I T U R A                       *
      *================================================================*

       1200-LER-REGISTRO.
           INITIALIZE                   E1-REGIS.

           READ E1TC1AR                 INTO E1-REGIS
               AT END
                   SET SW-SIM-FIM-ARQUIVO 
                                        TO TRUE
           END-READ

           IF  WS-STATUS-E1 EQUAL CT-0 OR
               WS-STATUS-E1 EQUAL CT-10
               MOVE E1-REGIS TO WS-OUT-FILE
               MOVE SPACES TO WS-O-MASK
               CONTINUE
           ELSE
               DISPLAY ' ERRO NA LEITURA DO REGISTRO '
               DISPLAY ' ERROR = ' WS-STATUS-E1
               PERFORM 3000-FINALIZA
           END-IF.
       1200-LER-REGISTRO-EXIT.
           EXIT.

      *================================================================*
      *   T R A T A     E S P A C O S                                  *
      *================================================================*
      * discontinued in 2020-08-25
       1210-TRATA-ESPACO.

           PERFORM VARYING WS-X
             FROM 1 BY 1
             UNTIL WS-X GREATER WS-TAMANHO

               IF E1-NAME-BYTE(WS-X) EQUAL SPACE
                   PERFORM VARYING WS-X
                     FROM WS-X BY 1
                     UNTIL E1-NAME-BYTE(WS-X) NOT EQUAL SPACE
                     OR WS-X GREATER WS-TAMANHO
                   END-PERFORM

                   IF E1-NAME-BYTE(WS-X) NOT EQUAL SPACE
                       MOVE '*' TO E1-NAME-BYTE(WS-X - 1)
                   END-IF
               END-IF

           END-PERFORM
           .
       1210-TRATA-ESPACO-EXIT.
           EXIT.

      * FUNTION TO FIND REAL LENGTH INSIDE 30 POSITION LONGER VARIABLE
       1220-DATA-LENGTH.
           INITIALIZE                  WS-TAMANHO.
           
           INSPECT E1-NAME CONVERTING
           " " to "*".
           
           PERFORM 
             VARYING WS-AUX
             FROM 1 BY 1
             UNTIL WS-AUX EQUAL 30
               OR  WS-AUX GREATER THAN 30

             IF  E1-NAME-BYTE(WS-AUX)   EQUAL '*'
                 SUBTRACT 1             FROM WS-AUX
                                        GIVING WS-TAMANHO

                 PERFORM TEST BEFORE
                     VARYING WS-AUX
                     FROM WS-AUX BY 1
                     UNTIL E1-NAME-BYTE(WS-AUX) NOT EQUAL '*'
                     OR WS-AUX EQUAL 30
                 END-PERFORM
             END-IF
           END-PERFORM
           .
       1220-DATA-LENGTH-EXIT.
           EXIT.
      *================================================================*
      *        P A R A G R A F O   V A L I D A                         *
      *================================================================*
      * discontinued in 2020-08-25
       1300-VALIDA-REGISTRO.
           MOVE 1 TO WS-X

           IF E1-NAME EQUAL SPACES
               DISPLAY ' RESGISTRO VAZIO '
               PERFORM 3000-FINALIZA
           END-IF

           PERFORM VARYING WS-X
             FROM 1 BY 1
             UNTIL WS-X GREATER WS-TAMANHO

               PERFORM VARYING WS-Y
                 FROM 1 BY 1
                 UNTIL WS-Y GREATER 27
                 OR E1-NAME-BYTE(WS-X) EQUAL WS-LETRA(WS-Y)
                 OR E1-NAME-BYTE(WS-X) EQUAL SPACE OR LOW-VALUE
               END-PERFORM

      *       IF WS-Y GREATER 27
      *          PERFORM 3000-FINALIZA
      *       END-IF
           END-PERFORM
           .
       1300-VALIDA-REGISTRO-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O  ACHA ULTIMO CHAR                     *
      *================================================================*
      * discontinued in 2020-08-25
       1400-ACHA-ULTIMO-CHAR.
           PERFORM VARYING WS-X
             FROM 1 BY 1
             UNTIL WS-X GREATER 30

               IF E1-NAME-BYTE(WS-X) NOT EQUAL SPACE
                   ADD 1 TO WS-ULTIMO-CHAR
               ELSE
                   IF E1-NAME-BYTE(WS-X) EQUAL SPACE
                       PERFORM VARYING WS-X
                         FROM WS-X BY 1
                         UNTIL WS-X GREATER 30
                         OR E1-NAME-BYTE(WS-X) NOT EQUAL SPACE
                           ADD 1 TO CN-CONT
                       END-PERFORM

                       IF E1-NAME-BYTE(WS-X) NOT EQUAL SPACE
                           ADD CN-CONT TO WS-ULTIMO-CHAR
                       END-IF
                   END-IF

                   MOVE ZEROS TO CN-CONT
               END-IF
           END-PERFORM
           .
       1400-ACHA-ULTIMO-CHAR-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   P R O C E S S A                     *
      *================================================================*

       2000-PROCESSA.
           PERFORM 1220-DATA-LENGTH.

           PERFORM 2400-PROCESSA-MASCARA.

           PERFORM 2500-GRAVAR-REGISTRO.

           PERFORM 2100-INICIALIZA-VARIAVEIS.

           PERFORM 1200-LER-REGISTRO.
           
       2000-PROCESSA-EXIT.
           EXIT.

      *================================================================*
      *        C O N T A    C A R A C T E R E S                        *
      *================================================================*

       2100-INICIALIZA-VARIAVEIS.

           INITIALIZE                   E1-REGIS
                                        WS-OUT-FILE.
           .
       2100-INICIALIZA-VARIAVEIS-EXIT.
           EXIT.

      *================================================================*
      *        I D E N T I F I C A   1º  C A R A C T E R               *
      *================================================================*
      * discontinued in 2020-08-25
       2200-ACHA-PRIMEIRO-CHAR.

           PERFORM
             VARYING WS-Y
             FROM 26 BY -1
             UNTIL E1-NAME-BYTE(WS-X) EQUAL WS-LETRA(WS-Y)
             OR WS-Y EQUAL 1
           END-PERFORM

           IF WS-Y NOT EQUAL ZEROS
               SUBTRACT 1 FROM WS-Y
           ELSE
               ADD 1 TO WS-X
               GO TO 2200-ACHA-PRIMEIRO-CHAR
           END-IF
           .
       2200-ACHA-PRIMEIRO-CHAR-EXIT.
           EXIT.

      *================================================================*
      *        I N I C I O   D O   P R O C E S S A M E N T O           *
      *================================================================*
      * discontinued in 2020-08-25
       2300-TRATA-CARACTER.

           MOVE WS-PRESERVA TO WS-X
           MOVE WS-PRESERVA TO CN-CONT
           MOVE CN-CONT TO WS-AUX
           ADD 1 TO WS-AUX

           PERFORM
             VARYING WS-X
             FROM WS-X BY -1
             UNTIL WS-X EQUAL ZERO
               IF E1-NAME-BYTE(WS-X) EQUAL '*'
                   MOVE SPACE TO WS-O-NAME(WS-X)
               ELSE
                   MOVE E1-NAME-BYTE(WS-X)
                     TO WS-O-NAME(WS-X)
                   MOVE SPACE TO E1-NAME-BYTE(WS-X)
               END-IF
           END-PERFORM

           ADD 1 TO WS-X
           .
       2300-TRATA-CARACTER-EXIT.
           EXIT.
      *================================================================*
      *       P R O C E S S A M E N T O                                *
      *================================================================*
       2400-PROCESSA-MASCARA.
           INITIALIZE                   WS-AUX.
           
           PERFORM
             VARYING WS-Y
             FROM 1 BY 1
             UNTIL WS-AUX GREATER THAN WS-TAMANHO

               PERFORM
                 VARYING WS-X
                 FROM 1 BY 1
                 UNTIL WS-LETRA(WS-Y) EQUAL E1-NAME-BYTE(WS-X)
                    OR WS-X EQUAL WS-TAMANHO
               END-PERFORM

               IF WS-LETRA(WS-Y) EQUAL E1-NAME-BYTE(WS-X)
                   ADD 1                TO WS-AUX
                   
                   IF WS-LETRA(WS-Y) EQUAL '*'
                       MOVE SPACE       TO WS-O-NAME(WS-AUX)
                   ELSE
                       MOVE WS-LETRA(WS-Y)
                                        TO WS-O-NAME(WS-AUX)
                   END-IF
               END-IF
               
               IF  WS-Y EQUAL 27 
                   MOVE 1               TO WS-Y
               END-IF
               
           END-PERFORM

           DISPLAY WS-O-MASK
           .
       2400-PROCESSA-MASCARA-EXIT.
           EXIT.
      *================================================================*
      *        G R A V A   R E G I S T R O   M A S C A R A D O         *
      *================================================================*

       2500-GRAVAR-REGISTRO.
           WRITE REG-SAI FROM WS-OUT-FILE
           END-WRITE

           IF WS-STATUS-S1 EQUAL CT-0 OR
             WS-STATUS-S1 EQUAL CT-10
               CONTINUE
           ELSE
               DISPLAY ' ERRO NA GRAVACAO DO REGISTRO '
               DISPLAY ' ERROR = ' WS-STATUS-S1
               PERFORM 3000-FINALIZA
           END-IF
           .
       2500-GRAVAR-REGISTRO-EXIT.
           EXIT.

      *================================================================*
      *               P A R A G R A F O    F I N A L I Z A             *
      *================================================================*

       3000-FINALIZA.

           CLOSE E1TC1AR

           CLOSE S1TC1AR

           STOP RUN
           .
       3000-FINALIZA-EXIT.
           EXIT.
