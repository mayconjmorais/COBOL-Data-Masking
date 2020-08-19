      *================================================================*
      *        E N V I R O N M E N T     D I V I S I O N               *
      *================================================================*

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT E1TC1AR ASSIGN TO SYS010
           FILE STATUS IS  WS-STATUS-E1.

           SELECT S1TC1AR ASSIGN TO SYS020
           FILE STATUS IS  WS-STATUS-S1.

      *================================================================*
      *                D A T A    D I V I S I O N                      *
      *================================================================*

       DATA DIVISION.

       FILE SECTION.
       FD E1TC1AR.
          01 REG-ENT                   PIC X(020).

       FD S1TC1AR.
          01 REG-SAI                   PIC X(020).


       WORKING-STORAGE SECTION.

      *================================================================*
      *                   A R E A   DE   C O P Y                       *
      *================================================================*

       COPY REGE1.
       COPY REGS1.
      *================================================================*
      *      W O R K I N G - S T O R A G E   S E C T I O N             *
      *================================================================*

       01 WS-ARQUIVOS-STATUS.

          05 WS-STATUS-E1              PIC 9(002) VALUE ZEROS.
          05 WS-STATUS-S1              PIC 9(002) VALUE ZEROS.


       01 WS-ALFABETO.
           05 WS-ALFA                  PIC X(027) VALUE
                                       '*ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

       01 LETRA                        REDEFINES WS-ALFABETO.
          05 WS-LETRA                  PIC X(001) OCCURS 27.


       01  WS-DADOS.
           05 WS-DADOS-BYTE            PIC X(001) OCCURS 100.

       01 WS-VARIAVEIS-AUXILIARES.
           05 WS-X                     PIC 9(002) VALUE ZEROES.
           05 WS-Y                     PIC 9(002) VALUE ZEROES.
           05 WS-TAM                   PIC 9(002) VALUE ZEROES.
           05 WS-AUX                   PIC 9(002) VALUE ZEROES.
           05 WS-CONT                  PIC 9(003) VALUE ZEROES.

           05 WS-ULTIMO-CHAR           PIC 9(002) VALUE ZEROS.

           05 WS-TAMANHO               PIC 9(002) VALUE 20.
           05 WS-VALIDADE              PIC 9(001) VALUE ZEROES.

           05 WS-PRESERVA              PIC 9(002) VALUE ZEROS.

      ******************************************************************
       01 CT-CONSTANTES.
          05 CT-0                      PIC 9(002) VALUE ZEROS.
          05 CT-10                     PIC 9(002) VALUE 10.

      ******************************************************************
       01 CN-CONTADORES.
          05 CN-CONT                   PIC 9(002) VALUE ZEROS.

      ******************************************************************

       01 SW-SWITCHES.

          05 SW-ARQUIVO                PIC X(03) VALUE 'NAO'.
             88 SW-SIM-FIM-ARQUIVO               VALUE 'SIM'.
             88 SW-NAO-FIM-ARQUIVO               VALUE 'NAO'.

      *================================================================*
      *        P R O C E D U R E     D I V I S I O N                   *
      *================================================================*

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           PERFORM 1000-INICIALIZA

           PERFORM 2000-PROCESSA
                   UNTIL SW-SIM-FIM-ARQUIVO

           PERFORM 3000-FINALIZA

           .
       0000-PRINCIPAL-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   I N I C I A L I Z A                 *
      *================================================================*

       1000-INICIALIZA.

           INITIALIZE                  E1-REGIS
                                       S1-REGIS

           PERFORM 1100-ABRIR-ARQUIVOS

           PERFORM 1200-LER-REGISTRO

           PERFORM 1210-TRATA-ESPACO

           PERFORM 1300-VALIDA-REGISTRO

           PERFORM 1400-ACHA-ULTIMO-CHAR

           .
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
           END-IF
           .
       1100-ABRIR-ARQUIVOS-EXIT.
           EXIT.

      *================================================================*
      *        P A R A G R A F O   L E I T U R A                       *
      *================================================================*

       1200-LER-REGISTRO.

           READ E1TC1AR INTO E1-REGIS
                AT END
                SET SW-SIM-FIM-ARQUIVO TO TRUE
           END-READ

           IF WS-STATUS-E1 EQUAL CT-0 OR
              WS-STATUS-E1 EQUAL CT-10
              CONTINUE
           ELSE
               DISPLAY ' ERRO NA LEITURA DO REGISTRO '
               DISPLAY ' ERROR = ' WS-STATUS-E1
               PERFORM 3000-FINALIZA
           END-IF
           .
       1200-LER-REGISTRO-EXIT.
           EXIT.

      *================================================================*
      *   T R A T A     E S P A C O S                                  *
      *================================================================*

       1210-TRATA-ESPACO.

           MOVE E1-TITULAR             TO WS-DADOS

           PERFORM VARYING WS-X
               FROM 1 BY 1
               UNTIL WS-X GREATER WS-TAMANHO

               IF WS-DADOS-BYTE(WS-X) EQUAL SPACE
                  PERFORM VARYING WS-X
                     FROM WS-X BY 1
                     UNTIL WS-DADOS-BYTE(WS-X) NOT EQUAL SPACE
                     OR    WS-X GREATER WS-TAMANHO
                  END-PERFORM

                  IF WS-DADOS-BYTE(WS-X) NOT EQUAL SPACE
                     MOVE '*'          TO WS-DADOS-BYTE(WS-X - 1)
                  END-IF
                END-IF

           END-PERFORM

           .
       1210-TRATA-ESPACO-EXIT.         EXIT.



      *================================================================*
      *        P A R A G R A F O   V A L I D A                         *
      *================================================================*

       1300-VALIDA-REGISTRO.
           MOVE 1                      TO WS-X

           IF WS-DADOS EQUAL SPACES
              DISPLAY ' RESGISTRO VAZIO '
              PERFORM 3000-FINALIZA
           END-IF

           PERFORM VARYING WS-X
              FROM 1 BY 1
              UNTIL WS-X GREATER WS-TAMANHO

              PERFORM VARYING WS-Y
                 FROM  1 BY 1
                 UNTIL WS-Y GREATER 27
                 OR    WS-DADOS-BYTE(WS-X) EQUAL WS-LETRA(WS-Y)
                 OR    WS-DADOS-BYTE(WS-X) EQUAL SPACE
              END-PERFORM

                 IF WS-Y GREATER 27
                    PERFORM 3000-FINALIZA
                 END-IF
           END-PERFORM

           .
       1300-VALIDA-REGISTRO-EXIT.      EXIT.

      *================================================================*
      *        P A R A G R A F O  ACHA ULTIMO CHAR                     *
      *================================================================*
       1400-ACHA-ULTIMO-CHAR.
           PERFORM VARYING WS-X
              FROM 1 BY 1
              UNTIL WS-X GREATER WS-TAMANHO

              IF WS-DADOS-BYTE(WS-X) NOT EQUAL SPACE
                 ADD 1                 TO WS-ULTIMO-CHAR
              ELSE
                 IF WS-DADOS-BYTE(WS-X) EQUAL SPACE
                    PERFORM VARYING WS-X
                       FROM WS-X BY 1
                       UNTIL WS-X GREATER WS-TAMANHO
                       OR    WS-DADOS-BYTE(WS-X) NOT EQUAL SPACE
                       ADD 1            TO CN-CONT
                    END-PERFORM

                    IF WS-DADOS-BYTE(WS-X) NOT EQUAL SPACE
                       ADD CN-CONT     TO WS-ULTIMO-CHAR
                    END-IF
                  END-IF

                  MOVE ZEROS           TO CN-CONT
              END-IF
           END-PERFORM
           .
       1400-ACHA-ULTIMO-CHAR-EXIT.     EXIT.



      *================================================================*
      *        P A R A G R A F O   P R O C E S S A                     *
      *================================================================*

       2000-PROCESSA.


           PERFORM 2100-INICIALIZA-VARIAVEIS

           PERFORM 2200-ACHA-PRIMEIRO-CHAR

           PERFORM 2300-TRATA-CARACTER

           PERFORM 2400-PROCESSA-MASCARA

           PERFORM 2500-GRAVAR-REGISTRO

           GO TO 1200-LER-REGISTRO
           .
       2000-PROCESSA-EXIT.
           EXIT.

      *================================================================*
      *        C O N T A    C A R A C T E R E S                        *
      *================================================================*

       2100-INICIALIZA-VARIAVEIS.

           INITIALIZE                  WS-Y
                                       WS-AUX
                                       WS-TAM
                                       WS-TAMANHO
                                       WS-VALIDADE


           MOVE 20                     TO WS-TAMANHO
           MOVE 1                      TO WS-X
           MOVE 2                      TO WS-PRESERVA
           .
       2100-INICIALIZA-VARIAVEIS-EXIT. EXIT.

      *================================================================*
      *        I D E N T I F I C A   1º  C A R A C T E R               *
      *================================================================*

       2200-ACHA-PRIMEIRO-CHAR.

              PERFORM
                VARYING WS-Y
                FROM  26 BY -1
                UNTIL WS-DADOS-BYTE(WS-X) EQUAL WS-LETRA(WS-Y)
              END-PERFORM

              SUBTRACT 1               FROM WS-Y

           .
       2200-ACHA-PRIMEIRO-CHAR-EXIT.
           EXIT.

      *================================================================*
      *        I N I C I O   D O   P R O C E S S A M E N T O           *
      *================================================================*

       2300-TRATA-CARACTER.

           MOVE WS-PRESERVA            TO WS-X
           MOVE WS-PRESERVA            TO CN-CONT
           MOVE CN-CONT                TO WS-AUX
           ADD 1                       TO WS-AUX

           PERFORM VARYING WS-X
              FROM WS-X BY -1
              UNTIL WS-X EQUAL ZERO
              IF WS-DADOS-BYTE(WS-X) EQUAL '*'
                 MOVE SPACE            TO S1-TITULAR(WS-X)
              ELSE
                 MOVE WS-DADOS-BYTE(WS-X) TO S1-TITULAR(WS-X)
                 MOVE SPACE               TO WS-DADOS-BYTE(WS-X)
              END-IF
           END-PERFORM

           ADD 1                       TO WS-X
           .
       2300-TRATA-CARACTER-EXIT.       EXIT.
      *================================================================*
      *       P R O C E S S A M E N T O                                *
      *================================================================*
       2400-PROCESSA-MASCARA.

           PERFORM
               VARYING WS-Y
               FROM WS-Y BY -1
               UNTIL WS-DADOS EQUAL SPACES

               IF WS-Y EQUAL CT-0
                  ADD 1                  TO WS-VALIDADE
                  MOVE 27                TO WS-Y
               END-IF

               PERFORM
                  VARYING WS-X
                  FROM WS-AUX BY 1
                  UNTIL WS-X GREATER WS-ULTIMO-CHAR
                  OR    WS-DADOS-BYTE(WS-X) EQUAL WS-LETRA(WS-Y)
               END-PERFORM

                  IF WS-DADOS-BYTE(WS-X)   EQUAL WS-LETRA(WS-Y)
                     IF WS-LETRA(WS-Y) EQUAL '*'
                        ADD 1                 TO CN-CONT
                        MOVE WS-LETRA(WS-Y)   TO S1-TITULAR(CN-CONT)
                        MOVE SPACE            TO WS-DADOS-BYTE(WS-X)
                                                 S1-TITULAR(CN-CONT)
                     ELSE
                        ADD 1                 TO CN-CONT
                        MOVE WS-LETRA(WS-Y)   TO S1-TITULAR(CN-CONT)
                        MOVE SPACE            TO WS-DADOS-BYTE(WS-X)
                     END-IF
                  END-IF

               INITIALIZE              WS-X
               MOVE WS-PRESERVA        TO WS-AUX
               ADD 1                   TO WS-AUX
           END-PERFORM
           .
       2400-PROCESSA-MASCARA-EXIT.
           EXIT.


      *================================================================*
      *        G R A V A   R E G I S T R O   M A S C A R A D O         *
      *================================================================*

       2500-GRAVAR-REGISTRO.
           WRITE REG-SAI               FROM S1-REGIS
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
