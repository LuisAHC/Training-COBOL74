      ******************************************************************
      * Author: LUIS CECILIANO
      * Date: DECEMBER 12, 2019
      * Purpose: PRACTICE
      * Tectonics: cobc
      * Detalles: Simulación del funcionamiento de una cajero automatico
      * Cuenta: 123456789012
      * NIP: 1234
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WKS-OPCION PIC 9(01).
           01 WKS-ACTIVO PIC 9(01).
           01 WKS-CONT   PIC X(01).
           01 WKS-CUENTA PIC 9(12).
           01 WKS-PASS   PIC 9(04).
           01 WKS-DINERO PIC S9(04)V9(02).
           01 WKS-OPER   PIC S9(04)V9(02).
           01 WKS-TABLA-USUARIOS.
               03 WK-USUARIO.
      *            ASCENDING KEY IS WK-I
      *            INDEXED BY WK-INDICE.
      *            05 WK-I         PIC 9(02).
                   05 WK-NUMCUENTA PIC X(12).
                   05 WK-NIP       PIC X(04).
                   05 WK-SALDO     PIC S9(04)V9(02).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MOVE 1 TO WKS-ACTIVO
           PERFORM 0500-CREAR-USUARIO
           PERFORM 0600-LOGIN
           STOP RUN.
       0500-CREAR-USUARIO.
           MOVE "123456789012" TO WK-NUMCUENTA
           MOVE "1234" TO WK-NIP
           MOVE 1000 TO WK-SALDO.
       0600-LOGIN.
           DISPLAY "INGRESE SU NUMERO DE CUENTA"
           ACCEPT WKS-CUENTA
           DISPLAY "INGRESE SU NIP"
           ACCEPT WKS-PASS
           IF WK-NUMCUENTA=WKS-CUENTA AND WK-NIP=WKS-PASS
               PERFORM 1000-MENU UNTIL WKS-ACTIVO = 0
           ELSE
               DISPLAY "CUENTA O CONTRASEÃ‘A INCORRECTOS"
               PERFORM 0600-LOGIN.
       1000-MENU.
           DISPLAY " "
           DISPLAY "CAJERO AUTOMATICO"
           DISPLAY "SELECCIONE LA OPCION QUE DESEE"
           DISPLAY "1. DEPOSITO"
           DISPLAY "2. RETIRO"
           DISPLAY "3. CONSULTA"
           DISPLAY "4. SALIR"
           ACCEPT WKS-OPCION
           DISPLAY " "
           EVALUATE WKS-OPCION
               WHEN 1
                   PERFORM 2000-DEPOSITO
               WHEN 2
                   PERFORM 3000-RETIRO
               WHEN 3
                   PERFORM 4000-CONSULTA
               WHEN 4
                   PERFORM 5000-SALIR
               WHEN OTHER
                   DISPLAY "OPCION INVALIDA, SELECCIONE OTRA".
       2000-DEPOSITO.
           DISPLAY "SU SALDO ACTUAL ES DE: " WK-SALDO
           DISPLAY "INGRESE LA CANTIDAD QUE DESEA ABONAR, "
                   "SI NO DESEA ABONAR INGRESE '0'"
           ACCEPT WKS-DINERO
           IF WKS-DINERO=100 OR WKS-DINERO=200 OR WKS-DINERO=500
              OR WKS-DINERO=0
               ADD WKS-DINERO TO WK-SALDO GIVING WKS-OPER
               SET WK-SALDO TO WKS-OPER
               DISPLAY "OPERACION EXITOSA"
               DISPLAY " "
               DISPLAY "SU NUEVO SALDO ES DE: " WK-SALDO
               DISPLAY " "
               DISPLAY "PRESIONE ENTER PARA CONTINUAR"
               ACCEPT WKS-CONT
           ELSE
               DISPLAY "SOLO PUEDES ABONAR $100.00, $200.00 O $500"
                           ".00"
               DISPLAY " "
               PERFORM 2000-DEPOSITO.
       3000-RETIRO.
           IF WK-SALDO=100
               DISPLAY "NO PUEDE REALIZAR ESTA OPERACION, "
                       "NO CUENTA CON SALDO SUFICIENTE"
               DISPLAY "SU SALDO ACTUAL ES DE: " WK-SALDO
               DISPLAY " "
           ELSE
               DISPLAY "SU SALDO ACTUAL ES DE: " WK-SALDO
               DISPLAY "INGRESE LA CANTIDAD QUE DESEA RETIRAR, "
                       "SI NO DESEA RETIRAR INGRESE '0'"
               ACCEPT WKS-DINERO
               IF WKS-DINERO=100 OR WKS-DINERO=200 OR WKS-DINERO=500
                   OR WKS-DINERO=0
                   SUBTRACT WKS-DINERO FROM WK-SALDO GIVING WKS-OPER
                   IF WKS-OPER < 100
                       DISPLAY "NO PUEDE REALIZAR ESTA OPERACION, "
                           "NO CUENTA CON SALDO SUFICIENTE"
                       DISPLAY " "
                       PERFORM 3000-RETIRO
                   ELSE
                       SET WK-SALDO TO WKS-OPER
                       DISPLAY "OPERACION EXITOSA"
                       DISPLAY " "
                       DISPLAY "SU NUEVO SALDO ES DE: " WK-SALDO
                       DISPLAY " "
                       DISPLAY "PRESIONE ENTER PARA CONTINUAR"
                       ACCEPT WKS-CONT
               ELSE
                   DISPLAY "SOLO PUEDES RETIRAR $100.00, $200.00 O $500"
                           ".00"
                   DISPLAY " "
                   PERFORM 3000-RETIRO.
       4000-CONSULTA.
           DISPLAY "SU SALDO ACTUAL ES DE: " WK-SALDO
           DISPLAY " "
           DISPLAY "PRESIONE ENTER PARA CONTINUAR"
           ACCEPT WKS-CONT.
       5000-SALIR.
           DISPLAY "HASTA PRONTO."
           MOVE 0 TO WKS-ACTIVO.
       END PROGRAM ATM.
