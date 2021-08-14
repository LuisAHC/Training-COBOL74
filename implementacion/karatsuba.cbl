      ******************************************************************
      * Autor: Hernández Ceciliano Luis Ángel
      * Fecha: 19-02-2020
      * Detalles: Implementación sencilla del algoritmo Karatsuba
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KARATSUBA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WKS-NUMERO-A.
               03 WKS-NUMSA  PIC 9(01) OCCURS 4 TIMES.
           01 WKS-NUMERO-B.
               03 WKS-NUMSB  PIC 9(01) OCCURS 4 TIMES.
           01 WKS-MATRIZ-PRODUCTOS.
               03 WS-MATRIZ OCCURS 4 TIMES
                   INDEXED BY WK-INDICE.
                   05 WS-NUMSM  PIC 9(01) OCCURS 8 TIMES.
           01 WKS-PRODUCTO.
               03 WKS-NUMSP  PIC 9(01) OCCURS 8 TIMES.
           01 WKS-I          PIC 9(01).
           01 WKS-J          PIC 9(01).
           01 WKS-K          PIC 9(01).
           01 WKS-S          PIC 9(01).
           01 WKS-AUX        PIC 9(02).
           01 WKS-AUX-SUMA   PIC 9(02).
           01 WKS-K2         PIC 9(01).
           01 WKS-AUX-INICIO PIC 9(01).
           01 WKS-MULT       PIC 9(02).
           01 WKS-IZQUIERDO  PIC 9(01).
           01 WKS-DERECHO    PIC 9(01).

      *VARIABLES PARA KARATSUBA
           01 WKS-KARAT-A.
               03 WKS-KARA PIC 9(01) OCCURS 3 TIMES.
           01 WKS-KARAT-B.
               03 WKS-KARB PIC 9(01) OCCURS 3 TIMES.
           01 WKS-KARAT-C.
               03 WKS-KARC PIC 9(01) OCCURS 3 TIMES.
           01 WKS-KARAT-D.
               03 WKS-KARD PIC 9(01) OCCURS 3 TIMES.

           01 WKS-NUMEROK-A.
               03 WKS-NUMSAK  PIC 9(01) OCCURS 3 TIMES.
           01 WKS-NUMEROK-B.
               03 WKS-NUMSBK  PIC 9(01) OCCURS 3 TIMES.

           01 WKS-MATRIZK-PRODUCTOS.
               03 WS-MATRIZK OCCURS 3 TIMES
                   INDEXED BY WK-INDICEK.
                   05 WS-NUMSMK  PIC 9(01) OCCURS 5 TIMES.
           01 WKS-PRODUCTOK.
               03 WKS-NUMSPK  PIC 9(01) OCCURS 5 TIMES.

           01 WKS-MATRIZK-SUMAS.
               03 WS-MATRIZSK OCCURS 2 TIMES
                   INDEXED BY WK-INDICESK.
                   05 WS-NUMSMSK  PIC 9(01) OCCURS 3 TIMES.
           01 WKS-SUMASK.
               03 WKS-NUMSSK  PIC 9(01) OCCURS 3 TIMES.


           01 WKS-RES-1.
               03 WKS-RES1 PIC 9(01) OCCURS 5 TIMES.
           01 WKS-RES-1-10ALA4.
               03 WKS-RES1-ALA4 PIC 9(01) OCCURS 9 TIMES.
           01 WKS-RES-2.
               03 WKS-RES2 PIC 9(01) OCCURS 5 TIMES.
           01 WKS-RES-3 PIC 9(06).
           01 WKS-FINAL PIC 9(08).

           01 WKS-RES-FINAL.
               03 WKS-RESF  PIC X(01) OCCURS 8 TIMES.

       PROCEDURE DIVISION.
       0000-MAIN.

            MOVE 0 TO WKS-I
            PERFORM 0500-ASIGNAR-NUMEROS

            MOVE 0 TO WKS-K
            MOVE 5 TO WKS-K2
            MOVE 0 TO WKS-AUX-INICIO
            MOVE 0 TO WKS-MULT

      *      PERFORM 3000-MULTIPLICACION

            DISPLAY "MULTIPLICACION POR KARATSUBA"
            PERFORM 4000-DIVIDIR
            PERFORM 4100-PASO-1

            MOVE 0 TO WKS-K
            MOVE 5 TO WKS-K2
            MOVE 0 TO WKS-AUX-INICIO
            MOVE 0 TO WKS-MULT
            PERFORM 4200-PASO-2

            MOVE 0 TO WKS-K
            MOVE 5 TO WKS-K2
            MOVE 0 TO WKS-AUX-INICIO
            MOVE 0 TO WKS-MULT
            PERFORM 4300-PASO-3

            DISPLAY WKS-NUMERO-A  "*"  WKS-NUMERO-B "="
            PERFORM 5000-PASO-4

            STOP RUN.

       0500-ASIGNAR-NUMEROS.
      *LLENAMOS EL NUMERO A
            MOVE 0 TO WKS-NUMSA(1)
            MOVE 0 TO WKS-NUMSA(2)
            MOVE 1 TO WKS-NUMSA(3)
            MOVE 2 TO WKS-NUMSA(4)
      *LLENAMOS EL NUMERO B
            MOVE 0 TO WKS-NUMSB(1)
            MOVE 0 TO WKS-NUMSB(2)
            MOVE 1 TO WKS-NUMSB(3)
            MOVE 2 TO WKS-NUMSB(4)
      *LLENAMOS CON CEROS EL ARREGLO PRODUCTO Y LA MATRIZ DE PRODUCTOS
            MOVE 1 TO WKS-I
            SET WK-INDICE TO 0
            PERFORM 0750-LLENAR-CEROS 8 TIMES.

       0750-LLENAR-CEROS.
            SET WK-INDICE UP BY 1
            MOVE 0 TO WKS-NUMSP(WKS-I)
            IF WK-INDICE < 5
               MOVE 0 TO WS-NUMSM(WK-INDICE,WKS-I)
            ADD 1 TO WKS-I.

       1000-MULTIPLICAR.

            SET WKS-AUX TO 0
            SUBTRACT WKS-AUX-INICIO FROM WKS-K2 GIVING WKS-K

            MOVE 3 TO WKS-J
            PERFORM 1500-MULTIPLICAR-2 UNTIL WKS-J IS EQUAL TO 0

            ADD 1 TO WKS-AUX-INICIO

            SUBTRACT 1 FROM WKS-I.
       1500-MULTIPLICAR-2.
            COMPUTE WKS-MULT=WKS-NUMSAK(WKS-J)*WKS-NUMSBK(WKS-I)
            COMPUTE WKS-MULT = WKS-MULT+ WKS-AUX
            SET WKS-AUX TO 0
            IF WKS-MULT GREATER THAN 9
                COMPUTE WKS-IZQUIERDO = WKS-MULT/10
                COMPUTE WKS-DERECHO = WKS-MULT - WKS-IZQUIERDO*10
                MOVE WKS-IZQUIERDO TO WKS-AUX
                MOVE WKS-DERECHO TO WS-NUMSMK(WKS-I,WKS-K)
                SUBTRACT 1 FROM WKS-K
            ELSE
                MOVE WKS-MULT TO WKS-DERECHO
                MOVE WKS-DERECHO TO WS-NUMSMK(WKS-I,WKS-K)
                SUBTRACT 1 FROM WKS-K.
            IF WKS-J IS EQUAL TO 1 AND WKS-AUX IS NOT EQUAL TO 0
               MOVE WKS-AUX TO WS-NUMSMK(WKS-I,WKS-K).

            SUBTRACT 1 FROM WKS-J.

       2000-SUMAR.
            MOVE 3 TO WKS-J
            MOVE 0 TO WKS-AUX-SUMA
            PERFORM 2500-SUMAR-2 UNTIL WKS-J IS EQUAL TO 0
            IF WKS-AUX-SUMA IS GREATER THAN 9
                COMPUTE WKS-IZQUIERDO = WKS-AUX-SUMA / 10
                MOVE WKS-IZQUIERDO TO WKS-AUX
                COMPUTE WKS-DERECHO = WKS-AUX-SUMA- WKS-IZQUIERDO*10
                MOVE WKS-DERECHO TO WKS-NUMSPK(WKS-S)
            ELSE
                MOVE WKS-AUX-SUMA TO WKS-NUMSPK(WKS-S).
            SUBTRACT 1 FROM WKS-S.
       2500-SUMAR-2.
            COMPUTE WKS-AUX-SUMA = WKS-AUX-SUMA+WKS-AUX+
                       WS-NUMSMK(WKS-J,WKS-S)
            MOVE 0 TO WKS-AUX
            SUBTRACT 1 FROM WKS-J.

       3000-MULTIPLICACION.
            PERFORM 1000-MULTIPLICAR UNTIL WKS-I IS EQUAL TO 0
            MOVE 0 TO WKS-AUX
            MOVE 0 TO WKS-AUX-SUMA
            PERFORM 2000-SUMAR UNTIL WKS-S IS EQUAL TO 0.

      *DESDE ESTE PUNTO EMPIEZAN LOS PASOS PARA EL METODO KARATSUBA
       4000-DIVIDIR.
            MOVE 0 TO WKS-KARA(1)
            MOVE WKS-NUMSA(1) TO WKS-KARA(2)
            MOVE WKS-NUMSA(2) TO WKS-KARA(3)
            MOVE 0 TO WKS-KARB(1)
            MOVE WKS-NUMSA(3) TO WKS-KARB(2)
            MOVE WKS-NUMSA(4) TO WKS-KARB(3)

            MOVE 0 TO WKS-KARC(1)
            MOVE WKS-NUMSB(1) TO WKS-KARC(2)
            MOVE WKS-NUMSB(2) TO WKS-KARC(3)
            MOVE 0 TO WKS-KARD(1)
            MOVE WKS-NUMSB(3) TO WKS-KARD(2)
            MOVE WKS-NUMSB(4) TO WKS-KARD(3).

       4100-PASO-1.
      *MULTPLICAR A POR C Y AÑADIR CUATRO CEROS
            MOVE WKS-KARAT-A TO WKS-NUMEROK-A
            MOVE WKS-KARAT-C TO WKS-NUMEROK-B

            MOVE 3 TO WKS-I
            MOVE 5 TO WKS-S
            PERFORM 3000-MULTIPLICACION.

            MOVE WKS-PRODUCTOK TO WKS-RES-1
            MOVE "00000" TO WKS-PRODUCTOK

            MOVE WKS-RES-1 TO WKS-RES-1-10ALA4
            MOVE "0000" TO WKS-RES-1-10ALA4(6 :).
      *      DISPLAY "RESULTADO PASO 1:"
      *      DISPLAY WKS-RES-1
      *      DISPLAY WKS-RES-1-10ALA4.

       4200-PASO-2.
      *MULTIPLICAR B POR D
            MOVE WKS-KARAT-B TO WKS-NUMEROK-A
            MOVE WKS-KARAT-D TO WKS-NUMEROK-B

            MOVE 3 TO WKS-I
            MOVE 5 TO WKS-S
            PERFORM 3000-MULTIPLICACION.

            MOVE WKS-PRODUCTOK TO WKS-RES-2
            MOVE "00000" TO WKS-PRODUCTOK.
      *      DISPLAY "RESULTADO PASO 2:"
      *      DISPLAY WKS-RES-2.

       4300-PASO-3.
            MOVE WKS-KARAT-A TO WS-MATRIZSK(1)
            MOVE WKS-KARAT-B TO WS-MATRIZSK(2)

            MOVE 3 TO WKS-I
            PERFORM 4350-SUMA-PASO-3 UNTIL WKS-I IS EQUAL TO 0
            MOVE WKS-SUMASK TO WKS-NUMEROK-A


            MOVE "000" TO WKS-SUMASK
            MOVE WKS-KARAT-C TO WS-MATRIZSK(1)
            MOVE WKS-KARAT-D TO WS-MATRIZSK(2)

            MOVE 3 TO WKS-I
            PERFORM 4350-SUMA-PASO-3 UNTIL WKS-I IS EQUAL TO 0
            MOVE WKS-SUMASK TO WKS-NUMEROK-B
            MOVE 3 TO WKS-I
            MOVE 5 TO WKS-S
            PERFORM 3000-MULTIPLICACION

            SUBTRACT FUNCTION NUMVAL(WKS-RES-1) FROM FUNCTION
            NUMVAL(WKS-PRODUCTOK) GIVING WKS-RES-3
            SUBTRACT FUNCTION NUMVAL(WKS-RES-2) FROM WKS-RES-3
                       GIVING WKS-RES-3
            COMPUTE WKS-RES-3 = WKS-RES-3 * 100.
      *      DISPLAY "RESULTADO PASO 3:"
      *      DISPLAY WKS-RES-3.

       4350-SUMA-PASO-3.
            MOVE 2 TO WKS-J
            MOVE 0 TO WKS-AUX-SUMA
            PERFORM 4355-SUMA-2-PASO-3 UNTIL WKS-J IS EQUAL TO 0
            IF WKS-AUX-SUMA IS GREATER THAN 9
                COMPUTE WKS-IZQUIERDO=WKS-AUX-SUMA / 10
                MOVE WKS-IZQUIERDO TO WKS-AUX
                COMPUTE WKS-DERECHO=WKS-AUX-SUMA- WKS-IZQUIERDO*10
                MOVE WKS-DERECHO TO WKS-NUMSSK(WKS-I)
            ELSE
                MOVE WKS-AUX-SUMA TO WKS-NUMSSK(WKS-I).
            SUBTRACT 1 FROM WKS-I.
       4355-SUMA-2-PASO-3.
            COMPUTE WKS-AUX-SUMA=WKS-AUX-SUMA+WKS-AUX+
                       WS-NUMSMSK(WKS-J,WKS-I)
            MOVE 0 TO WKS-AUX
            SUBTRACT 1 FROM WKS-J.

       5000-PASO-4.
            COMPUTE WKS-FINAL = FUNCTION NUMVAL(WKS-RES-1-10ALA4)+
                               FUNCTION NUMVAL(WKS-RES-2) +
                               WKS-RES-3
            MOVE WKS-FINAL TO WKS-RES-FINAL

            MOVE 1 TO WKS-I
            PERFORM UNTIL WKS-RESF(WKS-I) IS NOT EQUAL TO 0
               IF WKS-RESF(WKS-I) IS EQUAL TO '0' THEN
                    MOVE ' ' TO WKS-RESF(WKS-I)
                    ADD 1 TO WKS-I
               END-IF
            END-PERFORM
            DISPLAY "     "WKS-RES-FINAL.
       END PROGRAM KARATSUBA.
