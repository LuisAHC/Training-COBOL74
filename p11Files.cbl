000100 IDENTIFICATION DIVISION.
000110 PROGRAM-ID. PRAC11.
000120 AUTHOR. LUIS CECILIANO.
000130 DATE-WRITTEN. DECEMBER 11, 2019.
000140*ESTE PROGRAMA HACE LEE UN ARCHIVO Y GUARDA LO LEIDO EN UNA TABLA
000150 SECURITY. FOREVER.
000200 ENVIRONMENT DIVISION.
000300 INPUT-OUTPUT SECTION.
000400 FILE-CONTROL.
000500     SELECT LISTA ASSIGN TO DISK.
000600 DATA DIVISION.
000700 FILE SECTION.
000800     FD LISTA
000900         VALUE OF DEPENDENTSPECTS IS TRUE
001000     01 REG LISTA
001100         02 REG-NUMLISTA  PIC 9(04)
001200         02 REG-NOMBRE    PIC X(18)
001300         02 REG-APELLIDOS PIC X(20)
001400         02 REG-NUMGPO    PIC 9(02)
001500 WORKING-STORAGE SECTION.
001600     01 WKS-EOF-LISTA      PIC 9(02).
001700     01 WK-TOTAL-REGISTROS PIC 9(03).
001800     01 WL-INDICE          PIC 9(03).
001900     01 WS-LISTA-PARTICIPANTES.
002000         03 WS-LISTA OCCURS 50 TIMES
002100             ASCENDING KEY IS WS-NUMLISTA
002200             INDEXED BY WK-INDICE.
002300             05 WS-NUMLISTA  PIC 9(04).
002400             05 WS-NOMBRE    PIC X(18).
002500             05 WS-APELLIDOS PIC X(20).
002600             05 WS-NUMGPO    PIC 9(02).
002700 PROCEDURE DIVISION.
002800 0000-PRINCIPAL.
002900     PERFORM 1000-INICIO
003000     PERFORM 2000-PROCESO
003100     PERFORM 3000-TERMINA
003200     STOP RUN.
003300 1000-INICIO.
003400     CHANGE ATTRIBUTE TITLE OF LISTO TO "LAHC/FILE/191212/TXT."
003500     IF ATTRIBUTE RESIDENT OF LISTA = VALUE TRUE
003600         OPEN INPUT LISTA
003700         PERFORM 9999-LEE-LISTA
003800         SET WK-INDICE TO 0
003900     ELSE
004000         DISPLAY "NO EXISTE EL ARCHIVO LISTA: "
004100         MOVE 1 TO WKS-EOF-LISTA.
004200 2000-PROCESO.
004300     PERFORM 2500-LLENA-TABLA UNTIL WKS-EOF-LISTA = 1
004400     MOVE WS-NUMLISTA (WK-INDICE) TO WK-TOTAL-REGISTROS
004500     DISPLAY "WK-TOTAL-REGISTROS: " WK-TOTAL-REGISTROS
004600     PERFORM 2500-DESPLIEGA-LISTA
004700         VARYING WL-INDICE FROM 1 BY 1
004800         UNTIL WL-INDICE > WK-TOTAL-REGISTROS.
004900 2500-LLENA-TABLA.
005000     SET WK-INDICE UP BY 1
005100     MOVE REG-NUMLISTA TO WS-NUMLISTA   (WK-INDICE)
005200     MOVE REG-NOMBRE TO WS-NOMBRE       (WK-INDICE)
005300     MOVE REG-APELLIDOS TO WS-APELLIDOS (WK-INDICE)
005400     MOVE REG-NUMGPO TO WS-NUMGPO       (WK-INDICE)
005500     PERFORM 9999-LEE-LISTA.
005600 2500-DESPLIEGA-LISTA.
005700     SET WK-INDICE TO WL-INDICE
005800     DISPLAY "REGISTRO: " WS-NUMLISTA (WK-INDICE)
005900             ".-: " WS-APELLIDOS      (WK-INDICE)
006000             "    " WS-NOMBRE         (WK-INDICE).
006100 3000-TERMINA.
006200     CLOSE LISTA.
006300 9999-LEE-LISTA.
006400     READ LISTA AT END MOVE 1 TO WKS-EOF-LISTA.
