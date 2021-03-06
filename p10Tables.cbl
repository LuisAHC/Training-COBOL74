000100 IDENTIFICATION DIVISION.
000110 PROGRAM-ID. PRAC10.
000120 AUTHOR. LUIS CECILIANO.
000130 DATE-WRITTEN. DECEMBER 11, 2019.
000140*ESTE PROGRAMA HACE USO DE TABLAS
000150 SECURITY. FOREVER.
000200 ENVIRONMENT DIVISION.
000300 DATA DIVISION.
000400 WORKING-STORAGE SECTION.
000500     01 WK-EOA    PIC 9(02).
000600     01 WK-EOB    PIC 9(02).
000700     01 WKS-LINEA-CAPTURA.
000750        02 WKS-NUMLISTA     PIC 9(04).
000760        02 WKS-NOMBRE       PIC X(18).
000770        02 WKS-APELLIDOS    PIC X(20).
000780        02 WKS-NUMGPO       PIC 9(02).
000800     01 WS-LISTA-PARTICIPANTES.
000850        03 WS-LISTA OCCURS 3 TIMES
000860           ASCENDING KEY IS WS-NUMLISTA
000870           INDEXED BY WK-INDICE.
000880           05 WS-NUMLISTA     PIC 9(04).
000890           05 WS-NOMBRE       PIC X(18).
000900           05 WS-APELLIDOS    PIC X(20).
000910           05 WS-NUMGPO       PIC 9(02).
001000 PROCEDURE DIVISION.
001100 0000-PRINCIPAL.
001200     SET WK-INDICE TO 0
           DISPLAY "INGRESE TRES REGISTROS CON FORMATO:"
           DISPLAY "####______________________________________##"
           DISPLAY "EJ. 0003ANA ISABEL        GARCIA VERA         02"
001300     PERFORM 1000-LLENA-TABLA UNTIL WK-INDICE > 2 OR WK-EOA = 1
001400     PERFORM 2000-BUSCA       UNTIL WK-EOB = 1
001500     STOP RUN.
001600*SECCION PARA LLENAR LA TABLA, LINEA POR LINEA
001700 1000-LLENA-TABLA.
001800     ACCEPT WKS-LINEA-CAPTURA
001900     IF WKS-NUMLISTA > 0
002000         SET WK-INDICE UP BY 1
002100         MOVE WKS-NUMLISTA  TO WS-NUMLISTA  (WK-INDICE)
002200         MOVE WKS-NOMBRE    TO WS-NOMBRE    (WK-INDICE)
002300         MOVE WKS-APELLIDOS TO WS-APELLIDOS (WK-INDICE)
002400         MOVE WKS-NUMGPO    TO WS-NUMGPO    (WK-INDICE)
002500     ELSE
002600         MOVE 1 TO WK-EOB.
002700*SECCION PARA PREGUNTAR EL NUMERO DEL REGISTRO DESEADO
002800 2000-BUSCA.
002900     DISPLAY "QUE NUMERO DE LA LISTA QUIERES VER"
003000     ACCEPT WKS-NUMLISTA
003100     IF WKS-NUMLISTA > 0
003200         PERFORM 2000-SEARCH
003300     ELSE
003400         MOVE 1 TO WK-EOB.
003500*SECCION DONDE SE REALIZA LA BUSQUEDA EN LA TABLA
003600 2000-SEARCH.
003700     SET WK-INDICE TO 1
003800     SEARCH ALL WS-LISTA AT END
003900         DISPLAY "NO EXISTE REGISTRO EN EL NUMERO: " WKS-NUMLISTA
004000     WHEN
004100         WS-NUMLISTA (WK-INDICE) = WKS-NUMLISTA
004200         DISPLAY "REGISTRO: " WS-NUMLISTA  (WK-INDICE)
004300             ".-: "           WS-APELLIDOS (WK-INDICE)
004400             ",    "           WS-NOMBRE    (WK-INDICE).
