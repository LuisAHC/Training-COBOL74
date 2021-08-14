000100 IDENTIFICATION DIVISION.
000110 PROGRAM-ID. PRAC15.
000120 AUTHOR. LUIS CECILIANO.
000130 DATE-WRITTEN. DECEMBER 18, 2019.
000140*ESTE PROGRAMA GENERA UN REPORTE DE UNA BASE DE DATOS
000150 SECURITY. FOREVER.
000200 ENVIRONMENT DIVISION.
000300 DATA DIVISION.
       DATA-BASE SECTION.
           DB BD01CURSOS VALUE OF TITLE IS
             "BD01CURSOS ON CREDITO".
       WORKING-STORAGE SECTION.
           01 WKS-EOF-BD01 PIC 9(02) VA 0.
           01 WKS-EOF-BD02 PIC 9(02) VA 0.
           01 WKS-INDICE   PIC 9(02).
           01 WKS-LISTA-CURSOS.
               02 FILLER PIC X(11) VA "0502CANDE03".
               02 FILLER PIC X(11) VA "0202COBOL06".
               02 FILLER PIC X(11) VA "0604DMSII02".
               02 FILLER PIC X(11) VA "0404DASDL03".
               02 FILLER PIC X(11) VA "0301ALGOL21".
           01 WKS-LISTA-CURSO REDEFINES WKS-LISTA-CURSOS.
               02 WKS-CURSO OCCURS 5 TIMES.
                   03 WKS-CVE-CURSO       PIC 9(04).
                   03 WKS-DESCRIPCION     PIC X(05).
                   03 WKS-HORAS-DEDICADAS PIC 9(02).
       REPORT SECTION.
           RD LISTADO
               PAGE LIMIT   058
               HEADING      001
               FIRST DETAIL 009
               LAST DETAIL  052.
       01 RD-IMPRE-TITULOS TYPE IS PH.
           10 RD-IMPRE-TITULO-01 LINE 001.
               15 COLUMN 001 PIC X(13) VALUE "S999/P600-001".
               15 COLUMN 040 PIC X(36) VALUE 
                 "GRUPO CAPACITACIÓN STEFANINI -".
               15 COLUMN 084 PIC X(10) SOURCE BD02-DESCRIP.
               15 COLUMN 106 PIC X(17) VA "HORAS ASIGNADAS: ".
               15 COLUMN 124 PIC 9(02) SOURCE BD02-HORAS-ASIG.
           10 RD-IMPRE-TITULO-02 LINE 003.
               15 COLUMN 001 PIC X(14) VALUE "FECHA PROCESO:".
               15 COLUMN 016 PIC 99/99/99 SOURCE TODAYS-DATE.
               15 COLUMN 060 PIC X(25) VALUE 
                 "SISTEMA DE CAPACITACIONES".
               15 COLUMN 120 PIC X(07) VALUE "HOJA:".
               15 COLUMN 127 PIC Z,ZZZ SOURCE PAGE-COUNTER.
           10 RD-IMPRE-TITULO-03 LINE 004.
               15 COLUMN 002 PIC X(003) VA "GPO".
               15 COLUMN 006 PIC X(004) VA "NUM.".
               15 COLUMN 015 PIC X(006) VA "NOMBRE".
               15 COLUMN 040 PIC X(009) VA "APELLIDOS".
       01 RD-IMPRE-DETALLE TYPE DETAIL.
           10 RD-IMPRE-DET01 LINE PLUS 001.
               15 COLUMN 002 GROUP INDICATE PIC x(002) VALUE "00".
               15 COLUMN 006 PIC Z(004)     SOURCE BD01-IDCONSULTOR.
               15 COLUMN 015 PIC X(040)     SOURCE BD01-NOMBRE.
               15 COLUMN 060 PIC 9999/99/99 SOURCE BD01-FECHA-CURSO.
       01 RD-IMPRE-CORTE TYPE DETAIL.
           10 RD-IMPRE-PIE-01 LINE PLUS 002.
               15 COLUMN 006 PIC X(027) VALUE
                 "--------------------------".
           10 RD-IMPRE-PIE-02 LINE PLUS 002.
               15 COLUMN 001 PIC X(10) VA SPACES.
       01 TYPE PAGE FOOTING.
           10 COLUMN 10 PIC X(35) VALUE
             "REPORTE DE ASISTENCIA CAPACITACION ".
           10 COLUMN 47 PIC X(06) SOURCE BD02-DESCRIP.
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-TERMINA
           STOP RUN.
       1000-INICIO.
           OPEN INQUIRY BD01CURSOS.
           PERFORM 999-FIND-BD02
           IF WKS-EOF-BD02 = 0
               SET BD01IDXFECURSO TO BEGINNING 
               PERFORM 999-FIND-FIRST-BD01
               IF WKS-EOF-BD01 = 0
                   CHANGE ATTRIBUTE TITLE OF REPORTE TO
                     "LAHC/LIST/P126/191218."
                   ACCEPT WKS-HORA-PROC FROM TIME
                   OPEN OUTPUT REPORTE
                   ACCEPT WKS-HORA-PROC FROM TIME
                   INITIATE LISTADO.
       2000-PROCESO.
           PERFORM 2500-GENERA-REPORTE UNTIL WKS-EOF-BD01 = 1
           TERMINATE LISTADO.
       2500-GENERA-REPORTE.
           GENERATE RD-IMPRE-DETALLE
           PERFORM 999-FIND-NEXT-BD01.
       3000-TERMINA.
           CLOSE BD01CURSOS
           CLOSE REPORTE.
       999-FIND-BD02.
           FIND B02XCVE AT
             BD02-CVE-CURSO = 202
             ON EXCEPTION 
               MOVE 1 TO WKS-EOF-BD02.
       999-FIND-FIRST-BD01.
           FIND FIRST BD01IDXFECURSO AT 
               BD01-IDCONSULTOR > 0 AND 
               BD01-FECHA-CURSO > 0
               ON EXCEPTION
                 MOVE 1 TO WKS-EOF-BD01.
       999-FIND-NEXT-BD01.
           FIND NEXT BD01IDXFECURSO AT 
               BD01-IDCONSULTOR > 0 AND 
               BD01-FECHA-CURSO > 0
               ON EXCEPTION
                 MOVE 1 TO WKS-EOF-BD01.
