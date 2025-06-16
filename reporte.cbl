
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHERO-ENTRADA
           ASSIGN TO "nombres.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-STATUS.
           
           SELECT FICHERO-REPORTE
           ASSIGN TO "reporte.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-STATUS-OUT.
       DATA DIVISION.
       FILE SECTION.
       FD  FICHERO-ENTRADA.
       01  FICHERO-ENTRADA-REGISTRO.
           10 NOMBRE PIC X(20).
           10 APELLIDO1 PIC X(30).
           10 APELLIDO2 PIC X(30).
           
       FD  FICHERO-REPORTE.
       01  REGISTRO-REPORTE PIC X(90).
       WORKING-STORAGE SECTION.
       01  FS-STATUS PIC XX.
           88 FS-OK VALUE "00".
           88 FS-ERROR VALUE "35".
       01  FS-STATUS-OUT PIC XX.
           88 FS-OK-OUT VALUE "00".
           88 FS-ERROR-OUT VALUE "35".
           
       01  WS-NOMBRE PIC X(20) VALUE SPACE.
       
       01  WS-EOF                  PIC X VALUE 'N'.
           88 FIN-DE-FICHERO       VALUE 'S'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           
           PERFORM APERTURA.
           
           PERFORM PROCESAR UNTIL FIN-DE-FICHERO.
           
           PERFORM CIERRE.
           PERFORM FIN-PROGRAMA.
           
           
           
       APERTURA.
           DISPLAY "Introduzca el nombre a buscar: "
           ACCEPT WS-NOMBRE.
           
           OPEN INPUT FICHERO-ENTRADA.
           OPEN OUTPUT FICHERO-REPORTE.
           
           IF NOT FS-OK 
               DISPLAY "ERROR ABRIENDO FICHERO DE ENTRADA." 
                       "STATUS: " FS-STATUS
               SET FIN-DE-FICHERO TO TRUE
           END-IF.
           
           IF NOT FS-OK-OUT
               DISPLAY "ERROR CREANDO EL FICHERO DE REPORTE."
                   "STATUS: " FS-STATUS-OUT
               SET FIN-DE-FICHERO TO TRUE
           END-IF.
               
       PROCESAR.
           READ FICHERO-ENTRADA
               AT END
                   SET FIN-DE-FICHERO TO TRUE
               NOT AT END
                   PERFORM VALIDAR
           END-READ.
       
       VALIDAR.
           IF FUNCTION TRIM(NOMBRE) = FUNCTION TRIM(WS-NOMBRE)
               MOVE FICHERO-ENTRADA-REGISTRO TO REGISTRO-REPORTE
               WRITE REGISTRO-REPORTE
           END-IF.
           
       CIERRE.
           CLOSE FICHERO-ENTRADA.
           CLOSE FICHERO-REPORTE.
           
       FIN-PROGRAMA.
            STOP RUN.
       END PROGRAM REPORTE.

