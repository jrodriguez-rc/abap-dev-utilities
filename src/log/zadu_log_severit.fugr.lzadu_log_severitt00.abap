*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADU_LOG_SEVERIT................................*
DATA:  BEGIN OF STATUS_ZADU_LOG_SEVERIT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADU_LOG_SEVERIT              .
CONTROLS: TCTRL_ZADU_LOG_SEVERIT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZADU_LOG_SEVERIT              .
TABLES: ZADU_LOG_SEVERIT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
