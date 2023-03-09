*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADU_SWITCH.....................................*
DATA:  BEGIN OF STATUS_ZADU_SWITCH                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADU_SWITCH                   .
CONTROLS: TCTRL_ZADU_SWITCH
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZADU_SWITCH                   .
TABLES: ZADU_SWITCH                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
