*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADU_SWITCH_USER................................*
DATA:  BEGIN OF STATUS_ZADU_SWITCH_USER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADU_SWITCH_USER              .
CONTROLS: TCTRL_ZADU_SWITCH_USER
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZADU_SWITCH_USER              .
TABLES: ZADU_SWITCH_USER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
