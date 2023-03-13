*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADUVSWITCH.....................................*
TABLES: ZADUVSWITCH, *ZADUVSWITCH. "view work areas
CONTROLS: TCTRL_ZADUVSWITCH
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZADUVSWITCH. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZADUVSWITCH.
* Table for entries selected to show on screen
DATA: BEGIN OF ZADUVSWITCH_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZADUVSWITCH.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADUVSWITCH_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZADUVSWITCH_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZADUVSWITCH.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADUVSWITCH_TOTAL.

*.........table declarations:.................................*
TABLES: ZADU_SWITCH                    .
