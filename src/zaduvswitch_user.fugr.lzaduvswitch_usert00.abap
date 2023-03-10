*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADUVSWITCH_USER................................*
TABLES: ZADUVSWITCH_USER, *ZADUVSWITCH_USER. "view work areas
CONTROLS: TCTRL_ZADUVSWITCH_USER
TYPE TABLEVIEW USING SCREEN '0100'.
DATA: BEGIN OF STATUS_ZADUVSWITCH_USER. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZADUVSWITCH_USER.
* Table for entries selected to show on screen
DATA: BEGIN OF ZADUVSWITCH_USER_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZADUVSWITCH_USER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADUVSWITCH_USER_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZADUVSWITCH_USER_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZADUVSWITCH_USER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADUVSWITCH_USER_TOTAL.

*.........table declarations:.................................*
TABLES: ZADU_SWITCH                    .
TABLES: ZADU_SWITCH_USER               .
