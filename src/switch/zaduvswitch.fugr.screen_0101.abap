PROCESS BEFORE OUTPUT.
 MODULE detail_init.
*
PROCESS AFTER INPUT.
 MODULE DETAIL_EXIT_COMMAND AT EXIT-COMMAND.
 MODULE DETAIL_SET_PFSTATUS.
 CHAIN.
    FIELD ZADUVSWITCH-CODE .
    FIELD ZADUVSWITCH-STATUS .
    FIELD ZADUVSWITCH-NAME .
    FIELD ZADUVSWITCH-CUSTOM_CHECK .
  MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.
 endchain.
 chain.
    FIELD ZADUVSWITCH-CODE .
  module detail_pai.
 endchain.