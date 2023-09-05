INTERFACE zif_adu_tr_constants
  PUBLIC.

  CONSTANTS:
    BEGIN OF gc_path,
      trans   TYPE string VALUE `/usr/sap/trans`,
      data    TYPE string VALUE `data`,
      cofiles TYPE string VALUE `cofiles`,
    END OF gc_path.

  CONSTANTS:
    BEGIN OF gc_status,
      modificable           TYPE trstatus VALUE 'D',
      modificable_protected TYPE trstatus VALUE 'L',
      release_started       TYPE trstatus VALUE 'O',
      released              TYPE trstatus VALUE 'R',
      released_protection   TYPE trstatus VALUE 'N',
    END OF gc_status.

ENDINTERFACE.
