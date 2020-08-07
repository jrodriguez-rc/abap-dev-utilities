"! <p class="shorttext synchronized" lang="en">Constants</p>
INTERFACE zif_adu_constants
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">C (Create) - R (Read) - U (Update) - D (Delete)</p>
    BEGIN OF crud,
      create TYPE zadu_crud_ind VALUE 'C' ##NO_TEXT,
      read   TYPE zadu_crud_ind VALUE 'R' ##NO_TEXT,
      update TYPE zadu_crud_ind VALUE 'U' ##NO_TEXT,
      delete TYPE zadu_crud_ind VALUE 'D' ##NO_TEXT,
    END OF crud.

ENDINTERFACE.
