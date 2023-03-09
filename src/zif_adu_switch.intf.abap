INTERFACE zif_adu_switch
  PUBLIC.

  CONSTANTS:
    BEGIN OF gc_status,
      disabled         TYPE zadu_switch_status VALUE '',
      custom_condition TYPE zadu_switch_status VALUE 'CUSTOMCOND',
      user             TYPE zadu_switch_status VALUE 'USER',
      enabled          TYPE zadu_switch_status VALUE 'ENABLED',
    END OF gc_status.

  METHODS get_status
    RETURNING
      VALUE(result) TYPE zadu_switch_status.

  METHODS is_active
    IMPORTING
      is_custom_data_check TYPE any OPTIONAL
    RETURNING
      VALUE(result)        TYPE abap_bool.

ENDINTERFACE.
