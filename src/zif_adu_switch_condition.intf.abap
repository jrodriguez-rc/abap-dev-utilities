INTERFACE zif_adu_switch_condition
  PUBLIC.

  METHODS is_active
    IMPORTING
      is_data_check TYPE any OPTIONAL
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
