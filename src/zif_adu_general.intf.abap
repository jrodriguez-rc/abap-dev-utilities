INTERFACE zif_adu_general
  PUBLIC.

  CLASS-METHODS get
    RETURNING
      VALUE(result) TYPE REF TO zif_adu_general.

    METHODS get_structure_fields
      IMPORTING
        !structure_name        TYPE csequence OPTIONAL
        !structure_description TYPE REF TO cl_abap_datadescr OPTIONAL
        !recursive             TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result)          TYPE cl_abap_structdescr=>component_table.

ENDINTERFACE.
