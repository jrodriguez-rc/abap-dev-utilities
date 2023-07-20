INTERFACE zif_adu_log
  PUBLIC.

  METHODS add_exception
    IMPORTING
      ix_exception TYPE REF TO cx_root.

  METHODS add_message
    IMPORTING
      is_message TYPE bal_s_msg.

  METHODS save.

ENDINTERFACE.
