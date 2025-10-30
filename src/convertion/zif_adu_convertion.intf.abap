INTERFACE zif_adu_convertion
  PUBLIC.

  METHODS create_result_type
    RETURNING VALUE(result) TYPE REF TO data
    RAISING   zcx_adu_convertion.

  METHODS move_results
    IMPORTING ig_content TYPE any
    EXPORTING eg_result  TYPE any
    RAISING   zcx_adu_convertion.

ENDINTERFACE.
