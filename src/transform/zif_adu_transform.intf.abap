INTERFACE zif_adu_transform
  PUBLIC.

  METHODS create_result_type
    RETURNING VALUE(result) TYPE REF TO data
    RAISING   zcx_adu_transform.

  METHODS move_results
    IMPORTING !input  TYPE any
    EXPORTING !output TYPE any
    RAISING   zcx_adu_transform.

ENDINTERFACE.
