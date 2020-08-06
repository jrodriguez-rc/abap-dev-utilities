"! <p class="shorttext synchronized" lang="en">Check transport</p>
INTERFACE zif_adu_check_transport
  PUBLIC.

  TYPES:
    tt_result_cross_reference TYPE STANDARD TABLE OF /sdf/teap_envi_ana_result WITH EMPTY KEY,
    tt_result_sequence        TYPE STANDARD TABLE OF /sdf/teap_dgp_conflict WITH EMPTY KEY,
    tt_result_cross_release   TYPE STANDARD TABLE OF /sdf/teap_scv_crit_obj WITH EMPTY KEY.

  METHODS check_cross_reference
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_cross_reference
    RAISING
      zcx_adu_check_transport.

  METHODS check_sequence
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_sequence
    RAISING
      zcx_adu_check_transport.

  METHODS check_cross_release
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_cross_release
    RAISING
      zcx_adu_check_transport.

ENDINTERFACE.
