"! <p class="shorttext synchronized" lang="en">Check transport</p>
INTERFACE zif_adu_check_transport
  PUBLIC.

  TYPES:
    ty_run_code               TYPE c LENGTH 32,
    tt_result_cross_reference TYPE STANDARD TABLE OF /sdf/teap_envi_ana_result WITH EMPTY KEY,
    tt_result_sequence        TYPE STANDARD TABLE OF /sdf/teap_dgp_conflict WITH EMPTY KEY,
    tt_result_cross_release   TYPE STANDARD TABLE OF /sdf/teap_scv_crit_obj WITH EMPTY KEY,
    tt_result_import_time     TYPE STANDARD TABLE OF /sdf/teap_tr_imp_time WITH EMPTY KEY,
    tt_result_online_import   TYPE STANDARD TABLE OF /sdf/oi_result WITH EMPTY KEY.

  TYPES:
    BEGIN OF ts_result_all,
      run_code                TYPE zif_adu_check_transport=>ty_run_code,
      results_cross_reference TYPE zif_adu_check_transport=>tt_result_cross_reference,
      results_sequence        TYPE zif_adu_check_transport=>tt_result_sequence,
      results_cross_release   TYPE zif_adu_check_transport=>tt_result_cross_release,
      results_import_time     TYPE zif_adu_check_transport=>tt_result_import_time,
      results_online_import   TYPE zif_adu_check_transport=>tt_result_online_import,
    END OF ts_result_all.

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

  METHODS check_import_time
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_import_time
    RAISING
      zcx_adu_check_transport.

  METHODS check_online_import
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_online_import
    RAISING
      zcx_adu_check_transport.

  METHODS check_all
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>ts_result_all
    RAISING
      zcx_adu_check_transport.

  METHODS save_results
    IMPORTING
      commit TYPE abap_bool
    RAISING
      zcx_adu_check_transport.

ENDINTERFACE.
