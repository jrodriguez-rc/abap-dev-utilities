"! <p class="shorttext synchronized" lang="en">Check transport</p>
INTERFACE zif_adu_check_transport
  PUBLIC.

  TYPES:
    tt_result_cross_reference TYPE STANDARD TABLE OF /sdf/teap_envi_ana_result WITH DEFAULT KEY,
    tt_result_sequence        TYPE STANDARD TABLE OF /sdf/teap_dgp_conflict WITH DEFAULT KEY,
    tt_result_cross_release   TYPE STANDARD TABLE OF /sdf/teap_scv_crit_obj WITH DEFAULT KEY,
    tt_result_import_time     TYPE STANDARD TABLE OF /sdf/teap_tr_imp_time WITH DEFAULT KEY,
    tt_result_online_import   TYPE STANDARD TABLE OF /sdf/oi_result WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_result_all,
      run_code                TYPE zadu_run_code,
      results_cross_reference TYPE zif_adu_check_transport=>tt_result_cross_reference,
      results_sequence        TYPE zif_adu_check_transport=>tt_result_sequence,
      results_cross_release   TYPE zif_adu_check_transport=>tt_result_cross_release,
      results_import_time     TYPE zif_adu_check_transport=>tt_result_import_time,
      results_online_import   TYPE zif_adu_check_transport=>tt_result_online_import,
    END OF ts_result_all.

  "! <p class="shorttext synchronized" lang="en">Execute cross reference checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_cross_reference
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_cross_reference
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Execute sequence checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_sequence
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_sequence
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Execute cross release checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_cross_release
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_cross_release
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Execute import time checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_import_time
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_import_time
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Execute online import checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_online_import
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>tt_result_online_import
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Execute all checks</p>
  "!
  "! @parameter results | <p class="shorttext synchronized" lang="en">Results</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS check_all
    RETURNING
      VALUE(results) TYPE zif_adu_check_transport=>ts_result_all
    RAISING
      zcx_adu_check_transport.

  "! <p class="shorttext synchronized" lang="en">Save executed checks log</p>
  "!
  "! @parameter commit | <p class="shorttext synchronized" lang="en">Commit (default true)</p>
  "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check exception</p>
  METHODS save_results
    IMPORTING
      commit TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_adu_check_transport.

ENDINTERFACE.
