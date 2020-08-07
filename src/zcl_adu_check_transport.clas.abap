"! <p class="shorttext synchronized" lang="en">Check transport</p>
CLASS zcl_adu_check_transport DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adu_check_transport.

    CLASS-METHODS create
      IMPORTING
        transport_request TYPE trkorr
        rfc_source        TYPE rfcdest DEFAULT 'NONE'
        rfc_destination   TYPE rfcdest
      RETURNING
        VALUE(checker)    TYPE REF TO zif_adu_check_transport
      RAISING
        zcx_adu_check_transport.

    METHODS constructor
      IMPORTING
        transport_request TYPE trkorr
        rfc_source        TYPE rfcdest DEFAULT 'NONE'
        rfc_destination   TYPE rfcdest
      RAISING
        zcx_adu_check_transport.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      run_code                TYPE zif_adu_check_transport~ty_run_code,
      transport_request       TYPE trkorr,
      rfc_source              TYPE rfcdest,
      rfc_destination         TYPE rfcdest,
      source_system_name      TYPE sysname,
      destination_system_name TYPE sysname,
      results_cross_reference TYPE zif_adu_check_transport~tt_result_cross_reference,
      results_sequence        TYPE zif_adu_check_transport~tt_result_sequence,
      results_cross_release   TYPE zif_adu_check_transport~tt_result_cross_release,
      results_import_time     TYPE zif_adu_check_transport~tt_result_import_time,
      results_online_import   TYPE zif_adu_check_transport~tt_result_online_import.

    METHODS after_save.

    METHODS check_authorization
      IMPORTING
        rfcdest          TYPE rfcdest
        authority_object TYPE xuval
      RAISING
        zcx_adu_check_transport.

    METHODS fill_run_code
      RAISING
        zcx_adu_check_transport.

    METHODS fill_empty_run_code
      RAISING
        zcx_adu_check_transport.

    METHODS get_system_info
      IMPORTING
        rfcdest            TYPE rfcdest
      RETURNING
        VALUE(system_name) TYPE sysname
      RAISING
        zcx_adu_check_transport.

ENDCLASS.



CLASS zcl_adu_check_transport IMPLEMENTATION.


  METHOD create.

    checker = NEW zcl_adu_check_transport( transport_request = transport_request
                                           rfc_source        = rfc_source
                                           rfc_destination   = rfc_destination ).

  ENDMETHOD.


  METHOD constructor.

    me->transport_request = transport_request.
    me->rfc_source        = rfc_source.
    me->rfc_destination   = rfc_destination.

    source_system_name      = get_system_info( rfc_source ).
    destination_system_name = get_system_info( rfc_destination ).

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_all.

    fill_empty_run_code( ).

    results-run_code = run_code.

    results-results_cross_reference = zif_adu_check_transport~check_cross_reference( ).
    results-results_sequence        = zif_adu_check_transport~check_sequence( ).
    results-results_cross_release   = zif_adu_check_transport~check_cross_release( ).
    results-results_import_time     = zif_adu_check_transport~check_import_time( ).
    results-results_online_import   = zif_adu_check_transport~check_online_import( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_cross_reference.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE 'CNV_CDMC_CTS_GET_REF_OBJECTS' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/TEAP_LATEST_TR' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      requests TYPE STANDARD TABLE OF e070 WITH DEFAULT KEY.

    fill_empty_run_code( ).

    check_authorization( rfcdest = rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = rfc_destination authority_object = lc_authorization-destionation ).

    requests = VALUE #( ( trkorr = transport_request ) ).

    CALL FUNCTION '/SDF/TEAP_ENVI_ANA'
      EXPORTING
        iv_ana_rfc                 = rfc_source
        iv_tar_rfc                 = rfc_destination
      TABLES
        it_reqs                    = requests
        et_envanal_res_err         = results_cross_reference
      EXCEPTIONS
        empty_checked_tr_list      = 1
        destination_not_reached    = 2
        get_checked_obj_failed     = 3
        no_objs_specified_for_anal = 4
        no_referred_objects_found  = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_ENVI_ANA'.
    ENDIF.

    results_cross_reference = results_cross_reference.
    results = results_cross_reference.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_sequence.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/TEAP_GET_CSOL_FOR_TR' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/TEAP_TMS_GET_HISTORY' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      conflicts LIKE results.

    fill_empty_run_code( ).

    check_authorization( rfcdest = rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = rfc_destination authority_object = lc_authorization-destionation ).

    DATA(alog) = VALUE tmstpalogs( ( listname = '/SDF/CMO_TR_CHECK'
                                     trkorr   = transport_request
                                     trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    SELECT SINGLE param_value_i
      FROM /sdf/cmo_tr_conf
      INTO @DATA(period)
      WHERE config_param = 'PERIOD'.
    IF sy-subrc <> 0.
      period = 0.
    ENDIF.

    DATA(start_date) = COND d( WHEN period IS INITIAL THEN sy-datum - 90 ELSE sy-datum - period ).

    CALL FUNCTION '/SDF/TEAP_DOWNGRADE_PROTECT'
      EXPORTING
        iv_dev_sid                 = source_system_name
        iv_tar_sid                 = destination_system_name
        iv_dev_rfc                 = rfc_source
        iv_tar_rfc                 = rfc_destination
        iv_start_date              = start_date
        iv_end_date                = sy-datum
      TABLES
        it_checked_tr              = alog
        et_dgp_conf                = conflicts
      EXCEPTIONS
        get_dev_exp_history_error  = 1
        get_tar_imp_history_error  = 2
        no_exp_hist_for_checked_tr = 3
        empty_checked_tr_list      = 4
        get_csol_error             = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_DOWNGRADE_PROTECT'.
    ENDIF.

    results_sequence = conflicts.
    results = conflicts.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_cross_release.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/TEAP_TR_CHECK_TYPE' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/OCS_GET_INFO' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      cross_release LIKE results.

    fill_empty_run_code( ).

    check_authorization( rfcdest = rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = rfc_destination authority_object = lc_authorization-destionation ).

    DATA(alog) = VALUE tmstpalogs( ( listname = '/SDF/CMO_TR_CHECK'
                                     trkorr   = transport_request
                                     trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    CALL FUNCTION '/SDF/TEAP_SCV_CHECK'
      EXPORTING
        iv_dev_sid            = source_system_name
        iv_tar_sid            = destination_system_name
        iv_dev_rfc            = rfc_source
        iv_tar_rfc            = rfc_destination
        iv_start_date         = sy-datum
        iv_end_date           = sy-datum
      TABLES
        it_checked_tr         = alog
        et_scv_crit_obj       = cross_release
      EXCEPTIONS
        empty_checked_tr_list = 1
        get_dev_scv_error     = 2
        get_tar_scv_error     = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_SCV_CHECK'.
    ENDIF.

    results_cross_release = cross_release.
    results = cross_release.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_import_time.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source TYPE xuval VALUE '/SDF/TEAP_IMPORT_TIME' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      import_time LIKE results.

    fill_empty_run_code( ).

    check_authorization( rfcdest = rfc_source authority_object = lc_authorization-source ).

    DATA(alog) = VALUE tmstpalogs( ( listname = '/SDF/CMO_TR_CHECK'
                                     trkorr   = transport_request
                                     trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    CALL FUNCTION '/SDF/TEAP_IMPORT_TIME'
      DESTINATION source_system_name
      TABLES
        it_checked_tr            = alog
        et_tr_imp_time           = import_time
      EXCEPTIONS
        empty_checked_tr_list    = 1
        no_tpalog_for_checked_tr = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_IMPORT_TIME'.
    ENDIF.

    results_import_time = import_time.
    results = import_time.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_online_import.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/DD_DDIC_DEP_GET' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/READ_D010TAB' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      requests      TYPE STANDARD TABLE OF e070 WITH DEFAULT KEY,
      online_import LIKE results.

    fill_empty_run_code( ).

    check_authorization( rfcdest = rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = rfc_destination authority_object = lc_authorization-destionation ).

    requests = VALUE #( ( trkorr = transport_request ) ).

    CALL FUNCTION '/SDF/OI_CHECK'
      EXPORTING
        iv_ana_rfc                  = rfc_source
        iv_tar_rfc                  = rfc_destination
      TABLES
        it_reqs                     = requests
        et_result                   = online_import
      EXCEPTIONS
        destination_not_reached     = 1
        no_objs_specified_for_anal  = 2
        prerequisites_not_fulfilled = 3
        other                       = 4
        OTHERS                      = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/OI_CHECK'.
    ENDIF.

    results_online_import = online_import.
    results = online_import.

  ENDMETHOD.


  METHOD zif_adu_check_transport~save_results.

    IF run_code IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>no_checks_executed.
    ENDIF.

    after_save( ).

  ENDMETHOD.


  METHOD after_save.

    CLEAR: run_code, results_cross_reference, results_sequence, results_cross_release,
           results_import_time, results_online_import.

  ENDMETHOD.


  METHOD check_authorization.

    CALL FUNCTION 'AUTHORITY_CHECK'
      DESTINATION rfcdest
      EXPORTING
        object              = 'S_RFC'
        field1              = 'ACTVT'
        value1              = '16'
        field2              = 'RFC_NAME'
        value2              = authority_object
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      zcx_adu_check_transport=>raise_system( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_run_code.

    CLEAR: run_code.
    fill_empty_run_code( ).

  ENDMETHOD.


  METHOD fill_empty_run_code.

    IF run_code IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        run_code = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO DATA(uuid_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = uuid_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD get_system_info.

    DATA:
      rfc_user     TYPE rfcuser,
      rfc_password TYPE rfcauth,
      rfcsi_export TYPE rfcsi.

    IF rfcdest CS 'None'.
      system_name = sy-sysid.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RFC_READ_R3_DESTINATION'
      EXPORTING
        destination             = rfcdest
      IMPORTING
        user                    = rfc_user
        password                = rfc_password
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0. "RFC does not exist
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>rfc_error
          text1  = CONV #( rfcdest ).
    ENDIF.

    IF sy-batch = abap_false.
      CALL FUNCTION '/SDF/OCS_GET_INFO'
        DESTINATION rfcdest
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            textid = zcx_adu_check_transport=>rfc_error
            text1  = CONV #( rfcdest ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'RFC_SYSTEM_INFO'
      DESTINATION rfcdest
      IMPORTING
        rfcsi_export = rfcsi_export.

    system_name = rfcsi_export-rfcsysid.

  ENDMETHOD.


ENDCLASS.
