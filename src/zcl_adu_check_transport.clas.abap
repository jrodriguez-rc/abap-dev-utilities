"! <p class="shorttext synchronized" lang="en">Check transport</p>
CLASS zcl_adu_check_transport DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_adu_check_transport,
      if_serializable_object.

    CLASS-METHODS create
      IMPORTING
        transport_requests TYPE trkorrs
        rfc_source         TYPE rfcdest DEFAULT 'NONE'
        rfc_target         TYPE rfcdest
      RETURNING
        VALUE(result)      TYPE REF TO zif_adu_check_transport
      RAISING
        zcx_adu_check_transport.

    METHODS constructor
      IMPORTING
        transport_requests TYPE trkorrs
        rfc_source         TYPE rfcdest DEFAULT 'NONE'
        rfc_target         TYPE rfcdest
      RAISING
        zcx_adu_check_transport.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_run_data,
        run_code                TYPE zadu_run_code,
        rfc_source              TYPE rfcdest,
        rfc_destination         TYPE rfcdest,
        source_system_name      TYPE sysname,
        destination_system_name TYPE sysname,
        checked_cross_reference TYPE abap_bool,
        checked_sequence        TYPE abap_bool,
        checked_cross_release   TYPE abap_bool,
        checked_import_time     TYPE abap_bool,
        checked_online_import   TYPE abap_bool,
      END OF ts_run_data.

    DATA:
      run_data                TYPE ts_run_data,
      transport_requests      TYPE trkorrs,
      results_cross_reference TYPE zif_adu_check_transport~tt_result_cross_reference,
      results_sequence        TYPE zif_adu_check_transport~tt_result_sequence,
      results_cross_release   TYPE zif_adu_check_transport~tt_result_cross_release,
      results_import_time     TYPE zif_adu_check_transport~tt_result_import_time,
      results_online_import   TYPE zif_adu_check_transport~ts_result_online_import.

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
        rfcdest       TYPE rfcdest
      RETURNING
        VALUE(result) TYPE sysname
      RAISING
        zcx_adu_check_transport.

ENDCLASS.



CLASS zcl_adu_check_transport IMPLEMENTATION.


  METHOD create.

    result =
        NEW zcl_adu_check_transport(
            transport_requests = transport_requests
            rfc_source         = rfc_source
            rfc_target         = rfc_target ).

  ENDMETHOD.


  METHOD constructor.

    me->transport_requests = transport_requests.

    run_data-rfc_source      = rfc_source.
    run_data-rfc_destination = rfc_target.

    run_data-source_system_name      = get_system_info( rfc_source ).
    run_data-destination_system_name = get_system_info( rfc_target ).

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_all.

    fill_empty_run_code( ).

    result-run_code = run_data-run_code.

    result-results_cross_reference = zif_adu_check_transport~check_cross_reference( ).
    result-results_sequence        = zif_adu_check_transport~check_sequence( ).
    result-results_cross_release   = zif_adu_check_transport~check_cross_release( ).
    result-results_import_time     = zif_adu_check_transport~check_import_time( ).
    result-results_online_import   = zif_adu_check_transport~check_online_import( ).

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

    run_data-checked_cross_reference = abap_true.

    check_authorization( rfcdest = run_data-rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = run_data-rfc_destination authority_object = lc_authorization-destionation ).

    requests =
        VALUE #(
            FOR transport_request IN transport_requests
                ( trkorr = transport_request ) ).

    CALL FUNCTION '/SDF/TEAP_ENVI_ANA'
      EXPORTING
        iv_ana_rfc                 = run_data-rfc_source
        iv_tar_rfc                 = run_data-rfc_destination
      TABLES
        it_reqs                    = requests
        et_envanal_res_err         = results_cross_reference
      EXCEPTIONS
        empty_checked_tr_list      = 1
        destination_not_reached    = 2
        get_checked_obj_failed     = 3
        no_objs_specified_for_anal = 4
        no_referred_objects_found  = 5
        system_failure             = 6
        communication_failure      = 7
        resource_failure           = 8
        OTHERS                     = 9.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_ENVI_ANA'.
    ENDIF.

    results_cross_reference = results_cross_reference.
    result = results_cross_reference.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_sequence.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/TEAP_GET_CSOL_FOR_TR' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/TEAP_TMS_GET_HISTORY' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      alog      TYPE tmstpalogs,
      conflicts LIKE result.

    fill_empty_run_code( ).

    run_data-checked_sequence = abap_true.

    check_authorization( rfcdest = run_data-rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = run_data-rfc_destination authority_object = lc_authorization-destionation ).

    alog =
        VALUE #(
            FOR transport_request IN transport_requests
                ( listname = '/SDF/CMO_TR_CHECK'
                  trkorr   = transport_request
                  trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    SELECT SINGLE param_value_i
      FROM /sdf/cmo_tr_conf
      INTO @DATA(period)
      WHERE config_param = 'PERIOD'.
    IF sy-subrc <> 0.
      period = 0.
    ENDIF.

    DATA(start_date) =
        COND d(
            WHEN period IS INITIAL
                THEN sy-datum - 90
                ELSE sy-datum - period ).

    CALL FUNCTION '/SDF/TEAP_DOWNGRADE_PROTECT'
      EXPORTING
        iv_dev_sid                 = run_data-source_system_name
        iv_tar_sid                 = run_data-destination_system_name
        iv_dev_rfc                 = run_data-rfc_source
        iv_tar_rfc                 = run_data-rfc_destination
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
        system_failure             = 6
        communication_failure      = 7
        resource_failure           = 8
        OTHERS                     = 9.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_DOWNGRADE_PROTECT'.
    ENDIF.

    results_sequence = conflicts.
    result = conflicts.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_cross_release.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/TEAP_TR_CHECK_TYPE' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/OCS_GET_INFO' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      alog          TYPE tmstpalogs,
      cross_release LIKE result.

    fill_empty_run_code( ).

    run_data-checked_cross_release = abap_true.

    check_authorization( rfcdest = run_data-rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = run_data-rfc_destination authority_object = lc_authorization-destionation ).

    alog =
        VALUE #(
            FOR transport_request IN transport_requests
                ( listname = '/SDF/CMO_TR_CHECK'
                  trkorr   = transport_request
                  trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    CALL FUNCTION '/SDF/TEAP_SCV_CHECK'
      EXPORTING
        iv_dev_sid            = run_data-source_system_name
        iv_tar_sid            = run_data-destination_system_name
        iv_dev_rfc            = run_data-rfc_source
        iv_tar_rfc            = run_data-rfc_destination
        iv_start_date         = sy-datum
        iv_end_date           = sy-datum
      TABLES
        it_checked_tr         = alog
        et_scv_crit_obj       = cross_release
      EXCEPTIONS
        empty_checked_tr_list = 1
        get_dev_scv_error     = 2
        get_tar_scv_error     = 3
        system_failure        = 4
        communication_failure = 5
        resource_failure      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_SCV_CHECK'.
    ENDIF.

    results_cross_release = cross_release.
    result = cross_release.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_import_time.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source TYPE xuval VALUE '/SDF/TEAP_IMPORT_TIME' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      alog        TYPE tmstpalogs,
      import_time LIKE result.

    fill_empty_run_code( ).

    run_data-checked_import_time = abap_true.

    check_authorization( rfcdest = run_data-rfc_source authority_object = lc_authorization-source ).

    alog =
        VALUE #(
            FOR transport_request IN transport_requests
                ( listname = '/SDF/CMO_TR_CHECK'
                  trkorr   = transport_request
                  trtime   = CONV #( |{ sy-datum }{ sy-uzeit }| ) ) ).

    CALL FUNCTION '/SDF/TEAP_IMPORT_TIME'
      DESTINATION run_data-source_system_name
      TABLES
        it_checked_tr            = alog
        et_tr_imp_time           = import_time
      EXCEPTIONS
        empty_checked_tr_list    = 1
        no_tpalog_for_checked_tr = 2
        system_failure           = 3
        communication_failure    = 4
        resource_failure         = 5
        OTHERS                   = 6.
    IF sy-subrc > 2.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/TEAP_IMPORT_TIME'.
    ENDIF.

    results_import_time = import_time.
    result = import_time.

  ENDMETHOD.


  METHOD zif_adu_check_transport~check_online_import.

    CONSTANTS:
      BEGIN OF lc_authorization,
        source       TYPE xuval VALUE '/SDF/DD_DDIC_DEP_GET' ##NO_TEXT,
        destionation TYPE xuval VALUE '/SDF/READ_D010TAB' ##NO_TEXT,
      END OF lc_authorization.

    DATA:
      requests      TYPE STANDARD TABLE OF e070 WITH DEFAULT KEY,
      summary       LIKE result-summary,
      online_import LIKE result-all.

    fill_empty_run_code( ).

    run_data-checked_online_import = abap_true.

    check_authorization( rfcdest = run_data-rfc_source      authority_object = lc_authorization-source ).
    check_authorization( rfcdest = run_data-rfc_destination authority_object = lc_authorization-destionation ).

    requests =
        VALUE #(
            FOR transport_request IN transport_requests
                ( trkorr = transport_request ) ).

    CALL FUNCTION '/SDF/OI_CHECK'
      EXPORTING
        iv_ana_rfc                  = run_data-rfc_source
        iv_tar_rfc                  = run_data-rfc_destination
      TABLES
        it_reqs                     = requests
        et_result                   = online_import
        et_results                  = summary
      EXCEPTIONS
        destination_not_reached     = 1
        no_objs_specified_for_anal  = 2
        prerequisites_not_fulfilled = 3
        other                       = 4
        system_failure              = 5
        communication_failure       = 6
        resource_failure            = 7
        OTHERS                      = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>error_in
          text1  = '/SDF/OI_CHECK'.
    ENDIF.

    results_online_import-summary = summary.
    results_online_import-all     = online_import.

    result-summary = summary.
    result-all     = online_import.

  ENDMETHOD.


  METHOD zif_adu_check_transport~save_results.

    DATA: sequence TYPE i.

    IF run_data-run_code IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>no_checks_executed.
    ENDIF.

    GET TIME STAMP FIELD DATA(current_timestamp).

    DATA(cross_reference_updates) = CORRESPONDING zadu_t_chktr_crref_update( results_cross_reference ).
    DATA(sequence_updates) = CORRESPONDING zadu_t_chktr_seq_update( results_sequence ).
    DATA(cross_release_updates) = CORRESPONDING zadu_t_chktr_crrel_update( results_cross_release ).
    DATA(import_time_updates) = CORRESPONDING zadu_t_chktr_imtim_update( results_import_time ).
    DATA(online_import_summary_updates) = CORRESPONDING zadu_t_chktr_oisum_update( results_online_import-summary ).
    DATA(online_import_updates) = CORRESPONDING zadu_t_chktr_onlim_update( results_online_import-all ).

    LOOP AT transport_requests REFERENCE INTO DATA(transport_request).

      DATA(header) = VALUE zadu_s_chktr_head_update(
                                 client             = sy-mandt
                                 run_code           = run_data-run_code
                                 transport_request  = transport_request->*
                                 timestamp          = current_timestamp
                                 username           = sy-uname
                                 source             = run_data-rfc_source
                                 source_system      = run_data-source_system_name
                                 destination        = run_data-rfc_destination
                                 destination_system = run_data-destination_system_name
                                 cross_reference    = run_data-checked_cross_reference
                                 sequence           = run_data-checked_sequence
                                 cross_release      = run_data-checked_cross_release
                                 import_time        = run_data-checked_import_time
                                 online_import      = run_data-checked_online_import
                                 crud_ind           = zif_adu_constants=>crud-create ).

      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_HEADER'
        IN UPDATE TASK
        EXPORTING
          header = header.

      LOOP AT cross_reference_updates REFERENCE INTO DATA(cross_reference_update)
          WHERE chk_trkorr = transport_request->*.
        sequence = sy-tabix.
        cross_reference_update->client            = sy-mandt.
        cross_reference_update->run_code          = run_data-run_code.
        cross_reference_update->transport_request = transport_request->*.
        cross_reference_update->sequence          = sequence.
        cross_reference_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

      LOOP AT sequence_updates REFERENCE INTO DATA(sequence_update)
          WHERE checked_trkorr = transport_request->*.
        sequence = sy-tabix.
        sequence_update->client            = sy-mandt.
        sequence_update->run_code          = run_data-run_code.
        sequence_update->transport_request = transport_request->*.
        sequence_update->sequence          = sequence.
        sequence_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

      LOOP AT cross_release_updates REFERENCE INTO DATA(cross_release_update)
          WHERE trkorr = transport_request->*.
        sequence = sy-tabix.
        cross_release_update->client            = sy-mandt.
        cross_release_update->run_code          = run_data-run_code.
        cross_release_update->transport_request = transport_request->*.
        cross_release_update->sequence          = sequence.
        cross_release_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

      LOOP AT import_time_updates REFERENCE INTO DATA(import_time_update)
          WHERE trkorr = transport_request->*.
        sequence = sy-tabix.
        import_time_update->client            = sy-mandt.
        import_time_update->run_code          = run_data-run_code.
        import_time_update->transport_request = transport_request->*.
        import_time_update->sequence          = sequence.
        import_time_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

      LOOP AT online_import_summary_updates REFERENCE INTO DATA(online_import_summary_update)
          WHERE trkorr = transport_request->*.
        sequence = sy-tabix.
        online_import_summary_update->client            = sy-mandt.
        online_import_summary_update->run_code          = run_data-run_code.
        online_import_summary_update->transport_request = transport_request->*.
        online_import_summary_update->sequence          = sequence.
        online_import_summary_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

      LOOP AT online_import_updates REFERENCE INTO DATA(online_import_update)
          WHERE trkorr = transport_request->*.
        sequence = sy-tabix.
        online_import_update->client            = sy-mandt.
        online_import_update->run_code          = run_data-run_code.
        online_import_update->transport_request = transport_request->*.
        online_import_update->sequence          = sequence.
        online_import_update->crud_ind          = zif_adu_constants=>crud-create.
      ENDLOOP.

    ENDLOOP.

    IF cross_reference_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_CROSSREF'
        IN UPDATE TASK
        EXPORTING
          cross_reference_updates = cross_reference_updates.
    ENDIF.

    IF sequence_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_SEQUENCE'
        IN UPDATE TASK
        EXPORTING
          sequence_updates = sequence_updates.
    ENDIF.

    IF cross_release_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_CROSSREL'
        IN UPDATE TASK
        EXPORTING
          cross_release_updates = cross_release_updates.
    ENDIF.

    IF import_time_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_IMP_TIME'
        IN UPDATE TASK
        EXPORTING
          import_time_updates = import_time_updates.
    ENDIF.

    IF online_import_summary_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_OI_SUM'
        IN UPDATE TASK
        EXPORTING
          online_import_summary_updates = online_import_summary_updates.
    ENDIF.

    IF online_import_updates IS NOT INITIAL.
      CALL FUNCTION 'ZADU_TRANSPORT_UPDATE_ONL_IMP'
        IN UPDATE TASK
        EXPORTING
          online_import_updates = online_import_updates.
    ENDIF.

    IF commit = abap_true.
      COMMIT WORK.
    ENDIF.

    after_save( ).

  ENDMETHOD.


  METHOD after_save.

    CLEAR: run_data, results_cross_reference, results_sequence, results_cross_release,
         results_import_time, results_online_import.

  ENDMETHOD.


  METHOD check_authorization.

    CALL FUNCTION 'AUTHORITY_CHECK'
      DESTINATION rfcdest
      EXPORTING
        object                = 'S_RFC'
        field1                = 'ACTVT'
        value1                = '16'
        field2                = 'RFC_NAME'
        value2                = authority_object
      EXCEPTIONS
        user_dont_exist       = 1
        user_is_authorized    = 2
        user_not_authorized   = 3
        user_is_locked        = 4
        system_failure        = 5
        communication_failure = 6
        resource_failure      = 7
        OTHERS                = 8.
    IF sy-subrc <> 2.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>authority_check_failed
          text1  = CONV #( authority_object )
          text2  = CONV #( rfcdest ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_run_code.

    CLEAR: run_data-run_code.
    fill_empty_run_code( ).

  ENDMETHOD.


  METHOD fill_empty_run_code.

    IF run_data-run_code IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        run_data-run_code = cl_system_uuid=>create_uuid_c32_static( ).
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

    IF to_upper( rfcdest ) CS 'NONE'.
      result = sy-sysid.
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
        system_failure          = 5
        communication_failure   = 6
        resource_failure        = 7
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
          system_failure        = 1
          communication_failure = 2
          resource_failure      = 3
          OTHERS                = 4.
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
        rfcsi_export          = rfcsi_export
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>rfc_error
          text1  = CONV #( rfcdest ).
    ENDIF.

    result = rfcsi_export-rfcsysid.

  ENDMETHOD.


ENDCLASS.
