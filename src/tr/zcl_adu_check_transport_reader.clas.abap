"! <p class="shorttext synchronized" lang="en">Check transport reader</p>
CLASS zcl_adu_check_transport_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_adu_check_transport_reader,
      if_serializable_object.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter transport_request | <p class="shorttext synchronized" lang="en">Transport request</p>
    "! @parameter run_code | <p class="shorttext synchronized" lang="en">Run code</p>
    "! @raising zcx_adu_check_transport | <p class="shorttext synchronized" lang="en">Check transport exception</p>
    METHODS constructor
      IMPORTING
        transport_request TYPE trkorr
        run_code          TYPE zadu_run_code OPTIONAL
      RAISING
        zcx_adu_check_transport.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_logs,
        run_code              TYPE zadu_run_code,
        transport_request     TYPE trkorr,
        header                TYPE zif_adu_check_transport_reader=>ts_header,
        cross_reference       TYPE zif_adu_check_transport_reader=>tt_cross_reference,
        sequence              TYPE zif_adu_check_transport_reader=>tt_sequence,
        cross_release         TYPE zif_adu_check_transport_reader=>tt_cross_release,
        import_time           TYPE zif_adu_check_transport_reader=>tt_import_time,
        online_import_summary TYPE zif_adu_check_transport_reader=>tt_online_import_summary,
        online_import         TYPE zif_adu_check_transport_reader=>tt_online_import,
      END OF ts_logs,
      tt_logs TYPE HASHED TABLE OF ts_logs
          WITH UNIQUE KEY run_code transport_request
          WITH NON-UNIQUE SORTED KEY run COMPONENTS run_code
          WITH NON-UNIQUE SORTED KEY req COMPONENTS transport_request.

    TYPES:
      BEGIN OF ts_online_import_config,
        tabreads_y TYPE i,
        tabreads_r TYPE i,
        tabwrite_y TYPE i,
        tabwrite_r TYPE i,
        tabsize_y  TYPE i,
        tabsize_r  TYPE i,
        repexe_y   TYPE i,
        repexe_r   TYPE i,
        repexedd_y TYPE i,
        repexedd_r TYPE i,
      END OF ts_online_import_config.

    TYPES:
      tt_cross_reference_db       TYPE STANDARD TABLE OF zadu_chktr_crref WITH DEFAULT KEY,
      tt_sequence_db              TYPE STANDARD TABLE OF zadu_chktr_seq   WITH DEFAULT KEY,
      tt_cross_release_db         TYPE STANDARD TABLE OF zadu_chktr_crrel WITH DEFAULT KEY,
      tt_import_time_db           TYPE STANDARD TABLE OF zadu_chktr_imtim WITH DEFAULT KEY,
      tt_online_import_summary_db TYPE STANDARD TABLE OF zadu_chktr_oisum WITH DEFAULT KEY,
      tt_online_import_db         TYPE STANDARD TABLE OF zadu_chktr_onlim WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF alv_column,
        BEGIN OF object_name,
          length TYPE lvc_outlen VALUE 40,
        END OF object_name,
      END OF alv_column.

    DATA:
      report_configuration TYPE HASHED TABLE OF /sdf/cmo_tr_conf WITH UNIQUE KEY config_param,
      online_import_config TYPE ts_online_import_config,
      transport_request    TYPE trkorr,
      logs                 TYPE tt_logs,
      salv_data            TYPE REF TO lcl_salv_data.

    METHODS add_header_log
      IMPORTING
        header_log TYPE zadu_chktr_head.

    METHODS fill_header
      IMPORTING
        data                  TYPE zadu_chktr_head
        cross_reference       TYPE tt_cross_reference_db OPTIONAL
        sequence              TYPE tt_sequence_db OPTIONAL
        cross_release         TYPE tt_cross_release_db OPTIONAL
        import_time           TYPE tt_import_time_db OPTIONAL
        online_import_summary TYPE tt_online_import_summary_db OPTIONAL
        online_import         TYPE tt_online_import_db OPTIONAL
      RETURNING
        VALUE(result)         TYPE zif_adu_check_transport_reader=>ts_header.

    METHODS fill_cross_reference
      IMPORTING
        data          TYPE zadu_chktr_crref
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_cross_reference.

    METHODS fill_sequence
      IMPORTING
        data          TYPE zadu_chktr_seq
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_sequence.

    METHODS fill_cross_release
      IMPORTING
        data          TYPE zadu_chktr_crrel
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_cross_release.

    METHODS fill_import_time
      IMPORTING
        data          TYPE zadu_chktr_imtim
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_import_time.

    METHODS fill_online_import_summary
      IMPORTING
        data          TYPE zadu_chktr_oisum
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_online_import_summary.

    METHODS fill_online_import
      IMPORTING
        data          TYPE zadu_chktr_onlim
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ts_online_import.

    METHODS filter_logs
      IMPORTING
        run_code          TYPE zadu_run_code
        transport_request TYPE trkorr OPTIONAL
      RETURNING
        VALUE(result)     TYPE tt_logs.

    METHODS severity_for_cross_reference
      IMPORTING
        data          TYPE tt_cross_reference_db
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_sequence
      IMPORTING
        data          TYPE tt_sequence_db
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_cross_release
      IMPORTING
        data          TYPE tt_cross_release_db
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_import_time
      IMPORTING
        data          TYPE tt_import_time_db
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_online_import
      IMPORTING
        data          TYPE tt_online_import_db
      RETURNING
        VALUE(result) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS prepare_alv_log
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_log_columns
      IMPORTING
        salv_columns TYPE REF TO cl_salv_columns_table.

    METHODS prepare_alv_cross_reference
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_sequence
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_cross_release
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_import_time
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_online_import_sum
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS prepare_alv_online_import
      RETURNING
        VALUE(result) TYPE REF TO cl_salv_table
      RAISING
        zcx_adu_check_transport.

    METHODS set_online_import_config.

ENDCLASS.



CLASS zcl_adu_check_transport_reader IMPLEMENTATION.


  METHOD constructor.

    DATA:
      run_code_range          TYPE RANGE OF zadu_run_code,
      transport_request_range TYPE RANGE OF trkorr.

    me->transport_request = transport_request.

    run_code_range = COND #( WHEN run_code IS NOT INITIAL
                                 THEN VALUE #( ( sign = 'I' option = 'EQ' low = run_code ) ) ).

    transport_request_range = COND #( WHEN transport_request IS NOT INITIAL
                                          THEN VALUE #( ( sign = 'I' option = 'EQ' low = transport_request ) ) ).

    SELECT *
      INTO TABLE @DATA(header_logs)
      FROM zadu_chktr_head
      WHERE run_code          IN @run_code_range
        AND transport_request IN @transport_request_range.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_check_transport
        EXPORTING
          textid = zcx_adu_check_transport=>no_logs_found.
    ENDIF.

    SELECT *
      INTO TABLE @report_configuration
      FROM /sdf/cmo_tr_conf.
    IF sy-subrc <> 0.
      CLEAR: report_configuration.
    ENDIF.

    set_online_import_config( ).

    salv_data = NEW #( ).

    LOOP AT header_logs REFERENCE INTO DATA(header_log).
      add_header_log( header_log->* ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display.

    salv_data->header = zif_adu_check_transport_reader~get_header( run_code          = run_code
                                                                   transport_request = transport_request ).

    DATA(salv_table) = prepare_alv_log( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_cross_reference.

    salv_data->cross_reference = zif_adu_check_transport_reader~get_cross_reference(
                                                                            run_code          = run_code
                                                                            transport_request = transport_request ).

    DATA(salv_table) = prepare_alv_cross_reference( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_sequence.

    CLEAR: salv_data->sequence.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    salv_data->sequence =
        REDUCE #(
            INIT logs_sequence TYPE zif_adu_check_transport_reader=>tt_sequence
            FOR filtered_log IN filtered_logs
            NEXT logs_sequence =
                VALUE #(
                    BASE logs_sequence ( LINES OF filtered_log-sequence ) ) ).

    DATA(salv_table) = prepare_alv_sequence( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_cross_release.

    CLEAR: salv_data->cross_release.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    salv_data->cross_release =
        REDUCE #(
            INIT logs_sequence TYPE zif_adu_check_transport_reader=>tt_cross_release
            FOR filtered_log IN filtered_logs
            NEXT logs_sequence =
                VALUE #(
                    BASE logs_sequence ( LINES OF filtered_log-cross_release ) ) ).

    DATA(salv_table) = prepare_alv_cross_release( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_import_time.

    CLEAR: salv_data->import_time.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    salv_data->import_time =
        REDUCE #(
            INIT logs_sequence TYPE zif_adu_check_transport_reader=>tt_import_time
            FOR filtered_log IN filtered_logs
            NEXT logs_sequence =
                VALUE #(
                    BASE logs_sequence ( LINES OF filtered_log-import_time ) ) ).

    DATA(salv_table) = prepare_alv_import_time( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_online_import_summary.

    CLEAR: salv_data->online_import_summary.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    salv_data->online_import_summary =
        REDUCE #(
            INIT logs_sequence TYPE zif_adu_check_transport_reader=>tt_online_import_summary
            FOR filtered_log IN filtered_logs
            NEXT logs_sequence =
                VALUE #(
                    BASE logs_sequence ( LINES OF filtered_log-online_import_summary ) ) ).

    DATA(salv_table) = prepare_alv_online_import_sum( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_online_import.

    CLEAR: salv_data->online_import.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    salv_data->online_import =
        REDUCE #(
            INIT logs_sequence TYPE zif_adu_check_transport_reader=>tt_online_import
            FOR filtered_log IN filtered_logs
            NEXT logs_sequence =
                VALUE #(
                    BASE logs_sequence ( LINES OF filtered_log-online_import ) ) ).

    DATA(salv_table) = prepare_alv_online_import( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD add_header_log.

    INSERT VALUE #( run_code          = header_log-run_code
                    transport_request = header_log-transport_request
                  ) INTO TABLE logs REFERENCE INTO DATA(log).

    SELECT *
      INTO TABLE @DATA(cross_references)
      FROM zadu_chktr_crref
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->cross_reference =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR cross_reference IN cross_references
                    ( fill_cross_reference( cross_reference ) ) ) ).

    SELECT *
      INTO TABLE @DATA(sequences)
      FROM zadu_chktr_seq
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->sequence =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR sequence IN sequences
                    ( fill_sequence( sequence ) ) ) ).

    SELECT *
      INTO TABLE @DATA(cross_releases)
      FROM zadu_chktr_crrel
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->cross_release =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR cross_release IN cross_releases
                    ( fill_cross_release( cross_release ) ) ) ).

    SELECT *
      INTO TABLE @DATA(import_times)
      FROM zadu_chktr_imtim
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->import_time =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR import_time IN import_times
                    ( fill_import_time( import_time ) ) ) ).

    SELECT *
      INTO TABLE @DATA(online_imports)
      FROM zadu_chktr_onlim
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->online_import =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR online_import IN online_imports
                    ( fill_online_import( online_import ) ) ) ).

    SELECT *
      INTO TABLE @DATA(online_import_summaries)
      FROM zadu_chktr_oisum
      WHERE run_code          = @header_log-run_code
        AND transport_request = @header_log-transport_request.
    log->online_import_summary =
        COND #(
            WHEN sy-subrc = 0
                THEN VALUE #(
                    FOR online_import_summary IN online_import_summaries
                    ( fill_online_import_summary( online_import_summary ) ) ) ).

    log->header = fill_header( data                  = header_log
                               cross_reference       = CONV #( cross_references )
                               sequence              = CONV #( sequences )
                               cross_release         = CONV #( cross_releases )
                               import_time           = CONV #( import_times )
                               online_import_summary = CONV #( online_import_summaries )
                               online_import         = CONV #( online_imports ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_header.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        VALUE #(
            FOR log IN filtered_logs
            ( log-header ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_cross_reference.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-cross_reference ) ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_sequence.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-sequence ) ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_cross_release.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-cross_release ) ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_import_time.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-import_time ) ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_online_import.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-online_import ) ) ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~get_online_import_summary.

    DATA(filtered_logs) = filter_logs( run_code          = run_code
                                       transport_request = transport_request ).

    result =
        REDUCE #(
            INIT result_logs = result
            FOR log IN filtered_logs
            NEXT result_logs = VALUE #( BASE result_logs ( LINES OF log-online_import_summary ) ) ).

  ENDMETHOD.


  METHOD fill_header.

    DATA:
      timezone TYPE timezone.

    result = CORRESPONDING #( data ).

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = timezone " Time Zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      timezone = 'UTC'.
    ENDIF.

    CONVERT TIME STAMP data-timestamp TIME ZONE timezone INTO DATE result-date TIME result-time.

    result-cross_reference_messages = lines( cross_reference ).
    result-sequence_messages        = lines( sequence ).
    result-cross_release_messages   = lines( cross_release ).
    result-import_time_messages     = lines( import_time ).
    result-online_import_messages   = lines( online_import_summary ).

    DATA(severity) = severity_for_cross_reference( cross_reference ).

    IF severity IS INITIAL.
      severity = severity_for_sequence( sequence ).
    ENDIF.

    IF severity IS INITIAL.
      severity = severity_for_cross_release( cross_release ).
    ENDIF.

    IF severity IS INITIAL.
      severity = severity_for_import_time( import_time ).
    ENDIF.

    IF severity IS INITIAL.
      severity = severity_for_online_import( online_import ).
    ENDIF.

    CASE severity.
      WHEN zif_adu_constants=>severity-error.
        result-exception = '1'.
        result-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        result-exception = '2'.
        result-color     = VALUE #( ( color-col = col_key ) ).

      WHEN zif_adu_constants=>severity-info.
        result-exception = '3'.
        result-color     = VALUE #( ( color-col = col_normal ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD fill_cross_reference.

    result = CORRESPONDING #( data ).

    result-status_description = cl_proxy_utils=>get_domain_text_for_value( result-status ).

    CASE severity_for_cross_reference( VALUE #( ( data ) ) ).
      WHEN zif_adu_constants=>severity-error.
        result-exception = '1'.
        result-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        result-exception = '2'.
        result-color     = VALUE #( ( color-col = col_key ) ).

      WHEN OTHERS.
        result-exception = '3'.
        result-color     = VALUE #( ( color-col = col_normal ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD fill_sequence.

    result = CORRESPONDING #( data ).

    CASE severity_for_sequence( VALUE #( ( data ) ) ).
      WHEN zif_adu_constants=>severity-error.
        result-exception = '1'.
        result-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        result-exception = '2'.
        result-color     = VALUE #( ( color-col = col_key ) ).

      WHEN OTHERS.
        result-exception = '3'.
        result-color     = VALUE #( ( color-col = col_normal ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD fill_cross_release.

    result = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD fill_import_time.

    result = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD fill_online_import_summary.

    result = CORRESPONDING #( data ).

    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = icon_doc_item_detail
        info                  = 'Display details'
      IMPORTING
        result                = result-details_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      CLEAR: result-details_icon.
    ENDIF.

    CASE severity_for_online_import(
                            CORRESPONDING #( logs[ run_code          = data-run_code
                                                   transport_request = data-transport_request ]-online_import ) ).
      WHEN zif_adu_constants=>severity-error.
        result-exception = '1'.
        result-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        result-exception = '2'.
        result-color     = VALUE #( ( color-col = col_key ) ).

      WHEN OTHERS.
        result-exception = '3'.
        result-color     = VALUE #( ( color-col = col_positive ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD fill_online_import.

    result = CORRESPONDING #( data ).

    CASE severity_for_online_import( VALUE #( ( data ) ) ).
      WHEN zif_adu_constants=>severity-error.
        result-exception = '1'.
        result-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        result-exception = '2'.
        result-color     = VALUE #( ( color-col = col_key ) ).

      WHEN OTHERS.
        result-exception = '3'.
        result-color     = VALUE #( ( color-col = col_positive ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD filter_logs.

    result =
        COND #( WHEN transport_request IS NOT INITIAL
                     THEN FILTER #( logs USING KEY req WHERE transport_request = transport_request )
                     ELSE logs ).

    result =
        COND #( WHEN run_code IS NOT INITIAL
                     THEN FILTER #( logs USING KEY run WHERE run_code = run_code )
                     ELSE result ).

  ENDMETHOD.


  METHOD severity_for_cross_reference.

    IF data IS INITIAL.
      RETURN.
    ENDIF.

    result =
        COND #(
            WHEN line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-only_in_source ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-only_in_target ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-inconsistent_source ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-locked_target ] )
                THEN zif_adu_constants=>severity-error
                ELSE zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_sequence.

    result = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_cross_release.

    result = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_import_time.

    result = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-info ).

  ENDMETHOD.


  METHOD severity_for_online_import.

    result =
        REDUCE #(
            INIT severity = result
            FOR data_result IN data
            NEXT severity =
                COND #(
                    WHEN data_result-chcnt > online_import_config-tabwrite_r
                      OR data_result-accnt > online_import_config-tabreads_r
                      OR ( data_result-action IS NOT INITIAL AND
                           data_result-occtb > online_import_config-tabsize_r )
                      OR ( data_result-action IS NOT INITIAL AND
                           data_result-occtb > online_import_config-tabsize_y AND
                           ( data_result-chcnt > online_import_config-tabwrite_y OR
                             data_result-accnt > online_import_config-tabreads_y ) )
                      OR data_result-execnt > online_import_config-repexe_r
                      OR data_result-execnt_dd > online_import_config-repexedd_r
                      OR data_result-criobj = abap_true
                        THEN zif_adu_constants=>severity-error
                    WHEN severity <> zif_adu_constants=>severity-error
                     AND ( data_result-chcnt > online_import_config-tabwrite_y
                          OR data_result-accnt > online_import_config-tabreads_y
                          OR data_result-execnt > online_import_config-repexe_y
                          OR data_result-execnt_dd > online_import_config-repexedd_y OR
                             ( data_result-action IS NOT INITIAL AND
                               data_result-occtb > online_import_config-tabsize_y ) )
                        THEN zif_adu_constants=>severity-warning
                    WHEN severity IS INITIAL
                        THEN zif_adu_constants=>severity-info
                        ELSE severity ) ).

  ENDMETHOD.


  METHOD prepare_alv_log.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->header ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header(
                COND #( LET text = |{ 'Transport Check Logs'(001) }|
                        IN WHEN transport_request IS INITIAL
                               THEN text
                               ELSE |{ text }: { transport_request }| ) ).

    result->get_functions( )->set_all( ).

    DATA(salv_sorts) = result->get_sorts( ).

    TRY.
        salv_sorts->add_sort( columnname = 'DATE' position = 1 sequence = if_salv_c_sort=>sort_down ).
        salv_sorts->add_sort( columnname = 'TIME' position = 2 sequence = if_salv_c_sort=>sort_down ).
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
    ENDTRY.

    prepare_alv_log_columns( result->get_columns( ) ).

    DATA(salv_events) = result->get_event( ).

    DATA(event_handler) = NEW lcl_event_handler( reader     = me
                                                 salv_table = result ).

    SET HANDLER event_handler->header_link_click FOR salv_events ACTIVATION abap_true.

  ENDMETHOD.


  METHOD prepare_alv_log_columns.

    salv_columns->set_column_position( columnname = 'DATE' position = 3 ).
    salv_columns->set_column_position( columnname = 'TIME' position = 4 ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT' OR 'TIMESTAMP'.
          column-r_column->set_technical( ).

        WHEN 'TRANSPORT_REQUEST'.
          column-r_column->set_visible( abap_false ).

        WHEN 'CROSS_REFERENCE'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'XRefExec'(023) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Ref. Executed'(024) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Reference Executed'(025) ) ).

        WHEN 'SEQUENCE'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'SeqExec'(026) ) ).
          column-r_column->set_medium_text( CONV #( 'Sequence Executed'(027) ) ).
          column-r_column->set_long_text( CONV #( 'Sequence Executed'(027) ) ).

        WHEN 'CROSS_RELEASE'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'XRelExec'(028) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Rel. Executed'(029) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Release Executed'(030) ) ).

        WHEN 'IMPORT_TIME'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'ImpTimeExec'(031) ) ).
          column-r_column->set_medium_text( CONV #( 'Import Time Executed'(032) ) ).
          column-r_column->set_long_text( CONV #( 'Import Time Executed'(032) ) ).

        WHEN 'ONLINE_IMPORT'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'OnlImpExec'(033) ) ).
          column-r_column->set_medium_text( CONV #( 'Online Imp. Executed'(034) ) ).
          column-r_column->set_long_text( CONV #( 'Online Import Executed'(035) ) ).

        WHEN 'CROSS_REFERENCE_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_optimized( ).
          column-r_column->set_short_text( CONV #( 'XRefMsg'(010) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Ref. Messages'(011) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Reference Messages'(012) ) ).

        WHEN 'SEQUENCE_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_optimized( ).
          column-r_column->set_short_text( CONV #( 'SeqMsg'(013) ) ).
          column-r_column->set_medium_text( CONV #( 'Sequence Messages'(014) ) ).
          column-r_column->set_long_text( CONV #( 'Sequence Messages'(014) ) ).

        WHEN 'CROSS_RELEASE_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_optimized( ).
          column-r_column->set_short_text( CONV #( 'XRelMsg'(015) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Rel. Messages'(016) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Release Messages'(017) ) ).

        WHEN 'IMPORT_TIME_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_optimized( ).
          column-r_column->set_short_text( CONV #( 'ImpTimeMsg'(018) ) ).
          column-r_column->set_medium_text( CONV #( 'Import Time Messages'(019) ) ).
          column-r_column->set_long_text( CONV #( 'Import Time Messages'(019) ) ).

        WHEN 'ONLINE_IMPORT_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_optimized( ).
          column-r_column->set_short_text( CONV #( 'OnlImpMsg'(020) ) ).
          column-r_column->set_medium_text( CONV #( 'Online Imp. Messages'(021) ) ).
          column-r_column->set_long_text( CONV #( 'Online Import Messages'(022) ) ).

        WHEN 'SOURCE' OR 'DESTINATION'.
          column-r_column->set_output_length( 15 ).
          column-r_column->set_visible( abap_false ).

        WHEN 'DATE'.
          column-r_column->set_output_length( 10 ).
          column-r_column->set_short_text( CONV #( 'Date'(008) ) ).
          column-r_column->set_medium_text( CONV #( 'Date'(008) ) ).
          column-r_column->set_long_text( CONV #( 'Date'(008) ) ).

        WHEN 'TIME'.
          column-r_column->set_output_length( 10 ).
          column-r_column->set_short_text( CONV #( 'Time'(009) ) ).
          column-r_column->set_medium_text( CONV #( 'Time'(009) ) ).
          column-r_column->set_long_text( CONV #( 'Time'(009) ) ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_alv_cross_reference.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->cross_reference ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Cross reference checks'(002) ).

    result->get_functions( )->set_all( ).

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    salv_columns->set_column_position( columnname = 'STATUS_DESCRIPTION' position = 2 ).

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT' OR 'SEVERITY' OR 'ANA_TRKORR' OR 'TAR_TRKORR' OR 'ANA_SID' OR 'TAR_SID'
          OR 'ANA_TR_AS4DATE' OR 'ANA_TR_AS4TIME' OR 'TAR_TR_AS4DATE' OR 'TAR_TR_AS4TIME'
          OR 'ANA_TR_OWNER' OR 'TAR_TR_OWNER' OR 'REF_OBJ_OWNER'.
          column-r_column->set_technical( ).

        WHEN 'RUN_CODE' OR 'SEQUENCE' OR 'CHK_TRKORR' OR 'AS4POS'.
          column-r_column->set_visible( abap_false ).

        WHEN 'OBJ_NAME'.
          column-r_column->set_output_length( alv_column-object_name-length ).

        WHEN 'REF_OBJ_NAME'.
          column-r_column->set_output_length( 60 ).

        WHEN 'PGMID' OR 'OBJ_TYPE' OR 'REF_OBJ_TYPE'.
          column-r_column->set_output_length( 5 ).

        WHEN 'REM_ANAL'.
          column-r_column->set_output_length( 3 ).

        WHEN 'STATUS_DESCRIPTION'.
          column-r_column->set_output_length( 20 ).
          column-r_column->set_short_text( CONV #( 'Status'(007) ) ).
          column-r_column->set_medium_text( CONV #( 'Status'(007) ) ).
          column-r_column->set_long_text( CONV #( 'Status'(007) ) ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_alv_sequence.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->sequence ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Sequence checks'(003) ).

    result->get_functions( )->set_all( ).

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT'.
          column-r_column->set_technical( ).

        WHEN 'RUN_CODE' OR 'SEQUENCE' OR 'TRANSPORT_REQUEST' OR 'CHECKED_TRKORR'
          OR 'CRITICALITY_1' OR 'CRITICALITY_2' OR 'CHECKED_TR_EXP_TIME' OR 'CONFLICT_TR_EXP_TIME'
          OR 'SOLVE_TR_EXP_TIME' OR 'CONFLICT_TR_CTS_PRJ' OR 'CONFLICT_TR_OWNER'.
          column-r_column->set_visible( abap_false ).

        WHEN 'CONFLICT_OBJNAME'.
          column-r_column->set_output_length( alv_column-object_name-length ).

        WHEN 'CONFLICT_PGMID' OR 'CONFLICT_OBJECT'.
          column-r_column->set_output_length( 5 ).

        WHEN 'CONFLICT_TABLE'.
          column-r_column->set_output_length( 20 ).

        WHEN 'CONFLICT_TABKEY'.
          column-r_column->set_output_length( 40 ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_alv_cross_release.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->cross_release ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Cross release checks'(004) ).

    result->get_functions( )->set_all( ).

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT'.
          column-r_column->set_technical( ).

        WHEN 'RUN_CODE'.
          column-r_column->set_visible( abap_false ).

        WHEN 'SEQUENCE'.
          column-r_column->set_visible( abap_false ).

        WHEN 'TRANSPORT_REQUEST'.
          column-r_column->set_visible( abap_false ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_alv_import_time.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->import_time ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Import time checks'(005) ).

    result->get_functions( )->set_all( ).

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT'.
          column-r_column->set_technical( ).

        WHEN 'RUN_CODE'.
          column-r_column->set_visible( abap_false ).

        WHEN 'SEQUENCE'.
          column-r_column->set_visible( abap_false ).

        WHEN 'TRANSPORT_REQUEST'.
          column-r_column->set_visible( abap_false ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_alv_online_import_sum.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = result
                                CHANGING  t_table      = salv_data->online_import_summary ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Online import summary checks'(036) ).

    result->get_functions( )->set_all( ).

    DATA(salv_sorts) = result->get_sorts( ).

    TRY.
        salv_sorts->add_sort( columnname = 'EXCEPTION' position = 1 sequence = if_salv_c_sort=>sort_up ).
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
    ENDTRY.

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    salv_columns->set_column_position( columnname = 'DETAILS_ICON' position = 3 ).

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT' OR 'RUN_CODE' OR 'SEQUENCE' OR 'TRKORR' OR 'CRITICALITY' OR 'AS4USER'.
          column-r_column->set_technical( ).

        WHEN 'DETAILS_ICON'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_output_length( 5 ).
          column-r_column->set_short_text( CONV #( 'Details'(053) ) ).
          column-r_column->set_medium_text( CONV #( 'Details'(053) ) ).
          column-r_column->set_long_text( CONV #( 'Details)'(053) ) ).

        WHEN 'ACCNT'.
          column-r_column->set_short_text( CONV #( 'Tab.Read/H'(037) ) ).
          column-r_column->set_medium_text( CONV #( 'Table Reads per Hour'(038) ) ).
          column-r_column->set_long_text( CONV #( 'Table Reads per Hour'(038) ) ).

        WHEN 'CHCNT'.
          column-r_column->set_short_text( CONV #( 'TabWrite/H'(039) ) ).
          column-r_column->set_medium_text( CONV #( 'Table Writes per Hour'(040) ) ).
          column-r_column->set_long_text( CONV #( 'Table Writes per Hour'(040) ) ).

        WHEN 'OCCTB'.
          column-r_column->set_short_text( CONV #( 'Table Size'(041) ) ).
          column-r_column->set_medium_text( CONV #( 'Table Size [KB]'(042) ) ).
          column-r_column->set_long_text( CONV #( 'Table Size [KB]'(042) ) ).

        WHEN 'ACTION'.
          column-r_column->set_short_text( CONV #( 'DB Action'(043) ) ).
          column-r_column->set_medium_text( CONV #( 'DB Action'(043) ) ).
          column-r_column->set_long_text( CONV #( 'DB Action'(043) ) ).

        WHEN 'EXECNT'.
          column-r_column->set_short_text( CONV #( 'RepExec/h'(044) ) ).
          column-r_column->set_medium_text( CONV #( 'Report Exec.per Hour'(045) ) ).
          column-r_column->set_long_text( CONV #( 'Report Exec.per Hour'(045) ) ).

        WHEN 'EXECNT_DD'.
          column-r_column->set_short_text( CONV #( 'RepEx/hDD'(046) ) ).
          column-r_column->set_medium_text( CONV #( 'Report Exec./Hour DD'(047) ) ).
          column-r_column->set_long_text( CONV #( 'Report Exec.per Hour (DD)'(048) ) ).

        WHEN 'CRIOBJ'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'Crit.Obj.'(049) ) ).
          column-r_column->set_medium_text( CONV #( 'Critical Object'(050) ) ).
          column-r_column->set_long_text( CONV #( 'Critical Object)'(050) ) ).

        WHEN 'REQ_IN_TAR'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>checkbox ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'ReqInTarg'(051) ) ).
          column-r_column->set_medium_text( CONV #( 'Request in Target'(052) ) ).
          column-r_column->set_long_text( CONV #( 'Request in Target'(052) ) ).

      ENDCASE.

    ENDLOOP.

    DATA(salv_events) = result->get_event( ).

    DATA(event_handler) = NEW lcl_event_handler( reader     = me
                                                 salv_table = result ).

    SET HANDLER event_handler->oi_summary_link_click FOR salv_events ACTIVATION abap_true.

  ENDMETHOD.


  METHOD prepare_alv_online_import.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = result
                                CHANGING  t_table       = salv_data->online_import ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RAISE EXCEPTION TYPE zcx_adu_check_transport
          EXPORTING
            previous = salv_msg_exception.
    ENDTRY.

    result->get_display_settings( )->set_list_header( 'Online import checks'(006) ).

    result->get_functions( )->set_all( ).

    DATA(salv_sorts) = result->get_sorts( ).

    TRY.
        salv_sorts->add_sort( columnname = 'EXCEPTION' position = 1 sequence = if_salv_c_sort=>sort_up ).
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
    ENDTRY.

    DATA(salv_columns) = result->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT' OR 'RUN_CODE' OR 'SEQUENCE' OR 'CRITICALITY'.
          column-r_column->set_technical( ).

        WHEN 'TRKORR' OR 'COMPONENT' OR 'CRIOBJ'.
          column-r_column->set_visible( abap_false ).

        WHEN 'OBJ_NAME'.
          column-r_column->set_output_length( alv_column-object_name-length ).

        WHEN 'OBJECT'.
          column-r_column->set_output_length( 5 ).

        WHEN 'ACCNT' OR 'CHCNT' OR 'OCCTB' OR 'EXECNT' OR 'EXECNT_DD'.
          column-r_column->set_output_length( 15 ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_online_import_config.

    TRY.
        online_import_config-tabreads_y = report_configuration[ config_param = 'TABREADS_Y' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabreads_y = 10000.
    ENDTRY.

    TRY.
        online_import_config-tabreads_r = report_configuration[ config_param = 'TABREADS_R' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabreads_r = 100000.
    ENDTRY.

    TRY.
        online_import_config-tabwrite_y = report_configuration[ config_param = 'TABWRITE_Y' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabwrite_y = 500.
    ENDTRY.

    TRY.
        online_import_config-tabwrite_r = report_configuration[ config_param = 'TABWRITE_R' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabwrite_r = 5000.
    ENDTRY.

    TRY.
        online_import_config-tabsize_y = report_configuration[ config_param = 'TABSIZE_Y' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabsize_y = 100000.
    ENDTRY.

    TRY.
        online_import_config-tabsize_r = report_configuration[ config_param = 'TABSIZE_R' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-tabsize_r = 600000.
    ENDTRY.

    TRY.
        online_import_config-repexe_y = report_configuration[ config_param = 'REPEXE_Y' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-repexe_y = 5000000.
    ENDTRY.

    TRY.
        online_import_config-repexe_r = report_configuration[ config_param = 'REPEXE_R' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-repexe_r = 50000000.
    ENDTRY.

    TRY.
        online_import_config-repexedd_y = report_configuration[ config_param = 'REPEXEDD_Y' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-repexedd_y = 1000000.
    ENDTRY.

    TRY.
        online_import_config-repexedd_r = report_configuration[ config_param = 'REPEXEDD_R' ]-param_value_i.
      CATCH cx_sy_itab_line_not_found.
        online_import_config-repexedd_r = 10000000.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
