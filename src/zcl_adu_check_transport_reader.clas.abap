"! <p class="shorttext synchronized" lang="en">Check transport reader</p>
CLASS zcl_adu_check_transport_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adu_check_transport_reader.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter transport_request | <p class="shorttext synchronized" lang="en">Transport request</p>
    "! @parameter run_code | <p class="shorttext synchronized" lang="en">Run code</p>
    METHODS constructor
      IMPORTING
        transport_request TYPE trkorr
        run_code          TYPE zadu_run_code OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_logs,
        run_code          TYPE zadu_run_code,
        transport_request TYPE trkorr,
        header            TYPE zif_adu_check_transport_reader=>ts_header,
        cross_reference   TYPE zif_adu_check_transport_reader=>tt_cross_reference,
        sequence          TYPE zif_adu_check_transport_reader=>tt_sequence,
        cross_release     TYPE zif_adu_check_transport_reader=>tt_cross_release,
        import_time       TYPE zif_adu_check_transport_reader=>tt_import_time,
        online_import     TYPE zif_adu_check_transport_reader=>tt_online_import,
      END OF ts_logs,
      tt_logs TYPE HASHED TABLE OF zcl_adu_check_transport_reader=>ts_logs
          WITH UNIQUE KEY run_code
          WITH NON-UNIQUE SORTED KEY req COMPONENTS transport_request.

    TYPES:
      tt_cross_reference_db TYPE STANDARD TABLE OF zadu_chktr_crref WITH DEFAULT KEY,
      tt_sequence_db        TYPE STANDARD TABLE OF zadu_chktr_seq   WITH DEFAULT KEY,
      tt_cross_release_db   TYPE STANDARD TABLE OF zadu_chktr_crrel WITH DEFAULT KEY,
      tt_import_time_db     TYPE STANDARD TABLE OF zadu_chktr_imtim WITH DEFAULT KEY,
      tt_online_import_db   TYPE STANDARD TABLE OF zadu_chktr_onlim WITH DEFAULT KEY.

    DATA:
      logs TYPE zcl_adu_check_transport_reader=>tt_logs.

    METHODS add_header_log
      IMPORTING
        header_log TYPE zadu_chktr_head.

    METHODS fill_header
      IMPORTING
        data            TYPE zadu_chktr_head
        cross_reference TYPE zcl_adu_check_transport_reader=>tt_cross_reference_db OPTIONAL
        sequence        TYPE zcl_adu_check_transport_reader=>tt_sequence_db OPTIONAL
        cross_release   TYPE zcl_adu_check_transport_reader=>tt_cross_release_db OPTIONAL
        import_time     TYPE zcl_adu_check_transport_reader=>tt_import_time_db OPTIONAL
        online_import   TYPE zcl_adu_check_transport_reader=>tt_online_import_db OPTIONAL
      RETURNING
        VALUE(filled)   TYPE zif_adu_check_transport_reader=>ts_header.

    METHODS fill_cross_reference
      IMPORTING
        data          TYPE zadu_chktr_crref
      RETURNING
        VALUE(filled) TYPE zif_adu_check_transport_reader=>ts_cross_reference.

    METHODS fill_sequence
      IMPORTING
        data          TYPE zadu_chktr_seq
      RETURNING
        VALUE(filled) TYPE zif_adu_check_transport_reader=>ts_sequence.

    METHODS fill_cross_release
      IMPORTING
        data          TYPE zadu_chktr_crrel
      RETURNING
        VALUE(filled) TYPE zif_adu_check_transport_reader=>ts_cross_release.

    METHODS fill_import_time
      IMPORTING
        data          TYPE zadu_chktr_imtim
      RETURNING
        VALUE(filled) TYPE zif_adu_check_transport_reader=>ts_import_time.

    METHODS fill_online_import
      IMPORTING
        data          TYPE zadu_chktr_onlim
      RETURNING
        VALUE(filled) TYPE zif_adu_check_transport_reader=>ts_online_import.

    METHODS severity_for_cross_reference
      IMPORTING
        data            TYPE zcl_adu_check_transport_reader=>tt_cross_reference_db
      RETURNING
        VALUE(severity) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_sequence
      IMPORTING
        data            TYPE zcl_adu_check_transport_reader=>tt_sequence_db
      RETURNING
        VALUE(severity) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_cross_release
      IMPORTING
        data            TYPE zcl_adu_check_transport_reader=>tt_cross_release_db
      RETURNING
        VALUE(severity) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_import_time
      IMPORTING
        data            TYPE zcl_adu_check_transport_reader=>tt_import_time_db
      RETURNING
        VALUE(severity) TYPE zif_adu_check_transport_reader=>ty_severity.

    METHODS severity_for_online_import
      IMPORTING
        data            TYPE zcl_adu_check_transport_reader=>tt_online_import_db
      RETURNING
        VALUE(severity) TYPE zif_adu_check_transport_reader=>ty_severity.

ENDCLASS.



CLASS zcl_adu_check_transport_reader IMPLEMENTATION.


  METHOD constructor.

    DATA:
      run_code_range TYPE RANGE OF zadu_run_code.

    run_code_range = COND #( WHEN run_code IS NOT INITIAL
                                 THEN VALUE #( ( sign = 'I' option = 'EQ' low = run_code ) ) ).

    SELECT *
      INTO TABLE @DATA(header_logs)
      FROM zadu_chktr_head
      WHERE run_code          IN @run_code_range
        AND transport_request = @transport_request.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT header_logs REFERENCE INTO DATA(header_log).
      add_header_log( header_log->* ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display.

    DATA:
      headers TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_header WITH DEFAULT KEY.

    LOOP AT logs REFERENCE INTO DATA(log).
      INSERT log->header INTO TABLE headers.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = DATA(salv_table)
                                CHANGING  t_table       = headers ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    DATA(salv_columns) = salv_table->get_columns( ).

    TRY.
        salv_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        salv_columns->set_exception_column( 'EXCEPTION' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    salv_columns->set_optimize( ).

    DATA(salv_column) = CAST cl_salv_column_table( salv_columns->get_column( 'GIT_URL' ) ).
    salv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    DATA(salv_events) = salv_table->get_event( ).

    salv_table->get_display_settings( )->set_list_header( 'Transport Check Logs'(001) ).
    salv_table->display( ).

  ENDMETHOD.


  METHOD add_header_log.

    INSERT VALUE #( run_code          = header_log-run_code
                    transport_request = header_log-transport_request
                  ) INTO TABLE logs REFERENCE INTO DATA(log).

    SELECT *
      INTO TABLE @DATA(cross_references)
      FROM zadu_chktr_crref
      WHERE run_code = @header_log-run_code.
    LOOP AT cross_references REFERENCE INTO DATA(cross_reference).
      INSERT fill_cross_reference( cross_reference->* ) INTO TABLE log->cross_reference.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(sequences)
      FROM zadu_chktr_seq
      WHERE run_code = @header_log-run_code.
    LOOP AT sequences REFERENCE INTO DATA(sequence).
      INSERT fill_sequence( sequence->* ) INTO TABLE log->sequence.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(cross_releases)
      FROM zadu_chktr_crrel
      WHERE run_code = @header_log-run_code.
    LOOP AT cross_releases REFERENCE INTO DATA(cross_release).
      INSERT fill_cross_release( cross_release->* ) INTO TABLE log->cross_release.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(import_times)
      FROM zadu_chktr_imtim
      WHERE run_code = @header_log-run_code.
    LOOP AT import_times REFERENCE INTO DATA(import_time).
      INSERT fill_import_time( import_time->* ) INTO TABLE log->import_time.
    ENDLOOP.

    SELECT *
      INTO TABLE @DATA(online_imports)
      FROM zadu_chktr_onlim
      WHERE run_code = @header_log-run_code.
    LOOP AT online_imports REFERENCE INTO DATA(online_import).
      INSERT fill_online_import( online_import->* ) INTO TABLE log->online_import.
    ENDLOOP.

    log->header = fill_header( data            = header_log
                               cross_reference = CONV #( cross_references )
                               sequence        = CONV #( sequences )
                               cross_release   = CONV #( cross_releases )
                               import_time     = CONV #( import_times )
                               online_import   = CONV #( online_imports ) ).

  ENDMETHOD.


  METHOD fill_header.

    filled = CORRESPONDING #( data ).

    filled-cross_reference_messages = lines( cross_reference ).
    filled-sequence_messages        = lines( sequence ).
    filled-cross_release_messages   = lines( cross_release ).
    filled-import_time_messages     = lines( import_time ).
    filled-online_import_messages   = lines( online_import ).

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
        filled-exception = '1'.
        filled-color     = VALUE #( ( color-col = col_negative ) ).

      WHEN zif_adu_constants=>severity-warning.
        filled-exception = '2'.
        filled-color     = VALUE #( ( color-col = col_key ) ).

      WHEN zif_adu_constants=>severity-info.
        filled-exception = '3'.
        filled-color     = VALUE #( ( color-col = col_normal ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD fill_cross_reference.

    filled = CORRESPONDING #( data ).

    filled-status_description = cl_proxy_utils=>get_domain_text_for_value( filled-status ).

  ENDMETHOD.


  METHOD fill_sequence.

    filled = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD fill_cross_release.

    filled = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD fill_import_time.

    filled = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD fill_online_import.

    filled = CORRESPONDING #( data ).

  ENDMETHOD.


  METHOD severity_for_cross_reference.

    IF data IS INITIAL.
      RETURN.
    ENDIF.

    severity =
        COND #(
            WHEN line_exists( data[ severity = zif_adu_constants=>check_cross_reference_status-only_in_source ] )
              OR line_exists( data[ severity = zif_adu_constants=>check_cross_reference_status-only_in_target ] )
              OR line_exists( data[ severity = zif_adu_constants=>check_cross_reference_status-different_version  ] )
              OR line_exists( data[ severity = zif_adu_constants=>check_cross_reference_status-inconsistent_source ] )
              OR line_exists( data[ severity = zif_adu_constants=>check_cross_reference_status-locked_target ] )
                THEN zif_adu_constants=>severity-error
                ELSE zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_sequence.

    severity = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_cross_release.

    severity = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-warning ).

  ENDMETHOD.


  METHOD severity_for_import_time.

    severity = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-info ).

  ENDMETHOD.


  METHOD severity_for_online_import.

    severity = COND #( WHEN data IS NOT INITIAL THEN zif_adu_constants=>severity-info ).

  ENDMETHOD.


ENDCLASS.
