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
      transport_request TYPE trkorr,
      logs              TYPE zcl_adu_check_transport_reader=>tt_logs,
      salv_data         TYPE REF TO lcl_salv_data.

    METHODS add_header_log
      IMPORTING
        header_log TYPE zadu_chktr_head.

    METHODS handle_header_link_click
        FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING
        row
        column
        sender.

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

    METHODS prepare_alv_log
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

    METHODS prepare_alv_cross_reference
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

    METHODS prepare_alv_sequence
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

    METHODS prepare_alv_cross_release
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

    METHODS prepare_alv_import_time
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

    METHODS prepare_alv_online_import
      RETURNING
        VALUE(salv_table) TYPE REF TO cl_salv_table.

ENDCLASS.



CLASS zcl_adu_check_transport_reader IMPLEMENTATION.


  METHOD constructor.

    DATA:
      run_code_range TYPE RANGE OF zadu_run_code.

    me->transport_request = transport_request.

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

    salv_data = NEW #( ).

    LOOP AT header_logs REFERENCE INTO DATA(header_log).
      add_header_log( header_log->* ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display.

    CLEAR: salv_data->header.

    LOOP AT logs REFERENCE INTO DATA(log).
      INSERT log->header INTO TABLE salv_data->header.
    ENDLOOP.

    DATA(salv_table) = prepare_alv_log( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

    DATA(salv_events) = salv_table->get_event( ).
    SET HANDLER handle_header_link_click FOR salv_events ACTIVATION abap_false.

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_cross_reference.

    TRY.
        DATA(log) = logs[ run_code = run_code ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    salv_data->cross_reference = log-cross_reference.

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

    TRY.
        DATA(log) = logs[ run_code = run_code ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    salv_data->sequence = log-sequence.

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

    TRY.
        DATA(log) = logs[ run_code = run_code ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    salv_data->cross_release = log-cross_release.

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

    TRY.
        DATA(log) = logs[ run_code = run_code ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    salv_data->import_time = log-import_time.

    DATA(salv_table) = prepare_alv_import_time( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD zif_adu_check_transport_reader~display_online_import.

    TRY.
        DATA(log) = logs[ run_code = run_code ].
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    salv_data->online_import = log-online_import.

    DATA(salv_table) = prepare_alv_online_import( ).

    IF as_popup = abap_true.
      salv_table->set_screen_popup( start_column = 30
                                    end_column   = 120
                                    start_line   = 10
                                    end_line     = 20 ).
    ENDIF.

    salv_table->display( ).

  ENDMETHOD.


  METHOD handle_header_link_click.

    DATA(log_header) = salv_data->header[ row ].

    CASE column.
      WHEN 'CROSS_REFERENCE_MESSAGES'.
        zif_adu_check_transport_reader~display_cross_reference( log_header-run_code ).

      WHEN 'SEQUENCE_MESSAGES'.
        zif_adu_check_transport_reader~display_sequence( log_header-run_code ).

      WHEN 'CROSS_RELEASE_MESSAGES'.
        zif_adu_check_transport_reader~display_cross_release( log_header-run_code ).

      WHEN 'IMPORT_TIME_MESSAGES'.
        zif_adu_check_transport_reader~display_import_time( log_header-run_code ).

      WHEN 'ONLINE_IMPORT_MESSAGES'.
        zif_adu_check_transport_reader~display_online_import( log_header-run_code ).

    ENDCASE.

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

    DATA:
      timezone TYPE timezone.

    filled = CORRESPONDING #( data ).

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = timezone " Time Zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      timezone = 'UTC'.
    ENDIF.

    CONVERT TIME STAMP data-timestamp TIME ZONE timezone INTO DATE filled-date TIME filled-time.

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
            WHEN line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-only_in_source ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-only_in_target ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-different_version  ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-inconsistent_source ] )
              OR line_exists( data[ status = zif_adu_constants=>check_cross_reference_status-locked_target ] )
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


  METHOD prepare_alv_log.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->header ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( |{ 'Transport Check Logs'(001) }: { transport_request }| ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_sorts) = salv_table->get_sorts( ).

    TRY.
        salv_sorts->add_sort( columnname = 'DATE' position = 1 sequence = if_salv_c_sort=>sort_down ).
        salv_sorts->add_sort( columnname = 'TIME' position = 2 sequence = if_salv_c_sort=>sort_down ).
      CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
    ENDTRY.

    DATA(salv_columns) = salv_table->get_columns( ).

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
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'XRefMsg'(010) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Ref. Messages'(011) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Reference Messages'(012) ) ).

        WHEN 'SEQUENCE_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'SeqMsg'(013) ) ).
          column-r_column->set_medium_text( CONV #( 'Sequence Messages'(014) ) ).
          column-r_column->set_long_text( CONV #( 'Sequence Messages'(014) ) ).

        WHEN 'CROSS_RELEASE_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'XRelMsg'(015) ) ).
          column-r_column->set_medium_text( CONV #( 'Cross Rel. Messages'(016) ) ).
          column-r_column->set_long_text( CONV #( 'Cross Release Messages'(017) ) ).

        WHEN 'IMPORT_TIME_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'ImpTimeMsg'(018) ) ).
          column-r_column->set_medium_text( CONV #( 'Import Time Messages'(019) ) ).
          column-r_column->set_long_text( CONV #( 'Import Time Messages'(019) ) ).

        WHEN 'ONLINE_IMPORT_MESSAGES'.
          CAST cl_salv_column_table( column-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column-r_column->set_output_length( 3 ).
          column-r_column->set_short_text( CONV #( 'OnlImpMsg'(020) ) ).
          column-r_column->set_medium_text( CONV #( 'Online Imp. Messages'(021) ) ).
          column-r_column->set_long_text( CONV #( 'Online Import Messages'(022) ) ).

        WHEN 'SOURCE' OR 'DESTINATION'.
          column-r_column->set_output_length( 15 ).

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

    DATA(salv_events) = salv_table->get_event( ).
    SET HANDLER handle_header_link_click FOR salv_events ACTIVATION abap_true.

  ENDMETHOD.


  METHOD prepare_alv_cross_reference.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->cross_reference ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( 'Cross reference checks'(002) ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_columns) = salv_table->get_columns( ).

    salv_columns->set_column_position( columnname = 'SEVERITY' position = 1 ).
    salv_columns->set_column_position( columnname = 'STATUS_DESCRIPTION' position = 2 ).

    LOOP AT salv_columns->get( ) INTO DATA(column).

      CASE column-columnname.
        WHEN 'CLIENT' OR 'ANA_TRKORR' OR 'TAR_TRKORR' OR 'ANA_TR_AS4DATE' OR 'ANA_TR_AS4TIME'
          OR 'TAR_TR_AS4DATE' OR 'TAR_TR_AS4TIME' OR 'ANA_TR_OWNER' OR 'TAR_TR_OWNER' OR 'REF_OBJ_OWNER'.
          column-r_column->set_technical( ).

        WHEN 'RUN_CODE' OR 'SEQUENCE' OR 'CHK_TRKORR' OR 'AS4POS'.
          column-r_column->set_visible( abap_false ).

        WHEN 'OBJ_NAME'.
          column-r_column->set_output_length( 30 ).

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
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->sequence ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( 'Sequence checks'(003) ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_columns) = salv_table->get_columns( ).

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


  METHOD prepare_alv_cross_release.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->cross_release ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( 'Cross release checks'(004) ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_columns) = salv_table->get_columns( ).

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
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->import_time ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( 'Import time checks'(005) ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_columns) = salv_table->get_columns( ).

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


  METHOD prepare_alv_online_import.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table  = salv_table
                                CHANGING  t_table       = salv_data->online_import ).
      CATCH cx_salv_msg INTO DATA(salv_msg_exception).
        RETURN. " TODO: Pending raise exception
    ENDTRY.

    salv_table->get_display_settings( )->set_list_header( 'Online import checks'(006) ).

    salv_table->get_functions( )->set_all( ).

    DATA(salv_columns) = salv_table->get_columns( ).

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


ENDCLASS.
