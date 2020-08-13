CLASS lcl_event_handler IMPLEMENTATION.


  METHOD constructor.

    me->reader     = reader.
    me->salv_table = salv_table.

  ENDMETHOD.


  METHOD header_link_click.

    DATA(log_header) = reader->salv_data->header[ row ].

    TRY.

        CASE column.
          WHEN 'CROSS_REFERENCE_MESSAGES'.
            reader->zif_adu_check_transport_reader~display_cross_reference(
                                                                run_code          = log_header-run_code
                                                                transport_request = log_header-transport_request ).

          WHEN 'SEQUENCE_MESSAGES'.
            reader->zif_adu_check_transport_reader~display_sequence(
                                                                run_code          = log_header-run_code
                                                                transport_request = log_header-transport_request ).

          WHEN 'CROSS_RELEASE_MESSAGES'.
            reader->zif_adu_check_transport_reader~display_cross_release(
                                                                run_code          = log_header-run_code
                                                                transport_request = log_header-transport_request ).

          WHEN 'IMPORT_TIME_MESSAGES'.
            reader->zif_adu_check_transport_reader~display_import_time(
                                                                run_code          = log_header-run_code
                                                                transport_request = log_header-transport_request ).

          WHEN 'ONLINE_IMPORT_MESSAGES'.
            reader->zif_adu_check_transport_reader~display_online_import_summary(
                                                                run_code          = log_header-run_code
                                                                transport_request = log_header-transport_request ).

        ENDCASE.

      CATCH zcx_adu_check_transport INTO DATA(check_transport_exception).
        MESSAGE check_transport_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD oi_summary_link_click.

    DATA(online_import_summary) = reader->salv_data->online_import_summary[ row ].

    TRY.

        IF column = 'DETAILS_ICON'.
          reader->zif_adu_check_transport_reader~display_online_import(
                                                      run_code          = online_import_summary-run_code
                                                      transport_request = online_import_summary-transport_request ).
        ENDIF.

      CATCH zcx_adu_check_transport INTO DATA(check_transport_exception).
        MESSAGE check_transport_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
