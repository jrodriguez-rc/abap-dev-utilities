CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS zcl_adu_check_transport_reader DEFINITION LOCAL FRIENDS lcl_event_handler.
CLASS lcl_event_handler DEFINITION
  FRIENDS zcl_adu_check_transport_reader.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        reader     TYPE REF TO zcl_adu_check_transport_reader
        salv_table TYPE REF TO cl_salv_table.

  PRIVATE SECTION.
    DATA:
      reader     TYPE REF TO zcl_adu_check_transport_reader,
      salv_table TYPE REF TO cl_salv_table.

    METHODS header_link_click
        FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING
        row
        column.

    METHODS oi_summary_link_click
        FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING
        row
        column.

ENDCLASS.

CLASS lcl_salv_data DEFINITION
  FRIENDS zcl_adu_check_transport_reader lcl_event_handler.

  PRIVATE SECTION.
    DATA:
      header                TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_header
                                                                                        WITH DEFAULT KEY,
      cross_reference       TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_cross_reference
                                                                                        WITH DEFAULT KEY,
      sequence              TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_sequence
                                                                                        WITH DEFAULT KEY,
      cross_release         TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_cross_release
                                                                                        WITH DEFAULT KEY,
      import_time           TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_import_time
                                                                                        WITH DEFAULT KEY,
      online_import_summary TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_online_import_summary
                                                                                        WITH DEFAULT KEY,
      online_import         TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_online_import
                                                                                        WITH DEFAULT KEY.

ENDCLASS.
