CLASS lcl_salv_data DEFINITION.

  PUBLIC SECTION.
    DATA:
      header            TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_header WITH DEFAULT KEY,
      cross_reference   TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_cross_reference WITH DEFAULT KEY,
      sequence          TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_sequence WITH DEFAULT KEY,
      cross_release     TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_cross_release WITH DEFAULT KEY,
      import_time       TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_import_time WITH DEFAULT KEY,
      online_import     TYPE STANDARD TABLE OF zif_adu_check_transport_reader=>ts_online_import WITH DEFAULT KEY.

ENDCLASS.
