INTERFACE zif_adu_log
  PUBLIC.

  CONSTANTS:
    BEGIN OF gc_parameter,
      content_type TYPE balpar VALUE 'CTYPE',
      content      TYPE balpar VALUE 'DATA',
    END OF gc_parameter.

  CONSTANTS:
    BEGIN OF gc_message_class,
      very_important        TYPE balprobcl VALUE '1',
      important             TYPE balprobcl VALUE '2',
      medium                TYPE balprobcl VALUE '3',
      additinal_information TYPE balprobcl VALUE '4',
      other                 TYPE balprobcl VALUE '',
    END OF gc_message_class.

  METHODS add_exception
    IMPORTING ix_exception     TYPE REF TO cx_root
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_message
    IMPORTING is_message       TYPE bal_s_msg
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_content_json
    IMPORTING iv_json          TYPE string
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_content_xml
    IMPORTING iv_xml           TYPE string
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS save
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

ENDINTERFACE.
