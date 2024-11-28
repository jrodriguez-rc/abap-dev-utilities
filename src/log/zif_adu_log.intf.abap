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

  TYPES:
    BEGIN OF ty_message,
      type      TYPE msgty,
      id        TYPE msgid,
      number    TYPE msgno,
      variable1 TYPE msgv1,
      variable2 TYPE msgv2,
      variable3 TYPE msgv3,
      variable4 TYPE msgv4,
    END OF ty_message.

  METHODS add_exception
    IMPORTING ix_exception     TYPE REF TO cx_root
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_message
    IMPORTING is_message       TYPE bal_s_msg
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_bapiret_message
    IMPORTING is_message       TYPE bapiret2
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS add_content
    IMPORTING iv_json           TYPE string
              is_custom_message TYPE ty_message OPTIONAL
    RETURNING VALUE(ri_result)  TYPE REF TO zif_adu_log.

  METHODS add_content_by_type
    IMPORTING iv_content        TYPE string
              iv_content_type   TYPE balpval
              is_custom_message TYPE ty_message OPTIONAL
    RETURNING VALUE(ri_result)  TYPE REF TO zif_adu_log.

  METHODS add_content_json
    IMPORTING iv_json           TYPE string
              is_custom_message TYPE ty_message OPTIONAL
    RETURNING VALUE(ri_result)  TYPE REF TO zif_adu_log.

  METHODS add_content_xml
    IMPORTING iv_xml            TYPE string
              is_custom_message TYPE ty_message OPTIONAL
    RETURNING VALUE(ri_result)  TYPE REF TO zif_adu_log.

  METHODS add_text
    IMPORTING iv_type          TYPE sy-msgty DEFAULT zif_adu_messages=>gc_severity-error
              iv_text          TYPE zif_adu_messages=>ty_message_text
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

  METHODS save
    RETURNING VALUE(ri_result) TYPE REF TO zif_adu_log.

ENDINTERFACE.
