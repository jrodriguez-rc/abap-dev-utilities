INTERFACE zif_adu_messages
  PUBLIC.

  TYPES tt_message_types TYPE STANDARD TABLE OF syst_msgty WITH DEFAULT KEY.
  TYPES tt_messages      TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY
            WITH NON-UNIQUE SORTED KEY type COMPONENTS type.

  CONSTANTS:
    BEGIN OF severity,
      error       TYPE symsgty VALUE 'E',
      warning     TYPE symsgty VALUE 'W',
      information TYPE symsgty VALUE 'I',
      success     TYPE symsgty VALUE 'S',
      exception   TYPE symsgty VALUE 'X',
      abort       TYPE symsgty VALUE 'A',
    END OF severity.

  CONSTANTS error_types TYPE string VALUE 'AEX' ##NO_TEXT.

  METHODS add_message
    IMPORTING message_type   TYPE sy-msgty           DEFAULT severity-error
              message_id     TYPE sy-msgid
              message_number TYPE sy-msgno
              message_var_1  TYPE any                OPTIONAL
              message_var_2  TYPE any                OPTIONAL
              message_var_3  TYPE any                OPTIONAL
              message_var_4  TYPE any                OPTIONAL
              !parameter     TYPE bapiret2-parameter OPTIONAL
              !row           TYPE bapiret2-row       OPTIONAL
              !field         TYPE bapiret2-field     OPTIONAL.

  METHODS add_messages
    IMPORTING !messages TYPE bapiret2_t.

  METHODS add_exception
    IMPORTING !exception TYPE REF TO cx_root
              !parameter TYPE bapiret2-parameter OPTIONAL
              !row       TYPE bapiret2-row       OPTIONAL
              !field     TYPE bapiret2-field     OPTIONAL.

  METHODS add_t100_message
    IMPORTING !message TYPE REF TO if_t100_message.

  METHODS add_text_message
    IMPORTING message_type   TYPE sy-msgty           DEFAULT severity-error
              message_id     TYPE sy-msgid
              message_number TYPE sy-msgno
              !text          TYPE string
              !parameter     TYPE bapiret2-parameter OPTIONAL
              !row           TYPE bapiret2-row       OPTIONAL
              !field         TYPE bapiret2-field     OPTIONAL.

  METHODS display_messages
    IMPORTING initialize_after_display TYPE abap_bool DEFAULT abap_true
              send_if_one              TYPE abap_bool DEFAULT abap_false.

  METHODS get_messages
    RETURNING VALUE(result) TYPE tt_messages.

  METHODS initialize.

  METHODS is_error
    RETURNING VALUE(result) TYPE abap_bool.

  METHODS raise_gateway
    RAISING /iwbep/cx_gateway.

  METHODS raise_gateway_busi_exception
    RAISING /iwbep/cx_mgw_busi_exception.

  METHODS raise_gateway_tech_exception
    RAISING /iwbep/cx_mgw_tech_exception.

ENDINTERFACE.
