"! <p class="shorttext synchronized" lang="en">Check transport Exceptions</p>
CLASS zcx_adu_transport_request DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      "! Transport request &1 doesn't exist
      BEGIN OF not_exists,
        msgid TYPE symsgid VALUE 'ZADU_TRANSPORT_REQ' ##NO_TEXT,
        msgno TYPE symsgno VALUE '001' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF not_exists.

    DATA:
      text1 TYPE string READ-ONLY,
      text2 TYPE string READ-ONLY,
      text3 TYPE string READ-ONLY,
      text4 TYPE string READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Raise exception with system attribute message</p>
    "!
    "! @raising zcx_adu_transport_request | <p class="shorttext synchronized" lang="en">Exception</p>
    CLASS-METHODS raise_system
      RAISING
        zcx_adu_transport_request.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !text1    TYPE string OPTIONAL
        !text2    TYPE string OPTIONAL
        !text3    TYPE string OPTIONAL
        !text4    TYPE string OPTIONAL
        !previous LIKE previous OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_adu_transport_request IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise_system.

    RAISE EXCEPTION TYPE zcx_adu_transport_request
      EXPORTING
        textid = VALUE scx_t100key( msgid = sy-msgid
                                    msgno = sy-msgno
                                    attr1 = 'TEXT1'
                                    attr2 = 'TEXT2'
                                    attr3 = 'TEXT3'
                                    attr4 = 'TEXT4' )
        text1  = CONV #( sy-msgv1 )
        text2  = CONV #( sy-msgv2 )
        text3  = CONV #( sy-msgv3 )
        text4  = CONV #( sy-msgv4 ).

  ENDMETHOD.


ENDCLASS.
