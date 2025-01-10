CLASS zcx_adu_static_check DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    DATA text1 TYPE string READ-ONLY.
    DATA text2 TYPE string READ-ONLY.
    DATA text3 TYPE string READ-ONLY.
    DATA text4 TYPE string READ-ONLY.

    CLASS-METHODS get_system_textid
      RETURNING VALUE(result) TYPE scx_t100key.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                text1     TYPE csequence                OPTIONAL
                text2     TYPE csequence                OPTIONAL
                text3     TYPE csequence                OPTIONAL
                text4     TYPE csequence                OPTIONAL
                !previous LIKE previous                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_adu_static_check IMPLEMENTATION.


  METHOD get_system_textid.

    result-msgid = sy-msgid.
    result-msgno = sy-msgno.
    result-attr1 = 'TEXT1'.
    result-attr2 = 'TEXT2'.
    result-attr3 = 'TEXT3'.
    result-attr4 = 'TEXT4'.

  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

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


ENDCLASS.
