"! <p class="shorttext synchronized" lang="en">Messages Exceptions</p>
CLASS zcx_adu_messages DEFINITION
  PUBLIC
  INHERITING FROM zcx_adu_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adu_exception_dyn_info.

    CONSTANTS:
      "! &amp;1 &amp;2 (&amp;3 line &amp;4)
      BEGIN OF text_exception_message,
        msgid TYPE symsgid      VALUE 'ZADU_MESSAGES' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '001' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF text_exception_message.

    CONSTANTS:
      "! Data type not compatible.
      BEGIN OF data_type_not_compatible,
        msgid TYPE symsgid      VALUE 'ZADU_MESSAGES' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '002' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE '' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE '' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE '' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE '' ##NO_TEXT,
      END OF data_type_not_compatible.

    CONSTANTS:
      "! &amp;1&amp;2&amp;3&amp;4
      BEGIN OF free_text,
        msgid TYPE symsgid      VALUE 'ZADU_MESSAGES',
        msgno TYPE symsgno      VALUE '999',
        attr1 TYPE scx_attrname VALUE 'TEXT1',
        attr2 TYPE scx_attrname VALUE 'TEXT2',
        attr3 TYPE scx_attrname VALUE 'TEXT3',
        attr4 TYPE scx_attrname VALUE 'TEXT4',
      END OF free_text.

    "! <p class="shorttext synchronized">Raise exception with system attribute message</p>
    "!
    "! @raising zcx_adu_messages | <p class="shorttext synchronized">Exception</p>
    CLASS-METHODS raise_system
      RAISING zcx_adu_messages.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING textid     LIKE if_t100_message=>t100key OPTIONAL
                text1      TYPE string                   OPTIONAL
                text2      TYPE string                   OPTIONAL
                text3      TYPE string                   OPTIONAL
                text4      TYPE string                   OPTIONAL
                !parameter TYPE bapiret2-parameter       OPTIONAL
                !row       TYPE bapiret2-row             OPTIONAL
                !field     TYPE bapiret2-field           OPTIONAL
                !previous  LIKE previous                 OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_adu_messages IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor(
        textid   = textid
        text1    = text1
        text2    = text2
        text3    = text3
        text4    = text4
        previous = previous ).

    zif_adu_exception_dyn_info~parameter = parameter.
    zif_adu_exception_dyn_info~row       = row.
    zif_adu_exception_dyn_info~field     = field.

  ENDMETHOD.


  METHOD raise_system.

    RAISE EXCEPTION TYPE zcx_adu_messages
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
