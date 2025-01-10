"! <p class="shorttext synchronized">Serialization objects</p>
CLASS zcx_adu_oo_serialize DEFINITION
  PUBLIC
  INHERITING FROM zcx_adu_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      "! Type is empty
      BEGIN OF type_is_empty,
        msgid TYPE symsgid      VALUE 'ZADU_OO_SERIALIZE' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '001' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF type_is_empty.

    CONSTANTS:
      "! Code is empty
      BEGIN OF code_is_empty,
        msgid TYPE symsgid      VALUE 'ZADU_OO_SERIALIZE' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '002' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF code_is_empty.

    CONSTANTS:
      "! Error saving data
      BEGIN OF error_saving_data,
        msgid TYPE symsgid      VALUE 'ZADU_OO_SERIALIZE' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '003' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF error_saving_data.

    "! <p class="shorttext synchronized">Raise exception with system attribute message</p>
    "!
    "! @raising zcx_adu_oo_serialize | <p class="shorttext synchronized">Exception</p>
    CLASS-METHODS raise_system
      RAISING zcx_adu_oo_serialize.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
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



CLASS zcx_adu_oo_serialize IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid   = textid
                        text1    = text1
                        text2    = text2
                        text3    = text3
                        text4    = text4
                        previous = previous ).

  ENDMETHOD.


  METHOD raise_system.

    RAISE EXCEPTION TYPE zcx_adu_oo_serialize
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
