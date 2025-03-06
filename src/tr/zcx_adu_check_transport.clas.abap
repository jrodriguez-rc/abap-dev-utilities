"! <p class="shorttext synchronized">Check transport Exceptions</p>
CLASS zcx_adu_check_transport DEFINITION
  PUBLIC
  INHERITING FROM zcx_adu_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      "! RFC Error &1
      BEGIN OF rfc_error,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '001' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF rfc_error.

    CONSTANTS:
      "! Error in &1
      BEGIN OF error_in,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '002' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF error_in.

    CONSTANTS:
      "! No checks executed
      BEGIN OF no_checks_executed,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '003' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF no_checks_executed.

    CONSTANTS:
      "! Error insert &1 in table &2
      BEGIN OF error_insert_table,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '004' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF error_insert_table.

    CONSTANTS:
      "! Error updating &1 in table &2
      BEGIN OF error_update_table,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '005' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF error_update_table.

    CONSTANTS:
      "! Error deleting &1 in table &2
      BEGIN OF error_delete_table,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '006' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF error_delete_table.

    CONSTANTS:
      "! Authority check &1 failed for &2
      BEGIN OF authority_check_failed,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '007' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF authority_check_failed.

    CONSTANTS:
      "! No logs found
      BEGIN OF no_logs_found,
        msgid TYPE symsgid      VALUE 'ZADU_CHECK_TRANSPORT' ##NO_TEXT,
        msgno TYPE symsgno      VALUE '008' ##NO_TEXT,
        attr1 TYPE scx_attrname VALUE 'TEXT1' ##NO_TEXT,
        attr2 TYPE scx_attrname VALUE 'TEXT2' ##NO_TEXT,
        attr3 TYPE scx_attrname VALUE 'TEXT3' ##NO_TEXT,
        attr4 TYPE scx_attrname VALUE 'TEXT4' ##NO_TEXT,
      END OF no_logs_found.

    "! <p class="shorttext synchronized">Raise exception with system attribute message</p>
    "!
    "! @raising zcx_adu_check_transport | <p class="shorttext synchronized">Check exception</p>
    CLASS-METHODS raise_system
      RAISING zcx_adu_check_transport.

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



CLASS zcx_adu_check_transport IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid   = textid
                        text1    = text1
                        text2    = text2
                        text3    = text3
                        text4    = text4
                        previous = previous ).

  ENDMETHOD.


  METHOD raise_system.

    RAISE EXCEPTION TYPE zcx_adu_check_transport
      EXPORTING
        textid = zcx_adu_static_check=>get_system_textid( )
        text1  = sy-msgv1
        text2  = sy-msgv2
        text3  = sy-msgv3
        text4  = sy-msgv4.

  ENDMETHOD.


ENDCLASS.
