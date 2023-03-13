CLASS zcl_adu_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adu_texts.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zif_adu_texts.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA:
      mi_instance TYPE REF TO zif_adu_texts.

ENDCLASS.



CLASS zcl_adu_texts IMPLEMENTATION.


  METHOD constructor.

    mi_instance = me.

  ENDMETHOD.


  METHOD get_instance.

    result = COND #( WHEN mi_instance IS BOUND THEN mi_instance ELSE NEW zcl_adu_texts( ) ).

  ENDMETHOD.


  METHOD zif_adu_texts~itf_to_string.

    DATA(lv_initial) = abap_false.
    DATA(lv_length)  = 0.
    DATA(lv_string)  = ``.

    LOOP AT itf INTO DATA(ls_text_line).

      IF lv_initial = abap_false.
        lv_initial = abap_true.
      ELSEIF ls_text_line-tdformat(1) = '/' OR ls_text_line-tdformat(1) = '*'.
        lv_string = |{ lv_string }{ newline_char }|.
        lv_length = strlen( lv_string ).
      ENDIF.

      lv_string = |{ lv_string WIDTH = lv_length }{ ls_text_line-tdline }|.

      lv_length = lv_length + 132.

    ENDLOOP.

    result = lv_string.

  ENDMETHOD.


  METHOD zif_adu_texts~itf_to_text_stream.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        language    = language
      TABLES
        itf_text    = itf
        text_stream = result.

  ENDMETHOD.


  METHOD zif_adu_texts~read_standard_text.

    result =
        zif_adu_texts~itf_to_text_stream(
            itf      = zif_adu_texts~read_standard_text_itf( text        = text
                                                             language    = language
                                                             tag_replace = tag_replace )
            language = language ).

  ENDMETHOD.


  METHOD zif_adu_texts~read_standard_text_itf.

    DATA:
      lt_tlines TYPE tline_tab.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = language
        name                    = text
        object                  = 'TEXT'
      TABLES
        lines                   = lt_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 9.
    IF sy-subrc <> 0.
      RETURN. " TODO: Raise Exception
    ENDIF.

    LOOP AT tag_replace REFERENCE INTO DATA(lr_tag_replace).
      REPLACE ALL OCCURRENCES OF lr_tag_replace->tag IN TABLE lt_tlines WITH lr_tag_replace->text.
    ENDLOOP.

    result = lt_tlines.

  ENDMETHOD.


  METHOD zif_adu_texts~read_textpool.

    READ TEXTPOOL program INTO result LANGUAGE language.

  ENDMETHOD.


  METHOD zif_adu_texts~read_textpool_single.

    DATA(textpool) =
        zif_adu_texts~read_textpool(
            program  = program
            language = language ).

    TRY.
        result = textpool[ id = id key = key ]-entry.
      CATCH cx_sy_itab_line_not_found.
        CLEAR: result.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_adu_texts~text_string_to_tab.

    IF text IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = text
      TABLES
        ftext_tab = result.

  ENDMETHOD.
ENDCLASS.
