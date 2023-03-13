INTERFACE zif_adu_texts
  PUBLIC.

  TYPES:
    BEGIN OF ts_tag_replace,
      tag  TYPE string,
      text TYPE string,
    END OF ts_tag_replace,
    tt_tag_replace TYPE HASHED TABLE OF ts_tag_replace WITH UNIQUE KEY tag.

  CLASS-METHODS get
    RETURNING
      VALUE(result) TYPE REF TO zif_adu_texts.

  METHODS itf_to_string
    IMPORTING
      itf           TYPE tline_tab
      language      TYPE sy-langu DEFAULT sy-langu
      newline_char  TYPE abap_char1 DEFAULT cl_abap_char_utilities=>newline
    RETURNING
      VALUE(result) TYPE string.

  METHODS itf_to_text_stream
    IMPORTING
      itf           TYPE tline_tab
      language      TYPE sy-langu DEFAULT sy-langu
    RETURNING
      VALUE(result) TYPE soli_tab.

  METHODS read_standard_text
    IMPORTING
      text          TYPE tdobname
      language      TYPE sy-langu DEFAULT sy-langu
      tag_replace   TYPE zif_adu_texts=>tt_tag_replace OPTIONAL
    RETURNING
      VALUE(result) TYPE soli_tab.

  METHODS read_standard_text_itf
    IMPORTING
      text          TYPE tdobname
      language      TYPE sy-langu DEFAULT sy-langu
      tag_replace   TYPE zif_adu_texts=>tt_tag_replace OPTIONAL
    RETURNING
      VALUE(result) TYPE tline_tab.

  METHODS read_textpool
    IMPORTING
      program       TYPE program
      language      TYPE langu DEFAULT sy-langu
    RETURNING
      VALUE(result) TYPE textpool_table.

  METHODS read_textpool_single
    IMPORTING
      program       TYPE program
      id            TYPE textpoolid
      key           TYPE textpoolky
      language      TYPE langu DEFAULT sy-langu
    RETURNING
      VALUE(result) TYPE textpooltx.

  METHODS text_string_to_tab
    IMPORTING
      !text         TYPE string
    RETURNING
      VALUE(result) TYPE soli_tab.

ENDINTERFACE.
