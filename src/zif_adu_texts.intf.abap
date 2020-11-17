INTERFACE zif_adu_texts
  PUBLIC.

  TYPES:
    BEGIN OF ts_tag_replace,
      tag  TYPE string,
      text TYPE string,
    END OF ts_tag_replace,
    tt_tag_replace TYPE HASHED TABLE OF ts_tag_replace WITH UNIQUE KEY tag.

  METHODS itf_to_text_stream
    IMPORTING
      itf           TYPE tline_tab
      language      TYPE sy-langu DEFAULT sy-langu
    RETURNING
      VALUE(result) TYPE soli_tab.

  METHODS text_string_to_tab
    IMPORTING
      !text         TYPE string
    RETURNING
      VALUE(result) TYPE soli_tab.

  METHODS read_standard_text
    IMPORTING
      text          TYPE tdobname
      language      TYPE sy-langu DEFAULT sy-langu
      tag_replace   TYPE zif_adu_texts=>tt_tag_replace OPTIONAL
    RETURNING
      VALUE(result) TYPE soli_tab.

ENDINTERFACE.
