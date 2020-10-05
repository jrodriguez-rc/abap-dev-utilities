"! <p class="shorttext synchronized" lang="en">Email utilities</p>
INTERFACE zif_adu_email
  PUBLIC.

  TYPES:
    BEGIN OF ts_attachment,
      type        TYPE so_obj_tp,
      subject     TYPE so_obj_des,
      size        TYPE so_obj_len,
      content_hex TYPE solix_tab,
    END OF ts_attachment,
    tt_attachment TYPE STANDARD TABLE OF ts_attachment WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ts_tag_replace,
      tag  TYPE string,
      text TYPE string,
    END OF ts_tag_replace,
    tt_tag_replace TYPE HASHED TABLE OF ts_tag_replace WITH UNIQUE KEY tag.

  METHODS read_text_body
    IMPORTING
      text          TYPE tdobname
      language      TYPE sy-langu DEFAULT sy-langu
      tag_replace   TYPE zif_adu_email=>tt_tag_replace OPTIONAL
    RETURNING
      VALUE(result) TYPE soli_tab.

  METHODS send_email
    IMPORTING
      text          TYPE soli_tab OPTIONAL
      subject       TYPE so_obj_des
      recipients    TYPE bcsy_smtpa
      attachments   TYPE zif_adu_email=>tt_attachment OPTIONAL
      commit_work   TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      cx_send_req_bcs
      cx_address_bcs
      cx_document_bcs.

  METHODS text_string_to_tab
    IMPORTING
      text          TYPE string
    RETURNING
      VALUE(result) TYPE soli_tab.

ENDINTERFACE.
