"! <p class="shorttext synchronized" lang="en">Email utilities</p>
INTERFACE zif_adu_email
  PUBLIC.

  CONSTANTS:
    BEGIN OF document_type,
      binary TYPE so_obj_tp VALUE `BIN`,
      html   TYPE so_obj_tp VALUE `HTM`,
      raw    TYPE so_obj_tp VALUE `RAW`,
    END OF document_type.

  TYPES:
    BEGIN OF ts_attachment,
      type        TYPE so_obj_tp,
      subject     TYPE so_obj_des,
      size        TYPE so_obj_len,
      content_hex TYPE solix_tab,
      header      TYPE REF TO cl_bcs_objhead,
    END OF ts_attachment,
    tt_attachment TYPE STANDARD TABLE OF zif_adu_email=>ts_attachment WITH DEFAULT KEY.

  METHODS send_email
    IMPORTING
      text          TYPE soli_tab OPTIONAL
      subject       TYPE so_obj_des
      recipients    TYPE bcsy_smtpa
      attachments   TYPE zif_adu_email=>tt_attachment OPTIONAL
      document_type TYPE so_obj_tp DEFAULT zif_adu_email=>document_type-raw
      commit_work   TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      cx_send_req_bcs
      cx_address_bcs
      cx_document_bcs.

  METHODS send_from_user
    IMPORTING
      username      TYPE uname
      text          TYPE soli_tab OPTIONAL
      subject       TYPE so_obj_des
      recipients    TYPE bcsy_smtpa
      attachments   TYPE zif_adu_email=>tt_attachment OPTIONAL
      document_type TYPE so_obj_tp DEFAULT zif_adu_email=>document_type-raw
      commit_work   TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      cx_send_req_bcs
      cx_address_bcs
      cx_document_bcs.

  METHODS send_from_smtp_address
    IMPORTING
      sender        TYPE ad_smtpadr
      sender_name   TYPE ad_smtpadr OPTIONAL
      text          TYPE soli_tab OPTIONAL
      subject       TYPE so_obj_des
      recipients    TYPE bcsy_smtpa
      attachments   TYPE zif_adu_email=>tt_attachment OPTIONAL
      document_type TYPE so_obj_tp DEFAULT zif_adu_email=>document_type-raw
      commit_work   TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result) TYPE abap_bool
    RAISING
      cx_send_req_bcs
      cx_address_bcs
      cx_document_bcs.

  METHODS xstring_to_attachment
    IMPORTING
      content       TYPE xstring
      type          TYPE so_obj_tp
      subject       TYPE so_obj_des
      size          TYPE so_obj_len OPTIONAL
    RETURNING
      VALUE(result) TYPE zif_adu_email=>ts_attachment.

ENDINTERFACE.
