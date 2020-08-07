FUNCTION ZADU_TRANSPORT_UPDATE_HEADER.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(HEADER) TYPE  ZADU_S_CHKTR_HEAD_UPDATE
*"----------------------------------------------------------------------
  DATA: header_db TYPE zadu_chktr_head.

  header_db = header.

  CASE header-crud_ind.
    WHEN zif_adu_constants=>crud-create.
      INSERT zadu_chktr_head FROM header_db.
      IF sy-subrc <> 0.
        MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                TYPE 'X'
                NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                WITH header_db-run_code 'ZADU_CHKTR_HEAD'.
      ENDIF.

    WHEN zif_adu_constants=>crud-update.
      UPDATE zadu_chktr_head FROM header_db.
      IF sy-subrc <> 0.
        MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                TYPE 'X'
                NUMBER zcx_adu_check_transport=>error_update_table-msgno
                WITH header_db-run_code 'ZADU_CHKTR_HEAD'.
      ENDIF.

    WHEN zif_adu_constants=>crud-delete.
      DELETE zadu_chktr_head FROM header_db.
      IF sy-subrc <> 0.
        MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                TYPE 'X'
                NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                WITH header_db-run_code 'ZADU_CHKTR_HEAD'.
      ENDIF.

  ENDCASE.

ENDFUNCTION.
