class ZCL_FETCH_MOVIEDATA definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT .
  methods FETCH_MOVIEDATA_EXL .
  methods START_APP .
protected section.
private section.

  data MV_LOCATION type ZSW_DATALOCATION .
  data MR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT .
  constants MC_LOCATION_LOCAL type ZSW_DATALOCATION value 'L' ##NO_TEXT.
  constants MC_LOCATION_SERVER type ZSW_DATALOCATION value 'S' ##NO_TEXT.
  constants MC_LOCALMOVIEDATA type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_excel.xlsx' ##NO_TEXT.
  constants MC_SERVERMOVIEDATA type STRING value 'NA' ##NO_TEXT.

  methods CONVERSION_BIN2XSTRING
    importing
      !IV_FILELENGTH type I
      !IT_RECORDS type SOLIX_TAB
    returning
      value(RETURN) type XSTRING .
  methods DATA_UPLOAD
    importing
      !IV_FILENAME type STRING
    exporting
      !IT_RECORDS type SOLIX_TAB
    returning
      value(IV_FILELENGTH) type I .
  methods XSTRING_2_ITAB
    importing
      !IV_HEADERXSTRING type XSTRING
      !IV_FILENAME type STRING
    exporting
      !ET_TABLE type TABLE .
  methods FETCH_MOVIEORDER_EXL .
  methods FETCH_MOVIECHARACTER_EXL .
  methods LOCATION_DETERMINATION
    returning
      value(RETURN) type ZSW_DATALOCATION .
ENDCLASS.



CLASS ZCL_FETCH_MOVIEDATA IMPLEMENTATION.


  METHOD constructor.

    me->mr_input = ir_input.

  ENDMETHOD.


  method CONVERSION_BIN2XSTRING.



CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_filelength
      IMPORTING
        buffer       = return
      TABLES
        binary_tab   = it_records
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

  endmethod.


  method DATA_UPLOAD.
        CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = iv_filelength
      TABLES
        data_tab                = it_records
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  endmethod.


  method FETCH_MOVIECHARACTER_EXL.
  endmethod.


  METHOD fetch_moviedata_exl.

    DATA lv_filelength TYPE i.
    DATA lt_records TYPE solix_tab.
    DATA lv_headerxstring TYPE xstring.


    me->mv_location = me->location_determination( ).
    CASE me->mv_location.
      WHEN me->mc_location_local.
        DATA(lv_filename) = me->mc_localmoviedata.
      WHEN OTHERS.
        lv_filename = me->mc_servermoviedata.
    ENDCASE.

    lv_filelength = me->data_upload(
                      EXPORTING
                        iv_filename = lv_filename
                     IMPORTING
                        it_records  = lt_records               " GBT: SOLIX as Table Type
                    ).

*    CALL FUNCTION 'GUI_UPLOAD'
*      EXPORTING
*        filename                = lv_filename
*        filetype                = 'BIN'
*      IMPORTING
*        filelength              = lv_filelength
*      TABLES
*        data_tab                = lt_records
*      EXCEPTIONS
*        file_open_error         = 1
*        file_read_error         = 2
*        no_batch                = 3
*        gui_refuse_filetransfer = 4
*        invalid_type            = 5
*        no_authority            = 6
*        unknown_error           = 7
*        bad_data_format         = 8
*        header_not_allowed      = 9
*        separator_not_allowed   = 10
*        header_too_long         = 11
*        unknown_dp_error        = 12
*        access_denied           = 13
*        dp_out_of_memory        = 14
*        disk_full               = 15
*        dp_timeout              = 16
*        OTHERS                  = 17.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
    lv_headerxstring = me->conversion_bin2xstring(
                         iv_filelength = lv_filelength
                         it_records    = lt_records
                       ).

*    me->xstring_2_itab(
*      EXPORTING
*        iv_headerxstring = lv_headerxstring
*        iv_filename      = lv_filename
*      IMPORTING
*        et_table         = <table>
*    ).

  ENDMETHOD.


  method FETCH_MOVIEORDER_EXL.
  endmethod.


  METHOD location_determination.
    IF me->mr_input->mv_radbut4_local EQ abap_true.
      return = me->mc_location_local.
    ELSEIF me->mr_input->mv_radbut5_server EQ abap_true.
      return = me->mc_location_server.
    ELSE.
      "no other options
    ENDIF.
  ENDMETHOD.


  METHOD start_app.

    IF me->mr_input->mv_radbut1_moviedata EQ abap_true.
      me->fetch_moviedata_exl( ).
    ELSEIF me->mr_input->mv_radbut2_movieorder EQ abap_true.
      me->fetch_movieorder_exl( ).
    ELSEIF me->mr_input->mv_radbut3_moviechar EQ abap_true.
      me->fetch_moviecharacter_exl( ).
    ELSE.
      "other options
    ENDIF.

  ENDMETHOD.


  METHOD xstring_2_itab.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .
    FIELD-SYMBOLS <gt_data> TYPE STANDARD TABLE .

    TRY .
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
                                document_name = iv_filename
                                xdocument     = iv_headerxstring ) .
      CATCH cx_fdt_excel_core.
        " Implement suitable error handling here
    ENDTRY .

    " Get List of possible Worksheets
    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
  IMPORTING
        worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.

      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                                   lv_woksheetname ).
      " now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <gt_data>.

      IF <gt_data> IS NOT INITIAL.
        et_table = <gt_data>.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
