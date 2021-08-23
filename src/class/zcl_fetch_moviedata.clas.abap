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

  types:
    BEGIN OF ty_moviedata,
      custmovieid TYPE string,
      filmname    TYPE string,
      episode     TYPE string,
      swdate      TYPE string,
      headname    TYPE string,
      revenue     TYPE string,
    END OF ty_moviedata .
  types:
    tty_moviedata TYPE STANDARD TABLE OF ty_moviedata WITH NON-UNIQUE KEY custmovieid .

  data MV_LOCATION type ZSW_DATALOCATION .
  data MR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT .
  constants MC_LOCATION_LOCAL type ZSW_DATALOCATION value 'L' ##NO_TEXT.
  constants MC_LOCATION_SERVER type ZSW_DATALOCATION value 'S' ##NO_TEXT.
  constants MC_LOCALMOVIEDATA type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_excel.csv' ##NO_TEXT.
  constants MC_SERVERMOVIEDATA type STRING value 'NA' ##NO_TEXT.
  data MT_MOVIEDATA type TTY_MOVIEDATA .

  methods GET_NEXT_ID
    returning
      value(RETURN) type Z_MOVIEID .
  methods ITAB2_DDIC_MOVIEDATA .
  methods CONVERSION_BIN2XSTRING
    importing
      !IV_FILELENGTH type I
      !IT_RECORDS type SOLIX_TAB
    returning
      value(RETURN) type XSTRING .
  methods DATA_UPLOAD
    importing
      !IV_FILENAME type STRING
    returning
      value(RETURN) type STRING_TABLE .
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


  METHOD data_upload.

    DATA lt_records TYPE string_table.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
        filetype                = 'ASC'
*      IMPORTING
*       filelength              = iv_filelength
      TABLES
        data_tab                = lt_records
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
    ELSE.
      IF lt_records IS NOT INITIAL.
        Return = lt_records.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method FETCH_MOVIECHARACTER_EXL.
  endmethod.


  METHOD fetch_moviedata_exl.

    DATA lt_records TYPE string_table.

    me->mv_location = me->location_determination( ).
    CASE me->mv_location.
      WHEN me->mc_location_local.
        DATA(lv_filename) = me->mc_localmoviedata.
      WHEN OTHERS.
        lv_filename = me->mc_servermoviedata.
    ENDCASE.

    lt_records = me->data_upload( iv_filename = lv_filename ).

    DATA ls_moviedata TYPE ty_moviedata.
    DATA lt_moviedata TYPE tty_moviedata.

    LOOP AT lt_records
      INTO DATA(wa_records)
   FROM 2.
      SPLIT wa_records AT ';' INTO:
      ls_moviedata-custmovieid
      ls_moviedata-filmname
      ls_moviedata-episode
      ls_moviedata-swdate
      ls_moviedata-headname
      ls_moviedata-revenue.

      APPEND ls_moviedata TO me->mt_moviedata.

      CLEAR ls_moviedata.

    ENDLOOP.

    IF me->mt_moviedata IS NOT INITIAL.

      me->itab2_ddic_moviedata( ).

    ENDIF.

  ENDMETHOD.


  method FETCH_MOVIEORDER_EXL.
  endmethod.


  METHOD get_next_id.

    DATA lv_nr_range_nr         TYPE inri-nrrangenr VALUE '01'.
    DATA lv_object              TYPE inri-object    VALUE 'ZSW_MOVIE'.
    DATA lv_retcode             TYPE inri-returncode.
    DATA lv_number TYPE nrLEVEL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lv_nr_range_nr
        object                  = lv_object
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = lv_number
*       QUANTITY                =
        returncode              = lv_retcode
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      return = lv_number.
    ENDIF.

  ENDMETHOD.


  METHOD itab2_ddic_moviedata.

    DATA ls_movie TYPE zsw_movie.

    LOOP AT me->mt_moviedata
      INTO DATA(ls_moviedata).
      MOVE-CORRESPONDING ls_moviedata TO ls_movie.
      ls_movie-currency = 'ÚSD'.
      ls_movie-movieid = me->get_next_id( ).

      IF ls_movie-movieid IS NOT INITIAL.
        INSERT zsw_movie FROM ls_movie.
      ELSE.
        "to do logging
      ENDIF.
      CLEAR ls_movie.

    ENDLOOP.



  ENDMETHOD.


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
