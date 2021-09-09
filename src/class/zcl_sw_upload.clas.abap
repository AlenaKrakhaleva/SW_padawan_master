class ZCL_SW_UPLOAD definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  methods START_APP
    importing
      !IR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT optional
      !IR_APP type ref to ZCL_SW_UPLOAD optional .
protected section.

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
  types:
    BEGIN OF ty_single_custmovid,
      custmovieid TYPE string,
    END OF ty_single_custmovid .
  types:
    tty_single_custmovieid TYPE STANDARD TABLE OF ty_single_custmovid WITH NON-UNIQUE KEY custmovieid .
  types:
    BEGIN OF ty_movieorder,
      custmovieid     TYPE string,
      filmname        TYPE string,
      episode         TYPE string,
      theatricalorder TYPE string,
      chronorder      TYPE string,
      bestsworder     TYPE string,
    END OF ty_movieorder .
  types:
    tty_movieorder TYPE STANDARD TABLE OF ty_movieorder WITH NON-UNIQUE KEY custmovieid .
  types:
    BEGIN OF ty_id_2columns,
      custmovieid TYPE z_custmovieid,
      movieid     TYPE Z_movieid,
    END OF ty_id_2columns .
  types:
    tty_id_2columns TYPE STANDARD TABLE OF ty_id_2columns WITH NON-UNIQUE KEY custmovieid .
  types:
    tty_movieorder_upload TYPE STANDARD TABLE OF zsw_movieorder WITH NON-UNIQUE KEY custmovieid .
  types:
    tty_movie_upload TYPE STANDARD TABLE OF zsw_movie WITH NON-UNIQUE KEY custmovieid .

  data MV_LOCATION type ZSW_DATALOCATION .
  data MR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT .
  constants MC_LOCATION_LOCAL type ZSW_DATALOCATION value 'L' ##NO_TEXT.
  constants MC_LOCATION_SERVER type ZSW_DATALOCATION value 'S' ##NO_TEXT.
  data MT_MOVIEDATA type TTY_MOVIEDATA .
  data MT_ID type TTY_ID_2COLUMNS .
  data MR_SUPER type ref to ZCL_SW_UPLOAD .

  methods PRESERVING_MOVIEID
    importing
      !IV_CUSTMOVIEID type STRING
    returning
      value(RETURN) type Z_MOVIEID .
  methods INSERT_DDIC .
  methods DATE_CONVERSION
    importing
      !IV_DATE type STRING
    returning
      value(RETURN) type Z_DATE .
  methods PREPARE_INSERT_DATA .
  methods PREPARE_UPDATE_DATA .
  methods UPDATE_DDIC .
  methods DATA_UPLOAD
    importing
      !IV_FILENAME type STRING
    returning
      value(RETURN) type STRING_TABLE .
  methods DETERMINE_NEW_BY_CUSTID .
private section.

  constants MC_LOCALMOVIEORDER type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_order_excel.csv' ##NO_TEXT.
  constants MC_SERVERMOVIEORDER type STRING value 'NA' ##NO_TEXT.
  data MT_MOVIEORDER type TTY_MOVIEORDER .
  data MT_MOVIEDATA_UPDATE type TTY_MOVIEDATA .
  data MT_MOVIEDATA_INSERT type TTY_MOVIEDATA .
  data MT_MOVIE_UPLOAD type TTY_MOVIE_UPLOAD .
  data MV_CUSTMOVIEID type Z_CUSTMOVIEID .
  data MT_MOVIEORDER_UPDATE type TTY_MOVIEORDER .
  data MT_MOVIEORDER_INSERT type TTY_MOVIEORDER .
  data MT_MOVIEORDER_UPLOAD type TTY_MOVIEORDER_UPLOAD .

  methods PREPARE_INSERT_DATA_ORDER .
  methods MOVIEID_DETERMINATION .
  methods UPDATE_MOVIEORDER_DDIC .
  methods INSERT_MOVIEORDER_DDIC .
  methods DETERMINE_NEW_ORDER_BY_CUSTID .
  methods PREPARE_UPDATE_DATA_ORDER .
  methods DISPLAY_ALV
    changing
      !T_TABLE type TABLE .
  methods GET_NEXT_ID
    returning
      value(RETURN) type Z_MOVIEID .
  methods ITAB2_DDIC_MOVIEDATA .
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
  methods ITAB2_DDIC_MOVIEORDER .
ENDCLASS.



CLASS ZCL_SW_UPLOAD IMPLEMENTATION.


  METHOD CONSTRUCTOR.

*    me->mr_input = ir_input.

  ENDMETHOD.


  METHOD DATA_UPLOAD.

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


  METHOD DATE_CONVERSION.

    DATA lv_convert_date_in(10) TYPE c.
    DATA lv_convert_date_out(10) TYPE c.

    lv_convert_date_in = iv_date.


    CALL FUNCTION '/SAPDMC/LSM_DATE_CONVERT'
      EXPORTING
        date_in             = lv_convert_date_in
        date_format_in      = 'DDMY'
        to_output_format    = ' '
        to_internal_format  = 'X'
      IMPORTING
        date_out            = lv_convert_date_out
      EXCEPTIONS
        illegal_date        = 1
        illegal_date_format = 2
        no_user_date_format = 3
        OTHERS              = 4.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      Return = lv_convert_date_out.
    ENDIF.


  ENDMETHOD.


  METHOD DETERMINE_NEW_BY_CUSTID.

    " would work but it would take up a lot of time
*    LOOP AT me->mt_moviedata
*      INTO DATA(wa_moviedata).
*      SELECT SINGLE custmovieid INTO @DATA(lv_custmovieid)
*               FROM zsw_movie
*              WHERE custmovieid = @wa_moviedata-custmovieid.
*
*    ENDLOOP.

    SELECT custmovieid, movieid
      INTO TABLE @DATA(lt_custmovieid)
      FROM zsw_movie.

    DATA ls_movie TYPE zsw_movie.
    LOOP AT me->mt_moviedata
          INTO DATA(wa_moviedata).

      READ TABLE lt_custmovieid
      WITH KEY custmovieid = wa_moviedata-custmovieid
      INTO DATA(ls_custmovieid).

      IF sy-subrc EQ 0.
        APPEND ls_custmovieid TO me->mt_id.
        APPEND wa_moviedata TO me->mt_moviedata_update.
      ELSE.
        APPEND wa_moviedata TO me->mt_moviedata_insert.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD DETERMINE_NEW_ORDER_BY_CUSTID.

    SELECT custmovieid, movieid
  INTO TABLE @DATA(lt_custmovieid)
  FROM zsw_movie.

    DATA ls_movie TYPE zsw_movieorder.
    LOOP AT me->mt_movieorder
          INTO DATA(wa_moviedata).

      READ TABLE lt_custmovieid
      WITH KEY custmovieid = wa_moviedata-custmovieid
      INTO DATA(ls_custmovieid).

      IF sy-subrc EQ 0.
        APPEND ls_custmovieid TO me->mt_id.
        APPEND wa_moviedata TO me->mt_movieorder_update.
      ELSE.
        APPEND wa_moviedata TO me->mt_movieorder_insert.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method DISPLAY_ALV.

*    DATA gr_alvtable TYPE REF TO cl_salv_table.
*    DATA message TYPE REF TO cx_salv_msg.
*    TRY.
*        cl_salv_table=>factory(
*          IMPORTING
*            r_salv_table = gr_alvtable
*          CHANGING
*            t_table      = me->mt_moviedata ).
*      CATCH cx_salv_msg INTO message.
*        " error handling
*    ENDTRY.
*    CALL METHOD gr_alvtable->display.

    DATA gr_alvtable TYPE REF TO cl_salv_table.
    DATA message TYPE REF TO cx_salv_msg.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = gr_alvtable
          CHANGING
            t_table      = t_table ).
      CATCH cx_salv_msg INTO message.
        " error handling
    ENDTRY.
    CALL METHOD gr_alvtable->display.

  endmethod.


  method FETCH_MOVIECHARACTER_EXL.
  endmethod.


  METHOD FETCH_MOVIEORDER_EXL.

    DATA lt_records TYPE string_table.

    me->mv_location = me->location_determination( ).
    CASE me->mv_location.
      WHEN me->mc_location_local.
        DATA(lv_filename) = me->mc_localmovieorder.
      WHEN OTHERS.
        lv_filename = me->mc_servermovieorder.
    ENDCASE.

    lt_records = me->data_upload( iv_filename = lv_filename ).

    DATA ls_movieorder TYPE ty_movieorder.
    DATA lt_movieorder TYPE tty_movieorder.

    LOOP AT lt_records
      INTO DATA(wa_records)
   FROM 2.
      SPLIT wa_records AT ';' INTO:
      ls_movieorder-custmovieid
      ls_movieorder-filmname
      ls_movieorder-episode
      ls_movieorder-theatricalorder
      ls_movieorder-chronorder
      ls_movieorder-bestsworder.

      APPEND ls_movieorder TO me->mt_movieorder.

      CLEAR ls_movieorder.

    ENDLOOP.

    IF me->mt_movieorder IS NOT INITIAL.

      me->determine_new_order_by_custid( ).
      me->prepare_update_data_order( ).
      IF me->mt_movieorder_upload IS NOT INITIAL.
        me->update_movieorder_ddic( ).
      ENDIF.
      me->prepare_insert_data( ).
      IF me->mt_movieorder_upload IS NOT INITIAL.
        me->insert_movieorder_ddic( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NEXT_ID.

*    DATA lv_nr_range_nr         TYPE inri-nrrangenr VALUE '1'.
*    DATA lv_object              TYPE inri-object    VALUE 'ZSW_MOVIE'.
*    DATA lv_retcode             TYPE inri-returncode.
*    DATA lv_number TYPE I.
*
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr             = lv_nr_range_nr
*        object                  = lv_object
**       QUANTITY                = '1'
**       SUBOBJECT               = ' '
**       TOYEAR                  = '0000'
**       IGNORE_BUFFER           = ' '
*      IMPORTING
*        number                  = lv_number
**       QUANTITY                =
*        returncode              = lv_retcode
*      EXCEPTIONS
*        interval_not_found      = 1
*        number_range_not_intern = 2
*        object_not_found        = 3
*        quantity_is_0           = 4
*        quantity_is_not_1       = 5
*        interval_overflow       = 6
*        buffer_overflow         = 7
*        OTHERS                  = 8.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ELSE.
*      return = lv_number.
*    ENDIF.

  ENDMETHOD.


  METHOD INSERT_DDIC.

    INSERT zsw_movie FROM TABLE me->mt_movie_upload.
    REFRESH me->mt_movie_upload[].

  ENDMETHOD.


  METHOD INSERT_MOVIEORDER_DDIC.
    INSERT zsw_movieorder FROM TABLE me->mt_movieorder_upload.
    REFRESH me->mt_movieorder_upload[].
  ENDMETHOD.


  METHOD ITAB2_DDIC_MOVIEDATA.

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


  method ITAB2_DDIC_MOVIEORDER.

*    DATA ls_movieorder TYPE zsw_movieorder.
*
*    LOOP AT me->mt_movieorder
*      INTO DATA(ls_order).
*      MOVE-CORRESPONDING ls_order TO ls_movieorder.
*      ls_movieorder-movieid = me->get_next_id( ).
*
*      IF ls_movieorder-movieid IS NOT INITIAL.
*        INSERT zsw_movieorder FROM ls_movieorder.
*      ELSE.
*        "to do logging
*      ENDIF.
*      CLEAR ls_movieorder.
*
*    ENDLOOP.
    DATA ls_movie TYPE zsw_movieorder.

    LOOP AT me->mt_movieorder
      INTO DATA(ls_movieorder).
      MOVE-CORRESPONDING ls_movieorder TO ls_movie.
      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_movieorder-custmovieid ).

      IF ls_movie-movieid IS NOT INITIAL.
        INSERT zsw_movieorder FROM ls_movie.
      ELSE.
        "to do logging
      ENDIF.
      CLEAR ls_movie.

    ENDLOOP.
  endmethod.


  METHOD LOCATION_DETERMINATION.
    IF me->mr_input->mv_radbut4_local EQ abap_true.
      return = me->mc_location_local.
    ELSEIF me->mr_input->mv_radbut5_server EQ abap_true.
      return = me->mc_location_server.
    ELSE.
      "no other options
    ENDIF.
  ENDMETHOD.


  method MOVIEID_DETERMINATION.

  endmethod.


  method PREPARE_INSERT_DATA.

   DATA ls_movie TYPE zsw_movie.
    LOOP AT me->mt_moviedata_insert
      INTO DATA(ls_update).
      MOVE-CORRESPONDING ls_update TO ls_movie.
      ls_movie-movieid = me->get_next_id( ).
      append ls_movie TO me->mt_movie_upload.
    ENDLOOP.

  endmethod.


  method PREPARE_INSERT_DATA_ORDER.

     DATA ls_movie TYPE zsw_movieorder.
    LOOP AT me->mt_movieorder_insert
      INTO DATA(ls_update).
      MOVE-CORRESPONDING ls_update TO ls_movie.
      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_update-custmovieid ).
      append ls_movie TO me->mt_movieorder_upload.
    ENDLOOP.
  endmethod.


  METHOD PREPARE_UPDATE_DATA.
*    DATA ls_movie TYPE zsw_movie.
*    LOOP AT me->mt_moviedata_update
*      INTO DATA(ls_update).
*      MOVE-CORRESPONDING ls_update TO ls_movie.
*      ls_movie-swdate = me->date_conversion( iv_date = ls_update-swdate ).
*      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_update-custmovieid ).
*      append ls_movie TO me->mt_movie_upload.
*    ENDLOOP.
  ENDMETHOD.


  method PREPARE_UPDATE_DATA_ORDER.
    DATA ls_movie TYPE zsw_movieorder.
    LOOP AT me->mt_movieorder_update
      INTO DATA(ls_update).
      MOVE-CORRESPONDING ls_update TO ls_movie.
      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_update-custmovieid ).
      append ls_movie TO me->mt_movieorder_upload.
    ENDLOOP.
  endmethod.


  METHOD PRESERVING_MOVIEID.

    READ TABLE me->mt_id
         WITH KEY custmovieid = iv_custmovieid
         INTO DATA(ls_custmovieid).

     IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      Return = ls_custmovieid-movieid.
    ENDIF.

  ENDMETHOD.


  METHOD start_app.

    me->mr_input = ir_input.
    me->mr_super = ir_app.

*    DATA lt_records TYPE string_table.

   me->mv_location = me->location_determination( ).

    IF me->mr_input->mv_radbut1_moviedata EQ abap_true.

      DATA(lr_sub_01) = zcl_sw_upload_01=>factory( ).

      lr_sub_01->start_app(
        EXPORTING
*          ir_input =                  " Input data
          ir_app   =   me->mr_super              " Fetching SW data
      ).


      IF me->mr_input->mv_cb1_alv EQ abap_true.
        me->display_alv(
          CHANGING
            t_table = me->mt_moviedata
        ).
      ENDIF.

*    ELSEIF me->mr_input->mv_radbut2_movieorder EQ abap_true.
*      me->fetch_movieorder_exl( ).
*      IF me->mr_input->mv_cb1_alv EQ abap_true.
*        me->display_alv(
*          CHANGING
*            t_table = me->mt_movieorder
*        ).
*      ENDIF.
*    ELSEIF me->mr_input->mv_radbut3_moviechar EQ abap_true.
*      me->fetch_moviecharacter_exl( ).
*    ELSE.
*      "other options
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_DDIC.

    UPDATE zsw_movie FROM TABLE me->mt_movie_upload.
    REFRESH me->mt_movie_upload[].

  ENDMETHOD.


  method UPDATE_MOVIEORDER_DDIC.
       UPDATE zsw_movieorder FROM TABLE me->mt_movieorder_upload.
    REFRESH me->mt_movieorder_upload[].
  endmethod.


  METHOD XSTRING_2_ITAB.

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
