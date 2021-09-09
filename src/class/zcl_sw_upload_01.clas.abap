class ZCL_SW_UPLOAD_01 definition
  public
  inheriting from ZCL_SW_UPLOAD
  final
  create private .

public section.

  class-methods FACTORY
    returning
      value(RETURN) type ref to ZCL_SW_UPLOAD_01 .

  methods START_APP
    redefinition .
protected section.

  methods DATE_CONVERSION
    redefinition .
  methods DETERMINE_NEW_BY_CUSTID
    redefinition .
  methods INSERT_DDIC
    redefinition .
  methods PREPARE_INSERT_DATA
    redefinition .
  methods PREPARE_UPDATE_DATA
    redefinition .
  methods UPDATE_DDIC
    redefinition .
  methods PRESERVING_MOVIEID
    redefinition .
private section.

  class-data MR_SINGLETON type ref to ZCL_SW_UPLOAD_01 .
  constants MC_LOCALMOVIEDATA type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_excel.csv' ##NO_TEXT.
  constants MC_SERVERMOVIEDATA type STRING value 'NA' ##NO_TEXT.
  data MT_MOVIEDATA_UPDATE type TTY_MOVIEDATA .
  data MT_MOVIEDATA_INSERT type TTY_MOVIEDATA .
  data MT_MOVIE_UPLOAD type TTY_MOVIE_UPLOAD .

  methods LOCATION_DETERMINATION
    returning
      value(RETURN) type ZSW_DATALOCATION .
  methods GET_NEXT_ID
    returning
      value(RETURN) type Z_MOVIEID .
ENDCLASS.



CLASS ZCL_SW_UPLOAD_01 IMPLEMENTATION.


  METHOD date_conversion.

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


  method DETERMINE_NEW_BY_CUSTID.
*CALL METHOD SUPER->DETERMINE_NEW_BY_CUSTID
*    .
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
  endmethod.


  METHOD factory.

    IF zcl_sw_upload_01=>mr_singleton IS INITIAL.
      zcl_sw_upload_01=>mr_singleton = NEW #( ).
      return = zcl_sw_upload_01=>mr_singleton.
    ENDIF.

  ENDMETHOD.


  METHOD GET_NEXT_ID.

    DATA lv_nr_range_nr         TYPE inri-nrrangenr VALUE '1'.
    DATA lv_object              TYPE inri-object    VALUE 'ZSW_MOVIE'.
    DATA lv_retcode             TYPE inri-returncode.
    DATA lv_number TYPE I.

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


  METHOD insert_ddic.
*CALL METHOD SUPER->INSERT_DDIC
*    .
    INSERT zsw_movie FROM TABLE me->mt_movie_upload.
    REFRESH me->mt_movie_upload[].

  ENDMETHOD.


  METHOD LOCATION_DETERMINATION.
    IF me->mr_input->mv_radbut4_local EQ abap_true.
      return = me->mc_location_local.
    ELSEIF me->mr_input->mv_radbut5_server EQ abap_true.
      return = me->mc_location_server.
    ELSE.
      "no other options
    ENDIF.
  ENDMETHOD.


  method PREPARE_INSERT_DATA.
*CALL METHOD SUPER->PREPARE_INSERT_DATA
*    .
       DATA ls_movie TYPE zsw_movie.
    LOOP AT me->mt_moviedata_insert
      INTO DATA(ls_update).
      MOVE-CORRESPONDING ls_update TO ls_movie.
      ls_movie-movieid = me->get_next_id( ).
      append ls_movie TO me->mt_movie_upload.
    ENDLOOP.

  endmethod.


  METHOD prepare_update_data.
*CALL METHOD SUPER->PREPARE_UPDATE_DATA
*    .
    DATA ls_movie TYPE zsw_movie.
    LOOP AT me->mt_moviedata_update
      INTO DATA(ls_update).
      MOVE-CORRESPONDING ls_update TO ls_movie.
      ls_movie-swdate = me->date_conversion( iv_date = ls_update-swdate ).
      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_update-custmovieid ).
      APPEND ls_movie TO me->mt_movie_upload.
    ENDLOOP.

  ENDMETHOD.


  method PRESERVING_MOVIEID.

    READ TABLE me->mt_id
         WITH KEY custmovieid = iv_custmovieid
         INTO DATA(ls_custmovieid).

     IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      Return = ls_custmovieid-movieid.
    ENDIF.

  endmethod.


  METHOD start_app.
*CALL METHOD SUPER->START_APP
*    .
    me->mr_super = ir_app.

    CASE me->mr_super->mv_location.
      WHEN me->mc_location_local.
        DATA(lv_filename) = me->mc_localmoviedata.
      WHEN OTHERS.
        lv_filename = me->mc_servermoviedata.
    ENDCASE.

    DATA(lt_records) = me->data_upload( iv_filename = lv_filename ).

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

      me->determine_new_by_custid( ).
      me->prepare_update_data( ).
      IF me->mt_movie_upload IS NOT INITIAL.
        me->update_ddic( ).
      ENDIF.
      me->prepare_insert_data( ).
      IF me->mt_movie_upload IS NOT INITIAL.
        me->insert_ddic( ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD update_ddic.
*CALL METHOD SUPER->UPDATE_DDIC
*    .
    UPDATE zsw_movie FROM TABLE me->mt_movie_upload.
    REFRESH me->mt_movie_upload[].
  ENDMETHOD.
ENDCLASS.
