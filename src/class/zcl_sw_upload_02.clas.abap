class ZCL_SW_UPLOAD_02 definition
  public
  inheriting from ZCL_SW_UPLOAD
  final
  create private .

public section.

  class-methods FACTORY
    returning
      value(RETURN) type ref to ZCL_SW_UPLOAD_02 .

  methods START_APP
    redefinition .
protected section.

  methods INSERT_DDIC
    redefinition .
  methods MODIFY_DDIC_TABLE
    redefinition .
  methods UPDATE_DDIC
    redefinition .
  methods PREPARE_MODIFY_DATA
    redefinition .
private section.

  class-data MR_SINGLETON type ref to ZCL_SW_UPLOAD_02 .
  data MT_MOVIEORDER type TTY_MOVIEORDER .
  data MT_MOVIEORDER_UPLOAD type TTY_MOVIEORDER_UPLOAD .
  constants MC_LOCALMOVIEORDER type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_order_excel.csv' ##NO_TEXT.
  constants MC_SERVERMOVIEORDER type STRING value 'NA' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_SW_UPLOAD_02 IMPLEMENTATION.


  METHOD factory.

    IF zcl_sw_upload_02=>mr_singleton IS INITIAL.
      zcl_sw_upload_02=>mr_singleton = NEW #( ).
      return = zcl_sw_upload_02=>mr_singleton.
    ENDIF.

  ENDMETHOD.


  method INSERT_DDIC.
*CALL METHOD SUPER->INSERT_DDIC
*    .
* no need
  endmethod.


  METHOD modify_ddic_table.
*CALL METHOD SUPER->MODIFY_DDIC_TABLE
*    .

    MODIFY zsw_movieorder FROM TABLE me->mt_movieorder_upload.
    REFRESH me->mt_movieorder_upload[].

  ENDMETHOD.


  METHOD prepare_modify_data.
*CALL METHOD SUPER->PREPARE_MODIFY_DATA
*    .
    DATA ls_movie TYPE zsw_movieorder.
    LOOP AT me->mt_movieorder
      INTO DATA(ls_modify).
      MOVE-CORRESPONDING ls_modify TO ls_movie.
      ls_movie-movieid = me->preserving_movieid( iv_custmovieid = ls_modify-custmovieid ).
      APPEND ls_movie TO me->mt_movieorder_upload.
    ENDLOOP.

  ENDMETHOD.


  METHOD start_app.
*CALL METHOD SUPER->START_APP
**  EXPORTING
**    ir_input =
**    ir_app   =
*    .
    me->mr_super = ir_app.

    CASE me->mr_super->mv_location.
      WHEN me->mc_location_local.
        DATA(lv_filename) = me->mc_localmovieorder.
      WHEN OTHERS.
        lv_filename = me->mc_servermovieorder.
    ENDCASE.

    DATA(lt_records) = me->data_upload( iv_filename = lv_filename ).

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

      me->prepare_modify_data( ).
      IF me->mt_movieorder_upload IS NOT INITIAL.
        me->modify_ddic_table( ).
      ENDIF.
    ENDIF.
    IF me->mr_super->mr_input->mv_cb1_alv EQ abap_true.
      me->display_alv(
        CHANGING
          t_table = me->mt_movieorder
      ).
    ENDIF.
  ENDMETHOD.


  method UPDATE_DDIC.
*CALL METHOD SUPER->UPDATE_DDIC
*    .
* no need
  endmethod.
ENDCLASS.
