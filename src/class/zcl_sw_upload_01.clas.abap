class ZCL_SW_UPLOAD_01 definition
  public
  inheriting from ZCL_SW_UPLOAD
  final
  create private .

public section.

  class-methods FACTORY
    importing
      !IR_INPUT type ref to ZCL_SW_UPLOADDATA_INPUT
    returning
      value(RETURN) type ref to ZCL_SW_UPLOAD_01 .

  methods START_APP
    redefinition .
protected section.
private section.

  class-data MR_SINGLETON type ref to ZCL_SW_UPLOAD_01 .
  constants MC_LOCALMOVIEDATA type STRING value 'C:\Users\akrakhal\Desktop\SW_PROJECT\sw_movies_excel.csv' ##NO_TEXT.
  constants MC_SERVERMOVIEDATA type STRING value 'NA' ##NO_TEXT.

  methods LOCATION_DETERMINATION
    returning
      value(RETURN) type ZSW_DATALOCATION .
ENDCLASS.



CLASS ZCL_SW_UPLOAD_01 IMPLEMENTATION.


  METHOD factory.

    IF zcl_sw_upload_01=>mr_singleton IS INITIAL.
      zcl_sw_upload_01=>mr_singleton = NEW #( ir_input ).
      return = zcl_sw_upload_01=>mr_singleton.
    ENDIF.

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


  METHOD start_app.
*CALL METHOD SUPER->START_APP
*    .

    me->mv_location = iv_location.

    CASE me->mv_location.
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

  ENDMETHOD.
ENDCLASS.
