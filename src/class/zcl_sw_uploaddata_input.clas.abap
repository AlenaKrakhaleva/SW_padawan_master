class ZCL_SW_UPLOADDATA_INPUT definition
  public
  create public

  global friends ZCL_FETCH_MOVIEDATA
                 ZCL_SW_DELETEDDIC .

public section.

  methods CONSTRUCTOR
    importing
      !I_RADIOBUT1 type SEU_RADIOB
      !I_RADIOBUT2 type SEU_RADIOB
      !I_RADIOBUT3 type SEU_RADIOB
      !I_RADIOBUT4 type SEU_RADIOB
      !I_RADIOBUT5 type SEU_RADIOB
      !I_CHECKBOX1 type RS_XFIELD optional
      !I_CHECKBOX2 type RS_XFIELD optional .
protected section.
private section.

  data MV_RADBUT1_MOVIEDATA type SEU_RADIOB .
  data MV_RADBUT2_MOVIEORDER type SEU_RADIOB .
  data MV_RADBUT3_MOVIECHAR type SEU_RADIOB .
  data MV_RADBUT4_LOCAL type SEU_RADIOB .
  data MV_RADBUT5_SERVER type SEU_RADIOB .
  data MV_CB1_ALV type RS_XFIELD .
  data MV_CB2_DEL_MOVIE type RS_XFIELD .
ENDCLASS.



CLASS ZCL_SW_UPLOADDATA_INPUT IMPLEMENTATION.


  METHOD constructor.

    me->mv_radbut1_moviedata  = i_radiobut1.
    me->mv_radbut2_movieorder = i_radiobut2.
    me->mv_radbut3_moviechar  = i_radiobut3.
    me->mv_radbut4_local      = i_radiobut4.
    me->mv_radbut5_server     = i_radiobut5.
    me->mv_cb1_alv            = i_checkbox1.
    me->mv_cb2_del_movie      = i_checkbox2.

  ENDMETHOD.
ENDCLASS.
