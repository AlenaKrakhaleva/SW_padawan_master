class ZCL_SW_UPLOADDATA_INPUT definition
  public
  create public

  global friends ZCL_FETCH_MOVIEDATA .

public section.

  methods CONSTRUCTOR
    importing
      !I_RADIOBUT1 type SEU_RADIOB
      !I_RADIOBUT2 type SEU_RADIOB
      !I_RADIOBUT3 type SEU_RADIOB
      !I_RADIOBUT4 type SEU_RADIOB
      !I_RADIOBUT5 type SEU_RADIOB .
protected section.
private section.

  data MV_RADBUT1_MOVIEDATA type SEU_RADIOB .
  data MV_RADBUT2_MOVIEORDER type SEU_RADIOB .
  data MV_RADBUT3_MOVIECHAR type SEU_RADIOB .
  data MV_RADBUT4_LOCAL type SEU_RADIOB .
  data MV_RADBUT5_SERVER type SEU_RADIOB .
ENDCLASS.



CLASS ZCL_SW_UPLOADDATA_INPUT IMPLEMENTATION.


  method CONSTRUCTOR.

    ME->mv_radbut1_moviedata = i_radiobut1.
    ME->mv_radbut2_movieorder = i_radiobut2.
    ME->mv_radbut3_moviechar = i_radiobut3.
    ME->mv_radbut4_local = i_radiobut4.
    ME->mv_radbut5_server = i_radiobut5.

  endmethod.
ENDCLASS.
