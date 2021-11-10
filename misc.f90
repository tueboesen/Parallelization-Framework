Module mMisc
  !Tue a module for small usefull routines that doesn't fit in anywhere else, but is used through different modules
  implicit none
  interface LoadOptionalParam
    module procedure LoadOptionalParam_int, LoadOptionalParam_real, LoadOptionalParam_real2
  end interface
  contains

  Function LoadOptionalParam_int(DefaultValue,iParam)
    !Tue Dec, 2014
    !This function is used to load optional parameters
    !IO
    !DefaultValue -> A default value which we load if the optional parameter does not exist.
    !iParam       -> the optional parameter that we want to load
    implicit none
    integer, intent(in)          :: DefaultValue
    integer, intent(in),optional :: iParam
    integer                      :: LoadOptionalParam_int
    if(present(iParam)) then
        LoadOptionalParam_int=iParam
      else 
        LoadOptionalParam_int=DefaultValue
      end if
  end function LoadOptionalParam_int
  Function LoadOptionalParam_real(DefaultValue,iParam)
    !Tue Dec, 2014
    !This function is used to load optional parameters
    !IO
    !DefaultValue -> A default value which we load if the optional parameter does not exist.
    !iParam       -> the optional parameter that we want to load
    implicit none
    real*8, intent(in)          :: DefaultValue
    real*8, intent(in),optional :: iParam
    real*8                      :: LoadOptionalParam_real
    if(present(iParam)) then
        LoadOptionalParam_real=iParam
      else 
        LoadOptionalParam_real=DefaultValue
      end if
  end function LoadOptionalParam_real
  Function LoadOptionalParam_real2(DefaultValue,iParam)
    !Tue Dec, 2014
    !This function is used to load optional parameters
    !IO
    !DefaultValue -> A default value which we load if the optional parameter does not exist.
    !iParam       -> the optional parameter that we want to load
    implicit none
    integer, intent(in)          :: DefaultValue
    real*8, intent(in),optional :: iParam
    real*8                      :: LoadOptionalParam_real2
    if(present(iParam)) then
        LoadOptionalParam_real2=iParam
      else 
        LoadOptionalParam_real2=DefaultValue
      end if
  end function LoadOptionalParam_real2
end module mMisc
