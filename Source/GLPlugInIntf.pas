//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  An interface unit to GLScene plug-ins.
  For more information see help file for writing plug-ins.

   History :  
   17/11/14 - PW - Renamed from PlugInIntf.pas to GLPlugInIntf.pas
   16/10/08 - UweR - Compatibility fix for Delphi 2009
  PlugIn interface stays at PAnsiChar
   02/04/07 - DaStr - Added $I GLScene.inc
   28/07/01 - EG - Creation
   
}

unit GLPlugInIntf;

interface

{$I GLScene.inc}

type
  TPIServiceType = (stRaw, stObject, stBitmap, stTexture, stImport, stExport);
  TPIServices = set of TPIServiceType;

  TEnumCallBack = procedure(Name: PAnsiChar); stdcall;

  TEnumResourceNames = procedure(Service: TPIServiceType;
    Callback: TEnumCallBack); stdcall;
  TGetServices = function: TPIServices; stdcall;
  TGetVendor = function: PAnsiChar; stdcall;
  TGetDescription = function: PAnsiChar; stdcall;
  TGetVersion = function: PAnsiChar; stdcall;

implementation

end.
