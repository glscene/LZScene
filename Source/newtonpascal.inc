{*******************************************************************************}
{                                                                               }
{      Newton Game Dynamics Header Translation Non-Delphi Compatibility         }
{                                                                               }
{      Copyright (c) 2004-2017                                                  }
{          contributors:                                                        }
{           Stuart "Stucuk" Carey                                               }
{           Sascha Willems                                                      }
{           Dmitriy "Executor" Bespalov                                         }
{           Jon Walton                                                          }
{           Dominique Louis                                                     }
{           S.Spasov (Sury)                                                     }
{                                                                               }
{*******************************************************************************}
{                                                                               }
{ License :                                                                     }
{                                                                               }
{  The contents of this file are used with permission, subject to               }
{  the Mozilla Public License Version 1.1 (the "License"); you may              }
{  not use this file except in compliance with the License. You may             }
{  obtain a copy of the License at                                              }
{  http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                               }
{  Software distributed under the License is distributed on an                  }
{  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{  implied. See the License for the specific language governing                 }
{  rights and limitations under the License.                                    }
{                                                                               }
{*******************************************************************************}
{                                                                               }
{ Note: The code in this file is a stripped down version of the Newton          }
{       Game Dynamics Pascal Header for 2.35. It gives the 3.0 version          }
{       compatiblity with compilers other than Delphi.                          }
{                                                                               }
{*******************************************************************************}

interface

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF UNIX}
 {$IFDEF FPC}
  {$IFDEF Ver1_0}
    linux,
  {$ELSE}
    pthreads,
    baseunix,
    unix,
  {$ENDIF}
  x,
  xlib,
 {$ELSE}
  Types,
  Libc,
  Xlib,
 {$ENDIF}
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll,
{$ENDIF}
  Classes;

const
{$IFDEF __USE_DOUBLE_PRECISION__}
 NEWTON_API_PRECISION = 'd';
{$ELSE}
 NEWTON_API_PRECISION = 'f';
{$ENDIF}

{$IFDEF WIN32}
 NEWTON_API = 'newton'+NEWTON_API_PRECISION+'.dll';
{$ELSE}
 {$IFDEF UNIX}
  {$IFDEF DARWIN} // MacOS X
   NEWTON_API = 'libnewton'+NEWTON_API_PRECISION+'.dylib';
  {$ELSE}
   NEWTON_API = 'libNewton'+NEWTON_API_PRECISION+'.so';
  {$ENDIF}
 {$ELSE}
  {$IFDEF MACOS}
   NEWTON_API = 'libnewton'+NEWTON_API_PRECISION;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
{$IFDEF WIN64}
 NEWTON_API = 'newton64'+NEWTON_API_PRECISION+'.dll';
{$ENDIF}
