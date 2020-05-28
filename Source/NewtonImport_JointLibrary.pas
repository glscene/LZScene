{*******************************************************************************}
{                                                                               }
{      Newton Game Dynamics Delphi-Headertranslation                            }
{       Current SDK version 2.26 (Beta)                                         }
{                                                                               }
{      Copyright (c) 09,2010 Dmitriy "Executor" Bespalov                        }
{                            Stuart "Stucuk" Carey                              }
{                            Sascha Willems                                     }
{                                                                               }
{      Initial Author : Dmitriy "Executor" Bespalov                             }
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
{  See "Readme_NewtonImport.txt" for more information and detailed history      }
{                                                                               }
{*******************************************************************************}

unit NewtonImport_JointLibrary;

// Note: Declare the following in Projects->Options->Conditionals not in this unit! - Stucuk
//{$DEFINE NEWTON_DOUBLE_PRECISION} // This is needed when you want to use double precision

interface

{$I pascaldefines.inc}

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF UNIX}
  Types,
  Libc,
  Xlib,
{$ENDIF}

{$IFDEF __MACH__}
  GPCMacOSAll,
{$ENDIF}
  Classes,
  NewtonImport;

const
{$IFDEF WIN32}
   JointLibraryDLL = 'dJointLibrary.dll';
{$ENDIF}
{$IFDEF WIN64}
   JointLibraryDLL = 'dJointLibrary64.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN} // MacOS X
  //JointLibraryDLL = 'libnewton.dylib';
{$ELSE}
  //JointLibraryDLL = 'libnewton.so';
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  //JointLibraryDLL = 'libnewton';
{$ENDIF}

type
// *****************************************************************************************************************************
//
//  JointLibrary Callbacks
//
// *****************************************************************************************************************************

NewtonUserJointDestructorCallback = procedure( const me : PNewtonUserJoint ); cdecl;
PNewtonUserJointDestructorCallback = ^NewtonUserJointDestructorCallback;

NewtonUserJointSubmitConstraintCallback = procedure( const me : PNewtonUserJoint; timestep : Float; threadIndex : int ); cdecl;
PNewtonUserJointSubmitConstraintCallback = ^NewtonUserJointSubmitConstraintCallback;

BlankJointGetInfo = procedure( const me : PNewtonUserJoint; info : PNewtonJointRecord ); cdecl;
PBlankJointGetInfo = ^BlankJointGetInfo;

DGRaycastVehicleTireTransformCallback = procedure( car : PNewtonUserJoint ); cdecl;
PDGRaycastVehicleTireTransformCallback = ^DGRaycastVehicleTireTransformCallback;

// *****************************************************************************************************************************
//
// JointLibrary functions
//
// *****************************************************************************************************************************

// generic joint functions
procedure CustomDestroyJoint( const joint : PNewtonUserJoint ); cdecl; external{$IFDEF __GPC__}name 'CustomDestroyJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetNewtonJoint( const joint : PNewtonUserJoint ) : PNewtonJoint; cdecl; external{$IFDEF __GPC__}name 'CustomGetNewtonJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetJointID( const joint : PNewtonUserJoint ) : int; cdecl; external{$IFDEF __GPC__}name 'CustomGetJointID'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetJointID( const joint : PNewtonUserJoint; rttI : int ); cdecl; external{$IFDEF __GPC__}name 'CustomSetJointID'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBody0( const joint : PNewtonUserJoint ) : PNewtonBody; cdecl; external{$IFDEF __GPC__}name 'CustomGetBody0'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBody1( const joint : PNewtonUserJoint ) : PNewtonBody; cdecl; external{$IFDEF __GPC__}name 'CustomGetBody1'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetBodiesCollisionState( const joint : PNewtonUserJoint ) : int; cdecl; external{$IFDEF __GPC__}name 'CustomGetBodiesCollisionState'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetBodiesCollisionState( const joint : PNewtonUserJoint; state : int ); cdecl; external{$IFDEF __GPC__}name 'CustomSetBodiesCollisionState'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomGetUserData( const joint : PNewtonUserJoint ) : Pointer; cdecl; external{$IFDEF __GPC__}name 'CustomGetUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetUserData( const joint : PNewtonUserJoint; userData : Pointer ); cdecl; external{$IFDEF __GPC__}name 'CustomSetUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetDestructorCallback( const joint : PNewtonUserJoint; callback : NewtonUserJointDestructorCallback ); cdecl; external{$IFDEF __GPC__}name 'CustomSetDestructorCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomSetSubmitContraintCallback( const joint : PNewtonUserJoint; callback : NewtonUserJointSubmitConstraintCallback ); cdecl; external{$IFDEF __GPC__}name 'CustomSetSubmitContraintCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// this is a plain blank joint that can be used by advanced users who want to make their own joints
// but that can only use languages that can only interface with C code.
// we recommend using the CPP library to make the joints and then add a C interface, but this join is here for completion
function  CustomCreateBlankJoint( maxDof : int; const body0 : PNewtonBody; const body1 : PNewtonBody; info : BlankJointGetInfo) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CustomCreateBlankJoint'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Kinematic control joint
function CreateCustomKinematicController( const targetBody : PNewtonBody; attachmentPointInGlobalSpace : PFloat ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomKinematicController'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetPickMode( const pick : PNewtonUserJoint; mode : int); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetPickMode'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetMaxLinearFriction( const pick : PNewtonUserJoint; accel : Float ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetMaxLinearFriction'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetMaxAngularFriction( const pick : PNewtonUserJoint; alpha : Float ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetMaxAngularFriction'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetPosit( const pick : PNewtonUserJoint; posit : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetPosit'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetRotation( const pick : PNewtonUserJoint; rotation : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetRotation'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerSetTargetMatrix( const pick : PNewtonUserJoint; matrix : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerSetTargetMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomKinematicControllerGetTargetMatrix( const pick : PNewtonUserJoint; matrix : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomKinematicControllerGetTargetMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Generic 6 degree of Freedom Joint
function  CreateCustomJoint6DOF( const pinsAndPivotChildFrame : PFloat; const pinsAndPivotParentFrame : PFloat; const child : PNewtonBody; const parent : PNewtonBody ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomJoint6DOF'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetLinearLimits( customJoint6DOF : PNewtonUserJoint; const minLinearLimits : PFloat; const maxLinearLimits : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetLinearLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetAngularLimits( customJoint6DOF : PNewtonUserJoint; const minAngularLimits : PFloat; const maxAngularLimits : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetAngularLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_GetLinearLimits( customJoint6DOF : PNewtonUserJoint; minLinearLimits, maxLinearLimits : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_GetLinearLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_GetAngularLimits( customJoint6DOF : PNewtonUserJoint; minAngularLimits, maxAngularLimits : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_GetAngularLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomJoint6DOF_SetReverseUniversal( customJoint6DOF : PNewtonUserJoint; order : int ); cdecl; external{$IFDEF __GPC__}name 'CustomJoint6DOF_SetReverseUniversal'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom BallAndSocket joint with Limits
function  CreateCustomBallAndSocket( const pinsAndPivotChildFrame : PFloat; const child : PNewtonBody; const parent : PNewtonBody) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomBallAndSocket'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure BallAndSocketSetConeAngle( ballJoint : PNewtonUserJoint; angle : Float ); cdecl; external{$IFDEF __GPC__}name 'BallAndSocketSetConeAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure BallAndSocketSetTwistAngle( ballJoint : PNewtonUserJoint; minAngle, maxAngle : Float ); cdecl; external{$IFDEF __GPC__}name 'BallAndSocketSetTwistAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom Hinge joint with Limits
function  CreateCustomHinge( const pinsAndPivotChildFrame : PFloat; const child : PNewtonBody; const parent : PNewtonBody ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomHinge'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure HingeEnableLimits( hingeJoint : PNewtonUserJoint; state : int ); cdecl; external{$IFDEF __GPC__}name 'HingeEnableLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure HingeSetLimits( hingeJoint : PNewtonUserJoint; minAngle, maxAngle : Float ); cdecl; external{$IFDEF __GPC__}name 'HingeSetLimis'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Function added - Sw
function  HingeGetJointAngle (const hingeJoint : PNewtonUserJoint) : Float; cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Procedure added - Sw
procedure HingeGetPinAxis (const hingeJoint : PNewtonUserJoint; Pin : PFloat); cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
// 2.15 - Function added - Sw
function  HingeCalculateJointOmega (const hingeJoint : PNewtonUserJoint) : Float; cdecl; external{$IFDEF __GPC__}name 'HingeGetJointAngle'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// Interface for a custom Slider joint with Limits
function  CreateCustomSlider( const pinsAndPivotChildFrame : PFloat; const child : PNewtonBody; const parent : PNewtonBody ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomSlider'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure SliderEnableLimits( sliderJoint : PNewtonUserJoint; state : int ); cdecl; external{$IFDEF __GPC__}name 'SliderEnableLimits'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure SliderSetLimits( sliderJoint : PNewtonUserJoint; mindist, maxdist : Float ); cdecl; external{$IFDEF __GPC__}name 'SliderSetLimis'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// player controller functions
function  CreateCustomPlayerController( const pins : PFloat; const player : PNewtonBody; maxStairStepFactor, cushion : Float ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'CreateCustomPlayerController'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerSetVelocity( const playerController : PNewtonUserJoint; forwardSpeed, sideSpeed, heading : Float ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerSetVelocity'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerGetVisualMaTrix( const playerController : PNewtonUserJoint; matrix : PFloat ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetVisualMaTrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomPlayerControllerGetMaxSlope( const playerController : PNewtonUserJoint ) : Float; cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetMaxSlope'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure CustomPlayerControllerSetMaxSlope( const playerController : PNewtonUserJoint; maxSlopeAngleIndRadian : Float ); cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerSetMaxSlope'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  CustomPlayerControllerGetSensorShape( const playerController : PNewtonUserJoint ) : PNewtonCollision; cdecl; external{$IFDEF __GPC__}name 'CustomPlayerControllerGetSensorShape'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

// k00m (Dave Gravel simple ray cast world vehicle)
function  DGRaycastVehicleCreate( maxTireCount : int; const cordenateSytemInLocalSpace : PFloat; carBody : PNewtonBody ) : PNewtonUserJoint; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleCreate'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleAddTire( car : PNewtonUserJoint; userData : Pointer; const localPosition : PFloat; mass, radius, width, friction, suspensionLength, springConst, springDamper : Float; castMode : int ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleAddTire'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleSetTireTransformCallback( car : PNewtonUserJoint; callback : DGRaycastVehicleTireTransformCallback ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleSetTireTransformCallback'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  DGRaycastVehicleGetTiresCount( car : PNewtonUserJoint ) : int; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTiresCount'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
function  DGRaycastVehicleGetTiresUserData( car : PNewtonUserJoint; tireIndex : int ) : Pointer; cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTiresUserData'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleGetTireMatrix( car : PNewtonUserJoint; tireIndex : int; tireMatrix : PFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleGetTireMatrix'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

procedure DGRaycastVehicleInitNormalizeTireLateralForce( car : PNewtonUserJoint; pointsCount : int; piceSizeStepAxis : PFloat; normalizedForceValue : PFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleInitNormalizeTireLateralForce'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};
procedure DGRaycastVehicleInitNormalizeTireLongitudinalForce( car : PNewtonUserJoint; pointsCount : int; piceSizeStepAxis : PFloat; normalizedForceValue : PFloat ); cdecl; external{$IFDEF __GPC__}name 'DGRaycastVehicleInitNormalizeTireLongitudinalForce'{$ELSE}JointLibraryDLL{$ENDIF __GPC__};

implementation

end.
