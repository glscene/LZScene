UNIT ODEImport;
{*************************************************************************
 *                                                                       *
 * Open Dynamics Engine, Copyright (C) 2001,2002 Russell L. Smith.       *
 * All rights reserved.  Email: russ@q12.org   Web: www.q12.org          *
 *                                                                       *
 * This library is free software; you can redistribute it and/or         *
 * modify it under the terms of EITHER:                                  *
 *   (1) The GNU Lesser General Public License as published by the Free  *
 *       Software Foundation; either version 2.1 of the License, or (at  *
 *       your option) any later version. The text of the GNU Lesser      *
 *       General Public License is included with this library in the     *
 *       file LICENSE.TXT.                                               *
 *   (2) The BSD-style license that is included with this library in     *
 *       the file LICENSE-BSD.TXT.                                       *
 *                                                                       *
 * This library is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
 * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
 *                                                                       *
 *************************************************************************}

{*************************************************************************
 *                                                                       *
 * ODE Delphi Import unit : 0.9.XX                                       *
 *                                                                       *
 *   Created by Mattias Fagerlund ( mattias@cambrianlabs.com )  and      *
 *              Christophe ( chroma@skynet.be ) Hosten                   *
 *                                                                       *
 *  Special thanks to Eric Grange for his help                           *
 *                                                                       *
 *  All real work was of course performed by Russell L. Smith,           *
 *    who created ODE.                                                   *
 *                                                                       *
 *  Convertion started 2002-09-16.                                       *
 *                                                                       *
 *  There is no Delphi documentation for ODE, see the original ODE files *
 *  for information http://opende.sourceforge.net/ode-docs.html          *
 *                                                                       *
 *************************************************************************}

 {CommentMarker}

 {
  Some notes;

  Sometimes it's easier and faster to refer to the members of the objects
  directly, like Body.Pos, Geom.Data or Body.Mass, instead of calling the built
  in routines. Be very careful if you do this, because some of the built in
  routines alter states when called.

  Examples

  Geom.Body := Body; // DON'T DO THIS
  bGeomSetBody(Geom, Body); // This method must be used

  Setting the mass of a body is another example. Typically, reading members is
  fine, but before writing directly to member fields yourself, you should check
  the c source and see what the function/procedure does. The source can be found
  at http://www.q12.org/ode/

  ***********************************************************************

  Change history
  2010.06.14 - YP - Updated DLLs
  2009.11.22 - DaStr - Improved Unix compatibility
                         (thanks Predator) (BugtrackerID = 2893580)
  2008.12.04 - PR - Updated to work with latest ODE from svn Rev(1606) (massive bug fixes and cleanup)
                    Breakable joints are no longer availed until patch is updated
                    ODE no longer allows multiple init's this is now checked for
                    ODE no longer allows zero mass objects
                    joint parms now have a 1 appended to them for first parm
                       example dParamLoStop is now dParamLoStop1 (old joint names remain for compatablity but should be updated

  2008.10.16 - UweR - Compatibility fix for Delphi 2009
  2008.02.06 - Mrqzzz - Upgrade to ODE 0.9 (upgrade by Paul Robello; "InitOde" and "CloseODE" in public interface declaration)
  2008.02.05 - PR - Fixes to breakable joints and class id's
  2008.01.23 - PR - Massive update for ode 0.9x

  2004.05.19 - CH - New single and double dll. Added support for the new QuickStep solver
  2004.04.25 - CH - New single and double dll. Trimesh now works in both mode.
  2004.04.22 - MF - Fixes to make DelphiODE behave better when used as dynamic
  2004.04.21 - CH - New single and double dll. Now handles Capped Cylinder vs Trimesh collision
                    Added dJointGetUniversalAngle and dJointGetUniversalAngleRate, ...
  2004.04.08 - DL - Changed calling convention of dJointCreateContact and
                    dBodySetMass to require pointers. Again for compatability reasons.
  2004.04.08 - DL - Minor compatibilit fixes (performed by MF)
  2004.04.05 - CH - New single and double dll. Now handles trimesh/trimesh collision
  2004.03.30 - DL - better crossplatform Module loading support using moduleloader.pas
  2004.03.11 - CH - New single and double dll. Now handles auto-disable and new terrain geom
  2004.03.08 - DL - Support for Delphi, Kylix, FreePascal, TMT Pascal and GnuPascal compilers
  2004.02.25 - CH - New single and double dll. Now handles breakable joints
  2004.02.18 - CH - New single and double dll
  2003.09.08 - CH - New single and double dll. Now handles cones and terrain
  2003.09.01 - EG - TriCollider bindings now dynamic to allow use of unit in Delphi5
                    (compatibility with other platforms unknow)
  2003.07.23 - CH - New single dll, now handles Plane2D
  2003.07.18 - CH - Added set and get UniversalParam, new dll deployed
  2003.07.07 - MF - DelphiODE now defaults to Single precision, because TriMesh
                    only works with Single precision. We're hoping this will be corrected in the
                    future
  2003.07.05 - CH - updated file to support new tri-collider code
  2003.06.13 - CH - Created new DLL, adding dWorldStepFast and dJointTypePlane2D
  2003.06.12 - MF - fixed Single support, which was slightly broken
  2003.06.10 - MF - Removed GeomTransformGroup as they're not in the DLL.
  2003.02.11 - JV - added syntax enforcement on some enumerated types
  2003.02.01 - CH - removed dGeomGroup and all it's procedures / functions due to deprecation
  2003.02.01 - MF - added a few new functions
  2003.01.20 - CH - compiled a new DLL and added the new functions.
  2002.10.31 - CH - compiled a new dll version, with some minor updates to friction among other things
  2002.10.10 - MF - added the functions needed to support cylinder and GeomTransformGroup.
  2002.10.10 - CH - compiled a new DLL with the new cylinder geom and the new GeomTransformGroup.
  2002.09.25 - MF - I'm having issues with the single precision DLL, it seems less stable.
                    DelphiODE will still default to double precision. The goal is single
                    precision though, so the Tri-Collider can be used.
  2002.09.24 - CH - New Single and Double precision DLLs created
  2002.09.22 - DL - Preliminary Linux support
  2002.09.16 - MF & CH - Conversion started

  MF = Mattias Fagerlund
  CH = Christophe Hosten (Chroma)
  JV = John Villar
  DL = Dominique Louis
  EG = Eric Grange
  PR = Paul Robello
  MO = Marcus Oblak (mrqzzz)
 }

{$I delphiode.inc}

interface
// remove . from line below if you are using a generic ODE DLL
{$DEFINE VanillaODE}
{$IFNDEF VanillaODE}
   {$DEFINE PARODE}
{$ENDIF}

uses
{$IFDEF __GPC__}
  system,
  gpc,
{$ENDIF}

{$IFDEF UNIX}

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


{$IFDEF __MACH__}
  GPCMacOSAll,
  {$ENDIF}
  Classes,
  GLSModuleLoader;

const
  // ********************************************************************
  // ********************************************************************
  //
  //   ODE precision:
  //
  //   ODE can be run in Single or Double precision, Single is less precise,
  //   but requires less memory.
  //
  //   If you choose to run in Single mode, you must deploy the single precision
  //   dll (this is default)
  //
  //   If you choose to run in Double mode, you must deploy the double precision
  //   dll (named ode_double.dll and located in the dll directory)

  {$define cSINGLE}  // Remove "$" from "$define" to make DelphiODE double based
  {.$define cDEBUG}

  {$IFDEF MSWINDOWS}
    {$IFDEF cSINGLE}
      {$IFDEF cDEBUG}
        ODEDLL = 'ode_singled.dll';
      {$ELSE}
        ODEDLL = 'ode_single.dll';
      {$ENDIF}
    {$ELSE}
      {$IFDEF cDEBUG}
        ODEDLL = 'ode_doubled.dll';
      {$ELSE}
        ODEDLL = 'ode_double.dll';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
  ODEDLL = 'libode.so';
  {$ENDIF}
  {$IFDEF MACOS}
  ODEDLL = 'libode.dylib';
  {$ENDIF}
  {$IFDEF DARWIN} // MacOS X
  ODEDLL = 'libode.dylib';
  {$ENDIF}


const
// enum { TRIMESH_FACE_NORMALS, TRIMESH_LAST_TRANSFORMATION };
    TRIMESH_FACE_NORMALS = 0;
    TRIMESH_LAST_TRANSFORMATION = 1;

// Just generate any contacts (disables any contact refining).
    CONTACTS_UNIMPORTANT = $80000000;

(* Change: New Type added, syntax enforcement *)
type TJointFlag = Integer;

(* These consts now have defined types *)
const
  // if this flag is set, the joint was allocated in a joint group
    dJOINT_INGROUP: TJointFlag = 1;
  // if this flag is set, the joint was attached with arguments (0,body).
  // our convention is to treat all attaches as (body,0), i.e. so node[0].body
  // is always nonzero, so this flag records the fact that the arguments were
  // swapped.
    dJOINT_REVERSE: TJointFlag = 2;
  // if this flag is set, the joint can not have just one body attached to it,
  // it must have either zero or two bodies attached.
    dJOINT_TWOBODIES: TJointFlag = 4;


(* Change: New Type added, syntax enforcement *)
type TdContactType = Integer;

(* These consts now have defined types *)
const
    dContactMu2: TdContactType		= $0001;
    dContactFDir1: TdContactType	= $0002;
    dContactBounce: TdContactType   	= $0004;
    dContactSoftERP: TdContactType  	= $0008;
    dContactSoftCFM: TdContactType  	= $0010;
    dContactMotion1: TdContactType  	= $0020;
    dContactMotion2: TdContactType	= $0040;
    dContactMotionN: TdContactType	= $0080;
    dContactSlip1: TdContactType	= $0100;
    dContactSlip2: TdContactType	= $0200;

    dContactApprox0: TdContactType	= $0000;
    dContactApprox1_1: TdContactType	= $1000;
    dContactApprox1_2: TdContactType	= $2000;
    dContactApprox1: TdContactType	= $3000;

(* Change: New Type added, syntax enforcement *)
type TBodyFlags = Integer;

(* These consts now have defined types *)
const
  dxBodyFlagFiniteRotation: TBodyFlags = 1;		  // use finite rotations
  dxBodyFlagFiniteRotationAxis: TBodyFlags = 2;	// use finite rotations only along axis
  dxBodyDisabled: TBodyFlags = 4;			          // body is disabled
  dxBodyNoGravity: TBodyFlags = 8;              // body is not influenced by gravity


type

  {$ifdef cSINGLE}
    TdReal = single;
  {$else}
    TdReal = double;
  {$endif}

  PdReal = ^TdReal;

  {define cODEDebugEnabled} // Debug mode

  // Pointers to internal ODE structures. These I haven't been able to reproduce
  // in delphi, they're C++ classes.
  PdxJointGroup = ^TdxJointGroup;
  TdJointGroupID = PdxJointGroup;

  TdxJointGroup = record
     num : integer;
     stack : pointer;
  end;

  PdxJointLimitMotor = ^TdxJointLimitMotor;
  TdxJointLimitMotor = record
     vel,fmax : TdReal;  // powered joint: velocity, max force
     lostop,histop : TdReal; // joint limits, relative to initial position
     fudge_factor : TdReal; // when powering away from joint limits
     normal_cfm : TdReal; // cfm to use when not at a stop
     stop_erp,stop_sfm : TdReal; // erp and cfm for when at joint limit
     bounce : TdReal; // restitution factor
  // variables used between getInfo1() and getInfo2()
     limit : integer; // 0=free, 1=at lo limit, 2=at hi limit
     limit_err : TdReal; // if at limit, amount over limit
  end;

  TdRealArray = array[0..15] of TdReal;
  PdRealArray = ^TdRealArray;

  // typedef dReal dVector33[4];
  // Q: Why isn't TdVector3 = array[0..2] of TdReal?
  // A: Because of SIMD alignment.
  TdVector3 = array[0..3] of TdReal;
  PdVector3 = ^TdVector3;

  Pd3Axis = ^Td3Axis;
  Td3Axis = array[0..2] of TdVector3;

  PdInteger3 = ^TdInteger3;
  TdInteger3 = array[0..2] of integer;

  PdxJointLimitMotor3 = ^TdxJointLimitMotor3;
  TdxJointLimitMotor3 = array[0..2] of TdxJointLimitMotor;

  // typedef dReal dVector4[4];
  TdVector4 = array[0..3] of TdReal;
  PdVector4 = ^TdVector4;

  // typedef dReal dMatrix3[4*3];
  TdMatrix3 = array[0..4*3-1] of TdReal;
  PdMatrix3 = ^TdMatrix3;

  TdMatrix3_As3x4 = array[0..2, 0..3] of TdReal;
  PdMatrix3_As3x4 = ^TdMatrix3_As3x4;

  // typedef dReal dMatrix4[4*4];
  TdMatrix4 = array[0..4*4-1] of TdReal;
  PdMatrix4 = ^TdMatrix4;

  // typedef dReal dMatrix6[8*6];
  TdMatrix6 = array[0..8*6-1] of TdReal;
  PdMatrix6 = ^TdMatrix6;

  // typedef dReal dQuaternion[4];
  TdQuaternion = TdVector4;//array[0..3] of TdReal;
  PdQuaternion = ^TdQuaternion;

  // No typedef for AABB
  TdAABB = array[0..5] of TdReal;


  TdMass = record
    mass : TdReal; // total mass of the rigid body
    c : TdVector4; // center of gravity position in body frame (x,y,z)
    I : TdMatrix3; // 3x3 inertia tensor in body frame, about POR
  end;
  PdMass = ^TdMass;

   TdxAutoDisable = record
      idle_time : TdReal; // time the body needs to be idle to auto-disable it
      idle_steps : integer; // steps the body needs to be idle to auto-disable it
      linear_average_threashold : TdReal;  // linear (squared) average velocity threshold
      angular_average_threashold : TdReal;  // angular (squared) average velocity threshold
      average_samples : longword; // size of the average_lvel and average_avel buffers
   end;
   TdxDampingParameters = record
      linear_scale : TdReal; // multiply the linear velocity by (1 - scale)
      angular_scale : TdReal; // multiply the angular velocity by (1 - scale)
      linear_threahold : TdReal; // linear (squared) average speed threshold
      angular_threashold : TdReal; // angular (squared) average speed threshold
   end;
   TdxContactParameters = record
      max_vel : TdReal;   // maximum correcting velocity
      min_depth : TdReal; // thickness of 'surface layer'
   end;
   TdxQuickStepParameters = record
      num_iterations : integer; // number of SOR iterations to perform
      w : TdReal; // the SOR over-relaxation parameter
   end;
  PdxGeom = ^TdxGeom;
  PPdxGeom = ^PdxGeom;

// * Whenever a body has its position or rotation changed during the
// * timestep, the callback will be called (with body as the argument).
// * Use it to know which body may need an update in an external
// * structure (like a 3D engine).
  TdMovedCallback = procedure(o1: PdxGeom); cdecl;

// * Per triangle callback. Allows the user to say if he wants a collision with
// * a particular triangle.
  TdTriCallback = function(TriMesh,RefObject : PdxGeom; TriangleIndex : integer) : integer;

// * Per object callback. Allows the user to get the list of triangles in 1
// * shot. Maybe we should remove this one.
  TdTriArrayCallback = procedure(TriMesh,RefObject : PdxGeom; TriIndices:PIntegerArray; TriCount : integer);

// * Allows the user to say if a ray collides with a triangle on barycentric
// * coords. The user can for example sample a texture with alpha transparency
// * to determine if a collision should occur.
  TdTriRayCallback = function(TriMesh,Ray : PdxGeom; TriangleIndex : integer; u,v:TdReal) : integer;


  PdxWorld = ^TdxWorld;

  PdObject = ^TdObject;
  PPdObject = ^PdObject;
  TdObject = record
    World     : PdxWorld;  // world this object is in
    next      : PdObject;	// next object of this type in list
    tome      : PPdObject;	// pointer to previous object's next ptr
    userdata  : pointer;		// user settable data
    tag       : integer;		// used by dynamics algorithms
  end;


  PdxBody = ^TdxBody;
  PdxJoint= ^TdxJoint;
  TdJointID = PdxJoint;

  {$IFDEF PARODE}
  TdJointBreakCallback = procedure(joint: TdJointID); cdecl;

  TJointBreakMode = Integer;

  PdxJointBreakInfo = ^TdxJointBreakInfo;
  TdxJointBreakInfo = record
      flags : integer;
      b1MaxF:TdVector3; // maximum force on body 1
      b1MaxT:TdVector3; // maximum torque on body 1
      b2MaxF:TdVector3; // maximum force on body 2
      b2MaxT:TdVector3; // maximum torque on body 2
      callback:TdJointBreakCallback; // function that is called when this joint breaks
   end;
  {$ENDIF}

  PdxJointNode = ^TdxJointNode;
  TdxJointNode = record
     joint : PdxJoint; // pointer to enclosing dxJoint object
     body : PdxBody;   // *other* body this joint is connected to
     next : PdxJointNode; // next node in body's list of connected joints
  end;

  // info returned by getInfo1 function. the constraint dimension is m (<=6).
  // i.e. that is the total number of rows in the jacobian. `nub' is the
  // number of unbounded variables (which have lo,hi = -/+ infinity).
  TJointInfo1 = record
     m,nub : integer;
  end;

  // info returned by getInfo2 function
  TJointInfo2 = record
    // integrator parameters: frames per second (1/stepsize), default error
    // reduction parameter (0..1).
     fps,erp : TdReal;
    // for the first and second body, pointers to two (linear and angular)
    // n*3 jacobian sub matrices, stored by rows. these matrices will have
    // been initialized to 0 on entry. if the second body is zero then the
    // J2xx pointers may be 0.
     J1l,J1a,J2l,J2a : pdReal;
    // elements to jump from one row to the next in J's
     rowskip : integer;
    // right hand sides of the equation J*v = c + cfm * lambda. cfm is the
    // "constraint force mixing" vector. c is set to zero on entry, cfm is
    // set to a constant value (typically very small or zero) value on entry.
     c,cfm : pdReal;
    // lo and hi limits for variables (set to -/+ infinity on entry).
     lo,hi : pdReal;
    // findex vector for variables. see the LCP solver interface for a
    // description of what this does. this is set to -1 on entry.
    // note that the returned indexes are relative to the first index of
    // the constraint.
     findex : pinteger;
  end;
  TdxJoint = record
     baseObject : TdObject;
     Info1 : TJointInfo1;
     Info2 : TJointInfo2;
  end;
  TdxJointNull = TdxJoint;

  PdxJointBall = ^TdxJointBall;
  TdxJointBall = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3;// anchor w.r.t first body
     anchor2 : TdVector3;// anchor w.r.t second body
     erp : TdReal; // error reduction
     cfm : TdReal; // constraint force mix in
  end;

  PdxJointHinge = ^TdxJointHinge;
  TdxJointHinge = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3; // anchor w.r.t first body
     anchor2 : TdVector3; // anchor w.r.t second body
     axis1 : TdVector3; // axis w.r.t first body
     axis2 : TdVector3; // axis w.r.t second body
     qrel : tdQuaternion;  // initial relative rotation body1 -> body2
     limot : TdxJointLimitMotor; // limit and motor information
  end;

  PdxJointUniversial = ^TdxJointUniversial;
  TdxJointUniversial = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3; // anchor w.r.t first body
     anchor2 : TdVector3; // anchor w.r.t second body
     axis1 : TdVector3; // axis w.r.t first body
     axis2 : TdVector3; // axis w.r.t second body
     qrel1 : tdQuaternion; // initial relative rotation body1 -> virtual cross piece
     qrel2 : tdQuaternion; // initial relative rotation virtual cross piece -> body2
     limot1 : TdxJointLimitMotor; // limit and motor information for axis1
     limot2 : TdxJointLimitMotor;// limit and motor information for axis2
  end;

  PdxJointPR = ^TdxJointPR;
  TdxJointPR = record
     BaseJoint : TdxJoint;

     anchor2:TdVector3;     ///< @brief Position of the rotoide articulation
                            ///<        w.r.t second body.
                            ///< @note Position of body 2 in world frame +
                            ///< anchor2 in world frame give the position
                            ///< of the rotoide articulation
     axisR1:TdVector3 ;     ///< axis of the rotoide articulation w.r.t first body.
                            ///< @note This is considered as axis1 from the parameter
                            ///< view.
     axisR2:TdVector3 ;     ///< axis of the rotoide articulation w.r.t second body.
                            ///< @note This is considered also as axis1 from the
                            ///< parameter view
     axisP1:TdVector3;      ///< axis for the prismatic articulation w.r.t first body.
                            ///< @note This is considered as axis2 in from the parameter
                            ///< view
     qrel:TdQuaternion;     ///< initial relative rotation body1 -> body2.
     offset:TdVector3;      ///< @brief vector between the body1 and the rotoide
                            ///< articulation.
                            ///<
                            ///< Going from the first to the second in the frame
                            ///<  of body1.
                            ///< That should be aligned with body1 center along axisP
                            ///< This is calculated when the axis are set.
     limotR:TdxJointLimitMotor; ///< limit and motor information for the rotoide articulation.
     limotP:TdxJointLimitMotor; ///< limit and motor information for the prismatic articulation.
  end;

  PdxJointPiston = ^TdxJointPiston;
  TdxJointPiston = record
     BaseJoint : TdxJoint;

     axis1:TdVector3;          ///< Axis of the prismatic and rotoide w.r.t first body
     axis2:TdVector3;          ///< Axis of the prismatic and rotoide w.r.t second body

     qrel:TdQuaternion;        ///< Initial relative rotation body1 -> body2

  /// Anchor w.r.t first body.
  /// This is the same as the offset for the Slider joint
  /// @note To find the position of the anchor when the body 1 has moved
  ///       you must add the position of the prismatic joint
  ///       i.e anchor = R1 * anchor1 + dJointGetPistonPosition() * (R1 * axis1)
     anchor1:TdVector3;
     anchor2:TdVector3;        //< anchor w.r.t second body

  /// limit and motor information for the prismatic
  /// part of the joint
     limotP:TdxJointLimitMotor;

  /// limit and motor information for the rotoide
  /// part of the joint
     limotR:TdxJointLimitMotor;
  end;

  PdxJointSlider = ^TdxJointSlider;
  TdxJointSlider = record
     BaseJoint : TdxJoint;
     axis1:TdVector3;		// axis w.r.t first body
     qrel:TdQuaternion;		// initial relative rotation body1 -> body2
     offset:TdVector3;		// point relative to body2 that should be
                				// aligned with body1 center along axis1
     limot:TdxJointLimitMotor;	// limit and motor information
  end;

  PdxJointHinge2 = ^TdxJointHinge2;
  TdxJointHinge2 = record
     BaseJoint : TdxJoint;

     anchor1:TdVector3 ;		// anchor w.r.t first body
     anchor2:TdVector3 ;		// anchor w.r.t second body
     axis1:TdVector3;		// axis 1 w.r.t first body
     axis2:TdVector3;		// axis 2 w.r.t second body
     c0,s0:TdReal;			// cos,sin of desired angle between axis 1,2
     v1,v2:TdVector3;		// angle ref vectors embedded in first body
     limot1:TdxJointLimitMotor;	// limit+motor info for axis 1
     limot2:TdxJointLimitMotor;	// limit+motor info for axis 2
     susp_erp,susp_cfm:TdReal;	// suspension parameters (erp,cfm)
  end;

  TdxJointAMotor = record
     BaseJoint : TdxJoint;

     num:integer;			// number of axes (0..3)
     mode:integer;			// a dAMotorXXX constant
     rel:TdInteger3;			// what the axes are relative to (global,b1,b2)
     axis:Td3Axis;		// three axes
     limot:TdxJointLimitMotor3;	// limit+motor info for axes
     angle:TdVector3;		// user-supplied angles for axes
  // these vectors are used for calculating euler angles
     reference1:TdVector3;		// original axis[2], relative to body 1
     reference2:TdVector3;		// original axis[0], relative to body 2
  end;

  TdxJointLMotor = record
     BaseJoint : TdxJoint;

     num: integer;
     rel:TdInteger3;
     axis:Td3Axis;		// three axes
     limot:TdxJointLimitMotor3;	// limit+motor info for axes
  end;

  TdxJointPlane2D = record
     BaseJoint : TdxJoint;

     row_motor_x:integer;
     row_motor_y:integer;
     row_motor_angle:integer;
     motor_x:TdxJointLimitMotor;
     motor_y:TdxJointLimitMotor;
     motor_angle:TdxJointLimitMotor;
  end;

  TdxJointFixed = record
     BaseJoint : TdxJoint;

     qrel:TdQuaternion;		// initial relative rotation body1 -> body2
     offset:TdVector3;		// relative offset between the bodies
     erp:TdReal;			// error reduction parameter
     cfm:TdReal;			// constraint force mix-in
  end;


  // position vector and rotation matrix for geometry objects that are not
  // connected to bodies.
  PdxPosR = ^TdxPosR;
  TdxPosR = record
    pos : TdVector3;
    R : TdMatrix3;
  end;

  TdxBody = record
    BaseObject : TdObject;

    {$ifdef cSINGLE}
    Padding : byte;
    {$endif}

    firstjoint : TdJointID;	// list of attached joints
    flags : integer;			  // some dxBodyFlagXXX flags
    geom : PdxGeom;          // first collision geom associated with body
    mass : TdMass;			    // mass parameters about POR
    invI : TdMatrix3 ;		  // inverse of mass.I
    invMass : TdReal;		    // 1 / mass.mass
    posr : TdxPosR;			  // position and orientation of point of reference
    q : TdQuaternion;		    // orientation quaternion
    lvel,avel : TdVector3;	// linear and angular velocity of POR
    facc,tacc : TdVector3 ;	// force and torque accululators
    finite_rot_axis : TdVector3 ;	// finite rotation axis, unit length or 0=none
    adis : TdxAutoDisable; // auto-disable parameters
    adis_timeleft : TdReal; // time left to be idle
    adis_stepsleft : integer;  // steps left to be idle
    average_lvel_buffer : pdVector3; // buffer for the linear average velocity calculation
    average_avel_buffer : pdVector3; // buffer for the angular average velocity calculation
    average_counter : longword; // counter/index to fill the average-buffers
    average_ready : integer; // indicates ( with = 1 ), if the Body's buffers are ready for average-calculations
    moved_callback : TdMovedCallback; // let the user know the body moved
    dampingp : TdxDampingParameters; // damping parameters, depends on flags
    max_angular_speed : TdReal; // limit the angular velocity to this magnitude
  end;

  TBodyList = class(TList)
  private
    function GetItems(i: integer): PdxBody;
    procedure SetItems(i: integer; const Value: PdxBody);
  public
    property Items[i : integer] : PdxBody read GetItems write SetItems; default;
    procedure DeleteAllBodies;
  end;


(*struct dxWorld : public dBase {
  dxBody *firstbody;		// body linked list
  dxJoint *firstjoint;	// joint linked list
  int nb,nj;			      // number of bodies and joints in lists
  dVector3 gravity;		  // gravity vector (m/s/s)
  dReal global_erp;		  // global error reduction parameter
  dReal global_cfm;		  // global costraint force mixing parameter
};*)

  TdxWorld = record //(TdBase)
    firstbody : PdxBody;		// body linked list
    firstjoint : PdxJoint;	// joint linked list
    nb,nj : integer;			  // number of bodies and joints in lists
    gravity : TdVector3;		// gravity vector (m/s/s)
    global_erp : TdReal;		// global error reduction parameter
    global_cfm : TdReal;		// global costraint force mixing parameter
    adis : TdxAutoDisable;
    body_flags : integer;
    qs : TdxQuickStepParameters;
    contactp : TdxContactParameters;
    dampingp : TdxDampingParameters;
    max_angular_speed : TdReal;
  end;


  TdJointFeedback = record
    f1 : TdVector3;       // force that joint applies to body 1
    t1 : TdVector3;       // torque that joint applies to body 1
    f2 : TdVector3;       // force that joint applies to body 2
    t2 : TdVector3;       // torque that joint applies to body 2
  end;

  PTdJointFeedback = ^TdJointFeedback;

  TdErrorType =
    (d_ERR_UNKNOWN, // unknown error
     d_ERR_IASSERT, // user assertion failed */
     d_ERR_UASSERT, // user assertion failed */
     d_ERR_LCP);


  TdJointTypeNumbers =
    (dJointTypeNone,		// or "unknown"
    dJointTypeBall,
    dJointTypeHinge,
    dJointTypeSlider,
    dJointTypeContact,
    dJointTypeUniversal,
    dJointTypeHinge2,
    dJointTypeFixed,
    dJointTypeNull,
    dJointTypeAMotor,
    dJointTypeLMotor,
    dJointTypePlane2D,
    dJointTypePR,
    dJointTypePU,
    dJointTypePiston
    );

  TdAngularMotorModeNumbers =
    (dAMotorUser,
     dAMotorEuler);


  TdSurfaceParameters = record
    // must always be defined
    mode : integer;
    mu : TdReal;

    // only defined if the corresponding flag is set in mode
    mu2,
    bounce,
    bounce_vel,
    soft_erp,
    soft_cfm,
    motion1,motion2,motionN,
    slip1,slip2 : TdReal
  end;

  TdContactGeom = record
    pos : TdVector3;
    normal : TdVector3;
    depth : TdReal;
    g1,g2 : PdxGeom;
    side1,side2 : integer;
  end;

  PdContactGeom = ^TdContactGeom;

  TdContact = record
    surface : TdSurfaceParameters;
    geom : TdContactGeom;
    fdir1 : TdVector3;
  end;
  PdContact = ^TdContact;

  // Collission callback structure
  TdNearCallback = procedure(data : pointer; o1, o2 : PdxGeom); cdecl;


  TdColliderFn = function(o1, o2 : PdxGeom; flags : Integer;
                  contact : PdContactGeom; skip : Integer) : Integer; cdecl;
  TdGetColliderFnFn = function(num : Integer) : TdColliderFn; cdecl;
  TdGetAABBFn = procedure(g : PdxGeom; var aabb : TdAABB); cdecl;
  TdGeomDtorFn = procedure(o : PdxGeom); cdecl;
  TdAABBTestFn = function(o1, o2 : PdxGeom; const aabb2 : TdAABB) : Integer; cdecl;

(*typedef struct dGeomClass {
  int bytes;
  dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
} dGeomClass;*)

  TdGeomClass = record
    bytes : integer;                 // extra storage size
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;       // bounding box function
    aabb_test : TdAABBTestFn; // aabb tester, can be 0 for none
    dtor : TdGeomDtorFn;      // destructor, can be 0 for none
  end;

  PdGeomClass = ^TdGeomClass;

  (*struct dxSpace : public dBase {
  int type;			// don't want to use RTTI
  virtual void destroy()=0;
  virtual void add (dGeomID)=0;
  virtual void remove (dGeomID)=0;
  virtual void collide (void *data, dNearCallback *callback)=0;
  virtual int query (dGeomID)=0;
};*)

  PdxSpace = ^TdxSpace;

  TdRealHugeArray = array[0..65535] of TdReal;
  PdRealHugeArray = ^TdRealHugeArray;

  // Tri-list collider
  TdIntegerArray = array[0..65535] of Integer;
  PdIntegerArray = ^TdIntegerArray;

  TdVector3Array = array[0..65535] of TdVector3;
  PdVector3Array = ^TdVector3Array;

(*struct dxTriMeshData{
	Model BVTree;
	MeshInterface Mesh;

    dxTriMeshData();
    ~dxTriMeshData();

    void Build(const void* Vertices, int VertexStide, int VertexCount,
	       const void* Indices, int IndexCount, int TriStride,
	       const void* Normals,
	       bool Single);

        /* aabb in model space */
        dVector3 AABBCenter;
        dVector3 AABBExtents;

    /* data for use in collison resolution */
    const void* Normals;
    Matrix4x4   last_trans;
};*)
  TdxTriMeshData = record
    unknown : byte; //
  end;
  PdxTriMeshData = ^TdxTriMeshData;

  TdxHeightfieldData = record
    m_fWidth : TdReal;
    m_fDepth : TdReal;
    m_fSampleWidth : TdReal;
    m_fSampleDepth : TdReal;
    m_fInvSampleWidth : TdReal;
    m_fInvSampleDepth : TdReal;
    m_fHalfWidth : TdReal;
    m_fHalfDepth : TdReal;
    m_fMinHeight : TdReal;
    m_fMaxHeight : TdReal;
    m_fThickness : TdReal;
    m_fScale : TdReal;
    m_fOffset : TdReal;
    m_nWidthSamples : integer;
    m_nDepthSamples : integer;
    m_bCopyHeightData : integer;
    m_bWrapMode : integer;
    m_nGetHeightMode : integer;
    m_pHeightData : pointer;
    m_pUserData : pointer;
     m_contacts : PdContactGeom;
  end;
  PdxHeightfieldData = ^TdxHeightfieldData;


(*//simple space - reports all n^2 object intersections
struct dxSimpleSpace : public dxSpace {
  dGeomID first;
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)


  PdxSimpleSpace = ^TdxSimpleSpace;

(*//currently the space 'container' is just a list of the geoms in the space.
struct dxHashSpace : public dxSpace {
  dxGeom *first;
  int global_minlevel;	// smallest hash table level to put AABBs in
  int global_maxlevel;	// objects that need a level larger than this will be
			// put in a "big objects" list instead of a hash table
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)


  PdxHashSpace = ^TdxHashSpace;

  (*typedef struct dGeomSpaceData {
  dGeomID next;
} dGeomSpaceData; *)

  TdGeomSpaceData = record
     next : PdxGeom;
  end;

  (*// common data for all geometry objects. the class-specific data area follows
  // this structure. pos and R will either point to a separately allocated
  // buffer (if body is 0 - pos points to the dxPosR object) or to the pos and
  // R of the body (if body nonzero).
  struct dxGeom {		// a dGeomID is a pointer to this
    dxGeomClass *_class;	// class of this object
    void *data;		// user data pointer
    dBodyID body;		// dynamics body associated with this object (if any)
    dReal *pos;		// pointer to object's position vector
    dReal *R;		// pointer to object's rotation matrix
    dSpaceID spaceid;	// the space this object is in
    dGeomSpaceData space;	// reserved for use by space this object is in
    dReal *space_aabb;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  };*)

  TdxGeom = record // a dGeomID is a pointer to this
     _type : integer;	// class of this object

     {$ifdef cSINGLE}
     Padding : byte;
     {$endif}
     gflags : integer;

     data : pointer;		// user data pointer
     Body : PdxBody ;		// dynamics body associated with this object (if any)
     body_next : PdxGeom; // next geom in body's linked list of associated geoms
     final_posr : PdxPosR; // final position of the geom in world coordinates
     offset_posr : PdxPosR; // offset from body in local coordinates

     next : PdxGeom;
     tome : PPdxGeom;
     parent_space : Pdxspace;
     aabb : TdAABB;
     category_bits,collide_bits : longword;
  end;

  TGeomList = class(TList)
  private
    function GetItems(i: integer): PdxGeom;
    procedure SetItems(i: integer; const Value: PdxGeom);
  public
    property Items[i : integer] : PdxGeom read GetItems write SetItems; default;
    procedure DeleteAllGeoms(DeleteDataAsObject : boolean=false);
  end;


  TdxSpace = record
    baseGeom : TdxGeom;
    count : integer;
    first : PdxGeom;
    cleanup : integer;
    current_index : integer;
    current_geom : PdxGeom;
    lock_count : integer;
  end;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxSimpleSpace = TdxSpace;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxHashSpace = record
    BaseSpace : TdxSpace;
    global_minlevel : integer;
    global_maxlevel : integer;
  end;

  TdxQuadTreeSpace = record
    BaseSpace : TdxSpace;
    Blocks : pointer;
    DirtyList : pointer;
  end;

//  TJointParams = (
// parameters for limits and motors
(* Change: New Type added, sintax enforcement *)
    TJointParams = Integer;

(* These consts now have defined types *)

{$IFDEF PARODE}
  const
     dJOINT_BREAK_UNKNOWN:TJointBreakMode =      $0000;
     dJOINT_BROKEN:TJointBreakMode =             $0001;
     dJOINT_DELETE_ON_BREAK:TJointBreakMode =    $0002;
     dJOINT_BREAK_AT_B1_FORCE:TJointBreakMode =  $0004;
     dJOINT_BREAK_AT_B1_TORQUE:TJointBreakMode = $0008;
     dJOINT_BREAK_AT_B2_FORCE:TJointBreakMode =  $0010;
     dJOINT_BREAK_AT_B2_TORQUE:TJointBreakMode = $0020;
{$ENDIF}

  const
    _priv_dParamLoStop  = $000;
    _priv_dParamLoStop2 = $100;
    _priv_dParamLoStop3 = $200;
  const
    dParamLoStop: TJointParams = _priv_dParamLoStop;
    dParamHiStop: TJointParams = _priv_dParamLoStop + 1;
    dParamVel: TJointParams = _priv_dParamLoStop + 2;
    dParamFMax: TJointParams = _priv_dParamLoStop + 3;
    dParamFudgeFactor: TJointParams = _priv_dParamLoStop + 4;
    dParamBounce: TJointParams = _priv_dParamLoStop + 5;
    dParamCFM: TJointParams = _priv_dParamLoStop + 6;
    dParamStopERP: TJointParams = _priv_dParamLoStop + 7;
    dParamStopCFM: TJointParams = _priv_dParamLoStop + 8;
    // parameters for suspension
    dParamSuspensionERP: TJointParams = _priv_dParamLoStop + 9;
    dParamSuspensionCFM: TJointParams = _priv_dParamLoStop + 10;
    dParamERP: TJointParams = _priv_dParamLoStop + 11;

    dParamGroup1: TJointParams = $000;
    dParamLoStop1: TJointParams = _priv_dParamLoStop;
    dParamHiStop1: TJointParams = _priv_dParamLoStop + 1;
    dParamVel1: TJointParams = _priv_dParamLoStop + 2;
    dParamFMax1: TJointParams = _priv_dParamLoStop + 3;
    dParamFudgeFactor1: TJointParams = _priv_dParamLoStop + 4;
    dParamBounce1: TJointParams = _priv_dParamLoStop + 5;
    dParamCFM1: TJointParams = _priv_dParamLoStop + 6;
    dParamStopERP1: TJointParams = _priv_dParamLoStop + 7;
    dParamStopCFM1: TJointParams = _priv_dParamLoStop + 8;
    // parameters for suspension
    dParamSuspensionERP1: TJointParams = _priv_dParamLoStop + 9;
    dParamSuspensionCFM1: TJointParams = _priv_dParamLoStop + 10;
    dParamERP1: TJointParams = _priv_dParamLoStop + 11;

    // SECOND AXEL
    // parameters for limits and motors
    dParamGroup2: TJointParams = $100;
    dParamLoStop2: TJointParams = _priv_dParamLoStop2;
    dParamHiStop2: TJointParams = _priv_dParamLoStop2 + 1;
    dParamVel2: TJointParams = _priv_dParamLoStop2 + 2;
    dParamFMax2: TJointParams = _priv_dParamLoStop2 + 3;
    dParamFudgeFactor2: TJointParams = _priv_dParamLoStop2 + 4;
    dParamBounce2: TJointParams = _priv_dParamLoStop2 + 5;
    dParamCFM2: TJointParams = _priv_dParamLoStop2 + 6;
    dParamStopERP2: TJointParams = _priv_dParamLoStop2 + 7;
    dParamStopCFM2: TJointParams = _priv_dParamLoStop2 + 8;
    // parameters for suspension
    dParamSuspensionERP2: TJointParams = _priv_dParamLoStop2 + 9;
    dParamSuspensionCFM2: TJointParams = _priv_dParamLoStop2 + 10;
    dParamERP2: TJointParams = _priv_dParamLoStop2 + 11;

    // THIRD AXEL
    // parameters for limits and motors
    dParamGroup3: TJointParams = $200;
    dParamLoStop3: TJointParams = _priv_dParamLoStop3;
    dParamHiStop3: TJointParams = _priv_dParamLoStop3 + 1;
    dParamVel3: TJointParams = _priv_dParamLoStop3 + 2;
    dParamFMax3: TJointParams = _priv_dParamLoStop3 + 3;
    dParamFudgeFactor3: TJointParams = _priv_dParamLoStop3 + 4;
    dParamBounce3: TJointParams = _priv_dParamLoStop3 + 5;
    dParamCFM3: TJointParams = _priv_dParamLoStop3 + 6;
    dParamStopERP3: TJointParams = _priv_dParamLoStop3 + 7;
    dParamStopCFM3: TJointParams = _priv_dParamLoStop3 + 8;
    // parameters for suspension
    dParamSuspensionERP3: TJointParams = _priv_dParamLoStop3 + 9;
    dParamSuspensionCFM3: TJointParams = _priv_dParamLoStop3 + 10;
    dParamERP3: TJointParams = _priv_dParamLoStop3 + 11;

    dParamGroup: TJointParams = $100;



  // added by PAR
   {$IFDEF PARODE}
   function dSphereGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dSphereGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dBoxGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dBoxGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dCylinderGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dCylinderGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dCapsuleGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dCapsuleGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dRayGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dRayGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dPlaneGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dPlaneGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dConvexGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dConvexGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dTriMeshGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dTriMeshGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dHeightfieldGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dHeightfieldGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dGeomTransformGetClass: integer; cdecl; external {$IFDEF __GPC__}name 'dGeomTransformGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
   {$ENDIF}

   procedure dInitODE; cdecl; external {$IFDEF __GPC__}name 'dInitODE'{$ELSE} ODEDLL{$ENDIF __GPC__};
   function dInitODE2(uiInitFlags : longword) : integer; cdecl; external {$IFDEF __GPC__}name 'dInitODE2'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dCloseODE; cdecl; external {$IFDEF __GPC__}name 'dCloseODE'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //----- dWorld -----
  function dWorldCreate: PdxWorld; cdecl; external {$IFDEF __GPC__}name 'dWorldCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldDestroy(const World: PdxWorld); cdecl; external {$IFDEF __GPC__}name 'dWorldDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetCFM(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetCFM'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetERP(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetERP'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldGetGravity(const World: PdxWorld; var gravity: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dWorldGetGravity'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetGravity(const World: PdxWorld; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetGravity'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldImpulseToForce(const World: PdxWorld; const stepsize, ix, iy, iz: TdReal; var force: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dWorldImpulseToForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetCFM(const World: PdxWorld; cfm: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetCFM'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetERP(const World: PdxWorld; erp: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetERP'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetContactMaxCorrectingVel(const World: PdxWorld; const vel: TdReal); cdecl; external {$IFDEF __GPC__}name 'WorldSetContactMaxCorrectingVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetContactMaxCorrectingVel(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetContactMaxCorrectingVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetContactSurfaceLayer(const World: PdxWorld; const depth: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetContactSurfaceLayer'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetContactSurfaceLayer(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetContactSurfaceLayer'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldExportDIF(const World: PdxWorld; fileHandle: cardinal; const world_name:PAnsiChar); cdecl; external {$IFDEF __GPC__}name 'dWorldExportDIF'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // Damping
  function dWorldGetLinearDampingThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetLinearDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetLinearDampingThreshold(const World: PdxWorld; const threshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetLinearDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAngularDampingThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAngularDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAngularDampingThreshold(const World: PdxWorld; const threshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAngularDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetLinearDamping(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetLinearDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetLinearDamping(const World: PdxWorld; const scale: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetLinearDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAngularDamping(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAngularDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAngularDamping(const World: PdxWorld; const scale: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAngularDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetDamping(const World: PdxWorld; const linear_scale,angular_scale: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetMaxAngularSpeed(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetMaxAngularSpeed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetMaxAngularSpeed(const World: PdxWorld; const max_speed: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetMaxAngularSpeed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  // Step
  procedure dWorldStep(const World: PdxWorld; const stepsize: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldStep'{$ELSE} ODEDLL{$ENDIF __GPC__};
  // QuickStep
  procedure dWorldQuickStep(const World: PdxWorld; const stepsize: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldQuickStep'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetQuickStepNumIterations(const World: PdxWorld; const num: integer); cdecl; external {$IFDEF __GPC__}name 'dWorldSetQuickStepNumIterations'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetQuickStepNumIterations(const World: PdxWorld): integer; cdecl; external {$IFDEF __GPC__}name 'dWorldGetQuickStepNumIterations'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetQuickStepW(const World: PdxWorld; const param: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetQuickStepW'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetQuickStepW(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetQuickStepW'{$ELSE} ODEDLL{$ENDIF __GPC__};
  // Stepfast
  procedure dWorldStepFast1(const World: PdxWorld; const stepsize: TdReal; const iterations: Integer); cdecl; external {$IFDEF __GPC__}name 'dWorldStepFast1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoEnableDepthSF1(const World: PdxWorld; autodepth: Integer); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoEnableDepthSF1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoEnableDepthSF1(const World: PdxWorld): Integer; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoEnableDepthSF1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  // Auto-disable functions
  function dWorldGetAutoDisableLinearAverageThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableLinearAverageThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableLinearAverageThreshold(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableLinearAverageThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableAngularAverageThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableAngularAverageThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableAngularAverageThreshold(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableAngularAverageThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableAverageSamplesCount(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableAverageSamplesCount'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableAverageSamplesCount(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableAverageSamplesCount'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dWorldGetAutoDisableLinearThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableLinearThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableLinearThreshold(const World: PdxWorld; linThreshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableLinearThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableAngularThreshold(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableAngularThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableAngularThreshold(const World: PdxWorld; angThreshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableAngularThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableSteps(const World: PdxWorld): Integer; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableSteps'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableSteps(const World: PdxWorld; steps: Integer); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableSteps'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableTime(const World: PdxWorld): TdReal; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableTime'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableTime(const World: PdxWorld; time: TdReal); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableTime'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dWorldGetAutoDisableFlag(const World: PdxWorld): Integer; cdecl; external {$IFDEF __GPC__}name 'dWorldGetAutoDisableFlag'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dWorldSetAutoDisableFlag(const World: PdxWorld; do_auto_disable: Integer); cdecl; external {$IFDEF __GPC__}name 'dWorldSetAutoDisableFlag'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //----- dBody -----
  procedure dBodyAddForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddForceAtPos'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddForceAtRelPos'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddRelForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddRelForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddRelForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddRelForceAtPos'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddRelForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddRelForceAtRelPos'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddRelTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddRelTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyAddTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodyAddTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dBodyCreate(const World: PdxWorld): PdxBody; cdecl; external {$IFDEF __GPC__}name 'dBodyCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyDestroy(const Body: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dBodyDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyDisable(const Body: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dBodyDisable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyEnable(const Body: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dBodyEnable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAngularVel(const Body: PdxBody): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAngularVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetFiniteRotationAxis(const Body: PdxBody; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyGetFiniteRotationAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetFiniteRotationMode(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetFiniteRotationMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetForce(const Body: PdxBody): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetGravityMode(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetGravityMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetJoint(const Body: PdxBody; const index: Integer): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dBodyGetJoint'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetLinearVel(const Body: PdxBody): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetLinearVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetMass(const Body: PdxBody; var mass: TdMass); cdecl; external {$IFDEF __GPC__}name 'dBodyGetMass'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetNumJoints(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetNumJoints'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetPointVel(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyGetPointVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetPosRelPoint(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyGetPosRelPoint'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetPosition(const Body: PdxBody): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetQuaternion(const Body: PdxBody): PdQuaternion; cdecl; external {$IFDEF __GPC__}name 'dBodyGetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetRelPointPos(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyGetRelPointPos'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyGetRelPointVel(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyGetRelPointVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetRotation(const Body: PdxBody): PdMatrix3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetTorque(const Body: PdxBody): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dBodyGetTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyIsEnabled(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyIsEnabled'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAngularVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAngularVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetFiniteRotationAxis(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetFiniteRotationAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetFiniteRotationMode(const Body: PdxBody; const mode: Integer); cdecl; external {$IFDEF __GPC__}name 'dBodySetFiniteRotationMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetForce(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetGravityMode(const Body: PdxBody; const mode: Integer); cdecl; external {$IFDEF __GPC__}name 'dBodySetGravityMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetLinearVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetLinearVel'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetMass( const Body: PdxBody; const mass: PdMass); cdecl; external {$IFDEF __GPC__}name 'dBodySetMass'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetPosition(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetQuaternion(const Body: PdxBody; const q: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dBodySetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetRotation(const Body: PdxBody; const R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dBodySetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetTorque(const Body: PdxBody; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyVectorFromWorld(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyVectorFromWorld'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyVectorToWorld(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyVectorToWorld'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetData (const Body: PdxBody; data : pointer); cdecl; external {$IFDEF __GPC__}name 'dBodySetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetData (const Body: PdxBody) : pointer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetMovedCallback (const Body: PdxBody; Callback : TdMovedCallback); cdecl; external {$IFDEF __GPC__}name 'dBodySetMovedCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyCopyPosition(const Body: PdxBody; const pos: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dBodyCopyPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyCopyRotation(const Body: PdxBody; const R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dBodyCopyRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodyCopyQuaternion(const Body: PdxBody; const quat: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dBodyCopyQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // damping functions
  procedure dBodySetLinearDamping (const Body: PdxBody; scale : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetLinearDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetLinearDamping (const Body: PdxBody) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodySetLinearDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAngularDamping(const Body: PdxBody; scale : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAngularDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAngularDamping(const Body: PdxBody) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAngularDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetDamping(const Body: PdxBody; linear_scale, angular_scale : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetDamping'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetLinearDampingThreshold(const Body: PdxBody) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetLinearDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetLinearDampingThreshold(const Body: PdxBody; threshold : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetLinearDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAngularDampingThreshold(const Body: PdxBody) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAngularDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAngularDampingThreshold(const Body: PdxBody; threshold : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAngularDampingThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetDampingDefaults(const Body: PdxBody; threshold : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetDampingDefaults'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetMaxAngularSpeed(const Body: PdxBody; max_speed : TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetMaxAngularSpeed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetMaxAngularSpeed(const Body: PdxBody) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetMaxAngularSpeed'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // Auto-disable functions
  function dBodyGetAutoDisableLinearThreshold(const Body: PdxBody): TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAutoDisableLinearThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableLinearThreshold(const Body: PdxBody; linThreshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableLinearThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAutoDisableAngularThreshold(const Body: PdxBody): TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAutoDisableAngularThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableAngularThreshold(const Body: PdxBody; angThreshold: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableAngularThreshold'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAutoDisableSteps(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAutoDisableSteps'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableSteps(const Body: PdxBody; steps: Integer); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableSteps'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAutoDisableTime(const Body: PdxBody): TdReal; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAutoDisableTime'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableTime(const Body: PdxBody; time: TdReal); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableTime'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dBodyGetAutoDisableFlag(const Body: PdxBody): Integer; cdecl; external {$IFDEF __GPC__}name 'dBodyGetAutoDisableFlag'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableFlag(const Body: PdxBody; do_auto_disable: Integer); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableFlag'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableDefaults(const Body: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableDefaults'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dBodySetAutoDisableAverageSamplesCount(const Body: PdxBody; average_samples_count : longword); cdecl; external {$IFDEF __GPC__}name 'dBodySetAutoDisableAverageSamplesCount'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //----- dJoint -----
  {$IFDEF PARODE}
  // breakable joints
  procedure dJointSetBreakable (const dJointID : TdJointID; b: integer); cdecl; external {$IFDEF __GPC__}name 'dJointSetBreakable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetBreakCallback (const dJointID : TdJointID; callbackFunc:TdJointBreakCallback); cdecl; external {$IFDEF __GPC__}name 'JointSetBreakable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetBreakMode (const dJointID : TdJointID; mode: integer); cdecl; external {$IFDEF __GPC__}name 'dJointSetBreakMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetBreakMode (const dJointID : TdJointID): integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetBreakMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetBreakForce(const dJointID : TdJointID; body : integer; x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetBreakForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetBreakTorque(const dJointID : TdJointID; body : integer; x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetBreakTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointIsBreakable (const dJointID : TdJointID): integer; cdecl; external {$IFDEF __GPC__}name 'dJointIsBreakable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetBreakForce(const dJointID : TdJointID; body : integer; var force: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetBreakForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetBreakTorque(const dJointID : TdJointID; body : integer; var torque: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetBreakTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};
  {$ENDIF}

  // normal joints
  procedure dJointGroupDestroy(const dJointGroupID : TdJointGroupID); cdecl; external {$IFDEF __GPC__}name 'dJointGroupDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGroupCreate(const max_size: Integer): TdJointGroupID; cdecl; external {$IFDEF __GPC__}name 'dJointGroupCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGroupEmpty(const dJointGroupID : TdJointGroupID); cdecl; external {$IFDEF __GPC__}name 'dJointGroupEmpty'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dJointAttach(const dJointID : TdJointID; const body1, body2: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dJointAttach'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointDestroy(const dJointID : TdJointID); cdecl; external {$IFDEF __GPC__}name 'dJointDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dJointGetData (const dJointID : TdJointID) : pointer; cdecl; external {$IFDEF __GPC__}name 'dJointGetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetData (const dJointID : TdJointID; data : Pointer); cdecl; external {$IFDEF __GPC__}name 'dJointSetData'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // callback routines for feedback of joints
  procedure dJointSetFeedback (const dJointID : TdJointID; Feedback : PTdJointFeedback); cdecl; external {$IFDEF __GPC__}name 'dJointSetFeedback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetFeedback (const dJointID : TdJointID) : PTdJointFeedback; cdecl; external {$IFDEF __GPC__}name 'dJointGetFeedback'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dJointGetType(const dJointID : TdJointID): Integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetType'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetBody(const dJointID : TdJointID; const index: Integer): PdxBody; cdecl; external {$IFDEF __GPC__}name 'dJointGetBody'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Contact
  function dJointCreateContact(const World : PdxWorld; dJointGroupID : TdJointGroupID; const dContact: PdContact): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateContact'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //AMotor
  function dJointCreateAMotor(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateAMotor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetAMotorAngle(const dJointID : TdJointID; const anum: Integer; const angle: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetAMotorAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorAngle(const dJointID : TdJointID; const anum: Integer): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetAMotorAxis(const dJointID : TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetAMotorAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetAMotorAxis(const dJointID : TdJointID; const anum: Integer; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetAMotorNumAxes(const dJointID : TdJointID; const num: Integer); cdecl; external {$IFDEF __GPC__}name 'dJointSetAMotorNumAxes'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorNumAxes(const dJointID : TdJointID): Integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorNumAxes'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetAMotorParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetAMotorParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetAMotorMode(const dJointID : TdJointID; const mode: TdAngularMotorModeNumbers); cdecl; external {$IFDEF __GPC__}name 'dJointSetAMotorMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorMode(const dJointID : TdJointID): Integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorMode'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddAMotorTorques (const dJointID : TdJointID; torque1, torque2, torque3: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddAMotorTorques'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorAngleRate(const dJointID : TdJointID; const anum: Integer): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorAngleRate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetAMotorAxisRel(const dJointID : TdJointID; const anum: Integer): Integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetAMotorAxisRel'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //LMotor
  function dJointCreateLMotor(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateLMotor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetLMotorAxis(const dJointID : TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetLMotorAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetLMotorAxis(const dJointID : TdJointID; const anum: Integer; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetLMotorAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetLMotorNumAxes(const dJointID : TdJointID; const num: Integer); cdecl; external {$IFDEF __GPC__}name 'dJointSetLMotorNumAxes'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetLMotorNumAxes(const dJointID : TdJointID): Integer; cdecl; external {$IFDEF __GPC__}name 'dJointGetLMotorNumAxes'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetLMotorParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetLMotorParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetLMotorParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetLMotorParam'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Ball
  function dJointCreateBall(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateBall'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetBallAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetBallAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetBallAnchor(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetBallAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetBallAnchor2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetBallAnchor2'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Hinge
  function dJointCreateHinge(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateHinge'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHingeAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHingeAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHingeAnchor(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHingeAnchor2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeAnchor2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHingeAxis(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHingeAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHingeAxis(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHingeParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHingeParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHingeParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHingeAngle(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHingeAngleRate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHingeAngleRate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddHingeTorque (const dJointID : TdJointID; torque: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddHingeTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Hinge2
  function dJointCreateHinge2(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateHinge2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHinge2Anchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHinge2Anchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHinge2Anchor(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Anchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHinge2Anchor2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Anchor2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHinge2Axis1(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHinge2Axis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHinge2Axis1(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Axis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHinge2Axis2(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHinge2Axis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetHinge2Axis2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Axis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetHinge2Param(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetHinge2Param'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHinge2Param(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Param'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHinge2Angle1(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Angle1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHinge2Angle1Rate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Angle1Rate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetHinge2Angle2Rate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetHinge2Angle2Rate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddHinge2Torques (const dJointID : TdJointID; torque1, torque2: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddHinge2Torques'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointCorrectHinge2(const dJointID : TdJointID); cdecl; external {$IFDEF __GPC__}name 'dJointCorrectHinge2'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Slider
  function dJointCreateSlider(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateSlider'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetSliderAxis(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetSliderAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetSliderAxis(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetSliderAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetSliderParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetSliderParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetSliderParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetSliderParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetSliderPosition(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetSliderPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetSliderPositionRate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetSliderPositionRate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddSliderForce (const dJointID : TdJointID; force: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddSliderForce'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Universal
  function dJointCreateUniversal(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateUniversal'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetUniversalAnchor(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetUniversalAnchor2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAnchor2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetUniversalAxis1(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetUniversalAxis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetUniversalAxis1(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAxis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetUniversalAxis2(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetUniversalAxis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetUniversalAxis2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAxis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetUniversalParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetUniversalParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetUniversalParam(const dJointID : TdJointID; const parameter: TJointParams): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetUniversalAngle1(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAngle1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetUniversalAngle2(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAngle2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetUniversalAngle1Rate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAngle1Rate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetUniversalAngle2Rate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetUniversalAngle2Rate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetUniversalAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetUniversalAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddUniversalTorques (const dJointID : TdJointID; torque1, torque2: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddUniversalTorques'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Fixed
  function dJointCreateFixed(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreateFixed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetFixed(const dJointID : TdJointID); cdecl; external {$IFDEF __GPC__}name 'dJointSetFixed'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Plane2D
  function dJointCreatePlane2D(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreatePlane2D'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPlane2DXParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPlane2DXParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPlane2DYParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPlane2DYParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPlane2DAngleParam(const dJointID : TdJointID; const parameter: Integer; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPlane2DAngleParam'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //PR
  function dJointCreatePR(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreatePR'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPRAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPRAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPRAxis1(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPRAxis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetPRAxis1(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetPRAxis1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPRAxis2(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPRAxis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetPRAxis2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetPRAxis2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPRParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPRParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPRParam(const dJointID : TdJointID; parameter: integer): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPRParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddPRTorque (const dJointID : TdJointID; torque: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddPRTorque'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Piston
  function dJointCreatePiston(const World : PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external {$IFDEF __GPC__}name 'dJointCreatePiston'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPistonAnchor(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPistonAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetPistonAnchor(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonAnchor'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetPistonAnchor2(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonAnchor2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPistonAxis(const dJointID : TdJointID; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPistonAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointGetPistonAxis(const dJointID : TdJointID; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPistonParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPistonParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPistonParam(const dJointID : TdJointID; parameter : integer): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonParam'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointSetPistonAxisDelta(const dJointID : TdJointID; const x, y, z, ax, ay, az: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetPistonAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dJointAddPistonForce (const dJointID : TdJointID; force: TdReal); cdecl; external  {$IFDEF __GPC__}name 'dJointAddPistonForce'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPistonPosition(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPistonAngle(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPistonAngleRate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonAngleRate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dJointGetPistonRate(const dJointID : TdJointID): TdReal; cdecl; external {$IFDEF __GPC__}name 'dJointGetPistonRate'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //----- dGeom -----
  function dCreateGeom (classnum : Integer) : PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dCreateGeom'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomDestroy(const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dCreateGeomClass(const classptr : TdGeomClass) : Integer; cdecl; external {$IFDEF __GPC__}name 'dCreateGeomClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetClass(const Geom : PdxGeom): Integer; cdecl; external {$IFDEF __GPC__}name 'dGeomGetClass'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetClassData(o : PdxGeom) : Pointer; cdecl; external {$IFDEF __GPC__}name 'dGeomGetClassData'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dGeomGetSpace(const Geom : PdxGeom): PdxSpace; cdecl; external {$IFDEF __GPC__}name 'dGeomGetSpace'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomIsSpace (const Geom : PdxGeom) : integer; cdecl; external {$IFDEF __GPC__}name 'dGeomIsSpace'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetBody(const Geom : PdxGeom; Body: PdxBody); cdecl; external {$IFDEF __GPC__}name 'dGeomSetBody'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetBody(const Geom : PdxGeom): PdxBody; cdecl; external {$IFDEF __GPC__}name 'dGeomGetBody'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomSetPosition(const Geom : PdxGeom; const x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomSetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetPosition(const Geom : PdxGeom): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dGeomGetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetRotation(const Geom : PdxGeom; const R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dGeomSetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetRotation(const Geom : PdxGeom): PdMatrix3; cdecl; external {$IFDEF __GPC__}name 'dGeomGetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetQuaternion(const Geom : PdxGeom; const TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomSetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomGetQuaternion(const Geom : PdxGeom; var result: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomGetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCopyPosition(const Geom : PdxGeom; const pos: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomCopyPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCopyRotation(const Geom : PdxGeom; const R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dGeomCopyRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCopyQuaternion(const Geom : PdxGeom; const quat: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomCopyQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomSetData (const Geom : PdxGeom; data : pointer); cdecl; external {$IFDEF __GPC__}name 'dGeomSetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetData (const Geom : PdxGeom) : pointer; cdecl; external {$IFDEF __GPC__}name 'dGeomGetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomEnable (const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomEnable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomDisable (const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomDisable'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomIsEnabled (const Geom : PdxGeom) : integer; cdecl; external {$IFDEF __GPC__}name 'dGeomIsEnabled'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomGetAABB(const Geom : PdxGeom; var aabb: TdAABB); cdecl; external {$IFDEF __GPC__}name 'dGeomGetAABB'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetCategoryBits (const Geom : PdxGeom; bits : Cardinal); cdecl; external {$IFDEF __GPC__}name 'dGeomSetCategoryBits'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetCategoryBits (const Geom : PdxGeom) : cardinal; cdecl; external {$IFDEF __GPC__}name 'dGeomGetCategoryBits'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetCollideBits (const Geom : PdxGeom; bits : Cardinal); cdecl; external {$IFDEF __GPC__}name 'dGeomSetCollideBits'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetCollideBits (const Geom : PdxGeom) : cardinal; cdecl; external {$IFDEF __GPC__}name 'dGeomGetCollideBits'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomSetOffsetPosition (const Geom : PdxGeom; x,y,z:TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetOffsetPosition(const Geom : PdxGeom): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dGeomGetOffsetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetOffsetRotation (const Geom : PdxGeom; R:TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomGetOffsetRotation(const Geom : PdxGeom): PdVector3; cdecl; external {$IFDEF __GPC__}name 'dGeomGetOffsetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetOffsetQuaternion (const Geom : PdxGeom; const Q:TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomGetOffsetQuaternion (const Geom : PdxGeom; var Q:TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomGetOffsetQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomClearOffset (const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomClearOffset'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetOffsetWorldPosition (const Geom : PdxGeom; x,y,z:TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetWorldPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetOffsetWorldRotation (const Geom : PdxGeom; R:TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetWorldRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetOffsetWorldQuaternion (const Geom : PdxGeom; const Q:TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dGeomSetOffsetWorldQuaternion'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCopyOffsetPosition(const Geom : PdxGeom; var pos: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomCopyOffsetPosition'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCopyOffsetRotation(const Geom : PdxGeom; var R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dGeomCopyOffsetRotation'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomIsOffset (const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomIsOffset'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Transform
  function dCreateGeomTransform(const Space : PdxSpace): PdxGeom; cdecl; external ODEDLL name 'dCreateGeomTransform';
  procedure dGeomTransformSetGeom(const Geom, obj: PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomTransformSetGeom'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTransformGetGeom(const Geom : PdxGeom): PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dGeomTransformGetGeom'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTransformSetInfo (const Geom : PdxGeom; mode : integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTransformSetInfo'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTransformGetInfo (const Geom : PdxGeom) : integer; cdecl; external {$IFDEF __GPC__}name 'dGeomTransformGetInfo'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTransformSetCleanup(const Geom : PdxGeom; const mode: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTransformSetCleanup'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTransformGetCleanup(const Geom : PdxGeom): Integer; cdecl; external {$IFDEF __GPC__}name 'dGeomTransformGetCleanup'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Box
  function dCreateBox(const Space : PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateBox';
  procedure dGeomBoxGetLengths(const Geom : PdxGeom; var result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomBoxGetLengths'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomBoxSetLengths(const Geom : PdxGeom; const lx, ly, lz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomBoxSetLengths'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomBoxPointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomBoxPointDepth'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // dCylinder (not a capped cylinder).
  function dCreateCylinder(const Space : PdxSpace; r, lz : TdReal) : PdxGeom; cdecl; external ODEDLL name 'dCreateCylinder';
  procedure dGeomCylinderSetParams (const Geom : PdxGeom; radius, length : TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomCylinderSetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCylinderGetParams (const Geom : PdxGeom; var radius, length : TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomCylinderGetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // dCapsule (a capped cylinder).
  function dCreateCapsule(const Space : PdxSpace; const radius, length: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateCapsule';
  procedure dGeomCapsuleSetParams(const Geom : PdxGeom; const radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomCapsuleSetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomCapsuleGetParams(const Geom : PdxGeom; var radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomCapsuleGetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomCapsulePointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomCapsulePointDepth'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Plane
  function dCreatePlane(const Space : PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreatePlane';
  procedure dGeomPlaneSetParams (const Geom : PdxGeom; const a, b, c, d: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomPlaneSetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomPlaneGetParams(const Geom : PdxGeom; var result: TdVector4); cdecl; external {$IFDEF __GPC__}name 'dGeomPlaneGetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomPlanePointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomPlanePointDepth'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Sphere
  function dCreateSphere(const Space : PdxSpace; const radius: TdReal): PdxGeom; cdecl; external ODEDLL name 'dCreateSphere';
  procedure dGeomSphereSetRadius(const Geom : PdxGeom; const radius: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomSphereSetRadius'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomSphereGetRadius(const Geom : PdxGeom): TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomSphereGetRadius'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomSpherePointDepth (const Geom : PdxGeom; const x,y,z : TdReal) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomSpherePointDepth'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Convex
  function dCreateConvex(const Space : PdxSpace; _planes: PdReal; _planecount: longword; _points : PdReal; _pointcount: longword; const _polygons:longword): PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dCreateConvex'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomSetConvex(const Geom: PdxGeom; _planes: PdReal; _planecount: longword; _points : PdReal; _pointcount: longword; const _polygons:longword); cdecl; external {$IFDEF __GPC__}name 'dGeomSetConvex'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Heightfield  (incomplete)
  function dCreateHeightfield(const Space : PdxSpace; Data: PdxHeightfieldData; bPlaceable:Integer): PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dCreateHeightfield'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomHeightfieldDataCreate: PdxHeightfieldData; cdecl; external {$IFDEF __GPC__}name 'dGeomHeightfieldDataCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomHeightfieldDataDestroy(Data: PdxHeightfieldData); cdecl; external {$IFDEF __GPC__}name 'dGeomHeightfieldDataDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomHeightfieldSetHeightfieldData(const Geom : PdxGeom; Data: PdxHeightfieldData); cdecl; external {$IFDEF __GPC__}name 'dGeomHeightfieldSetHeightfieldData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomHeightfieldGetHeightfieldData(const Geom : PdxGeom):PdxHeightfieldData; cdecl; external {$IFDEF __GPC__}name 'dGeomHeightfieldGetHeightfieldData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomHeightfieldDataSetBounds (Data: PdxHeightfieldData; minHeight, MaxHeight : TdReal) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomHeightfieldDataSetBounds'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //dRay
  function dCreateRay(const Space : PdxSpace; length : TdReal) : PdxGeom; cdecl; external ODEDLL name 'dCreateRay';
  procedure dGeomRaySet(const Geom : PdxGeom; px, py, pz, dx, dy, dz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomRaySet'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomRayGet(const Geom : PdxGeom; var start, dir: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomRayGet'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomRaySetLength(const Geom : PdxGeom; length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dGeomRaySetLength'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomRayGetLength(const Geom : PdxGeom) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dGeomRayGetLength'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomRaySetParams(const Geom : PdxGeom; FirstContact, BackfacCull: integer); cdecl; external {$IFDEF __GPC__}name 'dGeomRaySetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomRayGetParams(const Geom : PdxGeom; var FirstContact, BackfacCull: integer); cdecl; external {$IFDEF __GPC__}name 'dGeomRayGetParams'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomRaySetClosestHit(const Geom : PdxGeom; closestHit: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomRaySetClosestHit'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomRayGetClosestHit(const Geom : PdxGeom) : Integer; cdecl; external {$IFDEF __GPC__}name 'dGeomRayGetClosestHit'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //TriMesh
  function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback:TdTriCallback; ArrayCallback:TdTriArrayCallback; RayCallback: TdTriRayCallback): PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dCreateTriMesh'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomTriMeshSetData(g: PdxGeom; Data: PdxTriMeshData); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshSetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshGetData(g: PdxGeom):PdxTriMeshData; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetData'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshGetTriMeshDataID(g: PdxGeom) : PdxTriMeshData; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetTriMeshDataID'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataUpdate(g: PdxTriMeshData); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataUpdate'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dGeomTriMeshIsTCEnabled(g: PdxGeom; geomClass: Integer): Integer; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshIsTCEnabled'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshEnableTC(g: PdxGeom; geomClass, enable: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshEnableTC'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshClearTCCache(g: PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshClearTCCache'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dGeomTriMeshGetTriangleCount(g: PdxGeom) : integer; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetTriangleCount'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshGetTriangle(g: PdxGeom; Index: Integer; v0, v1, v2: PdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetTriangle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshGetPoint(g: PdxGeom; Index: Integer; u, v: TdReal; result: TdVector3); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetPoint'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshGetArrayCallback(g: PdxGeom): Pointer; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetArrayCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshGetRayCallback(g: PdxGeom): Pointer; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetRayCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshSetArrayCallback(g: PdxGeom; ArrayCallback: Pointer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshSetArrayCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshSetRayCallback(g: PdxGeom; RayCallback: Pointer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshSetRayCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshSetCallback(g: PdxGeom; Callback: Pointer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshSetCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshGetCallback(g: PdxGeom): Pointer; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshGetCallback'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomTriMeshDataDestroy(g: PdxTriMeshData); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dGeomTriMeshDataCreate: PdxTriMeshData; cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataSet(g: PdxTriMeshData; data_id: Integer; data: Pointer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataSet'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dGeomTriMeshDataBuildSimple(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildSimple'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataBuildSimple1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer; Normals: PdVector3Array); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildSimple1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataBuildDouble(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildDouble'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataBuildDouble1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildDouble1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataBuildSingle(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildSingle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dGeomTriMeshDataBuildSingle1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl; external {$IFDEF __GPC__}name 'dGeomTriMeshDataBuildSingle1'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dInfiniteAABB(geom : PdxGeom; var aabb : TdAABB); cdecl; external {$IFDEF __GPC__}name 'dInfiniteAABB'{$ELSE} ODEDLL{$ENDIF __GPC__};


  //----- dSpace -----
  procedure dSpaceDestroy(const Space: PdxSpace); cdecl; external {$IFDEF __GPC__}name 'dSpaceDestroy'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dSimpleSpaceCreate(Space : PdxSpace): PdxSpace; cdecl; external {$IFDEF __GPC__}name 'dSimpleSpaceCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dHashSpaceCreate(Space : PdxSpace): PdxSpace; cdecl; external {$IFDEF __GPC__}name 'dHashSpaceCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dQuadTreeSpaceCreate(const Space : PdxSpace; const Center, Extents : TdVector3; const Depth : Integer): PdxSpace; cdecl; external {$IFDEF __GPC__}name 'dQuadTreeSpaceCreate'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dSpaceAdd(const Space : PdxSpace; const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dSpaceAdd'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dSpaceRemove(const Space : PdxSpace; const Geom : PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dSpaceRemove'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dSpaceClean (const Space : PdxSpace); cdecl; external {$IFDEF __GPC__}name 'dSpaceClean'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dSpaceQuery (const Space : PdxSpace; const Geom : PdxGeom): Integer; cdecl; external {$IFDEF __GPC__}name 'dSpaceQuery'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dSpaceGetNumGeoms (const Space: PdxSpace) : integer; cdecl; external {$IFDEF __GPC__}name 'dSpaceGetNumGeoms'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dSpaceGetGeom (const Space: PdxSpace; const i: Integer) : PdxGeom; cdecl; external {$IFDEF __GPC__}name 'dSpaceGetGeom'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dHashSpaceSetLevels(const Space: PdxSpace; const minlevel, maxlevel: Integer); cdecl; external {$IFDEF __GPC__}name 'dHashSpaceSetLevels'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dHashSpaceGetLevels(const Space: PdxSpace; var minlevel, maxlevel: Integer); cdecl; external {$IFDEF __GPC__}name 'dHashSpaceGetLevels'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dSpaceSetCleanup (space : PdxSpace; const mode : integer); cdecl; external {$IFDEF __GPC__}name 'dSpaceSetCleanup'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dSpaceGetCleanup(Space : PdxSpace): integer; cdecl; external {$IFDEF __GPC__}name 'dSpaceGetCleanup'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dCollide (o1, o2 : PdxGeom; flags : integer; var Contact : TdContactGeom; Skip : integer) : integer; cdecl; external {$IFDEF __GPC__}name 'dCollide'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dSpaceCollide (const Space : PdxSpace; data : pointer; callback : TdNearCallback); cdecl; external {$IFDEF __GPC__}name 'dSpaceCollide'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dSpaceCollide2 (o1, o2 : PdxGeom; data : pointer; callback : TdNearCallback); cdecl; external {$IFDEF __GPC__}name 'dSpaceCollide2'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //----- dMass -----
  procedure dMassSetParameters(var m: TdMass; themass, cgx, cgy, cgz, I11, I22, I33, I12, I13, I23: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetParameters'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassAdd(var a,b : TdMass); cdecl; external {$IFDEF __GPC__}name 'dMassAdd'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassAdjust(var m: TdMass; newmass: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassAdjust'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassTranslate(var m: TdMass; x, y, z: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassTranslate'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassRotate(var m: TdMass; var R: TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dMassRotate'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dMassSetZero(var m: TdMass); cdecl; external {$IFDEF __GPC__}name 'dMassSetZero'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetBox(var m: TdMass; density, lx, ly, lz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetBox'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetBoxTotal(var m: TdMass; total_mass, lx, ly, lz: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetBoxTotal'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetCylinder(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetCylinder'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetCylinderTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetCylinderTotal'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetCapsule(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetCapsule'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetCapsuleTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetCapsuleTotal'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetSphere(var m: TdMass; density, radius: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetSphere'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetSphereTotal(var m: TdMass; total_mass, radius: TdReal); cdecl; external {$IFDEF __GPC__}name 'dMassSetSphereTotal'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetTrimesh(var m: TdMass; density: TdReal; trimesh:PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dMassSetTrimesh'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMassSetTrimeshTotal(var m: TdMass; total_mass: TdReal; trimesh:PdxGeom); cdecl; external {$IFDEF __GPC__}name 'dMassSetTrimeshTotal'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //----- Rotation.h -----
  procedure dQFromAxisAndAngle (var q : TdQuaternion; const ax, ay ,az, angle : TdReal); cdecl; external {$IFDEF __GPC__}name 'dQFromAxisAndAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRFromAxisAndAngle (var R : TdMatrix3; const ax, ay ,az, angle : TdReal); cdecl; external {$IFDEF __GPC__}name 'dRFromAxisAndAngle'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRSetIdentity (var R : TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dRSetIdentity'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQSetIdentity (var Q : TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dQSetIdentity'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRFromEulerAngles (var R : TdMatrix3; const phi, theta, psi : TdReal); cdecl; external {$IFDEF __GPC__}name 'dRFromEulerAngles'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRFrom2Axes (var R: TdMatrix3; const ax, ay, az, bx, by, bz : TdReal); cdecl; external {$IFDEF __GPC__}name 'dRFrom2Axes'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRFromZAxis (var R: TdMatrix3; const ax, ay, az : TdReal); cdecl; external {$IFDEF __GPC__}name 'dRFromZAxis'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dMultiply0 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external {$IFDEF __GPC__}name 'dMultiply0'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMultiply1 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external {$IFDEF __GPC__}name 'dMultiply1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMultiply2 (const A : PdReal; const B, C : PdReal; p, q, r : integer); cdecl; external {$IFDEF __GPC__}name 'dMultiply2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQMultiply0 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dQMultiply0'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQMultiply1 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dQMultiply1'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQMultiply2 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dQMultiply2'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQMultiply3 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dQMultiply3'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRfromQ (var R : TdMatrix3; const q : TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dRfromQ'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dQfromR (var q : TdQuaternion; const R : TdMatrix3); cdecl; external {$IFDEF __GPC__}name 'dQfromR'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dDQfromW (var dq : TdVector4; const w : TdVector3; const q: TdQuaternion); cdecl; external {$IFDEF __GPC__}name 'dDQfromW'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //----- Math -----
  procedure dNormalize3 (var a : TdVector3); cdecl; external {$IFDEF __GPC__}name 'dNormalize3'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dNormalize4 (var a : TdVector4); cdecl; external {$IFDEF __GPC__}name 'dNormalize4'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //----- Misc -----
  procedure dClosestLineSegmentPoints (const a1, a2, b1, b2 : TdVector3; var cp1, cp2 : TdVector3); cdecl; external {$IFDEF __GPC__}name 'dClosestLineSegmentPoints'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dBoxTouchesBox (const _p1 : TdVector3; const R1 : TdMatrix3; const side1 : TdVector3; const _p2 : TdVector3; const R2 : TdMatrix3; const side2 : TdVector3) : integer; cdecl; external {$IFDEF __GPC__}name 'dBoxTouchesBox'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dMaxDifference (A, B : PdReal; n, m : integer) : TdReal; cdecl; external {$IFDEF __GPC__}name 'dMaxDifference'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dMakeRandomVector(var n1 : TdVector3; a : integer; f : TdReal); cdecl; external {$IFDEF __GPC__}name 'dMakeRandomVector'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dAreConnected (a, b : PdxBody) : integer; cdecl; external {$IFDEF __GPC__}name 'dAreConnected'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dAreConnectedExcluding (a, b : PdxBody; joint_type : TdJointTypeNumbers) : integer; cdecl; external {$IFDEF __GPC__}name 'dAreConnectedExcluding'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dMakeRandomMatrix (A : PdRealArray; n, m : integer; range :  TdReal); cdecl; external {$IFDEF __GPC__}name 'dMakeRandomMatrix'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dClearUpperTriangle (A : PdRealArray; n : integer); cdecl; external {$IFDEF __GPC__}name 'dClearUpperTriangle'{$ELSE} ODEDLL{$ENDIF __GPC__};

  function dRandGetSeed: Cardinal; cdecl; external {$IFDEF __GPC__}name 'dRandGetSeed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dRandSetSeed (const s: Cardinal); cdecl; external {$IFDEF __GPC__}name 'dRandSetSeed'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dRandInt (const n: Integer): Integer; cdecl; external {$IFDEF __GPC__}name 'dRandInt'{$ELSE} ODEDLL{$ENDIF __GPC__};
  function dRandReal: TdReal; cdecl; external {$IFDEF __GPC__}name 'dRandReal'{$ELSE} ODEDLL{$ENDIF __GPC__};

  // return 1 if the random number generator is working.
  function dTestRand: Integer; cdecl; external {$IFDEF __GPC__}name 'dTestRand'{$ELSE} ODEDLL{$ENDIF __GPC__};

  procedure dTestMatrixComparison; cdecl; external {$IFDEF __GPC__}name 'dTestMatrixComparison'{$ELSE} ODEDLL{$ENDIF __GPC__};
  procedure dTestSolveLCP; cdecl; external {$IFDEF __GPC__}name 'dTestSolveLCP'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //----- Recreated -----
  function dDot (const a, b : TdVector3) : TdReal; overload;
  function dDot (const a, b : PdVector3) : TdReal; overload;

  function dDOT14(const a,b : TdRealArray) : TdReal; overload;
  function dDOT14(const a,b : PdRealArray) : TdReal; overload;

  procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
  procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);

  function Vector3ScalarMul(const a : TdVector3; const Scalar : TdReal) : TdVector3;
  function Vector3ADD(const a, b : TdVector3) : TdVector3;
  function Vector3SUB(const a, b : TdVector3) : TdVector3;
  function Vector3Length(const a : TdVector3) : TdReal;
  function Vector3Cross(const V1, V2 : TdVector3) : TdVector3;
  function Vector3Make(const x,y,z : TdReal) : TdVector3;


  procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);

  {ExportInitODEMarker}

const MaxUserClasses = 4;

var

  dSphereClass : integer=0;
  dBoxClass : integer=1;
  dCapsuleClass : integer=2;
  dCylinderClass : integer=3;
  dPlaneClass : integer=4;
  dRayClass : integer=5;
  dConvexClass : integer=6;
  dGeomTransformClass : integer=7;
  dTriMeshClass : integer=8;
  dHeightFieldClass : integer=9;
  dFirstSpaceClass : integer = 10;
  dSimpleSpaceClass : integer = 10;
  dHashSpaceClass : integer = 11;
  dSweepAndPruneSpaceClass : integer = 12;
  dQuadTreeSpaceClass : integer = 13;
  dLastSpaceClass : integer = 13;
  dFirstUserClass : integer = 14;
  dLastUserClass : integer = 17;
  dGeomNumClasses : integer = 18;

  IsODEInitialized : boolean = False;
  DisabledDebugGeom : boolean = False;
  DisabledDebugCollision : boolean = False;

{$IFDEF cODEDebugEnabled}
var
   ODEDebugGeomList: TGeomList;
{$ENDIF}
{$IFDEF cODEDebugCollisionEnabled}
var
   ODEDebugCollisionList: array of TdContact;
{$ENDIF}


  // These are made public in the dynamic version MRQZZZ
  function InitODE(ADllName : PChar) : boolean;
  procedure CloseODE;



implementation

{ TBodyList }

procedure TBodyList.DeleteAllBodies;
var i : integer;
begin
  for i := 0 to Count-1 do  dBodyDestroy(Get(i));
  Clear;
end;

function TBodyList.GetItems(i: integer): PdxBody;
begin
  result := Get(i);
end;

procedure TBodyList.SetItems(i: integer; const Value: PdxBody);
begin
  Put(i, Value);
end;

{ TGeomList }

procedure TGeomList.DeleteAllGeoms(DeleteDataAsObject : boolean=false);
var i : integer;
    geom : PdxGeom;
begin
   for i := 0 to Count-1 do begin
      geom := Get(i);
      if DeleteDataAsObject and (geom.data<>nil) then TObject(geom.data).Free;
      dGeomDestroy(geom);
   end;
   Clear;
end;

function TGeomList.GetItems(i: integer): PdxGeom;
begin
   result := Get(i);
end;

procedure TGeomList.SetItems(i: integer; const Value: PdxGeom);
begin
   Put(i, Value);
end;

//----- Recreated -----

function dDot (const a, b : PdVector3) : TdReal;
begin
  Assert(Assigned(a),'a not assigned!');
  Assert(Assigned(b),'b not assigned!');
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

function dDot (const a, b : TdVector3) : TdReal;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

// #define dDOT(a,b)   ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2])
// #define dDOT14(a,b) ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8])
// #define dDOT41(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[1] + (a)[8]*(b)[2])
// #define dDOT44(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[4] + (a)[8]*(b)[8])

function dDOT14(const a,b : TdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

function dDOT14(const a,b : PdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);
{var
  v : PdVector3;}
begin
  // #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)

  //  #define dMULTIPLYOP0_331(A,op,B,C) \
  //    (A)[0] op dDOT((B),(C)); \
  //    (A)[1] op dDOT((B+4),(C)); \
  //    (A)[2] op dDOT((B+8),(C));


  A[0] := dDOT(PdVector3(@(B[0]))^, C);

  A[1] := dDOT(PdVector3(@(B[4]))^, C);
  A[2] := dDOT(PdVector3(@(B[8]))^, C);//}
end;

procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
begin
  // #define dMULTIPLY0_333(A,B,C) dMULTIPLYOP0_333(A,=,B,C)
  // #define dMULTIPLYOP0_333(A,op,B,C) \
  //   (A)[0] op dDOT14((B),(C)); \
  //   (A)[1] op dDOT14((B),(C+1)); \
  //   (A)[2] op dDOT14((B),(C+2)); \
  //   (A)[4] op dDOT14((B+4),(C)); \
  //   (A)[5] op dDOT14((B+4),(C+1)); \
  //   (A)[6] op dDOT14((B+4),(C+2)); \
  //   (A)[8] op dDOT14((B+8),(C)); \
  //   (A)[9] op dDOT14((B+8),(C+1)); \
  //   (A)[10] op dDOT14((B+8),(C+2));

  A[0] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[0])));
  A[1] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[1])));
  A[2] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[2])));

  A[4] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[0])));
  A[5] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[1])));
  A[6] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[2])));

  A[8] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[0])));
  A[9] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[1])));
  A[10] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[2])));
end;

function Vector3ScalarMul(const a : TdVector3; const Scalar : TdReal) : TdVector3;
begin
  result[0] := a[0]*Scalar;
  result[1] := a[1]*Scalar;
  result[2] := a[2]*Scalar;
end;

function Vector3ADD(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]+b[0];
  result[1] := a[1]+b[1];
  result[2] := a[2]+b[2];
end;

function Vector3SUB(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]-b[0];
  result[1] := a[1]-b[1];
  result[2] := a[2]-b[2];
end;

function Vector3Length(const a : TdVector3) : TdReal;
begin
  result := sqrt(sqr(a[0])+sqr(a[1])+sqr(a[2]));
end;

function Vector3Cross(const V1, V2 : TdVector3) : TdVector3;
begin
   Result[0]:=V1[1] * V2[2] - V1[2] * V2[1];
   Result[1]:=V1[2] * V2[0] - V1[0] * V2[2];
   Result[2]:=V1[0] * V2[1] - V1[1] * V2[0];
end;

function Vector3Make(const x,y,z : TdReal) : TdVector3;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

{
procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);
var
  Body : PdxBody;
  TempList : TList;
begin
  if not Assigned(WasStillBefore) then
  begin
    WasStillBefore := TList.Create;
    WasStillBeforeOld := TList.Create;
  end;

  Body := World.FirstBody;

  WasStillBefore.Clear;

  // We can't disable bodies just as soon as they're still - that could disable
  // bodies that are just slowed down or titering on an edge. If they've been
  // still for two frames, we consider them truly still.
  while Assigned(Body) do
  begin
    if dBodyIsEnabled(Body)=1 then
    begin
      // Is the body still?
      if (abs(Body.lvel[0])<Threshold) and (abs(Body.lvel[1])<Threshold) and (abs(Body.lvel[2])<Threshold) and
         (abs(Body.avel[0])<Threshold) and (abs(Body.avel[1])<Threshold) and (abs(Body.avel[2])<Threshold) then
      begin
        if WasStillBeforeOld.IndexOf(Body)<>-1 then
          dBodyDisable(Body)
        else
          WasStillBefore.Add(Body);
      end;
    end;

    Body := PdxBody(Body.BaseObject.next);
  end;

  TempList := WasStillBeforeOld;
  WasStillBeforeOld := WasStillBefore;
  WasStillBefore := TempList;
end;}

procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);
var
  m : TdMass;
  VerificationPointer : pointer;
begin
  VerificationPointer := pointer( -1 ); // A known pointer
  // Verify Body
  dBodySetData( Body, VerificationPointer );
  Assert( dBodyGetData( Body ) = VerificationPointer, 'Body test 1 fails' );
  Assert( Body.BaseObject.userdata = VerificationPointer, 'Body test 2 fails' );

  dBodyGetMass(Body, m);

  Assert(Body.mass.mass = m.mass, 'Body test 3 fails');

  // Verify Geom
  dGeomSetData( Geom, VerificationPointer );
  Assert( dGeomGetData( Geom ) = VerificationPointer, 'Geom test 1 fails' );
  Assert(dGeomGetBody(Geom)=Geom.Body, 'Geom test 2 fails');
  Assert( Geom.Data = VerificationPointer, 'Geom test 3 fails' );
end;

var
  vODEHandle : TModuleHandle;

procedure GetODEClassIDs;
begin
  {$IFDEF PARODE}
   dSphereClass:=dSphereGetClass;
   dBoxClass:=dBoxGetClass;
   dPlaneClass:=dPlaneGetClass;
   dCylinderClass:=dCylinderGetClass;
   dConvexClass:=dConvexGetClass;
   dGeomTransformClass:=dGeomTransformGetClass;
   dRayClass:=dRayGetClass;
   dTriMeshClass:=dTriMeshGetClass;
   dHeightfieldClass:=dHeightfieldGetClass;
   {$ENDIF}
end;

function InitODE(ADllName : PChar) : boolean;
var isODELoaded : boolean;
begin
  result := IsODEInitialized;
  if IsODEInitialized then exit;

  if ADllName = '' then ADllName := ODEDLL;

  isODELoaded := LoadModule( vODEHandle, ADllName );
  if not isODELoaded then exit;

  if isODELoaded and not IsODEInitialized then begin
     dInitODE2(0);
     GetODEClassIDs;
     IsODEInitialized:=True;
  end;
  result:=IsODEInitialized;
end;

procedure CloseODE;
begin
  if IsODEInitialized then dCloseODE;
  IsODEInitialized := false;
  UnLoadModule( vODEHandle );
end;


initialization
   InitODE(ODEDLL);

finalization
   CloseODE;

end.
