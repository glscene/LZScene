//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Conversion of OpenCL header file: cl_gl.h to Delphi,
   from http://www.khronos.org/registry/cl/.

    History :  
       05/03/11 - Yar - Rename size_t to TSize_t
       01/11/09 - DanB - Creation
    
}
///*****************************************************************************
// * Copyright (c) 2008-2009 The Khronos Group Inc.
// *
// * Permission is hereby granted, free of charge, to any person obtaining a
// * copy of this software and/or associated documentation files (the
// * "Materials"), to deal in the Materials without restriction, including
// * without limitation the rights to use, copy, modify, merge, publish,
// * distribute, sublicense, and/or sell copies of the Materials, and to
// * permit persons to whom the Materials are furnished to do so, subject to
// * the following conditions:
// *
// * The above copyright notice and this permission notice shall be included
// * in all copies or substantial portions of the Materials.
// *
// * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
// ****************************************************************************/

unit GLSCLGL;

interface

uses
  GLSCL, GLSCLPlatform, OpenGLTokens;

{$I cl.inc}

type

Tcl_gl_object_type = Tcl_uint;
Pcl_gl_object_type = ^Tcl_gl_object_type;

Tcl_gl_texture_info = Tcl_uint;
Pcl_gl_texture_info = ^Tcl_gl_texture_info;

Tcl_gl_platform_info = Tcl_uint;
Pcl_gl_platform_info = ^Tcl_gl_platform_info;

const
// cl_gl_object_type
CL_GL_OBJECT_BUFFER            = $2000;
CL_GL_OBJECT_TEXTURE2D         = $2001;
CL_GL_OBJECT_TEXTURE3D         = $2002;
CL_GL_OBJECT_RENDERBUFFER      = $2003;

// cl_gl_texture_info
CL_GL_TEXTURE_TARGET           = $2004;
CL_GL_MIPMAP_LEVEL             = $2005;

function clCreateFromGLBuffer(context: Tcl_context;
                              flags: Tcl_mem_flags;
                              bufobj: GLuint;
                              errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                              {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                              external LibOpenCL;

function clCreateFromGLTexture2D(context: Tcl_context;
                                 flags: Tcl_mem_flags;
                                 target: GLenum;
                                 miplevel: GLint;
                                 texture: GLuint;
                                 errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                 {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                 external LibOpenCL;

function clCreateFromGLTexture3D(context: Tcl_context;
                                 flags: Tcl_mem_flags;
                                 target: GLenum;
                                 miplevel: GLint;
                                 texture: GLuint;
                                 errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                 {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                 external LibOpenCL;

function clCreateFromGLRenderbuffer(context: Tcl_context;
                                    flags: Tcl_mem_flags;
                                    renderbuffer: GLuint;
                                    errcode_ret: Pcl_int): Tcl_mem; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                    {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                    external LibOpenCL;

function clGetGLObjectInfo(memobj: Tcl_mem;
                           gl_object_type: Pcl_gl_object_type;
                           gl_object_name: PGLuint): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                              {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                              external LibOpenCL;

function clGetGLTextureInfo(memobj: Tcl_mem;
                            param_name: Tcl_gl_texture_info;
                            param_value_size: TSize_t;
                            param_value: Pointer;
                            param_value_size_ret: PSize_t): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                                     {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                                     external LibOpenCL;

function clEnqueueAcquireGLObjects(command_queue: Tcl_command_queue;
                                   num_objects: Tcl_uint;
                                   mem_objects: Pcl_mem;
                                   num_events_in_wait_list: Tcl_uint;
                                   event_wait_list: Pcl_event;
                                   event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                               {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                               external LibOpenCL;

function clEnqueueReleaseGLObjects(command_queue: Tcl_command_queue;
                                   num_objects: Tcl_uint;
                                   mem_objects: Pcl_mem;
                                   num_events_in_wait_list: Tcl_uint;
                                   event_wait_list: Pcl_event;
                                   event: Pcl_event): Tcl_int; {$IFDEF CL_STDCALL} stdcall;{$ENDIF}
                                                               {$IFDEF CL_CDECL} cdecl;{$ENDIF}
                                                               external LibOpenCL;

implementation

end.
