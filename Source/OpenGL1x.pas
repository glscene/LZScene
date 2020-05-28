//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   OpenGL 1.x import unit for GLScene. Unit remains "general purpose", but with
   a more "pragmatic" approach :)

   This unit is based on OpenGL12.pas orginally written by Mike Lischke,
   please refer to OpenGL12.pas header.

	 History :  
       23/01/11 - DanB - Entry points now use procedural types from OpenGLTokens.pas
                            Added OpenGL 4.1 + ARB extensions
                            Switched to use GLS_REGIONS define
       23/08/10 - Yar - Moved tokens part to OpenGLTokens.pas
       22/07/10 - Yar - Added GL_ARB_debug_output constant
       01/06/10 - Yar - Fixes for Linux x64
       31/05/10 - Yar - Added WGL_NV_gpu_affinity
       12/05/10 - Yar - Added GL_ARB_texture_compression_bptc
       04/05/10 - Yar - Added GL_S3_s3tc extension (thanks to Rustam Asmandiarov aka Predator)
       01/05/10 - DanB - Fixed glGetTransformFeedbackVarying params
       16/04/10 - Yar - Added Graphics Remedy's Extensions
       28/03/10 - DanB - Added missing OpenGL 3.1/3.2 function lookups +
                            added bindless graphics extensions
       18/03/10 - Yar - Added more GLX extensions
                          (thanks to Rustam Asmandiarov aka Predator)
       12/03/10 - DanB - OpenGL 3.3/4.0 support (new ARB extensions), removed
                            _ARB suffix from functions/procedures in
                            GL_ARB_draw_buffers_blend + GL_ARB_sample_shading
       04/03/10 - DanB - Organised core into relevant + deprecated sections,
                            fixed a couple of function params + misc changes.
       12/02/10 - Yar -  Added GL_AMD_vertex_shader_tessellator
       07/02/10 - Yar -  Added GL_NV_primitive_restart
       21/01/10 - DaStr - Bugfixed wglChoosePixelFormatARB() and
                              wglCreatePbufferARB() parameters
       07/01/10 - DaStr - Added WGL_COLOR_SAMPLES_NV (thanks YarUndeoaker)
       25/12/09 - DaStr - Added GL_NV_copy_image, GL_LUMINANCE_INTEGER,
                              GL_LUMINANCE_ALPHA_INTEGER extentions and constants
                             Re-added $region declarations (thanks YarUndeoaker)
       13/12/09 - DaStr - Added missing stdcall/cdecl modifiers
       25/10/09 - DaStr - Added some texture compression extensions and updated
                              glTransformFeedbackVaryings()(thanks YarUndeoaker)
       28/09/09 - DaStr - Added some NVidia-specific extensions (thanks YarUndeoaker)
       30/08/09 - DanB - GLsync changed to NativeInt, fixes to glBindBufferRange calls
       14/08/09 - DanB - Added missing GL_ARB_framebuffer_object extension check + fixed typo
       04/08/09 - DanB - OpenGL 3.1/3.2 support + new ARB extensions added
       28/07/09 - DaStr - Added GL_GEOMETRY_PROGRAM_NV and related extensions
       20/01/08 - DanB - Fix due to Delphi6 not containing UInt64
       05/10/08 - DanB - Moved error handling code here from GLContext.pas
                            OpenGL 3.0 support, new core features + ARB extensions
       23/03/08 - DanB - Added more Vendor/EXT extensions
       17/03/08 - mrqzzz - uncommented some constants "GL_NORMAL_MAP_EXT,..."
                              to keep compatibility with dws2OpenGL1x.
       16/03/08 - DanB - Major rewrite of unit, including:
                            OpenGL 1.3, 1.4, 1.5, 2.0, 2.1 support.
                            removed TRCOptions (not used).
                            moved MRT_BUFFERS constant to GLContext.pas (isn't core openGL).
                            several new ARB extensions added.
                            several new Vendor/EXT exensions added.
                            new function IsOpenGLVersionMet added.
                            restructured unit so extensions are in numerical order.
       17/06/07 - LC - Added GL_ARB_pixel_buffer_object, GL_EXT_pixel_buffer_object
       22/03/07 - DaStr - Removed GetTextureRectangle (had many useless checks)
       16/03/07 - DaStr - Dropped Kylix support in favor of FPC
                             (thanks Burkhard Carstens) (BugTracekrID=1681585)
       09/03/07 - DaStr - Added GL_ARB_draw_buffers (thanks riz)
       03/03/07 - DaStr - Added GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT
       02/03/07 - DaStr - Added GL_[ARB/EXT]_texture_rectangle
                             Added GetTextureRectangle
       10/01/07 - LC - Added GL_EXT_framebuffer_object
       11/09/06 - NC - Added GL_ARB_texture_float, GL_ARB_texture_non_power_of_two
       13/10/04 - NC - Added GL_ATI_draw_buffers
       08/10/04 - LR - Added const in the prototype of the following function for compatibility :
                              TGLUTessCombineProc, TGLUTessCombineDataProc, gluPickMatrix
      gluProject, gluUnProject, gluTessVertex, gluLoadSamplingMatrices
       04/10/04 - NC - Added GL_ATI_texture_float, WGL_ATI_pixel_format_float,
                          WGL_NV_float_buffer, GL_NV_float_buffer
       08/07/04 - LR - Change case for Linux
       05/07/04 - LR - Corrections for Linux. Now glX function are directly load
                          by external action (like for Windows). So i suppress
                          the function LoadLinuxOpenGL.
       28/06/04 - LR - Removed ..\ from the GLScene.inc
       24/06/04 - SG - Added GL_ARB_fragment_program
       17/05/04 - EG - Dropped EXT_vertex_array (assumed as standard)
       06/04/04 - EG - Added GL_ARB_shader_objects, GL_ARB_vertex_shader
                          and GL_ARB_fragment_shader, dropped a few oldies
       13/02/04 - EG - Added GL_NV_texture_rectangle
       18/11/03 - EG - Fixed binding of core extensions, added GL_ARB_depth_texture
                          and GL_ARB_shadow support
       20/09/03 - EG - Added GL_NV_occlusion_query, dropped some more oldies
       09/09/03 - EG - Added GL_ARB_vertex_buffer_object, dropped some oldies
       04/09/03 - EG - Added GL_ARB_vertex_program
       23/07/03 - EG - Creation from OpenGL12.pas "morph": classic OpenGL
                          calls made static, obsolete/rare extensions support
                          dropped
    
}
unit OpenGL1x;

interface

{$I GLScene.inc}

 // DaStr: MULTITHREADOPENGL is defined in GLScene.inc, but you can override it
 // manually here, though I would not reccomend it. This is because other units
 // may depend on this option too. So if you need this option, please use the
 // _MULTITHREAD define in GLScene.inc.
{.$define MULTITHREADOPENGL}

uses
  SysUtils, OpenGLTokens, GLVectorTypes,
  {$IFDEF MSWINDOWS}
    Windows
  {$ENDIF }
  {$IFDEF GLS_X11_SUPPORT}
     X, Xlib, XUtil,
  {$ENDIF}
  {$IFDEF UNIX}
    {Libc,}Types, LCLType, dynlibs
  {$ENDIF}
  ;
{$IFDEF GLS_REGIONS} {$region 'OpenGL extension feature checks'} {$ENDIF}

{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}
   // supported version checks
   GL_VERSION_1_0,
   GL_VERSION_1_1,
   GL_VERSION_1_2,
   GL_VERSION_1_3,
   GL_VERSION_1_4,
   GL_VERSION_1_5,
   GL_VERSION_2_0,
   GL_VERSION_2_1,
   GL_VERSION_3_0,
   GL_VERSION_3_1,
   GL_VERSION_3_2,
   GL_VERSION_3_3,
   GL_VERSION_4_0,
   GL_VERSION_4_1,
   GL_VERSION_4_2,
   GL_VERSION_4_3,
   GL_VERSION_4_4,
   GLU_VERSION_1_1,
   GLU_VERSION_1_2,
   GLU_VERSION_1_3: Boolean;

   // ARB approved OpenGL extension checks
   GL_ARB_blend_func_extended,
   GL_ARB_color_buffer_float,
   GL_ARB_compatibility,
   GL_ARB_copy_buffer,
   GL_ARB_debug_output,
   GL_ARB_depth_buffer_float,
   GL_ARB_depth_clamp,
   GL_ARB_depth_texture,
   GL_ARB_draw_buffers,
   GL_ARB_draw_buffers_blend,
   GL_ARB_draw_elements_base_vertex,
   GL_ARB_draw_indirect,
   GL_ARB_draw_instanced,
   GL_ARB_ES2_compatibility,
   GL_ARB_explicit_attrib_location,
   GL_ARB_fragment_coord_conventions,
   GL_ARB_fragment_program,
   GL_ARB_fragment_program_shadow,
   GL_ARB_fragment_shader,
   GL_ARB_framebuffer_object,
   GL_ARB_framebuffer_sRGB,
   GL_ARB_geometry_shader4,
   GL_ARB_get_program_binary,
   GL_ARB_gpu_shader_fp64,
   GL_ARB_gpu_shader5,
   GL_ARB_half_float_pixel,
   GL_ARB_half_float_vertex,
   GL_ARB_imaging,
   GL_ARB_instanced_arrays,
   GL_ARB_map_buffer_range,
   GL_ARB_matrix_palette,
   GL_ARB_multisample,
   GL_ARB_multitexture,
   GL_ARB_occlusion_query,
   GL_ARB_occlusion_query2,
   GL_ARB_pixel_buffer_object,
   GL_ARB_point_parameters,
   GL_ARB_point_sprite,
   GL_ARB_provoking_vertex,
   GL_ARB_robustness,
   GL_ARB_sample_shading,
   GL_ARB_sampler_objects,
   GL_ARB_seamless_cube_map,
   GL_ARB_separate_shader_objects,
   GL_ARB_shader_bit_encoding,
   GL_ARB_shader_precision,
   GL_ARB_shader_stencil_export,
   GL_ARB_shader_subroutine,
   GL_ARB_shader_texture_lod,
   GL_ARB_shading_language_100,
   GL_ARB_shadow,
   GL_ARB_shadow_ambient,
   GL_ARB_shader_objects,
   GL_ARB_sync,
   GL_ARB_tessellation_shader,
   GL_ARB_texture_border_clamp,
   GL_ARB_texture_buffer_object,
   GL_ARB_texture_buffer_object_rgb32,
   GL_ARB_texture_compression,
   GL_ARB_texture_compression_rgtc,
   GL_ARB_texture_cube_map,
   GL_ARB_texture_cube_map_array,
   GL_ARB_texture_env_add,
   GL_ARB_texture_env_combine,
   GL_ARB_texture_env_crossbar,
   GL_ARB_texture_env_dot3,
   GL_ARB_texture_float,
   GL_ARB_texture_gather,
   GL_ARB_texture_mirrored_repeat,
   GL_ARB_texture_multisample,
   GL_ARB_texture_non_power_of_two,
   GL_ARB_texture_query_lod,
   GL_ARB_texture_rectangle,
   GL_ARB_texture_rg,
   GL_ARB_texture_rgb10_a2ui,
   GL_ARB_texture_swizzle,
   GL_ARB_timer_query,
   GL_ARB_transform_feedback2,
   GL_ARB_transform_feedback3,
   GL_ARB_transpose_matrix,
   GL_ARB_uniform_buffer_object,
   GL_ARB_vertex_array_bgra,
   GL_ARB_vertex_array_object,
   GL_ARB_vertex_attrib_64bit,
   GL_ARB_vertex_blend,
   GL_ARB_vertex_buffer_object,
   GL_ARB_vertex_program,
   GL_ARB_vertex_shader,
   GL_ARB_vertex_type_2_10_10_10_rev,
   GL_ARB_viewport_array,
   GL_ARB_window_pos,
   GL_ARB_texture_compression_bptc,

   // Vendor/EXT OpenGL extension checks
   GL_3DFX_multisample,
   GL_3DFX_tbuffer,
   GL_3DFX_texture_compression_FXT1,

   GL_ATI_draw_buffers,
   GL_ATI_texture_compression_3dc,
   GL_ATI_texture_float,
   GL_ATI_texture_mirror_once,

   GL_S3_s3tc,

   GL_EXT_abgr,
   GL_EXT_bgra,
   GL_EXT_bindable_uniform,
   GL_EXT_blend_color,
   GL_EXT_blend_equation_separate,
   GL_EXT_blend_func_separate,
   GL_EXT_blend_logic_op,
   GL_EXT_blend_minmax,
   GL_EXT_blend_subtract,
   GL_EXT_Cg_shader,
   GL_EXT_clip_volume_hint,
   GL_EXT_compiled_vertex_array,
   GL_EXT_copy_texture,
   GL_EXT_depth_bounds_test,
   GL_EXT_draw_buffers2,
   GL_EXT_draw_instanced,
   GL_EXT_draw_range_elements,
   GL_EXT_fog_coord,
   GL_EXT_framebuffer_blit,
   GL_EXT_framebuffer_multisample,
   GL_EXT_framebuffer_object,
   GL_EXT_framebuffer_sRGB,
   GL_EXT_geometry_shader4,
   GL_EXT_gpu_program_parameters,
   GL_EXT_gpu_shader4,
   GL_EXT_multi_draw_arrays,
   GL_EXT_multisample,
   GL_EXT_packed_depth_stencil,
   GL_EXT_packed_float,
   GL_EXT_packed_pixels,
   GL_EXT_paletted_texture,
   GL_EXT_pixel_buffer_object,
   GL_EXT_polygon_offset,
   GL_EXT_rescale_normal,
   GL_EXT_secondary_color,
   GL_EXT_separate_specular_color,
   GL_EXT_shadow_funcs,
   GL_EXT_shared_texture_palette,
   GL_EXT_stencil_clear_tag,
   GL_EXT_stencil_two_side,
   GL_EXT_stencil_wrap,
   GL_EXT_texture3D,
   GL_EXT_texture_array,
   GL_EXT_texture_buffer_object,
   GL_EXT_texture_compression_latc,
   GL_EXT_texture_compression_rgtc,
   GL_EXT_texture_compression_s3tc,
   GL_EXT_texture_cube_map,
   GL_EXT_texture_edge_clamp,
   GL_EXT_texture_env_add,
   GL_EXT_texture_env_combine,
   GL_EXT_texture_env_dot3,
   GL_EXT_texture_filter_anisotropic,
   GL_EXT_texture_integer,
   GL_EXT_texture_lod,
   GL_EXT_texture_lod_bias,
   GL_EXT_texture_mirror_clamp,
   GL_EXT_texture_object,
   GL_EXT_texture_rectangle,
   GL_EXT_texture_sRGB,
   GL_EXT_texture_shared_exponent,
   GL_EXT_timer_query,
   GL_EXT_transform_feedback,
   GL_EXT_vertex_array,

   GL_HP_occlusion_test,

   GL_IBM_rasterpos_clip,

   GL_KTX_buffer_region,

   GL_MESA_resize_buffers,

   GL_NV_blend_square,
   GL_NV_conditional_render,
   GL_NV_copy_image,
   GL_NV_depth_buffer_float,
   GL_NV_fence,
   GL_NV_float_buffer,
   GL_NV_fog_distance,
   GL_NV_geometry_program4,
   GL_NV_light_max_exponent,
   GL_NV_multisample_filter_hint,
   GL_NV_occlusion_query,
   GL_NV_point_sprite,
   GL_NV_primitive_restart,
   GL_NV_register_combiners,
   GL_NV_shader_buffer_load,
   GL_NV_texgen_reflection,
   GL_NV_texture_compression_vtc,
   GL_NV_texture_env_combine4,
   GL_NV_texture_rectangle,
   GL_NV_texture_shader,
   GL_NV_texture_shader2,
   GL_NV_texture_shader3,
   GL_NV_transform_feedback,
   GL_NV_vertex_array_range,
   GL_NV_vertex_array_range2,
   GL_NV_vertex_buffer_unified_memory,
   GL_NV_vertex_program,

   GL_SGI_color_matrix,

   GL_SGIS_generate_mipmap,
   GL_SGIS_multisample,
   GL_SGIS_texture_border_clamp,
   GL_SGIS_texture_color_mask,
   GL_SGIS_texture_edge_clamp,
   GL_SGIS_texture_lod,

   GL_SGIX_depth_texture,
   GL_SGIX_shadow,
   GL_SGIX_shadow_ambient,

   GL_AMD_vertex_shader_tessellator,

   GL_WIN_swap_hint,

   // ARB approved WGL extension checks
   WGL_ARB_buffer_region,
   WGL_ARB_create_context,
   WGL_ARB_create_context_profile,
   WGL_ARB_extensions_string,
   WGL_ARB_framebuffer_sRGB,
   WGL_ARB_make_current_read,
   WGL_ARB_multisample,
   WGL_ARB_pbuffer,
   WGL_ARB_pixel_format,
   WGL_ARB_pixel_format_float,
   WGL_ARB_render_texture,

   // Vendor/EXT WGL extension checks
   WGL_ATI_pixel_format_float,

   WGL_EXT_framebuffer_sRGB,
   WGL_EXT_pixel_format_packed_float,
   WGL_EXT_swap_control,
   WGL_NV_gpu_affinity,

   // GLX extension checks
   GLX_VERSION_1_1,
   GLX_VERSION_1_2,
   GLX_VERSION_1_3,
   GLX_VERSION_1_4,
   GLX_ARB_create_context,
   GLX_ARB_create_context_profile,
   GLX_ARB_framebuffer_sRGB,
   GLX_ARB_multisample,
   GLX_EXT_framebuffer_sRGB,
   GLX_EXT_fbconfig_packed_float,

   GLX_SGIS_multisample,
   GLX_EXT_visual_info,
   GLX_SGI_swap_control,
   GLX_SGI_video_sync,
   GLX_SGI_make_current_read,
   GLX_SGIX_video_source,
   GLX_EXT_visual_rating,
   GLX_EXT_import_context,
   GLX_SGIX_fbconfig,
   GLX_SGIX_pbuffer,
   GLX_SGI_cushion,
   GLX_SGIX_video_resize,
   GLX_SGIX_dmbuffer,
   GLX_SGIX_swap_group,
   GLX_SGIX_swap_barrier,
   GLX_SGIS_blended_overlay,
   GLX_SGIS_shared_multisample,
   GLX_SUN_get_transparent_index,
   GLX_3DFX_multisample,
   GLX_MESA_copy_sub_buffer,
   GLX_MESA_pixmap_colormap,
   GLX_MESA_release_buffers,
   GLX_MESA_set_3dfx_mode,
   GLX_SGIX_visual_select_group,
   GLX_SGIX_hyperpipe,

   // Graphics Remedy's Extensions
   GL_GREMEDY_frame_terminator,
   GL_GREMEDY_string_marker,

   // OpenGL Utility (GLU) extension checks
   GLU_EXT_object_space_tess,
   GLU_EXT_nurbs_tessellator,
   GLU_EXT_Texture: Boolean;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL v1.1 core functions and procedures'} {$ENDIF}
   procedure glBindTexture(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glBlendFunc(sfactor: TGLEnum; dfactor: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClear(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearColor(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearDepth(depth: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glClearStencil(s: TGLint ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glColorMask(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glCopyTexImage1D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexImage2D(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage1D(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCopyTexSubImage2D(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glCullFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDeleteTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthFunc(func: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthMask(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDepthRange(zNear, zFar: TGLclampd); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDisable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glDrawArrays(mode: TGLEnum; first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glDrawElements(mode: TGLEnum; count: TGLsizei; atype: TGLEnum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   

   procedure glEnable(cap: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFinish; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glFlush; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glFrontFace(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGenTextures(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetBooleanv(pname: TGLEnum; params: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetDoublev(pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glGetError: TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetFloatv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetIntegerv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glGetPointerv(pname: TGLEnum; var params); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;   
   procedure glGetTexImage(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameterfv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexLevelParameteriv(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glGetTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glHint(target, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   function  glIsEnabled(cap: TGLEnum): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   function  glIsTexture(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glLineWidth(width: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glLogicOp(opcode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glPixelStoref(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPixelStorei(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPointSize(size: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonMode(face, mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glPolygonOffset(factor, units: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glReadBuffer(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glReadPixels(x, y: TGLint; width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glScissor(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilFunc(func: TGLEnum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glStencilOp(fail, zfail, zpass: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glTexImage1D(target: TGLEnum; level, internalformat: TGLint; width: TGLsizei; border: TGLint; format,
                          atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexImage2D(target: TGLEnum; level, internalformat: TGLint; width, height: TGLsizei; border: TGLint;
                          format, atype: TGLEnum; Pixels:Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameterfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteri(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexParameteriv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage1D(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, atype: TGLEnum;
                             pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
   procedure glTexSubImage2D(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format,
                             atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

   procedure glViewport(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

{$IFDEF GLS_REGIONS} {$region 'OpenGL 1.1 deprecated'} {$ENDIF}
   procedure glAccum(op: TGLuint; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glAlphaFunc(func: TGLEnum; ref: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glAreTexturesResident(n: TGLsizei; Textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glArrayElement(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBegin(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glBitmap(width: TGLsizei; height: TGLsizei; xorig, yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; bitmap: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallList(list: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCallLists(n: TGLsizei; atype: TGLEnum; lists: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearAccum(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClearIndex(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColor3b(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3d(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3f(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3i(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3s(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ub(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3ui(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3us(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor3usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4b(red, green, blue, alpha: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4d(red, green, blue, alpha: TGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4f(red, green, blue, alpha: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4i(red, green, blue, alpha: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4s(red, green, blue, alpha: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4sv(v: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ub(red, green, blue, alpha: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ubv(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4ui(red, green, blue, alpha: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4uiv(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4us(red, green, blue, alpha: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColor4usv(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glColorMaterial(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glColorPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glCopyPixels(x, y: TGLint; width, height: TGLsizei; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDeleteLists(list: TGLuint; range: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDisableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glDrawPixels(width, height: TGLsizei; format, atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glEdgeFlag(flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagPointer(stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEdgeFlagv(flag: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnableClientState(aarray: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEnd; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEndList; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1d(u: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1f(u: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord1fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2d(u: TGLdouble; v: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2dv(u: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2f(u, v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalCoord2fv(u: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh1(mode: TGLEnum; i1, i2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalMesh2(mode: TGLEnum; i1, i2, j1, j2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint1(i: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glEvalPoint2(i, j: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glFeedbackBuffer(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFogiv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glFrustum(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glGenLists(range: TGLsizei): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetClipPlane(plane: TGLEnum; equation: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapdv(target, query: TGLEnum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapfv(target, query: TGLEnum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMapiv(target, query: TGLEnum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapfv(map: TGLEnum; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapuiv(map: TGLEnum; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPixelMapusv(map: TGLEnum; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glGetTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glIndexMask(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexd(c: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexdv(c: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexf(c: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexfv(c: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexi(c: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexiv(c: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexs(c: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexsv(c: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexub(c: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glIndexubv(c: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInitNames; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glInterleavedArrays(format: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glIsList(list: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModelfv(pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeli(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightModeliv(pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightf(light, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightfv(light, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLighti(light, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLightiv(light, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLineStipple(factor: TGLint; pattern: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glListBase(base: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadIdentity; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glLoadName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glMap1d(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap1f(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2d(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint; v1, v2: TGLdouble; vstride,
                     vorder: TGLint; points: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMap2f(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint; v1, v2: TGLfloat; vstride,
                     vorder: TGLint; points: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1d(un: TGLint; u1, u2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid1f(un: TGLint; u1, u2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2d(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMapGrid2f(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialf(face, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialfv(face, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMateriali(face, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMaterialiv(face, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMatrixMode(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixd(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glMultMatrixf(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNewList(list: TGLuint; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3b(nx, ny, nz: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3bv(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3d(nx, ny, nz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3f(nx, ny, nz: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3i(nx, ny, nz: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3s(nx, ny, nz: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormal3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glNormalPointer(atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glOrtho(left, right, bottom, top, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPassThrough(token: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapfv(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapuiv(map: TGLEnum; mapsize: TGLsizei; values: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelMapusv(map: TGLEnum; mapsize: TGLsizei; values: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelTransferf(pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelTransferi(pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPixelZoom(xfactor, yfactor: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPolygonStipple(mask: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopClientAttrib; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPopName; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPrioritizeTextures(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushClientAttrib(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushMatrix; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glPushName(name: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glRasterPos2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2s(x, y: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRasterPos4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectd(x1, y1, x2, y2: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectdv(v1, v2: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectf(x1, y1, x2, y2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectfv(v1, v2: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRecti(x1, y1, x2, y2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectiv(v1, v2: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRects(x1, y1, x2, y2: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRectsv(v1, v2: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   function  glRenderMode(mode: TGLEnum): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotated(angle, x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glRotatef(angle, x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glScaled(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glScalef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glSelectBuffer(size: TGLsizei; buffer: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glShadeModel(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1d(s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1f(s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1i(s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1s(s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord1sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2d(s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2f(s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2i(s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2s(s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3d(s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3f(s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3i(s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3s(s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4d(s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4f(s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4i(s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4s(s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoord4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexCoordPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvf(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvfv(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnvi(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexEnviv(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGend(coord, pname: TGLEnum; param: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGendv(coord, pname: TGLEnum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenf(coord, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGenfv(coord, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeni(coord, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTexGeniv(coord, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslated(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glTranslatef(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;

   procedure glVertex2d(x, y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2f(x, y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2i(x, y: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2s(x, y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex2sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3d(x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3f(x, y, z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3i(x, y, z: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3s(x, y, z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex3sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4d(x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4dv(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4f(x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4fv(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4i(x, y, z, w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4iv(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4s(x, y, z, w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertex4sv(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
   procedure glVertexPointer(size: TGLint; atype: TGLEnum; stride: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32; //deprecated;
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL utility (GLU) functions and procedures'} {$ENDIF}
   function  gluErrorString(errCode: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluGetString(name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluOrtho2D(left, right, bottom, top: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPickMatrix(x, y, width, height: TGLdouble; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluProject(objx, objy, objz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                        winx, winy, winz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluUnProject(winx, winy, winz: TGLdouble; const modelMatrix: TMatrix4d; const projMatrix: TMatrix4d; const viewport: TVector4i;
                          objx, objy, objz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluScaleImage(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout,
                           heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild1DMipmaps(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluBuild2DMipmaps(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum;
                               data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewQuadric: PGLUquadric; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteQuadric(state: PGLUquadric); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricNormals(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluCylinder(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices,
                         stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint;
                            startAngle, sweepAngle: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluQuadricCallback(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewTess: PGLUtesselator; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteTess(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessBeginContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessVertex(tess: PGLUtesselator; const coords: TVector3d; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessProperty(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessNormal(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluTessCallback(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetTessProperty(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   function  gluNewNurbsRenderer: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluPwlCurve(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; const modelMatrix: TMatrix4f; const projMatrix: TMatrix4f; const viewport: TVector4i); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNurbsCallback(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluBeginPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluNextContour(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
   procedure gluEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'Windows OpenGL (WGL) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   function wglGetProcAddress(ProcName: PGLChar): Pointer; stdcall; external opengl32;
   function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall; external opengl32;
   function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
   function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall; external opengl32;
   function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
   function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
   function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
   function wglGetCurrentDC: HDC; stdcall; external opengl32;
   function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall; external opengl32;
   function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
   function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
   function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall; external opengl32;
   function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD; stdcall; external opengl32;
   function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesA (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
   function wglUseFontOutlinesW (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
   function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name 'wglUseFontBitmapsA';
   function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name 'wglUseFontOutlinesA';
   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL Extension to the X Window System (GLX) support functions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   // GLX 1.0
   function glXChooseVisual(dpy: PDisplay; screen: TGLint; attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
   function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
   procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
   function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint); cdecl; external opengl32;
   procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
   function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo; pixmap: GLXPixmap): GLXPixmap; cdecl; external opengl32;
   procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
   function glXQueryExtension(dpy: PDisplay; errorb: PGLInt; event: PGLInt): TGLboolean; cdecl; external opengl32;
   function glXQueryVersion(dpy: PDisplay; maj: PGLInt; min: PGLINT): TGLboolean; cdecl; external opengl32;
   function glXIsDirect(dpy: PDisplay; ctx: GLXContext): TGLboolean; cdecl; external opengl32;
   function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLInt; value: PGLInt): TGLInt; cdecl; external opengl32;
   function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
   function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
   procedure glXWaitGL; cdecl; external opengl32;
   procedure glXWaitX; cdecl; external opengl32;
   procedure glXUseXFont(font: XFont; first: TGLInt; count: TGLInt; list: TGLint); cdecl; external opengl32;

   // GLX 1.1 and later
   function glXQueryExtensionsString(dpy: PDisplay; screen: TGLInt): PGLChar; cdecl; external opengl32;
   function glXQueryServerString(dpy: PDisplay; screen: TGLInt; name: TGLInt): PGLChar; cdecl; external opengl32;
   function glXGetClientString(dpy: PDisplay; name: TGLInt): PGLChar; cdecl; external opengl32;

   // GLX 1.2 and later
   function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;
   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}



{$IFDEF MULTITHREADOPENGL}
threadvar
{$else}
var
{$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL extension function/procedure definitions'} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 1.2'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.2 Core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor: PFNGLBLENDCOLORPROC;

   // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation: PFNGLBLENDEQUATIONPROC;

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements: PFNGLDRAWRANGEELEMENTSPROC;

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D: PFNGLTEXIMAGE3DPROC;
   glTexSubImage3D: PFNGLTEXSUBIMAGE3DPROC;

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D: PFNGLCOPYTEXSUBIMAGE3DPROC;

{$IFDEF GLS_REGIONS} {$region 'OpenGL 1.2 deprecated'} {$ENDIF}
   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable: PFNGLCOLORTABLEPROC;
   glColorTableParameterfv: PFNGLCOLORTABLEPARAMETERFVPROC;
   glColorTableParameteriv: PFNGLCOLORTABLEPARAMETERIVPROC;
   glCopyColorTable: PFNGLCOPYCOLORTABLEPROC;
   glGetColorTable: PFNGLGETCOLORTABLEPROC;
   glGetColorTableParameterfv: PFNGLGETCOLORTABLEPARAMETERFVPROC;
   glGetColorTableParameteriv: PFNGLGETCOLORTABLEPARAMETERIVPROC;

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable: PFNGLCOLORSUBTABLEPROC;
   glCopyColorSubTable: PFNGLCOPYCOLORSUBTABLEPROC;

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D: PFNGLCONVOLUTIONFILTER1DPROC;
   glConvolutionFilter2D: PFNGLCONVOLUTIONFILTER2DPROC;
   glConvolutionParameterf: PFNGLCONVOLUTIONPARAMETERFPROC;
   glConvolutionParameterfv: PFNGLCONVOLUTIONPARAMETERFVPROC;
   glConvolutionParameteri: PFNGLCONVOLUTIONPARAMETERIPROC;
   glConvolutionParameteriv: PFNGLCONVOLUTIONPARAMETERIVPROC;
   glCopyConvolutionFilter1D: PFNGLCOPYCONVOLUTIONFILTER1DPROC;
   glCopyConvolutionFilter2D: PFNGLCOPYCONVOLUTIONFILTER2DPROC;
   glGetConvolutionFilter: PFNGLGETCONVOLUTIONFILTERPROC;
   glGetConvolutionParameterfv: PFNGLGETCONVOLUTIONPARAMETERFVPROC;
   glGetConvolutionParameteriv: PFNGLGETCONVOLUTIONPARAMETERIVPROC;
   glGetSeparableFilter: PFNGLGETSEPARABLEFILTERPROC;
   glSeparableFilter2D: PFNGLSEPARABLEFILTER2DPROC;

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram: PFNGLGETHISTOGRAMPROC;
   glGetHistogramParameterfv: PFNGLGETHISTOGRAMPARAMETERFVPROC;
   glGetHistogramParameteriv: PFNGLGETHISTOGRAMPARAMETERIVPROC;
   glGetMinmax: PFNGLGETMINMAXPROC;
   glGetMinmaxParameterfv: PFNGLGETMINMAXPARAMETERFVPROC;
   glGetMinmaxParameteriv: PFNGLGETMINMAXPARAMETERIVPROC;
   glHistogram: PFNGLHISTOGRAMPROC;
   glMinmax: PFNGLMINMAXPROC;
   glResetHistogram: PFNGLRESETHISTOGRAMPROC;
   glResetMinmax: PFNGLRESETMINMAXPROC;
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.3 Core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture: PFNGLACTIVETEXTUREPROC;

   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage: PFNGLSAMPLECOVERAGEPROC;

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D: PFNGLCOMPRESSEDTEXIMAGE3DPROC;
   glCompressedTexImage2D: PFNGLCOMPRESSEDTEXIMAGE2DPROC;
   glCompressedTexImage1D: PFNGLCOMPRESSEDTEXIMAGE1DPROC;
   glCompressedTexSubImage3D: PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC;
   glCompressedTexSubImage2D: PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC;
   glCompressedTexSubImage1D: PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC;
   glGetCompressedTexImage: PFNGLGETCOMPRESSEDTEXIMAGEPROC;

{$IFDEF GLS_REGIONS} {$region 'OpenGL 1.3 deprecated'} {$ENDIF}
   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glClientActiveTexture: PFNGLCLIENTACTIVETEXTUREPROC;
   glMultiTexCoord1d: PFNGLMULTITEXCOORD1DPROC;
   glMultiTexCoord1dV: PFNGLMULTITEXCOORD1DVPROC;
   glMultiTexCoord1f: PFNGLMULTITEXCOORD1FPROC;
   glMultiTexCoord1fV: PFNGLMULTITEXCOORD1FVPROC;
   glMultiTexCoord1i: PFNGLMULTITEXCOORD1IPROC;
   glMultiTexCoord1iV: PFNGLMULTITEXCOORD1IVPROC;
   glMultiTexCoord1s: PFNGLMULTITEXCOORD1SPROC;
   glMultiTexCoord1sV: PFNGLMULTITEXCOORD1SVPROC;
   glMultiTexCoord2d: PFNGLMULTITEXCOORD2DPROC;
   glMultiTexCoord2dv: PFNGLMULTITEXCOORD2DVPROC;
   glMultiTexCoord2f: PFNGLMULTITEXCOORD2FPROC;
   glMultiTexCoord2fv: PFNGLMULTITEXCOORD2FVPROC;
   glMultiTexCoord2i: PFNGLMULTITEXCOORD2IPROC;
   glMultiTexCoord2iv: PFNGLMULTITEXCOORD2IVPROC;
   glMultiTexCoord2s: PFNGLMULTITEXCOORD2SPROC;
   glMultiTexCoord2sv: PFNGLMULTITEXCOORD2SVPROC;
   glMultiTexCoord3d: PFNGLMULTITEXCOORD3DPROC;
   glMultiTexCoord3dv: PFNGLMULTITEXCOORD3DVPROC;
   glMultiTexCoord3f: PFNGLMULTITEXCOORD3FPROC;
   glMultiTexCoord3fv: PFNGLMULTITEXCOORD3FVPROC;
   glMultiTexCoord3i: PFNGLMULTITEXCOORD3IPROC;
   glMultiTexCoord3iv: PFNGLMULTITEXCOORD3IVPROC;
   glMultiTexCoord3s: PFNGLMULTITEXCOORD3SPROC;
   glMultiTexCoord3sv: PFNGLMULTITEXCOORD3SVPROC;
   glMultiTexCoord4d: PFNGLMULTITEXCOORD4DPROC;
   glMultiTexCoord4dv: PFNGLMULTITEXCOORD4DVPROC;
   glMultiTexCoord4f: PFNGLMULTITEXCOORD4FPROC;
   glMultiTexCoord4fv: PFNGLMULTITEXCOORD4FVPROC;
   glMultiTexCoord4i: PFNGLMULTITEXCOORD4IPROC;
   glMultiTexCoord4iv: PFNGLMULTITEXCOORD4IVPROC;
   glMultiTexCoord4s: PFNGLMULTITEXCOORD4SPROC;
   glMultiTexCoord4sv: PFNGLMULTITEXCOORD4SVPROC;

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf: PFNGLLOADTRANSPOSEMATRIXFPROC;
   glLoadTransposeMatrixd: PFNGLLOADTRANSPOSEMATRIXDPROC;
   glMultTransposeMatrixf: PFNGLMULTTRANSPOSEMATRIXFPROC;
   glMultTransposeMatrixd: PFNGLMULTTRANSPOSEMATRIXDPROC;
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.4 Core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate: PFNGLBLENDFUNCSEPARATEPROC;

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays: PFNGLMULTIDRAWARRAYSPROC;
   glMultiDrawElements: PFNGLMULTIDRAWELEMENTSPROC;

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf: PFNGLPOINTPARAMETERFPROC;
   glPointParameterfv: PFNGLPOINTPARAMETERFVPROC;
   glPointParameteri: PFNGLPOINTPARAMETERIPROC;
   glPointParameteriv: PFNGLPOINTPARAMETERIVPROC;

{$IFDEF GLS_REGIONS} {$region 'OpenGL 1.4 deprecated'} {$ENDIF}
   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf: PFNGLFOGCOORDFPROC;
   glFogCoordfv: PFNGLFOGCOORDFVPROC;
   glFogCoordd: PFNGLFOGCOORDDPROC;
   glFogCoorddv: PFNGLFOGCOORDDVPROC;
   glFogCoordPointer: PFNGLFOGCOORDPOINTERPROC;

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b: PFNGLSECONDARYCOLOR3BPROC;
   glSecondaryColor3bv: PFNGLSECONDARYCOLOR3BVPROC;
   glSecondaryColor3d: PFNGLSECONDARYCOLOR3DPROC;
   glSecondaryColor3dv: PFNGLSECONDARYCOLOR3DVPROC;
   glSecondaryColor3f: PFNGLSECONDARYCOLOR3FPROC;
   glSecondaryColor3fv: PFNGLSECONDARYCOLOR3FVPROC;
   glSecondaryColor3i: PFNGLSECONDARYCOLOR3IPROC;
   glSecondaryColor3iv: PFNGLSECONDARYCOLOR3IVPROC;
   glSecondaryColor3s: PFNGLSECONDARYCOLOR3SPROC;
   glSecondaryColor3sv: PFNGLSECONDARYCOLOR3SVPROC;
   glSecondaryColor3ub: PFNGLSECONDARYCOLOR3UBPROC;
   glSecondaryColor3ubv: PFNGLSECONDARYCOLOR3UBVPROC;
   glSecondaryColor3ui: PFNGLSECONDARYCOLOR3UIPROC;
   glSecondaryColor3uiv: PFNGLSECONDARYCOLOR3UIVPROC;
   glSecondaryColor3us: PFNGLSECONDARYCOLOR3USPROC;
   glSecondaryColor3usv: PFNGLSECONDARYCOLOR3USVPROC;
   glSecondaryColorPointer: PFNGLSECONDARYCOLORPOINTERPROC;

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d: PFNGLWINDOWPOS2DPROC;
   glWindowPos2dv: PFNGLWINDOWPOS2DVPROC;
   glWindowPos2f: PFNGLWINDOWPOS2FPROC;
   glWindowPos2fv: PFNGLWINDOWPOS2FVPROC;
   glWindowPos2i: PFNGLWINDOWPOS2IPROC;
   glWindowPos2iv: PFNGLWINDOWPOS2IVPROC;
   glWindowPos2s: PFNGLWINDOWPOS2SPROC;
   glWindowPos2sv: PFNGLWINDOWPOS2SVPROC;
   glWindowPos3d: PFNGLWINDOWPOS3DPROC;
   glWindowPos3dv: PFNGLWINDOWPOS3DVPROC;
   glWindowPos3f: PFNGLWINDOWPOS3FPROC;
   glWindowPos3fv: PFNGLWINDOWPOS3FVPROC;
   glWindowPos3i: PFNGLWINDOWPOS3IPROC;
   glWindowPos3iv: PFNGLWINDOWPOS3IVPROC;
   glWindowPos3s: PFNGLWINDOWPOS3SPROC;
   glWindowPos3sv: PFNGLWINDOWPOS3SVPROC;
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 1.5 Core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries: PFNGLGENQUERIESPROC;
   glDeleteQueries: PFNGLDELETEQUERIESPROC;
   glIsQuery: PFNGLISQUERYPROC;
   glBeginQuery: PFNGLBEGINQUERYPROC;
   glEndQuery: PFNGLENDQUERYPROC;
   glGetQueryiv: PFNGLGETQUERYIVPROC;
   glGetQueryObjectiv: PFNGLGETQUERYOBJECTIVPROC;
   glGetQueryObjectuiv: PFNGLGETQUERYOBJECTUIVPROC;

   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer: PFNGLBINDBUFFERPROC;
   glDeleteBuffers: PFNGLDELETEBUFFERSPROC;
   glGenBuffers: PFNGLGENBUFFERSPROC;
   glIsBuffer: PFNGLISBUFFERPROC;
   glBufferData: PFNGLBUFFERDATAPROC;
   glBufferSubData: PFNGLBUFFERSUBDATAPROC;
   glGetBufferSubData: PFNGLGETBUFFERSUBDATAPROC;
   glMapBuffer: PFNGLMAPBUFFERPROC;
   glUnmapBuffer: PFNGLUNMAPBUFFERPROC;
   glGetBufferParameteriv: PFNGLGETBUFFERPARAMETERIVPROC;
   glGetBufferPointerv: PFNGLGETBUFFERPOINTERVPROC;

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.0 Core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate: PFNGLBLENDEQUATIONSEPARATEPROC;

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers: PFNGLDRAWBUFFERSPROC;

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate: PFNGLSTENCILOPSEPARATEPROC;
   glStencilFuncSeparate: PFNGLSTENCILFUNCSEPARATEPROC;
   glStencilMaskSeparate: PFNGLSTENCILMASKSEPARATEPROC;

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader: PFNGLATTACHSHADERPROC;
   glBindAttribLocation: PFNGLBINDATTRIBLOCATIONPROC;
   glCompileShader: PFNGLCOMPILESHADERPROC;
   glCreateProgram: PFNGLCREATEPROGRAMPROC;
   glCreateShader: PFNGLCREATESHADERPROC;
   glDeleteProgram: PFNGLDELETEPROGRAMPROC;
   glDeleteShader: PFNGLDELETESHADERPROC;
   glDetachShader: PFNGLDETACHSHADERPROC;
   glDisableVertexAttribArray: PFNGLDISABLEVERTEXATTRIBARRAYPROC;
   glEnableVertexAttribArray: PFNGLENABLEVERTEXATTRIBARRAYPROC;
   glGetActiveAttrib: PFNGLGETACTIVEATTRIBPROC;
   glGetActiveUniform: PFNGLGETACTIVEUNIFORMPROC;
   glGetAttachedShaders: PFNGLGETATTACHEDSHADERSPROC;
   glGetAttribLocation: PFNGLGETATTRIBLOCATIONPROC;
   glGetProgramiv: PFNGLGETPROGRAMIVPROC;
   glGetProgramInfoLog: PFNGLGETPROGRAMINFOLOGPROC;
   glGetShaderiv: PFNGLGETSHADERIVPROC;
   glGetShaderInfoLog: PFNGLGETSHADERINFOLOGPROC;
   glGetShaderSource: PFNGLGETSHADERSOURCEPROC;
   glGetUniformLocation: PFNGLGETUNIFORMLOCATIONPROC;
   glGetUniformfv: PFNGLGETUNIFORMFVPROC;
   glGetUniformiv: PFNGLGETUNIFORMIVPROC;
   glGetVertexAttribdv: PFNGLGETVERTEXATTRIBDVPROC;
   glGetVertexAttribfv: PFNGLGETVERTEXATTRIBFVPROC;
   glGetVertexAttribiv: PFNGLGETVERTEXATTRIBIVPROC;
   glGetVertexAttribPointerv: PFNGLGETVERTEXATTRIBPOINTERVPROC;
   glIsProgram: PFNGLISPROGRAMPROC;
   glIsShader: PFNGLISSHADERPROC;
   glLinkProgram: PFNGLLINKPROGRAMPROC;
   glShaderSource: PFNGLSHADERSOURCEPROC;
   glUseProgram: PFNGLUSEPROGRAMPROC;
   glUniform1f: PFNGLUNIFORM1FPROC;
   glUniform2f: PFNGLUNIFORM2FPROC;
   glUniform3f: PFNGLUNIFORM3FPROC;
   glUniform4f: PFNGLUNIFORM4FPROC;
   glUniform1i: PFNGLUNIFORM1IPROC;
   glUniform2i: PFNGLUNIFORM2IPROC;
   glUniform3i: PFNGLUNIFORM3IPROC;
   glUniform4i: PFNGLUNIFORM4IPROC;
   glUniform1fv: PFNGLUNIFORM1FVPROC;
   glUniform2fv: PFNGLUNIFORM2FVPROC;
   glUniform3fv: PFNGLUNIFORM3FVPROC;
   glUniform4fv: PFNGLUNIFORM4FVPROC;
   glUniform1iv: PFNGLUNIFORM1IVPROC;
   glUniform2iv: PFNGLUNIFORM2IVPROC;
   glUniform3iv: PFNGLUNIFORM3IVPROC;
   glUniform4iv: PFNGLUNIFORM4IVPROC;
   glUniformMatrix2fv: PFNGLUNIFORMMATRIX2FVPROC;
   glUniformMatrix3fv: PFNGLUNIFORMMATRIX3FVPROC;
   glUniformMatrix4fv: PFNGLUNIFORMMATRIX4FVPROC;
   glValidateProgram: PFNGLVALIDATEPROGRAMPROC;
   glVertexAttrib1d: PFNGLVERTEXATTRIB1DPROC;
   glVertexAttrib1dv: PFNGLVERTEXATTRIB1DVPROC;
   glVertexAttrib1f: PFNGLVERTEXATTRIB1FPROC;
   glVertexAttrib1fv: PFNGLVERTEXATTRIB1FVPROC;
   glVertexAttrib1s: PFNGLVERTEXATTRIB1SPROC;
   glVertexAttrib1sv: PFNGLVERTEXATTRIB1SVPROC;
   glVertexAttrib2d: PFNGLVERTEXATTRIB2DPROC;
   glVertexAttrib2dv: PFNGLVERTEXATTRIB2DVPROC;
   glVertexAttrib2f: PFNGLVERTEXATTRIB2FPROC;
   glVertexAttrib2fv: PFNGLVERTEXATTRIB2FVPROC;
   glVertexAttrib2s: PFNGLVERTEXATTRIB2SPROC;
   glVertexAttrib2sv: PFNGLVERTEXATTRIB2SVPROC;
   glVertexAttrib3d: PFNGLVERTEXATTRIB3DPROC;
   glVertexAttrib3dv: PFNGLVERTEXATTRIB3DVPROC;
   glVertexAttrib3f: PFNGLVERTEXATTRIB3FPROC;
   glVertexAttrib3fv: PFNGLVERTEXATTRIB3FVPROC;
   glVertexAttrib3s: PFNGLVERTEXATTRIB3SPROC;
   glVertexAttrib3sv: PFNGLVERTEXATTRIB3SVPROC;
   glVertexAttrib4Nbv: PFNGLVERTEXATTRIB4NBVPROC;
   glVertexAttrib4Niv: PFNGLVERTEXATTRIB4NIVPROC;
   glVertexAttrib4Nsv: PFNGLVERTEXATTRIB4NSVPROC;
   glVertexAttrib4Nub: PFNGLVERTEXATTRIB4NUBPROC;
   glVertexAttrib4Nubv: PFNGLVERTEXATTRIB4NUBVPROC;
   glVertexAttrib4Nuiv: PFNGLVERTEXATTRIB4NUIVPROC;
   glVertexAttrib4Nusv: PFNGLVERTEXATTRIB4NUSVPROC;
   glVertexAttrib4bv: PFNGLVERTEXATTRIB4BVPROC;
   glVertexAttrib4d: PFNGLVERTEXATTRIB4DPROC;
   glVertexAttrib4dv: PFNGLVERTEXATTRIB4DVPROC;
   glVertexAttrib4f: PFNGLVERTEXATTRIB4FPROC;
   glVertexAttrib4fv: PFNGLVERTEXATTRIB4FVPROC;
   glVertexAttrib4iv: PFNGLVERTEXATTRIB4IVPROC;
   glVertexAttrib4s: PFNGLVERTEXATTRIB4SPROC;
   glVertexAttrib4sv: PFNGLVERTEXATTRIB4SVPROC;
   glVertexAttrib4ubv: PFNGLVERTEXATTRIB4UBVPROC;
   glVertexAttrib4uiv: PFNGLVERTEXATTRIB4UIVPROC;
   glVertexAttrib4usv: PFNGLVERTEXATTRIB4USVPROC;
   glVertexAttribPointer: PFNGLVERTEXATTRIBPOINTERPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 2.1 Core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)

   // promoted to core v2.1 from GL_EXT_texture_sRGB (#315)
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv: PFNGLUNIFORMMATRIX2X3FVPROC;
   glUniformMatrix3x2fv: PFNGLUNIFORMMATRIX3X2FVPROC;
   glUniformMatrix2x4fv: PFNGLUNIFORMMATRIX2X4FVPROC;
   glUniformMatrix4x2fv: PFNGLUNIFORMMATRIX4X2FVPROC;
   glUniformMatrix3x4fv: PFNGLUNIFORMMATRIX3X4FVPROC;
   glUniformMatrix4x3fv: PFNGLUNIFORMMATRIX4X3FVPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.0 Core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i: PFNGLVERTEXATTRIBI1IPROC;
   glVertexAttribI2i: PFNGLVERTEXATTRIBI2IPROC;
   glVertexAttribI3i: PFNGLVERTEXATTRIBI3IPROC;
   glVertexAttribI4i: PFNGLVERTEXATTRIBI4IPROC;
   glVertexAttribI1ui: PFNGLVERTEXATTRIBI1UIPROC;
   glVertexAttribI2ui: PFNGLVERTEXATTRIBI2UIPROC;
   glVertexAttribI3ui: PFNGLVERTEXATTRIBI3UIPROC;
   glVertexAttribI4ui: PFNGLVERTEXATTRIBI4UIPROC;
   glVertexAttribI1iv: PFNGLVERTEXATTRIBI1IVPROC;
   glVertexAttribI2iv: PFNGLVERTEXATTRIBI2IVPROC;
   glVertexAttribI3iv: PFNGLVERTEXATTRIBI3IVPROC;
   glVertexAttribI4iv: PFNGLVERTEXATTRIBI4IVPROC;
   glVertexAttribI1uiv: PFNGLVERTEXATTRIBI1UIVPROC;
   glVertexAttribI2uiv: PFNGLVERTEXATTRIBI2UIVPROC;
   glVertexAttribI3uiv: PFNGLVERTEXATTRIBI3UIVPROC;
   glVertexAttribI4uiv: PFNGLVERTEXATTRIBI4UIVPROC;
   glVertexAttribI4bv: PFNGLVERTEXATTRIBI4BVPROC;
   glVertexAttribI4sv: PFNGLVERTEXATTRIBI4SVPROC;
   glVertexAttribI4ubv: PFNGLVERTEXATTRIBI4UBVPROC;
   glVertexAttribI4usv: PFNGLVERTEXATTRIBI4USVPROC;
   glVertexAttribIPointer: PFNGLVERTEXATTRIBIPOINTERPROC;
   glGetVertexAttribIiv: PFNGLGETVERTEXATTRIBIIVPROC;
   glGetVertexAttribIuiv: PFNGLGETVERTEXATTRIBIUIVPROC;
   glUniform1ui: PFNGLUNIFORM1UIPROC;
   glUniform2ui: PFNGLUNIFORM2UIPROC;
   glUniform3ui: PFNGLUNIFORM3UIPROC;
   glUniform4ui: PFNGLUNIFORM4UIPROC;
   glUniform1uiv: PFNGLUNIFORM1UIVPROC;
   glUniform2uiv: PFNGLUNIFORM2UIVPROC;
   glUniform3uiv: PFNGLUNIFORM3UIVPROC;
   glUniform4uiv: PFNGLUNIFORM4UIVPROC;
   glGetUniformuiv: PFNGLGETUNIFORMUIVPROC;
   glBindFragDataLocation: PFNGLBINDFRAGDATALOCATIONPROC;
   glGetFragDataLocation: PFNGLGETFRAGDATALOCATIONPROC;

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender: PFNGLBEGINCONDITIONALRENDERPROC;
   glEndConditionalRender: PFNGLENDCONDITIONALRENDERPROC;

   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor: PFNGLCLAMPCOLORPROC;

   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi: procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   //glClearColorIui: procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   glTexParameterIiv: PFNGLTEXPARAMETERIIVPROC;
   glTexParameterIuiv: PFNGLTEXPARAMETERIUIVPROC;
   glGetTexParameterIiv: PFNGLGETTEXPARAMETERIIVPROC;
   glGetTexParameterIuiv: PFNGLGETTEXPARAMETERIUIVPROC;

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski: PFNGLCOLORMASKIPROC;
   glGetBooleani_v: PFNGLGETBOOLEANI_VPROC;
   glGetIntegeri_v: PFNGLGETINTEGERI_VPROC;
   glEnablei: PFNGLENABLEIPROC;
   glDisablei: PFNGLDISABLEIPROC;
   glIsEnabledi: PFNGLISENABLEDIPROC;

   //promoted to core v3.0 from GL_EXT_transform_feedback
   glBindBufferRange: PFNGLBINDBUFFERRANGEPROC;
   glBindBufferBase: PFNGLBINDBUFFERBASEPROC;
   glBeginTransformFeedback: PFNGLBEGINTRANSFORMFEEDBACKPROC;
   glEndTransformFeedback: PFNGLENDTRANSFORMFEEDBACKPROC;
   glTransformFeedbackVaryings: PFNGLTRANSFORMFEEDBACKVARYINGSPROC;
   glGetTransformFeedbackVarying: PFNGLGETTRANSFORMFEEDBACKVARYINGPROC;

   // New commands in OpenGL 3.0
   glClearBufferiv: PFNGLCLEARBUFFERIVPROC;
   glClearBufferuiv: PFNGLCLEARBUFFERUIVPROC;
   glClearBufferfv: PFNGLCLEARBUFFERFVPROC;
   glClearBufferfi: PFNGLCLEARBUFFERFIPROC;
   glGetStringi: PFNGLGETSTRINGIPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 3.1'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.1 Core
   //  ###########################################################

   glDrawArraysInstanced: PFNGLDRAWARRAYSINSTANCEDPROC;
   glDrawElementsInstanced: PFNGLDRAWELEMENTSINSTANCEDPROC;
   glTexBuffer: PFNGLTEXBUFFERPROC;
   glPrimitiveRestartIndex: PFNGLPRIMITIVERESTARTINDEXPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 3.2'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.2 Core
   //  ###########################################################

   glGetInteger64i_v: PFNGLGETINTEGER64I_VPROC;
   glGetBufferParameteri64v: PFNGLGETBUFFERPARAMETERI64VPROC;
   glFramebufferTexture: PFNGLFRAMEBUFFERTEXTUREPROC;
//   glFramebufferTextureFace: procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; face: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   // OpenGL 3.2 also reuses entry points from these extensions:
   // GL_ARB_draw_elements_base_vertex
   // GL_ARB_provoking_vertex
   // GL_ARB_sync
   // GL_ARB_texture_multisample

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 3.3'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 3.3 Core
   //  ###########################################################

   // New commands in OpenGL 3.3
   glVertexAttribDivisor: PFNGLVERTEXATTRIBDIVISORPROC;

   // OpenGL 3.3 reuses entry points from these extensions:
   // GL_ARB_blend_func_extended (ARB #78)
   // GL_ARB_explicit_attrib_location (ARB #79) (none)
   // GL_ARB_occlusion_query2 (ARB #80)
   // GL_ARB_sampler_objects (ARB #81)
   // GL_ARB_shader_bit_encoding (ARB #82)
   // GL_ARB_texture_rgb10_a2ui (ARB #83)
   // GL_ARB_texture_swizzle (ARB #84)
   // GL_ARB_timer_query (ARB #85)
   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 4.0'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //            extensions integrated into OpenGL 4.0 Core
   //  ###########################################################

   // New commands in OpenGL 4.0
   glMinSampleShading: PFNGLMINSAMPLESHADINGPROC;
   glBlendEquationi: PFNGLBLENDEQUATIONIPROC;
   glBlendEquationSeparatei: PFNGLBLENDEQUATIONSEPARATEIPROC;
   glBlendFunci: PFNGLBLENDFUNCIPROC;
   glBlendFuncSeparatei: PFNGLBLENDFUNCSEPARATEIPROC;

   // OpenGL 4.0 uses entry points from these extensions:
   // GL_ARB_draw_indirect (ARB #87)
   // GL_ARB_gpu_shader5 (ARB #88) (none)
   // GL_ARB_gpu_shader_fp64 (ARB #89)
   // GL_ARB_shader_subroutine (ARB #90)
   // GL_ARB_tessellation_shader (ARB #91)
   // GL_ARB_texture_buffer_object_rgb32 (ARB #92) (none)
   // GL_ARB_transform_feedback2 (ARB #93)
   // GL_ARB_transform_feedback3 (ARB #94)

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'New core function/procedure definitions in OpenGL 4.1'} {$ENDIF}

   // OpenGL 4.1 uses entry points from these extensions:
   // ARB_ES2_compatibility
   // ARB_get_program_binary
   // ARB_separate_shader_objects
   // ARB_shader_precision (no entry points)
   // ARB_vertex_attrib_64bit
   // ARB_viewport_array

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL Utility (GLU) function/procedure definitions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                     GLU extensions
   //  ###########################################################

   // GLU extensions
   gluNurbsCallbackDataEXT: PFNGLUNURBSCALLBACKDATAEXTPROC;
   gluNewNurbsTessellatorEXT: PFNGLUNEWNURBSTESSELLATOREXTPROC;
   gluDeleteNurbsTessellatorEXT: PFNGLUDELETENURBSTESSELLATOREXTPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'Windows OpenGL (WGL) function/procedure definitions for ARB approved extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB: PFNWGLCREATEBUFFERREGIONARBPROC;
   wglDeleteBufferRegionARB: PFNWGLDELETEBUFFERREGIONARBPROC;
   wglSaveBufferRegionARB: PFNWGLSAVEBUFFERREGIONARBPROC;
   wglRestoreBufferRegionARB: PFNWGLRESTOREBUFFERREGIONARBPROC;

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC;
   wglGetPixelFormatAttribfvARB: PFNWGLGETPIXELFORMATATTRIBFVARBPROC;
   wglChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB: PFNWGLMAKECONTEXTCURRENTARBPROC;
   wglGetCurrentReadDCARB: PFNWGLGETCURRENTREADDCARBPROC;

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB: PFNWGLCREATEPBUFFERARBPROC;
   wglGetPbufferDCARB: PFNWGLGETPBUFFERDCARBPROC;
   wglReleasePbufferDCARB: PFNWGLRELEASEPBUFFERDCARBPROC;
   wglDestroyPbufferARB: PFNWGLDESTROYPBUFFERARBPROC;
   wglQueryPbufferARB: PFNWGLQUERYPBUFFERARBPROC;

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB: PFNWGLBINDTEXIMAGEARBPROC;
   wglReleaseTexImageARB: PFNWGLRELEASETEXIMAGEARBPROC;
   wglSetPbufferAttribARB: PFNWGLSETPBUFFERATTRIBARBPROC;

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC;

   // WGL_NV_gpu_affinity
   wglEnumGpusNV: PFNWGLENUMGPUSNVPROC;
   wglEnumGpuDevicesNV: PFNWGLENUMGPUDEVICESNVPROC;
   wglCreateAffinityDCNV: PFNWGLCREATEAFFINITYDCNVPROC;
   wglEnumGpusFromAffinityDCNV: PFNWGLENUMGPUSFROMAFFINITYDCNVPROC;
   wglDeleteDCNV: PFNWGLDELETEDCNVPROC;

   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'Windows OpenGL (WGL) function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT: PFNWGLSWAPINTERVALEXTPROC;
   wglGetSwapIntervalEXT: PFNWGLGETSWAPINTERVALEXTPROC;
   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'GLX function/procedure definitions for ARB approved extensions'} {$ENDIF}
 {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               ARB approved GLX extensions
   //  ###########################################################

   // GLX 1.3 and later
   glXChooseFBConfig: PFNGLXCHOOSEFBCONFIGPROC;
   glXGetFBConfigAttrib: PFNGLXGETFBCONFIGATTRIBPROC;
   glXGetFBConfigs: PFNGLXGETFBCONFIGSPROC;
   glXGetVisualFromFBConfig: PFNGLXGETVISUALFROMFBCONFIGPROC;
   glXCreateWindow: PFNGLXCREATEWINDOWPROC;
   glXDestroyWindow: PFNGLXDESTROYWINDOWPROC;
   glXCreatePixmap: PFNGLXCREATEPIXMAPPROC;
   glXDestroyPixmap: PFNGLXDESTROYPIXMAPPROC;
   glXCreatePbuffer: PFNGLXCREATEPBUFFERPROC;
   glXDestroyPbuffer: PFNGLXDESTROYPBUFFERPROC;
   glXQueryDrawable: PFNGLXQUERYDRAWABLEPROC;
   glXCreateNewContext: PFNGLXCREATENEWCONTEXTPROC;
   glXMakeContextCurrent: PFNGLXMAKECONTEXTCURRENTPROC;
   glXGetCurrentReadDrawable: PFNGLXGETCURRENTREADDRAWABLEPROC;
   glXQueryContext: PFNGLXQUERYCONTEXTPROC;
   glXSelectEvent: PFNGLXSELECTEVENTPROC;
   glXGetSelectedEvent: PFNGLXGETSELECTEDEVENTPROC;
   glXBindTexImageARB: PFNGLXBINDTEXIMAGEARBPROC;
   glXReleaseTexImageARB: PFNGLXRELEASETEXIMAGEARBPROC;
   glxDrawableAttribARB: PFNGLXDRAWABLEATTRIBARBPROC;

   //GLX 1.4
   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB: PFNGLXCREATECONTEXTATTRIBSARBPROC;
   glXGetProcAddress: PFNGLXGETPROCADDRESSPROC;
   glXGetProcAddressARB: PFNGLXGETPROCADDRESSARBPROC;

   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'GLX function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   //  ###########################################################
   //           function and procedure definitions for
   //               Vendor/EXT GLX extensions
   //  ###########################################################

   // GLX_SGI_swap_control (EXT #40)
   glXSwapIntervalSGI: PFNGLXSWAPINTERVALSGIPROC;
   glXGetVideoSyncSGI: PFNGLXGETVIDEOSYNCSGIPROC;
   glXWaitVideoSyncSGI: PFNGLXWAITVIDEOSYNCSGIPROC;
   glXFreeContextEXT: PFNGLXFREECONTEXTEXTPROC;
   glXGetContextIDEXT: PFNGLXGETCONTEXTIDEXTPROC;
   glXGetCurrentDisplayEXT: PFNGLXGETCURRENTDISPLAYEXTPROC;
   glXImportContextEXT: PFNGLXIMPORTCONTEXTEXTPROC;
   glXQueryContextInfoEXT: PFNGLXQUERYCONTEXTINFOEXTPROC;
   glXCopySubBufferMESA: PFNGLXCOPYSUBBUFFERMESAPROC;
   glXCreateGLXPixmapMESA: PFNGLXCREATEGLXPIXMAPMESAPROC;
   glXReleaseBuffersMESA: PFNGLXRELEASEBUFFERSMESAPROC;
   glXSet3DfxModeMESA: PFNGLXSET3DFXMODEMESAPROC;

   glXBindTexImageEXT: PFNGLXBINDTEXIMAGEEXTPROC;
   glXReleaseTexImageEXT: PFNGLXRELEASETEXIMAGEEXTPROC;

   //GLX 1.4
   glXMakeCurrentReadSGI: PFNGLXMAKECURRENTREADSGIPROC;
   glXGetCurrentReadDrawableSGI: PFNGLXGETCURRENTREADDRAWABLESGIPROC;
   glXGetFBConfigAttribSGIX: PFNGLXGETFBCONFIGATTRIBSGIXPROC;
   glXChooseFBConfigSGIX: PFNGLXCHOOSEFBCONFIGSGIXPROC;
   glXCreateGLXPixmapWithConfigSGIX: PFNGLXCREATEGLXPIXMAPWITHCONFIGSGIXPROC;
   glXCreateContextWithConfigSGIX: PFNGLXCREATECONTEXTWITHCONFIGSGIXPROC;
   glXGetVisualFromFBConfigSGIX: PFNGLXGETVISUALFROMFBCONFIGSGIXPROC;
   glXGetFBConfigFromVisualSGIX: PFNGLXGETFBCONFIGFROMVISUALSGIXPROC;
   glXCreateGLXPbufferSGIX: PFNGLXCREATEGLXPBUFFERSGIXPROC;
   glXDestroyGLXPbufferSGIX: PFNGLXDESTROYGLXPBUFFERSGIXPROC;
   glXQueryGLXPbufferSGIX: PFNGLXQUERYGLXPBUFFERSGIXPROC;
   glXSelectEventSGIX: PFNGLXSELECTEVENTSGIXPROC;
   glXGetSelectedEventSGIX: PFNGLXGETSELECTEDEVENTSGIXPROC;
   glXCushionSGI: PFNGLXCUSHIONSGIPROC;
   glXBindChannelToWindowSGIX: PFNGLXBINDCHANNELTOWINDOWSGIXPROC;
   glXChannelRectSGIX: PFNGLXCHANNELRECTSGIXPROC;
   glXQueryChannelRectSGIX: PFNGLXQUERYCHANNELRECTSGIXPROC;
   glXQueryChannelDeltasSGIX: PFNGLXQUERYCHANNELDELTASSGIXPROC;
   glXChannelRectSyncSGIX: PFNGLXCHANNELRECTSYNCSGIXPROC;
   glXJoinSwapGroupSGIX: PFNGLXJOINSWAPGROUPSGIXPROC;
   glXBindSwapBarrierSGIX: PFNGLXBINDSWAPBARRIERSGIXPROC;
   glXQueryMaxSwapBarriersSGIX: PFNGLXQUERYMAXSWAPBARRIERSSGIXPROC;
   glXQueryHyperpipeNetworkSGIX: PFNGLXQUERYHYPERPIPENETWORKSGIXPROC;
   glXHyperpipeConfigSGIX: PFNGLXHYPERPIPECONFIGSGIXPROC;
   glXQueryHyperpipeConfigSGIX: PFNGLXQUERYHYPERPIPECONFIGSGIXPROC;
   glXDestroyHyperpipeConfigSGIX: PFNGLXDESTROYHYPERPIPECONFIGSGIXPROC;
   glXBindHyperpipeSGIX: PFNGLXBINDHYPERPIPESGIXPROC;
   glXQueryHyperpipeBestAttribSGIX: PFNGLXQUERYHYPERPIPEBESTATTRIBSGIXPROC;
   glXHyperpipeAttribSGIX: PFNGLXHYPERPIPEATTRIBSGIXPROC;
   glXQueryHyperpipeAttribSGIX: PFNGLXQUERYHYPERPIPEATTRIBSGIXPROC;
   glXGetAGPOffsetMESA: PFNGLXGETAGPOFFSETMESAPROC;
   glXEnumerateVideoDevicesNV: PFNGLXENUMERATEVIDEODEVICESNVPROC;
   glXBindVideoDeviceNV: PFNGLXBINDVIDEODEVICENVPROC;
   glxGetVideoDeviceNV: PFNGLXGETVIDEODEVICENVPROC;

   glXAllocateMemoryNV: PFNGLXALLOCATEMEMORYNVPROC;
   glXFreeMemoryNV: PFNGLXFREEMEMORYNVPROC;

   glXReleaseVideoDeviceNV: PFNGLXRELEASEVIDEODEVICENVPROC;
   glXBindVideoImageNV: PFNGLXBINDVIDEOIMAGENVPROC;
   glXReleaseVideoImageNV: PFNGLXRELEASEVIDEOIMAGENVPROC;
   glXSendPbufferToVideoNV: PFNGLXSENDPBUFFERTOVIDEONVPROC;
   glXGetVideoInfoNV: PFNGLXGETVIDEOINFONVPROC;
   glXJoinSwapGroupNV: PFNGLXJOINSWAPGROUPNVPROC;
   glXBindSwapBarrierNV: PFNGLXBINDSWAPBARRIERNVPROC;
   glXQuerySwapGroupNV: PFNGLXQUERYSWAPGROUPNVPROC;
   glXQueryMaxSwapGroupsNV: PFNGLXQUERYMAXSWAPGROUPSNVPROC;
   glXQueryFrameCountNV: PFNGLXQUERYFRAMECOUNTNVPROC;
   glXResetFrameCountNV: PFNGLXRESETFRAMECOUNTNVPROC;
   glXBindVideoCaptureDeviceNV: PFNGLXBINDVIDEOCAPTUREDEVICENVPROC;
   glXEnumerateVideoCaptureDevicesNV: PFNGLXENUMERATEVIDEOCAPTUREDEVICESNVPROC;
   glxLockVideoCaptureDeviceNV: PFNGLXLOCKVIDEOCAPTUREDEVICENVPROC;
   glXQueryVideoCaptureDeviceNV: PFNGLXQUERYVIDEOCAPTUREDEVICENVPROC;
   glXReleaseVideoCaptureDeviceNV: PFNGLXRELEASEVIDEOCAPTUREDEVICENVPROC;
   glXSwapIntervalEXT: PFNGLXSWAPINTERVALEXTPROC;
   glXCopyImageSubDataNV: PFNGLXCOPYIMAGESUBDATANVPROC;

   {$ENDIF}
{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL function/procedure definitions for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                  ARB approved extensions
   //  ###########################################################

   // unknown ARB extension
   glSamplePassARB: PFNGLSAMPLEPASSARBPROC;
   
   // GL_ARB_multitexture (ARB #1)
   glActiveTextureARB: PFNGLACTIVETEXTUREARBPROC;
   glClientActiveTextureARB: PFNGLCLIENTACTIVETEXTUREARBPROC;
   glMultiTexCoord1dARB: PFNGLMULTITEXCOORD1DARBPROC;
   glMultiTexCoord1dVARB: PFNGLMULTITEXCOORD1DVARBPROC;
   glMultiTexCoord1fARB: PFNGLMULTITEXCOORD1FARBPROC;
   glMultiTexCoord1fVARB: PFNGLMULTITEXCOORD1FVARBPROC;
   glMultiTexCoord1iARB: PFNGLMULTITEXCOORD1IARBPROC;
   glMultiTexCoord1iVARB: PFNGLMULTITEXCOORD1IVARBPROC;
   glMultiTexCoord1sARB: PFNGLMULTITEXCOORD1SARBPROC;
   glMultiTexCoord1sVARB: PFNGLMULTITEXCOORD1SVARBPROC;
   glMultiTexCoord2dARB: PFNGLMULTITEXCOORD2DARBPROC;
   glMultiTexCoord2dvARB: PFNGLMULTITEXCOORD2DVARBPROC;
   glMultiTexCoord2fARB: PFNGLMULTITEXCOORD2FARBPROC;
   glMultiTexCoord2fvARB: PFNGLMULTITEXCOORD2FVARBPROC;
   glMultiTexCoord2iARB: PFNGLMULTITEXCOORD2IARBPROC;
   glMultiTexCoord2ivARB: PFNGLMULTITEXCOORD2IVARBPROC;
   glMultiTexCoord2sARB: PFNGLMULTITEXCOORD2SARBPROC;
   glMultiTexCoord2svARB: PFNGLMULTITEXCOORD2SVARBPROC;
   glMultiTexCoord3dARB: PFNGLMULTITEXCOORD3DARBPROC;
   glMultiTexCoord3dvARB: PFNGLMULTITEXCOORD3DVARBPROC;
   glMultiTexCoord3fARB: PFNGLMULTITEXCOORD3FARBPROC;
   glMultiTexCoord3fvARB: PFNGLMULTITEXCOORD3FVARBPROC;
   glMultiTexCoord3iARB: PFNGLMULTITEXCOORD3IARBPROC;
   glMultiTexCoord3ivARB: PFNGLMULTITEXCOORD3IVARBPROC;
   glMultiTexCoord3sARB: PFNGLMULTITEXCOORD3SARBPROC;
   glMultiTexCoord3svARB: PFNGLMULTITEXCOORD3SVARBPROC;
   glMultiTexCoord4dARB: PFNGLMULTITEXCOORD4DARBPROC;
   glMultiTexCoord4dvARB: PFNGLMULTITEXCOORD4DVARBPROC;
   glMultiTexCoord4fARB: PFNGLMULTITEXCOORD4FARBPROC;
   glMultiTexCoord4fvARB: PFNGLMULTITEXCOORD4FVARBPROC;
   glMultiTexCoord4iARB: PFNGLMULTITEXCOORD4IARBPROC;
   glMultiTexCoord4ivARB: PFNGLMULTITEXCOORD4IVARBPROC;
   glMultiTexCoord4sARB: PFNGLMULTITEXCOORD4SARBPROC;
   glMultiTexCoord4svARB: PFNGLMULTITEXCOORD4SVARBPROC;

   // GL_ARB_transpose_matrix (ARB #3)
   glLoadTransposeMatrixfARB: PFNGLLOADTRANSPOSEMATRIXFARBPROC;
   glLoadTransposeMatrixdARB: PFNGLLOADTRANSPOSEMATRIXDARBPROC;
   glMultTransposeMatrixfARB: PFNGLMULTTRANSPOSEMATRIXFARBPROC;
   glMultTransposeMatrixdARB: PFNGLMULTTRANSPOSEMATRIXDARBPROC;

   // GL_ARB_multisample (ARB #5)
   glSampleCoverageARB: PFNGLSAMPLECOVERAGEARBPROC;

   // GL_ARB_texture_compression (ARB #12)
   glCompressedTexImage3DARB: PFNGLCOMPRESSEDTEXIMAGE3DARBPROC;
   glCompressedTexImage2DARB: PFNGLCOMPRESSEDTEXIMAGE2DARBPROC;
   glCompressedTexImage1DARB: PFNGLCOMPRESSEDTEXIMAGE1DARBPROC;
   glCompressedTexSubImage3DARB: PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC;
   glCompressedTexSubImage2DARB: PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC;
   glCompressedTexSubImage1DARB: PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC;
   glGetCompressedTexImageARB: PFNGLGETCOMPRESSEDTEXIMAGEARBPROC;

   // GL_ARB_point_parameter (ARB #14)
   glPointParameterfARB: PFNGLPOINTPARAMETERFARBPROC;
   glPointParameterfvARB: PFNGLPOINTPARAMETERFVARBPROC;

   // GL_ARB_vertex_blend (ARB #15) {deprecated?}
   glWeightbvARB: PFNGLWEIGHTBVARBPROC;
   glWeightsvARB: PFNGLWEIGHTSVARBPROC;
   glWeightivARB: PFNGLWEIGHTIVARBPROC;
   glWeightfvARB: PFNGLWEIGHTFVARBPROC;
   glWeightdvARB: PFNGLWEIGHTDVARBPROC;
   glWeightubvARB: PFNGLWEIGHTUBVARBPROC;
   glWeightusvARB: PFNGLWEIGHTUSVARBPROC;
   glWeightuivARB: PFNGLWEIGHTUIVARBPROC;
   glWeightPointerARB: PFNGLWEIGHTPOINTERARBPROC;
   glVertexBlendARB: PFNGLVERTEXBLENDARBPROC;

   // GL_ARB_matrix_palette (ARB #16) {deprecated?}
   glCurrentPaletteMatrixARB: PFNGLCURRENTPALETTEMATRIXARBPROC;
   glMatrixIndexubvARB: PFNGLMATRIXINDEXUBVARBPROC;
   glMatrixIndexusvARB: PFNGLMATRIXINDEXUSVARBPROC;
   glMatrixIndexuivARB: PFNGLMATRIXINDEXUIVARBPROC;
   glMatrixIndexPointerARB: PFNGLMATRIXINDEXPOINTERARBPROC;

   // GL_ARB_window_pos (ARB #25)
   glWindowPos2dARB: PFNGLWINDOWPOS2DARBPROC;
   glWindowPos2dvARB: PFNGLWINDOWPOS2DVARBPROC;
   glWindowPos2fARB: PFNGLWINDOWPOS2FARBPROC;
   glWindowPos2fvARB: PFNGLWINDOWPOS2FVARBPROC;
   glWindowPos2iARB: PFNGLWINDOWPOS2IARBPROC;
   glWindowPos2ivARB: PFNGLWINDOWPOS2IVARBPROC;
   glWindowPos2sARB: PFNGLWINDOWPOS2SARBPROC;
   glWindowPos2svARB: PFNGLWINDOWPOS2SVARBPROC;
   glWindowPos3dARB: PFNGLWINDOWPOS3DARBPROC;
   glWindowPos3dvARB: PFNGLWINDOWPOS3DVARBPROC;
   glWindowPos3fARB: PFNGLWINDOWPOS3FARBPROC;
   glWindowPos3fvARB: PFNGLWINDOWPOS3FVARBPROC;
   glWindowPos3iARB: PFNGLWINDOWPOS3IARBPROC;
   glWindowPos3ivARB: PFNGLWINDOWPOS3IVARBPROC;
   glWindowPos3sARB: PFNGLWINDOWPOS3SARBPROC;
   glWindowPos3svARB: PFNGLWINDOWPOS3SVARBPROC;

   // GL_ARB_vertex_program (ARB #26)
   glVertexAttrib1dARB: PFNGLVERTEXATTRIB1DARBPROC;
   glVertexAttrib1dvARB: PFNGLVERTEXATTRIB1DVARBPROC;
   glVertexAttrib1fARB: PFNGLVERTEXATTRIB1FARBPROC;
   glVertexAttrib1fvARB: PFNGLVERTEXATTRIB1FVARBPROC;
   glVertexAttrib1sARB: PFNGLVERTEXATTRIB1SARBPROC;
   glVertexAttrib1svARB: PFNGLVERTEXATTRIB1SVARBPROC;
   glVertexAttrib2dARB: PFNGLVERTEXATTRIB2DARBPROC;
   glVertexAttrib2dvARB: PFNGLVERTEXATTRIB2DVARBPROC;
   glVertexAttrib2fARB: PFNGLVERTEXATTRIB2FARBPROC;
   glVertexAttrib2fvARB: PFNGLVERTEXATTRIB2FVARBPROC;
   glVertexAttrib2sARB: PFNGLVERTEXATTRIB2SARBPROC;
   glVertexAttrib2svARB: PFNGLVERTEXATTRIB2SVARBPROC;
   glVertexAttrib3dARB: PFNGLVERTEXATTRIB3DARBPROC;
   glVertexAttrib3dvARB: PFNGLVERTEXATTRIB3DVARBPROC;
   glVertexAttrib3fARB: PFNGLVERTEXATTRIB3FARBPROC;
   glVertexAttrib3fvARB: PFNGLVERTEXATTRIB3FVARBPROC;
   glVertexAttrib3sARB: PFNGLVERTEXATTRIB3SARBPROC;
   glVertexAttrib3svARB: PFNGLVERTEXATTRIB3SVARBPROC;
   glVertexAttrib4NbvARB: PFNGLVERTEXATTRIB4NBVARBPROC;
   glVertexAttrib4NivARB: PFNGLVERTEXATTRIB4NIVARBPROC;
   glVertexAttrib4NsvARB: PFNGLVERTEXATTRIB4NSVARBPROC;
   glVertexAttrib4NubARB: PFNGLVERTEXATTRIB4NUBARBPROC;
   glVertexAttrib4NubvARB: PFNGLVERTEXATTRIB4NUBVARBPROC;
   glVertexAttrib4NuivARB: PFNGLVERTEXATTRIB4NUIVARBPROC;
   glVertexAttrib4NusvARB: PFNGLVERTEXATTRIB4NUSVARBPROC;
   glVertexAttrib4bvARB: PFNGLVERTEXATTRIB4BVARBPROC;
   glVertexAttrib4dARB: PFNGLVERTEXATTRIB4DARBPROC;
   glVertexAttrib4dvARB: PFNGLVERTEXATTRIB4DVARBPROC;
   glVertexAttrib4fARB: PFNGLVERTEXATTRIB4FARBPROC;
   glVertexAttrib4fvARB: PFNGLVERTEXATTRIB4FVARBPROC;
   glVertexAttrib4ivARB: PFNGLVERTEXATTRIB4IVARBPROC;
   glVertexAttrib4sARB: PFNGLVERTEXATTRIB4SARBPROC;
   glVertexAttrib4svARB: PFNGLVERTEXATTRIB4SVARBPROC;
   glVertexAttrib4ubvARB: PFNGLVERTEXATTRIB4UBVARBPROC;
   glVertexAttrib4uivARB: PFNGLVERTEXATTRIB4UIVARBPROC;
   glVertexAttrib4usvARB: PFNGLVERTEXATTRIB4USVARBPROC;
   glVertexAttribPointerARB: PFNGLVERTEXATTRIBPOINTERARBPROC;
   glEnableVertexAttribArrayARB: PFNGLENABLEVERTEXATTRIBARRAYARBPROC;
   glDisableVertexAttribArrayARB: PFNGLDISABLEVERTEXATTRIBARRAYARBPROC;
   glProgramStringARB: PFNGLPROGRAMSTRINGARBPROC;
   glBindProgramARB: PFNGLBINDPROGRAMARBPROC;
   glDeleteProgramsARB: PFNGLDELETEPROGRAMSARBPROC;
   glGenProgramsARB: PFNGLGENPROGRAMSARBPROC;
   glProgramEnvParameter4dARB: PFNGLPROGRAMENVPARAMETER4DARBPROC;
   glProgramEnvParameter4dvARB: PFNGLPROGRAMENVPARAMETER4DVARBPROC;
   glProgramEnvParameter4fARB: PFNGLPROGRAMENVPARAMETER4FARBPROC;
   glProgramEnvParameter4fvARB: PFNGLPROGRAMENVPARAMETER4FVARBPROC;
   glProgramLocalParameter4dARB: PFNGLPROGRAMLOCALPARAMETER4DARBPROC;
   glProgramLocalParameter4dvARB: PFNGLPROGRAMLOCALPARAMETER4DVARBPROC;
   glProgramLocalParameter4fARB: PFNGLPROGRAMLOCALPARAMETER4FARBPROC;
   glProgramLocalParameter4fvARB: PFNGLPROGRAMLOCALPARAMETER4FVARBPROC;
   glGetProgramEnvParameterdvARB: PFNGLGETPROGRAMENVPARAMETERDVARBPROC;
   glGetProgramEnvParameterfvARB: PFNGLGETPROGRAMENVPARAMETERFVARBPROC;
   glGetProgramLocalParameterdvARB: PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC;
   glGetProgramLocalParameterfvARB: PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC;
   glGetProgramivARB: PFNGLGETPROGRAMIVARBPROC;
   glGetProgramStringARB: PFNGLGETPROGRAMSTRINGARBPROC;
   glGetVertexAttribdvARB: PFNGLGETVERTEXATTRIBDVARBPROC;
   glGetVertexAttribfvARB: PFNGLGETVERTEXATTRIBFVARBPROC;
   glGetVertexAttribivARB: PFNGLGETVERTEXATTRIBIVARBPROC;
   glGetVertexAttribPointervARB: PFNGLGETVERTEXATTRIBPOINTERVARBPROC;
   glIsProgramARB: PFNGLISPROGRAMARBPROC;

   // GL_ARB_vertex_buffer_object (ARB #28)
   glBindBufferARB: PFNGLBINDBUFFERARBPROC;
   glDeleteBuffersARB: PFNGLDELETEBUFFERSARBPROC;
   glGenBuffersARB: PFNGLGENBUFFERSARBPROC;
   glIsBufferARB: PFNGLISBUFFERARBPROC;
   glBufferDataARB: PFNGLBUFFERDATAARBPROC;
   glBufferSubDataARB: PFNGLBUFFERSUBDATAARBPROC;
   glGetBufferSubDataARB: PFNGLGETBUFFERSUBDATAARBPROC;
   glMapBufferARB: PFNGLMAPBUFFERARBPROC;
   glUnmapBufferARB: PFNGLUNMAPBUFFERARBPROC;
   glGetBufferParameterivARB: PFNGLGETBUFFERPARAMETERIVARBPROC;
   glGetBufferPointervARB: PFNGLGETBUFFERPOINTERVARBPROC;

   // GL_ARB_occlusion_query (ARB #29)
   glGenQueriesARB: PFNGLGENQUERIESARBPROC;
   glDeleteQueriesARB: PFNGLDELETEQUERIESARBPROC;
   glIsQueryARB: PFNGLISQUERYARBPROC;
   glBeginQueryARB: PFNGLBEGINQUERYARBPROC;
   glEndQueryARB: PFNGLENDQUERYARBPROC;
   glGetQueryivARB: PFNGLGETQUERYIVARBPROC;
   glGetQueryObjectivARB: PFNGLGETQUERYOBJECTIVARBPROC;
   glGetQueryObjectuivARB: PFNGLGETQUERYOBJECTUIVARBPROC;

   // GL_ARB_shader_objects (ARB #30)
   glDeleteObjectARB: PFNGLDELETEOBJECTARBPROC;
   glGetHandleARB: PFNGLGETHANDLEARBPROC;
   glDetachObjectARB: PFNGLDETACHOBJECTARBPROC;
   glCreateShaderObjectARB: PFNGLCREATESHADEROBJECTARBPROC;
   glShaderSourceARB: PFNGLSHADERSOURCEARBPROC;
   glCompileShaderARB: PFNGLCOMPILESHADERARBPROC;
   glCreateProgramObjectARB: PFNGLCREATEPROGRAMOBJECTARBPROC;
   glAttachObjectARB: PFNGLATTACHOBJECTARBPROC;
   glLinkProgramARB: PFNGLLINKPROGRAMARBPROC;
   glUseProgramObjectARB: PFNGLUSEPROGRAMOBJECTARBPROC;
   glValidateProgramARB: PFNGLVALIDATEPROGRAMARBPROC;
   glUniform1fARB: PFNGLUNIFORM1FARBPROC;
   glUniform2fARB: PFNGLUNIFORM2FARBPROC;
   glUniform3fARB: PFNGLUNIFORM3FARBPROC;
   glUniform4fARB: PFNGLUNIFORM4FARBPROC;
   glUniform1iARB: PFNGLUNIFORM1IARBPROC;
   glUniform2iARB: PFNGLUNIFORM2IARBPROC;
   glUniform3iARB: PFNGLUNIFORM3IARBPROC;
   glUniform4iARB: PFNGLUNIFORM4IARBPROC;
   glUniform1fvARB: PFNGLUNIFORM1FVARBPROC;
   glUniform2fvARB: PFNGLUNIFORM2FVARBPROC;
   glUniform3fvARB: PFNGLUNIFORM3FVARBPROC;
   glUniform4fvARB: PFNGLUNIFORM4FVARBPROC;
   glUniform1ivARB: PFNGLUNIFORM1IVARBPROC;
   glUniform2ivARB: PFNGLUNIFORM2IVARBPROC;
   glUniform3ivARB: PFNGLUNIFORM3IVARBPROC;
   glUniform4ivARB: PFNGLUNIFORM4IVARBPROC;
   glUniformMatrix2fvARB: PFNGLUNIFORMMATRIX2FVARBPROC;
   glUniformMatrix3fvARB: PFNGLUNIFORMMATRIX3FVARBPROC;
   glUniformMatrix4fvARB: PFNGLUNIFORMMATRIX4FVARBPROC;
   glGetObjectParameterfvARB: PFNGLGETOBJECTPARAMETERFVARBPROC;
   glGetObjectParameterivARB: PFNGLGETOBJECTPARAMETERIVARBPROC;
   glGetInfoLogARB: PFNGLGETINFOLOGARBPROC;
   glGetAttachedObjectsARB: PFNGLGETATTACHEDOBJECTSARBPROC;
   glGetUniformLocationARB: PFNGLGETUNIFORMLOCATIONARBPROC;
   glGetActiveUniformARB: PFNGLGETACTIVEUNIFORMARBPROC;
   glGetUniformfvARB: PFNGLGETUNIFORMFVARBPROC;
   glGetUniformivARB: PFNGLGETUNIFORMIVARBPROC;
   glGetShaderSourceARB: PFNGLGETSHADERSOURCEARBPROC;

   // GL_ARB_vertex_shader (ARB #31)
   glBindAttribLocationARB: PFNGLBINDATTRIBLOCATIONARBPROC;
   glGetActiveAttribARB: PFNGLGETACTIVEATTRIBARBPROC;
   glGetAttribLocationARB: PFNGLGETATTRIBLOCATIONARBPROC;

   // GL_ARB_DrawBuffers (ARB #37)
   glDrawBuffersARB: PFNGLDRAWBUFFERSARBPROC;

   // GL_ARB_color_buffer_float (ARB #39)
   glClampColorARB: PFNGLCLAMPCOLORARBPROC;

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB: PFNGLDRAWARRAYSINSTANCEDARBPROC;
   glDrawElementsInstancedARB: PFNGLDRAWELEMENTSINSTANCEDARBPROC;

   // GL_ARB_framebuffer_object (ARB #45)
   glIsRenderbuffer: PFNGLISRENDERBUFFERPROC;
   glBindRenderbuffer: PFNGLBINDRENDERBUFFERPROC;
   glDeleteRenderbuffers: PFNGLDELETERENDERBUFFERSPROC;
   glGenRenderbuffers: PFNGLGENRENDERBUFFERSPROC;
   glRenderbufferStorage: PFNGLRENDERBUFFERSTORAGEPROC;
   glRenderbufferStorageMultisample: PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC;
   glGetRenderbufferParameteriv: PFNGLGETRENDERBUFFERPARAMETERIVPROC;
   glIsFramebuffer: PFNGLISFRAMEBUFFERPROC;
   glBindFramebuffer: PFNGLBINDFRAMEBUFFERPROC;
   glDeleteFramebuffers: PFNGLDELETEFRAMEBUFFERSPROC;
   glGenFramebuffers: PFNGLGENFRAMEBUFFERSPROC;
   glCheckFramebufferStatus: PFNGLCHECKFRAMEBUFFERSTATUSPROC;
   glFramebufferTexture1D: PFNGLFRAMEBUFFERTEXTURE1DPROC;
   glFramebufferTexture2D: PFNGLFRAMEBUFFERTEXTURE2DPROC;
   glFramebufferTexture3D: PFNGLFRAMEBUFFERTEXTURE3DPROC;
   glFramebufferTextureLayer: PFNGLFRAMEBUFFERTEXTURELAYERPROC;
   glFramebufferRenderbuffer: PFNGLFRAMEBUFFERRENDERBUFFERPROC;
   glGetFramebufferAttachmentParameteriv: PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC;
   glBlitFramebuffer: PFNGLBLITFRAMEBUFFERPROC;
   glGenerateMipmap: PFNGLGENERATEMIPMAPPROC;

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB: PFNGLPROGRAMPARAMETERIARBPROC;
   glFramebufferTextureARB: PFNGLFRAMEBUFFERTEXTUREARBPROC;
   glFramebufferTextureLayerARB: PFNGLFRAMEBUFFERTEXTURELAYERARBPROC;
   glFramebufferTextureFaceARB: PFNGLFRAMEBUFFERTEXTUREFACEARBPROC;

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB: PFNGLVERTEXATTRIBDIVISORARBPROC;

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange: PFNGLMAPBUFFERRANGEPROC;
   glFlushMappedBufferRange: PFNGLFLUSHMAPPEDBUFFERRANGEPROC;

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB: PFNGLTEXBUFFERARBPROC;

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray: PFNGLBINDVERTEXARRAYPROC;
   glDeleteVertexArrays: PFNGLDELETEVERTEXARRAYSPROC;
   glGenVertexArrays: PFNGLGENVERTEXARRAYSPROC;
   glIsVertexArray: PFNGLISVERTEXARRAYPROC;

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices: PFNGLGETUNIFORMINDICESPROC;
   glGetActiveUniformsiv: PFNGLGETACTIVEUNIFORMSIVPROC;
   glGetActiveUniformName: PFNGLGETACTIVEUNIFORMNAMEPROC;
   glGetUniformBlockIndex: PFNGLGETUNIFORMBLOCKINDEXPROC;
   glGetActiveUniformBlockiv: PFNGLGETACTIVEUNIFORMBLOCKIVPROC;
   glGetActiveUniformBlockName: PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC;
   glUniformBlockBinding: PFNGLUNIFORMBLOCKBINDINGPROC;

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData: PFNGLCOPYBUFFERSUBDATAPROC;

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex: PFNGLDRAWELEMENTSBASEVERTEXPROC;
   glDrawRangeElementsBaseVertex: PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC;
   glDrawElementsInstancedBaseVertex: PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC;
   glMultiDrawElementsBaseVertex: PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC;

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex: PFNGLPROVOKINGVERTEXPROC;

   // GL_ARB_sync (ARB #66)
   glFenceSync: PFNGLFENCESYNCPROC;
   glIsSync: PFNGLISSYNCPROC;
   glDeleteSync: PFNGLDELETESYNCPROC;
   glClientWaitSync: PFNGLCLIENTWAITSYNCPROC;
   glWaitSync: PFNGLWAITSYNCPROC;
   glGetInteger64v: PFNGLGETINTEGER64VPROC;
   glGetSynciv: PFNGLGETSYNCIVPROC;

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample: PFNGLTEXIMAGE2DMULTISAMPLEPROC;
   glTexImage3DMultisample: PFNGLTEXIMAGE3DMULTISAMPLEPROC;
   glGetMultisamplefv: PFNGLGETMULTISAMPLEFVPROC;
   glSampleMaski: PFNGLSAMPLEMASKIPROC;

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationiARB: PFNGLBLENDEQUATIONIARBPROC;
   glBlendEquationSeparateiARB: PFNGLBLENDEQUATIONSEPARATEIARBPROC;
   glBlendFunciARB: PFNGLBLENDFUNCIARBPROC;
   glBlendFuncSeparateiARB: PFNGLBLENDFUNCSEPARATEIARBPROC;

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShadingARB: PFNGLMINSAMPLESHADINGARBPROC;

   // GL_ARB_blend_func_extended (ARB #78)
   glBindFragDataLocationIndexed: PFNGLBINDFRAGDATALOCATIONINDEXEDPROC;
   glGetFragDataIndex: PFNGLGETFRAGDATAINDEXPROC;

   // GL_ARB_sampler_objects (ARB #81)
   glGenSamplers: PFNGLGENSAMPLERSPROC;
   glDeleteSamplers: PFNGLDELETESAMPLERSPROC;
   glIsSampler: PFNGLISSAMPLERPROC;
   glBindSampler: PFNGLBINDSAMPLERPROC;
   glSamplerParameteri: PFNGLSAMPLERPARAMETERIPROC;
   glSamplerParameteriv: PFNGLSAMPLERPARAMETERIVPROC;
   glSamplerParameterf: PFNGLSAMPLERPARAMETERFPROC;
   glSamplerParameterfv: PFNGLSAMPLERPARAMETERFVPROC;
   glSamplerParameterIiv: PFNGLSAMPLERPARAMETERIIVPROC;
   glSamplerParameterIuiv: PFNGLSAMPLERPARAMETERIUIVPROC;
   glGetSamplerParameteriv: PFNGLGETSAMPLERPARAMETERIVPROC;
   glGetSamplerParameterIiv: PFNGLGETSAMPLERPARAMETERIIVPROC;
   glGetSamplerParameterfv: PFNGLGETSAMPLERPARAMETERFVPROC;
   glGetSamplerParameterIfv: PFNGLGETSAMPLERPARAMETERIFVPROC;

   // GL_ARB_timer_query (ARB #85)
   glQueryCounter: PFNGLQUERYCOUNTERPROC;
   glGetQueryObjecti64v: PFNGLGETQUERYOBJECTI64VPROC;
   glGetQueryObjectui64v: PFNGLGETQUERYOBJECTUI64VPROC;

   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
   glVertexP2ui: PFNGLVERTEXP2UIPROC;
   glVertexP2uiv: PFNGLVERTEXP2UIVPROC;
   glVertexP3ui: PFNGLVERTEXP3UIPROC;
   glVertexP3uiv: PFNGLVERTEXP3UIVPROC;
   glVertexP4ui: PFNGLVERTEXP4UIPROC;
   glVertexP4uiv: PFNGLVERTEXP4UIVPROC;
   glTexCoordP1ui: PFNGLTEXCOORDP1UIPROC;
   glTexCoordP1uiv: PFNGLTEXCOORDP1UIVPROC;
   glTexCoordP2ui: PFNGLTEXCOORDP2UIPROC;
   glTexCoordP2uiv: PFNGLTEXCOORDP2UIVPROC;
   glTexCoordP3ui: PFNGLTEXCOORDP3UIPROC;
   glTexCoordP3uiv: PFNGLTEXCOORDP3UIVPROC;
   glTexCoordP4ui: PFNGLTEXCOORDP4UIPROC;
   glTexCoordP4uiv: PFNGLTEXCOORDP4UIVPROC;
   glMultiTexCoordP1ui: PFNGLMULTITEXCOORDP1UIPROC;
   glMultiTexCoordP1uiv: PFNGLMULTITEXCOORDP1UIVPROC;
   glMultiTexCoordP2ui: PFNGLMULTITEXCOORDP2UIPROC;
   glMultiTexCoordP2uiv: PFNGLMULTITEXCOORDP2UIVPROC;
   glMultiTexCoordP3ui: PFNGLMULTITEXCOORDP3UIPROC;
   glMultiTexCoordP3uiv: PFNGLMULTITEXCOORDP3UIVPROC;
   glMultiTexCoordP4ui: PFNGLMULTITEXCOORDP4UIPROC;
   glMultiTexCoordP4uiv: PFNGLMULTITEXCOORDP4UIVPROC;
   glNormalP3ui: PFNGLNORMALP3UIPROC;
   glNormalP3uiv: PFNGLNORMALP3UIVPROC;
   glColorP3ui: PFNGLCOLORP3UIPROC;
   glColorP3uiv: PFNGLCOLORP3UIVPROC;
   glColorP4ui: PFNGLCOLORP4UIPROC;
   glColorP4uiv: PFNGLCOLORP4UIVPROC;
   glSecondaryColorP3ui: PFNGLSECONDARYCOLORP3UIPROC;
   glSecondaryColorP3uiv: PFNGLSECONDARYCOLORP3UIVPROC;
   glVertexAttribP1ui: PFNGLVERTEXATTRIBP1UIPROC;
   glVertexAttribP1uiv: PFNGLVERTEXATTRIBP1UIVPROC;
   glVertexAttribP2ui: PFNGLVERTEXATTRIBP2UIPROC;
   glVertexAttribP2uiv: PFNGLVERTEXATTRIBP2UIVPROC;
   glVertexAttribP3ui: PFNGLVERTEXATTRIBP3UIPROC;
   glVertexAttribP3uiv: PFNGLVERTEXATTRIBP3UIVPROC;
   glVertexAttribP4ui: PFNGLVERTEXATTRIBP4UIPROC;
   glVertexAttribP4uiv: PFNGLVERTEXATTRIBP4UIVPROC;

   // GL_ARB_draw_indirect (ARB #87)
   glDrawArraysIndirect: PFNGLDRAWARRAYSINDIRECTPROC;
   glDrawElementsIndirect: PFNGLDRAWELEMENTSINDIRECTPROC;

   // GL_ARB_gpu_shader_fp64 (ARB #89)
   glUniform1d: PFNGLUNIFORM1DPROC;
   glUniform2d: PFNGLUNIFORM2DPROC;
   glUniform3d: PFNGLUNIFORM3DPROC;
   glUniform4d: PFNGLUNIFORM4DPROC;
   glUniform1dv: PFNGLUNIFORM1DVPROC;
   glUniform2dv: PFNGLUNIFORM2DVPROC;
   glUniform3dv: PFNGLUNIFORM3DVPROC;
   glUniform4dv: PFNGLUNIFORM4DVPROC;
   glUniformMatrix2dv: PFNGLUNIFORMMATRIX2DVPROC;
   glUniformMatrix3dv: PFNGLUNIFORMMATRIX3DVPROC;
   glUniformMatrix4dv: PFNGLUNIFORMMATRIX4DVPROC;
   glUniformMatrix2x3dv: PFNGLUNIFORMMATRIX2X3DVPROC;
   glUniformMatrix2x4dv: PFNGLUNIFORMMATRIX2X4DVPROC;
   glUniformMatrix3x2dv: PFNGLUNIFORMMATRIX3X2DVPROC;
   glUniformMatrix3x4dv: PFNGLUNIFORMMATRIX3X4DVPROC;
   glUniformMatrix4x2dv: PFNGLUNIFORMMATRIX4X2DVPROC;
   glUniformMatrix4x3dv: PFNGLUNIFORMMATRIX4X3DVPROC;
   glGetUniformdv: PFNGLGETUNIFORMDVPROC;
   // glProgramUniformXXX only valid if EXT_direct_state_access is available
   glProgramUniform1dEXT: PFNGLPROGRAMUNIFORM1DEXTPROC;
   glProgramUniform2dEXT: PFNGLPROGRAMUNIFORM2DEXTPROC;
   glProgramUniform3dEXT: PFNGLPROGRAMUNIFORM3DEXTPROC;
   glProgramUniform4dEXT: PFNGLPROGRAMUNIFORM4DEXTPROC;
   glProgramUniform1dvEXT: PFNGLPROGRAMUNIFORM1DVEXTPROC;
   glProgramUniform2dvEXT: PFNGLPROGRAMUNIFORM2DVEXTPROC;
   glProgramUniform3dvEXT: PFNGLPROGRAMUNIFORM3DVEXTPROC;
   glProgramUniform4dvEXT: PFNGLPROGRAMUNIFORM4DVEXTPROC;
   glProgramUniformMatrix2dvEXT: PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC;
   glProgramUniformMatrix3dvEXT: PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC;
   glProgramUniformMatrix4dvEXT: PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC;
   glProgramUniformMatrix2x3dvEXT: PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC;
   glProgramUniformMatrix2x4dvEXT: PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC;
   glProgramUniformMatrix3x2dvEXT: PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC;
   glProgramUniformMatrix3x4dvEXT: PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC;
   glProgramUniformMatrix4x2dvEXT: PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC;
   glProgramUniformMatrix4x3dvEXT: PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC;

   // GL_ARB_shader_subroutine (ARB #90)
   glGetSubroutineUniformLocation: PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC;
   glGetSubroutineIndex: PFNGLGETSUBROUTINEINDEXPROC;
   glGetActiveSubroutineUniformiv: PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC;
   glGetActiveSubroutineUniformName: PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC;
   glGetActiveSubroutineName: PFNGLGETACTIVESUBROUTINENAMEPROC;
   glUniformSubroutinesuiv: PFNGLUNIFORMSUBROUTINESUIVPROC;
   glGetUniformSubroutineuiv: PFNGLGETUNIFORMSUBROUTINEUIVPROC;
   glGetProgramStageiv: PFNGLGETPROGRAMSTAGEIVPROC;

   // GL_ARB_tessellation_shader (ARB #91)
   glPatchParameteri: PFNGLPATCHPARAMETERIPROC;
   glPatchParameterfv: PFNGLPATCHPARAMETERFVPROC;

   // GL_ARB_transform_feedback2 (ARB #93)
   glBindTransformFeedback: PFNGLBINDTRANSFORMFEEDBACKPROC;
   glDeleteTransformFeedbacks: PFNGLDELETETRANSFORMFEEDBACKSPROC;
   glGenTransformFeedbacks: PFNGLGENTRANSFORMFEEDBACKSPROC;
   glIsTransformFeedback: PFNGLISTRANSFORMFEEDBACKPROC;
   glPauseTransformFeedback: PFNGLPAUSETRANSFORMFEEDBACKPROC;
   glResumeTransformFeedback: PFNGLRESUMETRANSFORMFEEDBACKPROC;
   glDrawTransformFeedback: PFNGLDRAWTRANSFORMFEEDBACKPROC;

   // GL_ARB_transform_feedback3 (ARB #94)
   glDrawTransformFeedbackStream: PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC;
   glBeginQueryIndexed: PFNGLBEGINQUERYINDEXEDPROC;
   glEndQueryIndexed: PFNGLENDQUERYINDEXEDPROC;
   glGetQueryIndexediv: PFNGLGETQUERYINDEXEDIVPROC;

   // GL_ARB_ES2_compatibility (ARB #95)
   glReleaseShaderCompiler: PFNGLRELEASESHADERCOMPILERPROC;
   glShaderBinary: PFNGLSHADERBINARYPROC;
   glGetShaderPrecisionFormat: PFNGLGETSHADERPRECISIONFORMATPROC;
   glDepthRangef: PFNGLDEPTHRANGEFPROC;
   glClearDepthf: PFNGLCLEARDEPTHFPROC;

   // GL_ARB_get_program_binary (ARB #96)
   glGetProgramBinary: PFNGLGetProgramBinaryPROC;
   glProgramBinary: PFNGLProgramBinaryPROC;
   glProgramParameteri: PFNGLProgramParameteriPROC; // not a core function

   // GL_ARB_separate_shader_objects (ARB #97)
   glUseProgramStages: PFNGLUSEPROGRAMSTAGESPROC;
   glActiveShaderProgram: PFNGLACTIVESHADERPROGRAMPROC;
   glCreateShaderProgramv: PFNGLCREATESHADERPROGRAMVPROC;
   glBindProgramPipeline: PFNGLBINDPROGRAMPIPELINEPROC;
   glDeleteProgramPipelines: PFNGLDELETEPROGRAMPIPELINESPROC;
   glGenProgramPipelines: PFNGLGENPROGRAMPIPELINESPROC;
   glIsProgramPipeline: PFNGLISPROGRAMPIPELINEPROC;
   glGetProgramPipelineiv: PFNGLGETPROGRAMPIPELINEIVPROC;
   glProgramUniform1i: PFNGLPROGRAMUNIFORM1IPROC;
   glProgramUniform1iv: PFNGLPROGRAMUNIFORM1IVPROC;
   glProgramUniform1f: PFNGLPROGRAMUNIFORM1FPROC;
   glProgramUniform1fv: PFNGLPROGRAMUNIFORM1FVPROC;
   glProgramUniform1d: PFNGLPROGRAMUNIFORM1DPROC;
   glProgramUniform1dv: PFNGLPROGRAMUNIFORM1DVPROC;
   glProgramUniform1ui: PFNGLPROGRAMUNIFORM1UIPROC;
   glProgramUniform1uiv: PFNGLPROGRAMUNIFORM1UIVPROC;
   glProgramUniform2i: PFNGLPROGRAMUNIFORM2IPROC;
   glProgramUniform2iv: PFNGLPROGRAMUNIFORM2IVPROC;
   glProgramUniform2f: PFNGLPROGRAMUNIFORM2FPROC;
   glProgramUniform2fv: PFNGLPROGRAMUNIFORM2FVPROC;
   glProgramUniform2d: PFNGLPROGRAMUNIFORM2DPROC;
   glProgramUniform2dv: PFNGLPROGRAMUNIFORM2DVPROC;
   glProgramUniform2ui: PFNGLPROGRAMUNIFORM2UIPROC;
   glProgramUniform2uiv: PFNGLPROGRAMUNIFORM2UIVPROC;
   glProgramUniform3i: PFNGLPROGRAMUNIFORM3IPROC;
   glProgramUniform3iv: PFNGLPROGRAMUNIFORM3IVPROC;
   glProgramUniform3f: PFNGLPROGRAMUNIFORM3FPROC;
   glProgramUniform3fv: PFNGLPROGRAMUNIFORM3FVPROC;
   glProgramUniform3d: PFNGLPROGRAMUNIFORM3DPROC;
   glProgramUniform3dv: PFNGLPROGRAMUNIFORM3DVPROC;
   glProgramUniform3ui: PFNGLPROGRAMUNIFORM3UIPROC;
   glProgramUniform3uiv: PFNGLPROGRAMUNIFORM3UIVPROC;
   glProgramUniform4i: PFNGLPROGRAMUNIFORM4IPROC;
   glProgramUniform4iv: PFNGLPROGRAMUNIFORM4IVPROC;
   glProgramUniform4f: PFNGLPROGRAMUNIFORM4FPROC;
   glProgramUniform4fv: PFNGLPROGRAMUNIFORM4FVPROC;
   glProgramUniform4d: PFNGLPROGRAMUNIFORM4DPROC;
   glProgramUniform4dv: PFNGLPROGRAMUNIFORM4DVPROC;
   glProgramUniform4ui: PFNGLPROGRAMUNIFORM4UIPROC;
   glProgramUniform4uiv: PFNGLPROGRAMUNIFORM4UIVPROC;
   glProgramUniformMatrix2fv: PFNGLPROGRAMUNIFORMMATRIX2FVPROC;
   glProgramUniformMatrix3fv: PFNGLPROGRAMUNIFORMMATRIX3FVPROC;
   glProgramUniformMatrix4fv: PFNGLPROGRAMUNIFORMMATRIX4FVPROC;
   glProgramUniformMatrix2dv: PFNGLPROGRAMUNIFORMMATRIX2DVPROC;
   glProgramUniformMatrix3dv: PFNGLPROGRAMUNIFORMMATRIX3DVPROC;
   glProgramUniformMatrix4dv: PFNGLPROGRAMUNIFORMMATRIX4DVPROC;
   glProgramUniformMatrix2x3fv: PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC;
   glProgramUniformMatrix3x2fv: PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC;
   glProgramUniformMatrix2x4fv: PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC;
   glProgramUniformMatrix4x2fv: PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC;
   glProgramUniformMatrix3x4fv: PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC;
   glProgramUniformMatrix4x3fv: PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC;
   glProgramUniformMatrix2x3dv: PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC;
   glProgramUniformMatrix3x2dv: PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC;
   glProgramUniformMatrix2x4dv: PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC;
   glProgramUniformMatrix4x2dv: PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC;
   glProgramUniformMatrix3x4dv: PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC;
   glProgramUniformMatrix4x3dv: PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC;
   glValidateProgramPipeline: PFNGLVALIDATEPROGRAMPIPELINEPROC;
   glGetProgramPipelineInfoLog: PFNGLGETPROGRAMPIPELINEINFOLOGPROC;

   // GL_ARB_shader_precision (ARB #98)
   // (no entry points)

   // GL_ARB_vertex_attrib_64bit (ARB #99)
   glVertexAttribL1d: PFNGLVERTEXATTRIBL1DPROC;
   glVertexAttribL2d: PFNGLVERTEXATTRIBL2DPROC;
   glVertexAttribL3d: PFNGLVERTEXATTRIBL3DPROC;
   glVertexAttribL4d: PFNGLVERTEXATTRIBL4DPROC;
   glVertexAttribL1dv: PFNGLVERTEXATTRIBL1DVPROC;
   glVertexAttribL2dv: PFNGLVERTEXATTRIBL2DVPROC;
   glVertexAttribL3dv: PFNGLVERTEXATTRIBL3DVPROC;
   glVertexAttribL4dv: PFNGLVERTEXATTRIBL4DVPROC;
   glVertexAttribLPointer: PFNGLVERTEXATTRIBLPOINTERPROC;
   glGetVertexAttribLdv: PFNGLGETVERTEXATTRIBLDVPROC;
   // glVertexArrayVertexAttribLOffsetEXT is only valid if EXT_direct_state_access is available
   glVertexArrayVertexAttribLOffsetEXT: PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC;

   // GL_ARB_viewport_array (ARB #100)
   glViewportArrayv: PFNGLVIEWPORTARRAYVPROC;
   glViewportIndexedf: PFNGLVIEWPORTINDEXEDFPROC;
   glViewportIndexedfv: PFNGLVIEWPORTINDEXEDFVPROC;
   glScissorArrayv: PFNGLSCISSORARRAYVPROC;
   glScissorIndexed: PFNGLSCISSORINDEXEDPROC;
   glScissorIndexedv: PFNGLSCISSORINDEXEDVPROC;
   glDepthRangeArrayv: PFNGLDEPTHRANGEARRAYVPROC;
   glDepthRangeIndexed: PFNGLDEPTHRANGEINDEXEDPROC;
   glGetFloati_v: PFNGLGETFLOATI_VPROC;
   glGetDoublei_v: PFNGLGETDOUBLEI_VPROC;

   // GL_ARB_debug_output (ARB #104)
   glDebugMessageControlARB: PFNGLDEBUGMESSAGECONTROLARBPROC;
   glDebugMessageInsertARB: PFNGLDEBUGMESSAGEINSERTARBPROC;
   glDebugMessageCallbackARB: PFNGLDEBUGMESSAGECALLBACKARBPROC;
   glGetDebugMessageLogARB: PFNGLGETDEBUGMESSAGELOGARBPROC;

   // GL_ARB_robustness (ARB #105)
   glGetGraphicsResetStatusARB: PFNGLGETGRAPHICSRESETSTATUSARBPROC;
   glGetnMapdvARB: PFNGLGETNMAPDVARBPROC;
   glGetnMapfvARB: PFNGLGETNMAPFVARBPROC;
   glGetnMapivARB: PFNGLGETNMAPIVARBPROC;
   glGetnPixelMapfvARB: PFNGLGETNPIXELMAPFVARBPROC;
   glGetnPixelMapuivARB: PFNGLGETNPIXELMAPUIVARBPROC;
   glGetnPixelMapusvARB: PFNGLGETNPIXELMAPUSVARBPROC;
   glGetnPolygonStippleARB: PFNGLGETNPOLYGONSTIPPLEARBPROC;
   glGetnColorTableARB: PFNGLGETNCOLORTABLEARBPROC;
   glGetnConvolutionFilterARB: PFNGLGETNCONVOLUTIONFILTERARBPROC;
   glGetnSeparableFilterARB: PFNGLGETNSEPARABLEFILTERARBPROC;
   glGetnHistogramARB: PFNGLGETNHISTOGRAMARBPROC;
   glGetnMinmaxARB: PFNGLGETNMINMAXARBPROC;
   glGetnTexImageARB: PFNGLGETNTEXIMAGEARBPROC;
   glReadnPixelsARB: PFNGLREADNPIXELSARBPROC;
   glGetnCompressedTexImageARB: PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC;
   glGetnUniformfvARB: PFNGLGETNUNIFORMFVARBPROC;
   glGetnUniformivARB: PFNGLGETNUNIFORMIVARBPROC;
   glGetnUniformuivARB: PFNGLGETNUNIFORMUIVARBPROC;
   glGetnUniformdvARB: PFNGLGETNUNIFORMDVARBPROC;

   // GL_ARB_shader_stencil_export (ARB #106)
   // (no entry points)

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'OpenGL function/procedure definitions for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //           function and procedure definitions for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // Unknown Vendor/EXT functions
   glArrayElementArrayEXT: PFNGLARRAYELEMENTARRAYEXTPROC;

   // GL_WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN: PFNGLADDSWAPHINTRECTWINPROC;

   // GL_EXT_blend_color (EXT #2)
   glBlendColorEXT: PFNGLBLENDCOLOREXTPROC;

   // GL_EXT_polygon_offset (EXT #3)
   glPolygonOffsetEXT: PFNGLPOLYGONOFFSETEXTPROC;

   // GL_EXT_texture3D (EXT #6)
   glTexImage3DEXT: PFNGLTEXIMAGE3DEXTPROC;

   // GL_EXT_subtexture (EXT #9)
   glTexSubImage1DEXT: PFNGLTEXSUBIMAGE1DEXTPROC;
   glTexSubImage2DEXT: PFNGLTEXSUBIMAGE2DEXTPROC;
   glTexSubImage3DEXT: PFNGLTEXSUBIMAGE3DEXTPROC;

   // GL_EXT_copy_texture (EXT #10)
   glCopyTexImage1DEXT: PFNGLCOPYTEXIMAGE1DEXTPROC;
   glCopyTexImage2DEXT: PFNGLCOPYTEXIMAGE2DEXTPROC;
   glCopyTexSubImage1DEXT: PFNGLCOPYTEXSUBIMAGE1DEXTPROC;
   glCopyTexSubImage2DEXT: PFNGLCOPYTEXSUBIMAGE2DEXTPROC;
   glCopyTexSubImage3DEXT: PFNGLCOPYTEXSUBIMAGE3DEXTPROC;

   // GL_EXT_texture_object (EXT #20)
   glGenTexturesEXT: PFNGLGENTEXTURESEXTPROC;
   glDeleteTexturesEXT: PFNGLDELETETEXTURESEXTPROC;
   glBindTextureEXT: PFNGLBINDTEXTUREEXTPROC;
   glPrioritizeTexturesEXT: PFNGLPRIORITIZETEXTURESEXTPROC;
   glAreTexturesResidentEXT: PFNGLARETEXTURESRESIDENTEXTPROC;
   glIsTextureEXT: PFNGLISTEXTUREEXTPROC;

   // GL_SGIS_multisample (EXT #25)
   glSampleMaskSGIS: PFNGLSAMPLEMASKSGISPROC;
   glSamplePatternSGIS: PFNGLSAMPLEPATTERNSGISPROC;

   // GL_EXT_blend_minmax (EXT #37)
   glBlendEquationEXT: PFNGLBLENDEQUATIONEXTPROC;

   // GL_EXT_paletted_texture (EXT #78)
   glColorTableEXT: PFNGLCOLORTABLEEXTPROC;
   glColorSubTableExt: PFNGLCOLORSUBTABLEEXTPROC;
   glGetColorTableEXT: PFNGLGETCOLORTABLEEXTPROC;
   glGetColorTableParameterfvEXT: PFNGLGETCOLORTABLEPARAMETERFVEXTPROC;
   glGetColorTableParameterivEXT: PFNGLGETCOLORTABLEPARAMETERIVEXTPROC;

//   glGetColorTableParameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
//   glGetColorTableParameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GL_EXT_index_material (EXT #94)
   glIndexMaterialEXT: PFNGLINDEXMATERIALEXTPROC;

   // GL_EXT_index_func (EXT #95)
   glIndexFuncEXT: PFNGLINDEXFUNCEXTPROC;

   // GL_EXT_compiled_vertex_array (EXT #97)
   glLockArraysEXT: PFNGLLOCKARRAYSEXTPROC;
   glUnlockArraysEXT: PFNGLUNLOCKARRAYSEXTPROC;

   // GL_EXT_draw_range_elements (EXT #112)
   glDrawRangeElementsEXT: PFNGLDRAWRANGEELEMENTSEXTPROC;

   // GL_EXT_scene_marker (EXT #120)
   glBeginSceneEXT: PFNGLBEGINSCENEEXTPROC;
   glEndSceneEXT: PFNGLENDSCENEEXTPROC;

   // GL_EXT_secondary_color (EXT #145)
   glSecondaryColor3bEXT: PFNGLSECONDARYCOLOR3BEXTPROC;
   glSecondaryColor3bvEXT: PFNGLSECONDARYCOLOR3BVEXTPROC;
   glSecondaryColor3dEXT: PFNGLSECONDARYCOLOR3DEXTPROC;
   glSecondaryColor3dvEXT: PFNGLSECONDARYCOLOR3DVEXTPROC;
   glSecondaryColor3fEXT: PFNGLSECONDARYCOLOR3FEXTPROC;
   glSecondaryColor3fvEXT: PFNGLSECONDARYCOLOR3FVEXTPROC;
   glSecondaryColor3iEXT: PFNGLSECONDARYCOLOR3IEXTPROC;
   glSecondaryColor3ivEXT: PFNGLSECONDARYCOLOR3IVEXTPROC;
   glSecondaryColor3sEXT: PFNGLSECONDARYCOLOR3SEXTPROC;
   glSecondaryColor3svEXT: PFNGLSECONDARYCOLOR3SVEXTPROC;
   glSecondaryColor3ubEXT: PFNGLSECONDARYCOLOR3UBEXTPROC;
   glSecondaryColor3ubvEXT: PFNGLSECONDARYCOLOR3UBVEXTPROC;
   glSecondaryColor3uiEXT: PFNGLSECONDARYCOLOR3UIEXTPROC;
   glSecondaryColor3uivEXT: PFNGLSECONDARYCOLOR3UIVEXTPROC;
   glSecondaryColor3usEXT: PFNGLSECONDARYCOLOR3USEXTPROC;
   glSecondaryColor3usvEXT: PFNGLSECONDARYCOLOR3USVEXTPROC;
   glSecondaryColorPointerEXT: PFNGLSECONDARYCOLORPOINTEREXTPROC;

   // GL_EXT_multi_draw_arrays (EXT #148)
   glMultiDrawArraysEXT: PFNGLMULTIDRAWARRAYSEXTPROC;
   glMultiDrawElementsEXT: PFNGLMULTIDRAWELEMENTSEXTPROC;

   // GL_EXT_fog_coord (EXT #149)
   glFogCoordfEXT: PFNGLFOGCOORDFEXTPROC;
   glFogCoordfvEXT: PFNGLFOGCOORDFVEXTPROC;
   glFogCoorddEXT: PFNGLFOGCOORDDEXTPROC;
   glFogCoorddvEXT: PFNGLFOGCOORDDVEXTPROC;
   glFogCoordPointerEXT: PFNGLFOGCOORDPOINTEREXTPROC;

   // GL_EXT_blend_func_separate (EXT #173)
   glBlendFuncSeparateEXT: PFNGLBLENDFUNCSEPARATEEXTPROC;

   // GL_NV_vertex_array_range (EXT #190)
   glFlushVertexArrayRangeNV: PFNGLFLUSHVERTEXARRAYRANGENVPROC;
   glVertexArrayRangeNV: PFNGLVERTEXARRAYRANGENVPROC;
   wglAllocateMemoryNV: PFNWGLALLOCATEMEMORYNVPROC;
   wglFreeMemoryNV: PFNWGLFREEMEMORYNVPROC;

   // GL_NV_register_combiners (EXT #191)
   glCombinerParameterfvNV: PFNGLCOMBINERPARAMETERFVNVPROC;
   glCombinerParameterfNV: PFNGLCOMBINERPARAMETERFNVPROC;
   glCombinerParameterivNV: PFNGLCOMBINERPARAMETERIVNVPROC;
   glCombinerParameteriNV: PFNGLCOMBINERPARAMETERINVPROC;
   glCombinerInputNV: PFNGLCOMBINERINPUTNVPROC;
   glCombinerOutputNV: PFNGLCOMBINEROUTPUTNVPROC;
   glFinalCombinerInputNV: PFNGLFINALCOMBINERINPUTNVPROC;
   glGetCombinerInputParameterfvNV: PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC;
   glGetCombinerInputParameterivNV: PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC;
   glGetCombinerOutputParameterfvNV: PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC;
   glGetCombinerOutputParameterivNV: PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC;
   glGetFinalCombinerInputParameterfvNV: PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC;
   glGetFinalCombinerInputParameterivNV: PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC;

   // GL_MESA_resize_buffers (EXT #196)
   glResizeBuffersMESA: PFNGLRESIZEBUFFERSMESAPROC;

   // GL_3DFX_tbuffer (EXT #208)
   glTbufferMask3DFX: PFNGLTBUFFERMASK3DFXPROC;

   // GL_EXT_multisample (EXT #209)
   glSampleMaskEXT: PFNGLSAMPLEMASKEXTPROC;
   glSamplePatternEXT: PFNGLSAMPLEPATTERNEXTPROC;

   // GL_SGIS_texture_color_mask (EXT #214)
   glTextureColorMaskSGIS: PFNGLTEXTURECOLORMASKSGISPROC;

   // GL_NV_fence (EXT #222)
   glGenFencesNV: PFNGLGENFENCESNVPROC;
   glDeleteFencesNV: PFNGLDELETEFENCESNVPROC;
   glSetFenceNV: PFNGLSETFENCENVPROC;
   glTestFenceNV: PFNGLTESTFENCENVPROC;
   glFinishFenceNV: PFNGLFINISHFENCENVPROC;
   glIsFenceNV: PFNGLISFENCENVPROC;
   glGetFenceivNV: PFNGLGETFENCEIVNVPROC;

   // GL_NV_vertex_program (EXT #233)
   glAreProgramsResidentNV: PFNGLAREPROGRAMSRESIDENTNVPROC;
   glBindProgramNV: PFNGLBINDPROGRAMNVPROC;
   glDeleteProgramsNV: PFNGLDELETEPROGRAMSNVPROC;
   glExecuteProgramNV: PFNGLEXECUTEPROGRAMNVPROC;
   glGenProgramsNV: PFNGLGENPROGRAMSNVPROC;
   glGetProgramParameterdvNV: PFNGLGETPROGRAMPARAMETERDVNVPROC;
   glGetProgramParameterfvNV: PFNGLGETPROGRAMPARAMETERFVNVPROC;
   glGetProgramivNV: PFNGLGETPROGRAMIVNVPROC;
   glGetProgramStringNV: PFNGLGETPROGRAMSTRINGNVPROC;
   glGetTrackMatrixivNV: PFNGLGETTRACKMATRIXIVNVPROC;
   glGetVertexAttribdvNV: PFNGLGETVERTEXATTRIBDVNVPROC;
   glGetVertexAttribfvNV: PFNGLGETVERTEXATTRIBFVNVPROC;
   glGetVertexAttribivNV: PFNGLGETVERTEXATTRIBIVNVPROC;
   glGetVertexAttribPointervNV: PFNGLGETVERTEXATTRIBPOINTERVNVPROC;
   glIsProgramNV: PFNGLISPROGRAMNVPROC;
   glLoadProgramNV: PFNGLLOADPROGRAMNVPROC;
   glProgramParameter4dNV: PFNGLPROGRAMPARAMETER4DNVPROC;
   glProgramParameter4dvNV: PFNGLPROGRAMPARAMETER4DVNVPROC;
   glProgramParameter4fNV: PFNGLPROGRAMPARAMETER4FNVPROC;
   glProgramParameter4fvNV: PFNGLPROGRAMPARAMETER4FVNVPROC;
   glProgramParameters4dvNV: PFNGLPROGRAMPARAMETERS4DVNVPROC;
   glProgramParameters4fvNV: PFNGLPROGRAMPARAMETERS4FVNVPROC;
   glRequestResidentProgramsNV: PFNGLREQUESTRESIDENTPROGRAMSNVPROC;
   glTrackMatrixNV: PFNGLTRACKMATRIXNVPROC;
   glVertexAttribPointerNV: PFNGLVERTEXATTRIBPOINTERNVPROC;
   glVertexAttrib1dNV: PFNGLVERTEXATTRIB1DNVPROC;
   glVertexAttrib1dvNV: PFNGLVERTEXATTRIB1DVNVPROC;
   glVertexAttrib1fNV: PFNGLVERTEXATTRIB1FNVPROC;
   glVertexAttrib1fvNV: PFNGLVERTEXATTRIB1FVNVPROC;
   glVertexAttrib1sNV: PFNGLVERTEXATTRIB1SNVPROC;
   glVertexAttrib1svNV: PFNGLVERTEXATTRIB1SVNVPROC;
   glVertexAttrib2dNV: PFNGLVERTEXATTRIB2DNVPROC;
   glVertexAttrib2dvNV: PFNGLVERTEXATTRIB2DVNVPROC;
   glVertexAttrib2fNV: PFNGLVERTEXATTRIB2FNVPROC;
   glVertexAttrib2fvNV: PFNGLVERTEXATTRIB2FVNVPROC;
   glVertexAttrib2sNV: PFNGLVERTEXATTRIB2SNVPROC;
   glVertexAttrib2svNV: PFNGLVERTEXATTRIB2SVNVPROC;
   glVertexAttrib3dNV: PFNGLVERTEXATTRIB3DNVPROC;
   glVertexAttrib3dvNV: PFNGLVERTEXATTRIB3DVNVPROC;
   glVertexAttrib3fNV: PFNGLVERTEXATTRIB3FNVPROC;
   glVertexAttrib3fvNV: PFNGLVERTEXATTRIB3FVNVPROC;
   glVertexAttrib3sNV: PFNGLVERTEXATTRIB3SNVPROC;
   glVertexAttrib3svNV: PFNGLVERTEXATTRIB3SVNVPROC;
   glVertexAttrib4dNV: PFNGLVERTEXATTRIB4DNVPROC;
   glVertexAttrib4dvNV: PFNGLVERTEXATTRIB4DVNVPROC;
   glVertexAttrib4fNV: PFNGLVERTEXATTRIB4FNVPROC;
   glVertexAttrib4fvNV: PFNGLVERTEXATTRIB4FVNVPROC;
   glVertexAttrib4sNV: PFNGLVERTEXATTRIB4SNVPROC;
   glVertexAttrib4svNV: PFNGLVERTEXATTRIB4SVNVPROC;
   glVertexAttrib4ubvNV: PFNGLVERTEXATTRIB4UBVNVPROC;
   glVertexAttribs1dvNV: PFNGLVERTEXATTRIBS1DVNVPROC;
   glVertexAttribs1fvNV: PFNGLVERTEXATTRIBS1FVNVPROC;
   glVertexAttribs1svNV: PFNGLVERTEXATTRIBS1SVNVPROC;
   glVertexAttribs2dvNV: PFNGLVERTEXATTRIBS2DVNVPROC;
   glVertexAttribs2fvNV: PFNGLVERTEXATTRIBS2FVNVPROC;
   glVertexAttribs2svNV: PFNGLVERTEXATTRIBS2SVNVPROC;
   glVertexAttribs3dvNV: PFNGLVERTEXATTRIBS3DVNVPROC;
   glVertexAttribs3fvNV: PFNGLVERTEXATTRIBS3FVNVPROC;
   glVertexAttribs3svNV: PFNGLVERTEXATTRIBS3SVNVPROC;
   glVertexAttribs4dvNV: PFNGLVERTEXATTRIBS4DVNVPROC;
   glVertexAttribs4fvNV: PFNGLVERTEXATTRIBS4FVNVPROC;
   glVertexAttribs4svNV: PFNGLVERTEXATTRIBS4SVNVPROC;
   glVertexAttribs4ubvNV: PFNGLVERTEXATTRIBS4UBVNVPROC;

   // GL_NV_occlusion_query (EXT #261)
   glGenOcclusionQueriesNV: PFNGLGENOCCLUSIONQUERIESNVPROC;
   glDeleteOcclusionQueriesNV: PFNGLDELETEOCCLUSIONQUERIESNVPROC;
   glIsOcclusionQueryNV: PFNGLISOCCLUSIONQUERYNVPROC;
   glBeginOcclusionQueryNV: PFNGLBEGINOCCLUSIONQUERYNVPROC;
   glEndOcclusionQueryNV: PFNGLENDOCCLUSIONQUERYNVPROC;
   glGetOcclusionQueryivNV: PFNGLGETOCCLUSIONQUERYIVNVPROC;
   glGetOcclusionQueryuivNV: PFNGLGETOCCLUSIONQUERYUIVNVPROC;

   // GL_NV_point_sprite (EXT #262)
   glPointParameteriNV: PFNGLPOINTPARAMETERINVPROC;
   glPointParameterivNV: PFNGLPOINTPARAMETERIVNVPROC;

   // GL_EXT_stencil_two_side (EXT #268)
   glActiveStencilFaceEXT: PFNGLACTIVESTENCILFACEEXTPROC;

   // GL_ATI_draw_buffers (EXT #277)
   glDrawBuffersATI: PFNGLDRAWBUFFERSATIPROC;

   // GL_NV_primitive_restart (EXT #285)
   glPrimitiveRestartNV: PFNGLPRIMITIVERESTARTNVPROC;
   glPrimitiveRestartIndexNV: PFNGLPRIMITIVERESTARTINDEXNVPROC;

   // GL_EXT_depth_bounds_test (EXT #297)
   glDepthBoundsEXT: PFNGLDEPTHBOUNDSEXTPROC;

   // GL_EXT_blend_equation_separate (EXT #299)
   glBlendEquationSeparateEXT: PFNGLBLENDEQUATIONSEPARATEEXTPROC;

   // GL_EXT_framebuffer_object (EXT #310)
   glIsRenderbufferEXT: PFNGLISRENDERBUFFEREXTPROC;
   glBindRenderbufferEXT: PFNGLBINDRENDERBUFFEREXTPROC;
   glDeleteRenderbuffersEXT: PFNGLDELETERENDERBUFFERSEXTPROC;
   glGenRenderbuffersEXT: PFNGLGENRENDERBUFFERSEXTPROC;
   glRenderbufferStorageEXT: PFNGLRENDERBUFFERSTORAGEEXTPROC;
   glGetRenderbufferParameterivEXT: PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC;
   glIsFramebufferEXT: PFNGLISFRAMEBUFFEREXTPROC;
   glBindFramebufferEXT: PFNGLBINDFRAMEBUFFEREXTPROC;
   glDeleteFramebuffersEXT: PFNGLDELETEFRAMEBUFFERSEXTPROC;
   glGenFramebuffersEXT: PFNGLGENFRAMEBUFFERSEXTPROC;
   glCheckFramebufferStatusEXT: PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC;
   glFramebufferTexture1DEXT: PFNGLFRAMEBUFFERTEXTURE1DEXTPROC;
   glFramebufferTexture2DEXT: PFNGLFRAMEBUFFERTEXTURE2DEXTPROC;
   glFramebufferTexture3DEXT: PFNGLFRAMEBUFFERTEXTURE3DEXTPROC;
   glFramebufferRenderbufferEXT: PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC;
   glGetFramebufferAttachmentParameterivEXT: PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC;
   glGenerateMipmapEXT: PFNGLGENERATEMIPMAPEXTPROC;

   // GL_GREMEDY_string_marker (EXT #311)
   glStringMarkerGREMEDY: PFNGLSTRINGMARKERGREMEDYPROC;

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT: PFNGLSTENCILCLEARTAGEXTPROC;

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT: PFNGLBLITFRAMEBUFFEREXTPROC;

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT: PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC;

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT: PFNGLGETQUERYOBJECTI64VEXTPROC;
   glGetQueryObjectui64vEXT: PFNGLGETQUERYOBJECTUI64VEXTPROC;

   // GL_EXT_gpu_program_parameters (EXT #320)
   glProgramEnvParameters4fvEXT: PFNGLPROGRAMENVPARAMETERS4FVEXTPROC;
   glProgramLocalParameters4fvEXT: PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC;

   // GL_NV_geometry_program4 (EXT #323)
   glProgramVertexLimitNV: PFNGLPROGRAMVERTEXLIMITNVPROC;

   // GL_EXT_geometry_shader4 (EXT #324)
   glProgramParameteriEXT: PFNGLPROGRAMPARAMETERIEXTPROC;
   glFramebufferTextureEXT: PFNGLFRAMEBUFFERTEXTUREEXTPROC;
   glFramebufferTextureLayerEXT: PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC;
   glFramebufferTextureFaceEXT: PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC;

   // GL_EXT_gpu_shader4 (EXT #326)
   glVertexAttribI1iEXT: PFNGLVERTEXATTRIBI1IEXTPROC;
   glVertexAttribI2iEXT: PFNGLVERTEXATTRIBI2IEXTPROC;
   glVertexAttribI3iEXT: PFNGLVERTEXATTRIBI3IEXTPROC;
   glVertexAttribI4iEXT: PFNGLVERTEXATTRIBI4IEXTPROC;
   glVertexAttribI1uiEXT: PFNGLVERTEXATTRIBI1UIEXTPROC;
   glVertexAttribI2uiEXT: PFNGLVERTEXATTRIBI2UIEXTPROC;
   glVertexAttribI3uiEXT: PFNGLVERTEXATTRIBI3UIEXTPROC;
   glVertexAttribI4uiEXT: PFNGLVERTEXATTRIBI4UIEXTPROC;
   glVertexAttribI1ivEXT: PFNGLVERTEXATTRIBI1IVEXTPROC;
   glVertexAttribI2ivEXT: PFNGLVERTEXATTRIBI2IVEXTPROC;
   glVertexAttribI3ivEXT: PFNGLVERTEXATTRIBI3IVEXTPROC;
   glVertexAttribI4ivEXT: PFNGLVERTEXATTRIBI4IVEXTPROC;
   glVertexAttribI1uivEXT: PFNGLVERTEXATTRIBI1UIVEXTPROC;
   glVertexAttribI2uivEXT: PFNGLVERTEXATTRIBI2UIVEXTPROC;
   glVertexAttribI3uivEXT: PFNGLVERTEXATTRIBI3UIVEXTPROC;
   glVertexAttribI4uivEXT: PFNGLVERTEXATTRIBI4UIVEXTPROC;
   glVertexAttribI4bvEXT: PFNGLVERTEXATTRIBI4BVEXTPROC;
   glVertexAttribI4svEXT: PFNGLVERTEXATTRIBI4SVEXTPROC;
   glVertexAttribI4ubvEXT: PFNGLVERTEXATTRIBI4UBVEXTPROC;
   glVertexAttribI4usvEXT: PFNGLVERTEXATTRIBI4USVEXTPROC;
   glVertexAttribIPointerEXT: PFNGLVERTEXATTRIBIPOINTEREXTPROC;
   glGetVertexAttribIivEXT: PFNGLGETVERTEXATTRIBIIVEXTPROC;
   glGetVertexAttribIuivEXT: PFNGLGETVERTEXATTRIBIUIVEXTPROC;
   glUniform1uiEXT: PFNGLUNIFORM1UIEXTPROC;
   glUniform2uiEXT: PFNGLUNIFORM2UIEXTPROC;
   glUniform3uiEXT: PFNGLUNIFORM3UIEXTPROC;
   glUniform4uiEXT: PFNGLUNIFORM4UIEXTPROC;
   glUniform1uivEXT: PFNGLUNIFORM1UIVEXTPROC;
   glUniform2uivEXT: PFNGLUNIFORM2UIVEXTPROC;
   glUniform3uivEXT: PFNGLUNIFORM3UIVEXTPROC;
   glUniform4uivEXT: PFNGLUNIFORM4UIVEXTPROC;
   glGetUniformuivEXT: PFNGLGETUNIFORMUIVEXTPROC;
   glBindFragDataLocationEXT: PFNGLBINDFRAGDATALOCATIONEXTPROC;
   glGetFragDataLocationEXT: PFNGLGETFRAGDATALOCATIONEXTPROC;

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT: PFNGLDRAWARRAYSINSTANCEDEXTPROC;
   glDrawElementsInstancedEXT: PFNGLDRAWELEMENTSINSTANCEDEXTPROC;

   // GL_EXT_packed_float (#328)
   // WGL_EXT_pixel_format_packed_float
   // GLX_EXT_fbconfig_packed_float


   // GL_EXT_texture_array (#329)
   //glFramebufferTextureLayerEXT: procedure(target: TGLenum; attachment: TGLenum;
   //                                texture: TGLuint; level: TGLint; layer: TGLint);


   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT: PFNGLTEXBUFFEREXTPROC;

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT: PFNGLCOLORMASKINDEXEDEXTPROC;
   glGetBooleanIndexedvEXT: PFNGLGETBOOLEANINDEXEDVEXTPROC;
   glGetIntegerIndexedvEXT: PFNGLGETINTEGERINDEXEDVEXTPROC;
   glEnableIndexedEXT: PFNGLENABLEINDEXEDEXTPROC;
   glDisableIndexedEXT: PFNGLDISABLEINDEXEDEXTPROC;
   glIsEnabledIndexedEXT: PFNGLISENABLEDINDEXEDEXTPROC;

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV: PFNGLBINDBUFFERRANGENVPROC;
   glBindBufferOffsetNV: PFNGLBINDBUFFEROFFSETNVPROC;
   glBindBufferBaseNV: PFNGLBINDBUFFERBASENVPROC;
   glTransformFeedbackAttribsNV: PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC;
   glTransformFeedbackVaryingsNV: PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC;
   glBeginTransformFeedbackNV: PFNGLBEGINTRANSFORMFEEDBACKNVPROC;
   glEndTransformFeedbackNV: PFNGLENDTRANSFORMFEEDBACKNVPROC;

   glGetVaryingLocationNV: PFNGLGETVARYINGLOCATIONNVPROC;
   glGetActiveVaryingNV: PFNGLGETACTIVEVARYINGNVPROC;
   glActiveVaryingNV: PFNGLACTIVEVARYINGNVPROC;
   glGetTransformFeedbackVaryingNV: PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC;

   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT: PFNGLUNIFORMBUFFEREXTPROC;
   glGetUniformBufferSizeEXT: PFNGLGETUNIFORMBUFFERSIZEEXTPROC;
   glGetUniformOffsetEXT: PFNGLGETUNIFORMOFFSETEXTPROC;

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT: PFNGLCLEARCOLORIIEXTPROC;
   glClearColorIuiEXT: PFNGLCLEARCOLORIUIEXTPROC;
   glTexParameterIivEXT: PFNGLTEXPARAMETERIIVEXTPROC;
   glTexParameterIuivEXT: PFNGLTEXPARAMETERIUIVEXTPROC;
   glGetTexParameterIivEXT: PFNGLGETTEXPARAMETERIIVEXTPROC;
   glGetTexParameterIuivEXT: PFNGLGETTEXPARAMETERIUIVEXTPROC;

   // GL_GREMEDY_frame_terminator (EXT #345)
   glFrameTerminatorGREMEDY: PFNGLFRAMETERMINATORGREMEDYPROC;

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV: PFNGLBEGINCONDITIONALRENDERNVPROC;
   glEndConditionalRenderNV: PFNGLENDCONDITIONALRENDERNVPROC;

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT: PFNGLBINDBUFFERRANGEEXTPROC;
   glBindBufferOffsetEXT: PFNGLBINDBUFFEROFFSETEXTPROC;
   glBindBufferBaseEXT: PFNGLBINDBUFFERBASEEXTPROC;
   glBeginTransformFeedbackEXT: PFNGLBEGINTRANSFORMFEEDBACKEXTPROC;
   glEndTransformFeedbackEXT: PFNGLENDTRANSFORMFEEDBACKEXTPROC;
   glTransformFeedbackVaryingsEXT: PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC;
   glGetTransformFeedbackVaryingEXT: PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC;

   // GL_AMD_vertex_shader_tessellator (#363)
   glTessellationFactorAMD: PFNGLTESSELLATIONFACTORAMDPROC;
   glTessellationModeAMD: PFNGLTESSELLATIONMODEAMDPROC;

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV: PFNGLCOPYIMAGESUBDATANVPROC;

   // GL_NV_shader_buffer_load (#379)
   glMakeBufferResidentNV: PFNGLMAKEBUFFERRESIDENTNVPROC;
   glMakeBufferNonResidentNV: PFNGLMAKEBUFFERNONRESIDENTNVPROC;
   glIsBufferResidentNV: PFNGLISBUFFERRESIDENTNVPROC;
   glMakeNamedBufferResidentNV: PFNGLMAKENAMEDBUFFERRESIDENTNVPROC;
   glMakeNamedBufferNonResidentNV: PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC;
   glIsNamedBufferResidentNV: PFNGLISNAMEDBUFFERRESIDENTNVPROC;
   glGetBufferParameterui64vNV: PFNGLGETBUFFERPARAMETERUI64VNVPROC;
   glGetNamedBufferParameterui64vNV: PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC;
   glGetIntegerui64vNV: PFNGLGETINTEGERUI64VNVPROC;
   glUniformui64NV: PFNGLUNIFORMUI64NVPROC;
   glUniformui64vNV: PFNGLUNIFORMUI64VNVPROC;
   glGetUniformui64vNV: PFNGLGETUNIFORMUI64VNVPROC;
   glProgramUniformui64NV: PFNGLPROGRAMUNIFORMUI64NVPROC;
   glProgramUniformui64vNV: PFNGLPROGRAMUNIFORMUI64VNVPROC;

   // GL_NV_vertex_buffer_unified_memory (#380)
   glBufferAddressRangeNV: PFNGLBUFFERADDRESSRANGENVPROC;
   glVertexFormatNV: PFNGLVERTEXFORMATNVPROC;
   glNormalFormatNV: PFNGLNORMALFORMATNVPROC;
   glColorFormatNV: PFNGLCOLORFORMATNVPROC;
   glIndexFormatNV: PFNGLINDEXFORMATNVPROC;
   glTexCoordFormatNV: PFNGLTEXCOORDFORMATNVPROC;
   glEdgeFlagFormatNV: PFNGLEDGEFLAGFORMATNVPROC;
   glSecondaryColorFormatNV: PFNGLSECONDARYCOLORFORMATNVPROC;
   glFogCoordFormatNV: PFNGLFOGCOORDFORMATNVPROC;
   glVertexAttribFormatNV: PFNGLVERTEXATTRIBFORMATNVPROC;
   glVertexAttribIFormatNV: PFNGLVERTEXATTRIBIFORMATNVPROC;
   glGetIntegerui64i_vNV: PFNGLGETINTEGERUI64I_VNVPROC;

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}


//------------------------------------------------------------------------------
function GLLibGetProcAddress(ProcName: PGLChar): Pointer;
function GLGetProcAddress(ProcName: PGLChar): Pointer;
procedure ReadExtensions;
procedure ReadImplementationProperties;
{$IFDEF SUPPORT_WGL}
procedure ReadWGLExtensions;
procedure ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
procedure ReadGLXExtensions;
procedure ReadGLXImplementationProperties;
{$ENDIF}

procedure CloseOpenGL;
function InitOpenGL : Boolean;
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
function IsOpenGLInitialized: Boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL : Boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
function IsOpenGLLoaded : Boolean;

function IsMesaGL : Boolean;
procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
function IsVersionMet(MajorVersion,MinorVersion, actualMajorVersion, actualMinorVersion:Integer): boolean;
function IsOpenGLVersionMet(MajorVersion,MinorVersion: Integer): boolean;

type
  EOpenGLError = class(Exception);

{ Gets the oldest error from OpenGL engine and tries to clear the error queue. }
procedure CheckOpenGLError;
{ Clears all pending OpenGL errors. }
procedure ClearGLError;
{ Raises an EOpenGLError with 'msg' error string. }
procedure RaiseOpenGLError(const msg : String);

var
   vIgnoreOpenGLErrors : Boolean = false;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ************** Windows specific ********************
{$IFDEF MSWINDOWS}
const
   INVALID_MODULEHANDLE = 0;

var
   GLHandle: HINST;
   GLUHandle: HINST;

function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := wglGetProcAddress(ProcName);
end;

{$ENDIF}

// ************** UNIX specific ********************
{$IFDEF Unix}
const
   INVALID_MODULEHANDLE = 0;//nil;

var
   GLHandle: TLibHandle = 0;//Pointer;
   GLUHandle: TLibHandle = 0;//Pointer;
   
function GLGetProcAddress(ProcName: PGLChar):Pointer;
begin
  {$IFDEF SUPPORT_GLX}
  if @glXGetProcAddress<>nil then
    result := glXGetProcAddress(ProcName);

  if result<> nil then exit;

  if @glXGetProcAddressARB<>nil then
    result := glXGetProcAddressARB(ProcName);

  if result<> nil then exit;
  {$ENDIF}
  result := GetProcAddress(GLHandle, ProcName);
end;
{$ENDIF}

function GLLibGetProcAddress(ProcName: PGLChar):Pointer;
begin
  result := GetProcAddress(GLHandle, ProcName);
end;

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError : LongWord;
	Count : Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
      if not vIgnoreOpenGLErrors then
   		raise EOpenGLError.Create(String(gluErrorString(GLError)));
	end;
end;

// ClearGLError
//
procedure ClearGLError;
var
   n : Integer;
begin
   n:=0;
   while (glGetError<>GL_NO_ERROR) and (n<6) do Inc(n);
end;

// RaiseOpenGLError
//
procedure RaiseOpenGLError(const msg : String);
begin
   raise EOpenGLError.Create(msg);
end;

// ************** Extensions ********************

// ReadExtensions
//
procedure ReadExtensions;
   // To be used in an active rendering context only!
begin

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 1.2'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.2 core
   //  ###########################################################

   // promoted to core v1.2 from GL_EXT_blend_color (#2)
   glBlendColor := GLGetProcAddress('glBlendColor');

   //promoted to core v1.2 from GL_EXT_blend_minmax (#37)
   glBlendEquation := GLGetProcAddress('glBlendEquation');

   // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
   glDrawRangeElements := GLGetProcAddress('glDrawRangeElements');

   // promoted to core v1.2 from GL_SGI_color_table (#14)
   glColorTable := GLGetProcAddress('glColorTable');
   glColorTableParameterfv := GLGetProcAddress('glColorTableParameterfv');
   glColorTableParameteriv := GLGetProcAddress('glColorTableParameteriv');
   glCopyColorTable := GLGetProcAddress('glCopyColorTable');
   glGetColorTable := GLGetProcAddress('glGetColorTable');
   glGetColorTableParameterfv := GLGetProcAddress('glGetColorTableParameterfv');
   glGetColorTableParameteriv := GLGetProcAddress('glGetColorTableParameteriv');

   // promoted to core v1.2 from GL_EXT_color_subtable (#74)
   glColorSubTable := GLGetProcAddress('glColorSubTable');
   glCopyColorSubTable := GLGetProcAddress('glCopyColorSubTable');

   // promoted to core v1.2 from GL_EXT_convolution (#12)
   glConvolutionFilter1D := GLGetProcAddress('glConvolutionFilter1D');
   glConvolutionFilter2D := GLGetProcAddress('glConvolutionFilter2D'); 
   glConvolutionParameterf := GLGetProcAddress('glConvolutionParameterf');
   glConvolutionParameterfv := GLGetProcAddress('glConvolutionParameterfv');
   glConvolutionParameteri := GLGetProcAddress('glConvolutionParameteri'); 
   glConvolutionParameteriv := GLGetProcAddress('glConvolutionParameteriv');
   glCopyConvolutionFilter1D := GLGetProcAddress('glCopyConvolutionFilter1D');
   glCopyConvolutionFilter2D := GLGetProcAddress('glCopyConvolutionFilter2D');
   glGetConvolutionFilter := GLGetProcAddress('glGetConvolutionFilter');
   glGetConvolutionParameterfv := GLGetProcAddress('glGetConvolutionParameterfv');
   glGetConvolutionParameteriv := GLGetProcAddress('glGetConvolutionParameteriv');
   glGetSeparableFilter := GLGetProcAddress('glGetSeparableFilter');
   glSeparableFilter2D := GLGetProcAddress('glSeparableFilter2D');

   // promoted to core v1.2 from GL_EXT_histogram (#11)
   glGetHistogram := GLGetProcAddress('glGetHistogram');
   glGetHistogramParameterfv := GLGetProcAddress('glGetHistogramParameterfv');
   glGetHistogramParameteriv := GLGetProcAddress('glGetHistogramParameteriv');
   glGetMinmax := GLGetProcAddress('glGetMinmax');
   glGetMinmaxParameterfv := GLGetProcAddress('glGetMinmaxParameterfv');
   glGetMinmaxParameteriv := GLGetProcAddress('glGetMinmaxParameteriv');
   glHistogram := GLGetProcAddress('glHistogram');
   glMinmax := GLGetProcAddress('glMinmax');
   glResetHistogram := GLGetProcAddress('glResetHistogram');
   glResetMinmax := GLGetProcAddress('glResetMinmax');

   // promoted to core v1.2 from GL_EXT_texture3D (#6)
   glTexImage3D := GLGetProcAddress('glTexImage3D');
   glTexSubImage3D := GLGetProcAddress('glTexSubImage3D');

   // promoted to core v1.2 from GL_EXT_copy_texture
   glCopyTexSubImage3D := GLGetProcAddress('glCopyTexSubImage3D');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 1.3'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.3 core
   //  ###########################################################

   // promoted to core v1.3 from GL_ARB_multitexture (#1)
   glActiveTexture := GLGetProcAddress('glActiveTexture');
   glClientActiveTexture := GLGetProcAddress('glClientActiveTexture');
   glMultiTexCoord1d := GLGetProcAddress('glMultiTexCoord1d');
   glMultiTexCoord1dV := GLGetProcAddress('glMultiTexCoord1dV');
   glMultiTexCoord1f := GLGetProcAddress('glMultiTexCoord1f');
   glMultiTexCoord1fV := GLGetProcAddress('glMultiTexCoord1fV');
   glMultiTexCoord1i := GLGetProcAddress('glMultiTexCoord1i');
   glMultiTexCoord1iV := GLGetProcAddress('glMultiTexCoord1iV');
   glMultiTexCoord1s := GLGetProcAddress('glMultiTexCoord1s');
   glMultiTexCoord1sV := GLGetProcAddress('glMultiTexCoord1sV'); 
   glMultiTexCoord2d := GLGetProcAddress('glMultiTexCoord2d');
   glMultiTexCoord2dv := GLGetProcAddress('glMultiTexCoord2dv');
   glMultiTexCoord2f := GLGetProcAddress('glMultiTexCoord2f');
   glMultiTexCoord2fv := GLGetProcAddress('glMultiTexCoord2fv');
   glMultiTexCoord2i := GLGetProcAddress('glMultiTexCoord2i');
   glMultiTexCoord2iv := GLGetProcAddress('glMultiTexCoord2iv');
   glMultiTexCoord2s := GLGetProcAddress('glMultiTexCoord2s'); 
   glMultiTexCoord2sv := GLGetProcAddress('glMultiTexCoord2sv');
   glMultiTexCoord3d := GLGetProcAddress('glMultiTexCoord3d');
   glMultiTexCoord3dv := GLGetProcAddress('glMultiTexCoord3dv'); 
   glMultiTexCoord3f := GLGetProcAddress('glMultiTexCoord3f');
   glMultiTexCoord3fv := GLGetProcAddress('glMultiTexCoord3fv');
   glMultiTexCoord3i := GLGetProcAddress('glMultiTexCoord3i');
   glMultiTexCoord3iv := GLGetProcAddress('glMultiTexCoord3iv'); 
   glMultiTexCoord3s := GLGetProcAddress('glMultiTexCoord3s'); 
   glMultiTexCoord3sv := GLGetProcAddress('glMultiTexCoord3sv');
   glMultiTexCoord4d := GLGetProcAddress('glMultiTexCoord4d'); 
   glMultiTexCoord4dv := GLGetProcAddress('glMultiTexCoord4dv');
   glMultiTexCoord4f := GLGetProcAddress('glMultiTexCoord4f');
   glMultiTexCoord4fv := GLGetProcAddress('glMultiTexCoord4fv');
   glMultiTexCoord4i := GLGetProcAddress('glMultiTexCoord4i');
   glMultiTexCoord4iv := GLGetProcAddress('glMultiTexCoord4iv');
   glMultiTexCoord4s := GLGetProcAddress('glMultiTexCoord4s');
   glMultiTexCoord4sv := GLGetProcAddress('glMultiTexCoord4sv');

   // promoted to core v1.3 from GL_ARB_transpose_matrix
   glLoadTransposeMatrixf := GLGetProcAddress('glLoadTransposeMatrixf');
   glLoadTransposeMatrixd := GLGetProcAddress('glLoadTransposeMatrixd');
   glMultTransposeMatrixf := GLGetProcAddress('glMultTransposeMatrixf');
   glMultTransposeMatrixd := GLGetProcAddress('glMultTransposeMatrixd');

   // promoted to core v1.3 from GL_ARB_multisample (#5)
   glSampleCoverage := GLGetProcAddress('glSampleCoverage');

   // promoted to core v1.3 from GL_ARB_texture_compression (#12)
   glCompressedTexImage3D := GLGetProcAddress('glCompressedTexImage3D');
   glCompressedTexImage2D := GLGetProcAddress('glCompressedTexImage2D');
   glCompressedTexImage1D := GLGetProcAddress('glCompressedTexImage1D');
   glCompressedTexSubImage3D := GLGetProcAddress('glCompressedTexSubImage3D');
   glCompressedTexSubImage2D := GLGetProcAddress('glCompressedTexSubImage2D');
   glCompressedTexSubImage1D := GLGetProcAddress('glCompressedTexSubImage1D');
   glGetCompressedTexImage := GLGetProcAddress('glGetCompressedTexImage');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 1.4'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.4 core
   //  ###########################################################

   // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparate := GLGetProcAddress('glBlendFuncSeparate');

   // promoted to core v1.4 from GL_EXT_fog_coord (#149)
   glFogCoordf := GLGetProcAddress('glFogCoordf');
   glFogCoordfv := GLGetProcAddress('glFogCoordfv');
   glFogCoordd := GLGetProcAddress('glFogCoordd');
   glFogCoorddv := GLGetProcAddress('glFogCoorddv');
   glFogCoordPointer := GLGetProcAddress('glFogCoordPointer');

   // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArrays := GLGetProcAddress('glMultiDrawArrays');
   glMultiDrawElements := GLGetProcAddress('glMultiDrawElements');

   // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
   glPointParameterf := GLGetProcAddress('glPointParameterf');
   glPointParameterfv := GLGetProcAddress('glPointParameterfv');
   glPointParameteri := GLGetProcAddress('glPointParameteri');
   glPointParameteriv := GLGetProcAddress('glPointParameteriv');

   // promoted to core v1.4 from GL_EXT_secondary_color (#145)
   glSecondaryColor3b := GLGetProcAddress('glSecondaryColor3b');
   glSecondaryColor3bv := GLGetProcAddress('glSecondaryColor3bv');
   glSecondaryColor3d := GLGetProcAddress('glSecondaryColor3d');
   glSecondaryColor3dv := GLGetProcAddress('glSecondaryColor3dv');
   glSecondaryColor3f := GLGetProcAddress('glSecondaryColor3f');
   glSecondaryColor3fv := GLGetProcAddress('glSecondaryColor3fv');
   glSecondaryColor3i := GLGetProcAddress('glSecondaryColor3i');
   glSecondaryColor3iv := GLGetProcAddress('glSecondaryColor3iv');
   glSecondaryColor3s := GLGetProcAddress('glSecondaryColor3s');
   glSecondaryColor3sv := GLGetProcAddress('glSecondaryColor3sv');
   glSecondaryColor3ub := GLGetProcAddress('glSecondaryColor3ub');
   glSecondaryColor3ubv := GLGetProcAddress('glSecondaryColor3ubv');
   glSecondaryColor3ui := GLGetProcAddress('glSecondaryColor3ui');
   glSecondaryColor3uiv := GLGetProcAddress('glSecondaryColor3uiv');
   glSecondaryColor3us := GLGetProcAddress('glSecondaryColor3us');
   glSecondaryColor3usv := GLGetProcAddress('glSecondaryColor3usv');
   glSecondaryColorPointer := GLGetProcAddress('glSecondaryColorPointer');

   // promoted to core v1.4 from GL_ARB_window_pos (#25)
   glWindowPos2d := GLGetProcAddress('glWindowPos2d');
   glWindowPos2dv := GLGetProcAddress('glWindowPos2dv');
   glWindowPos2f := GLGetProcAddress('glWindowPos2f');
   glWindowPos2fv := GLGetProcAddress('glWindowPos2fv');
   glWindowPos2i := GLGetProcAddress('glWindowPos2i');
   glWindowPos2iv := GLGetProcAddress('glWindowPos2iv');
   glWindowPos2s := GLGetProcAddress('glWindowPos2s');
   glWindowPos2sv := GLGetProcAddress('glWindowPos2sv');
   glWindowPos3d := GLGetProcAddress('glWindowPos3d');
   glWindowPos3dv := GLGetProcAddress('glWindowPos3dv');
   glWindowPos3f := GLGetProcAddress('glWindowPos3f');
   glWindowPos3fv := GLGetProcAddress('glWindowPos3fv');
   glWindowPos3i := GLGetProcAddress('glWindowPos3i');
   glWindowPos3iv := GLGetProcAddress('glWindowPos3iv');
   glWindowPos3s := GLGetProcAddress('glWindowPos3s');
   glWindowPos3sv := GLGetProcAddress('glWindowPos3sv');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 1.5'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 1.5 core
   //  ###########################################################

   // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
   glGenQueries := GLGetProcAddress('glGenQueries');
   glDeleteQueries := GLGetProcAddress('glDeleteQueries');
   glIsQuery := GLGetProcAddress('glIsQuery');
   glBeginQuery := GLGetProcAddress('glBeginQuery');
   glEndQuery := GLGetProcAddress('glEndQuery');
   glGetQueryiv := GLGetProcAddress('glGetQueryiv');
   glGetQueryObjectiv := GLGetProcAddress('glGetQueryObjectiv');
   glGetQueryObjectuiv := GLGetProcAddress('glGetQueryObjectuiv');


   // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
   glBindBuffer := GLGetProcAddress('glBindBuffer');
   glDeleteBuffers := GLGetProcAddress('glDeleteBuffers');
   glGenBuffers := GLGetProcAddress('glGenBuffers');
   glIsBuffer := GLGetProcAddress('glIsBuffer');
   glBufferData := GLGetProcAddress('glBufferData');
   glBufferSubData := GLGetProcAddress('glBufferSubData');
   glGetBufferSubData := GLGetProcAddress('glGetBufferSubData');
   glMapBuffer := GLGetProcAddress('glMapBuffer');
   glUnmapBuffer := GLGetProcAddress('glUnmapBuffer');
   glGetBufferParameteriv := GLGetProcAddress('glGetBufferParameteriv');
   glGetBufferPointerv := GLGetProcAddress('glGetBufferPointerv');

   // promoted to core v1.5 from GL_EXT_shadow_funcs (#267)
   // (no functions or procedures)

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 2.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.0 core
   //  ###########################################################

   // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparate := GLGetProcAddress('glBlendEquationSeparate');

   // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
   glDrawBuffers := GLGetProcAddress('glDrawBuffers');

   // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
   glStencilOpSeparate := GLGetProcAddress('glStencilOpSeparate');
   glStencilFuncSeparate := GLGetProcAddress('glStencilFuncSeparate');
   glStencilMaskSeparate := GLGetProcAddress('glStencilMaskSeparate');

   // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
   glAttachShader := GLGetProcAddress('glAttachShader');
   glBindAttribLocation := GLGetProcAddress('glBindAttribLocation');
   glCompileShader := GLGetProcAddress('glCompileShader');
   glCreateProgram := GLGetProcAddress('glCreateProgram');
   glCreateShader := GLGetProcAddress('glCreateShader');
   glDeleteProgram := GLGetProcAddress('glDeleteProgram');
   glDeleteShader := GLGetProcAddress('glDeleteShader');
   glDetachShader := GLGetProcAddress('glDetachShader');
   glDisableVertexAttribArray := GLGetProcAddress('glDisableVertexAttribArray');
   glEnableVertexAttribArray := GLGetProcAddress('glEnableVertexAttribArray');
   glGetActiveAttrib := GLGetProcAddress('glGetActiveAttrib');
   glGetActiveUniform := GLGetProcAddress('glGetActiveUniform');
   glGetAttachedShaders := GLGetProcAddress('glGetAttachedShaders');
   glGetAttribLocation := GLGetProcAddress('glGetAttribLocation');
   glGetProgramiv := GLGetProcAddress('glGetProgramiv');
   glGetProgramInfoLog := GLGetProcAddress('glGetProgramInfoLog');
   glGetShaderiv := GLGetProcAddress('glGetShaderiv');
   glGetShaderInfoLog := GLGetProcAddress('glGetShaderInfoLog');
   glGetShaderSource := GLGetProcAddress('glGetShaderSource');
   glGetUniformLocation := GLGetProcAddress('glGetUniformLocation');
   glGetUniformfv := GLGetProcAddress('glGetUniformfv');
   glGetUniformiv := GLGetProcAddress('glGetUniformiv');
   glGetVertexAttribdv := GLGetProcAddress('glGetVertexAttribdv');
   glGetVertexAttribfv := GLGetProcAddress('glGetVertexAttribfv');
   glGetVertexAttribiv := GLGetProcAddress('glGetVertexAttribiv');
   glGetVertexAttribPointerv := GLGetProcAddress('glGetVertexAttribPointerv');
   glIsProgram := GLGetProcAddress('glIsProgram');
   glIsShader := GLGetProcAddress('glIsShader');
   glLinkProgram := GLGetProcAddress('glLinkProgram');
   glShaderSource := GLGetProcAddress('glShaderSource');
   glUseProgram := GLGetProcAddress('glUseProgram');
   glUniform1f := GLGetProcAddress('glUniform1f');
   glUniform2f := GLGetProcAddress('glUniform2f');
   glUniform3f := GLGetProcAddress('glUniform3f');
   glUniform4f := GLGetProcAddress('glUniform4f');
   glUniform1i := GLGetProcAddress('glUniform1i');
   glUniform2i := GLGetProcAddress('glUniform2i');
   glUniform3i := GLGetProcAddress('glUniform3i');
   glUniform4i := GLGetProcAddress('glUniform4i');
   glUniform1fv := GLGetProcAddress('glUniform1fv');
   glUniform2fv := GLGetProcAddress('glUniform2fv');
   glUniform3fv := GLGetProcAddress('glUniform3fv');
   glUniform4fv := GLGetProcAddress('glUniform4fv');
   glUniform1iv := GLGetProcAddress('glUniform1iv');
   glUniform2iv := GLGetProcAddress('glUniform2iv');
   glUniform3iv := GLGetProcAddress('glUniform3iv');
   glUniform4iv := GLGetProcAddress('glUniform4iv');
   glUniformMatrix2fv := GLGetProcAddress('glUniformMatrix2fv');
   glUniformMatrix3fv := GLGetProcAddress('glUniformMatrix3fv');
   glUniformMatrix4fv := GLGetProcAddress('glUniformMatrix4fv');
   glValidateProgram := GLGetProcAddress('glValidateProgram');
   glVertexAttrib1d := GLGetProcAddress('glVertexAttrib1d');
   glVertexAttrib1dv := GLGetProcAddress('glVertexAttrib1dv');
   glVertexAttrib1f := GLGetProcAddress('glVertexAttrib1f');
   glVertexAttrib1fv := GLGetProcAddress('glVertexAttrib1fv');
   glVertexAttrib1s := GLGetProcAddress('glVertexAttrib1s');
   glVertexAttrib1sv := GLGetProcAddress('glVertexAttrib1sv');
   glVertexAttrib2d := GLGetProcAddress('glVertexAttrib2d');
   glVertexAttrib2dv := GLGetProcAddress('glVertexAttrib2dv');
   glVertexAttrib2f := GLGetProcAddress('glVertexAttrib2f');
   glVertexAttrib2fv := GLGetProcAddress('glVertexAttrib2fv');
   glVertexAttrib2s := GLGetProcAddress('glVertexAttrib2s');
   glVertexAttrib2sv := GLGetProcAddress('glVertexAttrib2sv');
   glVertexAttrib3d := GLGetProcAddress('glVertexAttrib3d');
   glVertexAttrib3dv := GLGetProcAddress('glVertexAttrib3dv');
   glVertexAttrib3f := GLGetProcAddress('glVertexAttrib3f');
   glVertexAttrib3fv := GLGetProcAddress('glVertexAttrib3fv');
   glVertexAttrib3s := GLGetProcAddress('glVertexAttrib3s');
   glVertexAttrib3sv := GLGetProcAddress('glVertexAttrib3sv');
   glVertexAttrib4Nbv := GLGetProcAddress('glVertexAttrib4Nbv');
   glVertexAttrib4Niv := GLGetProcAddress('glVertexAttrib4Niv');
   glVertexAttrib4Nsv := GLGetProcAddress('glVertexAttrib4Nsv');
   glVertexAttrib4Nub := GLGetProcAddress('glVertexAttrib4Nub');
   glVertexAttrib4Nubv := GLGetProcAddress('glVertexAttrib4Nubv');
   glVertexAttrib4Nuiv := GLGetProcAddress('glVertexAttrib4Nuiv');
   glVertexAttrib4Nusv := GLGetProcAddress('glVertexAttrib4Nusv');
   glVertexAttrib4bv := GLGetProcAddress('glVertexAttrib4bv');
   glVertexAttrib4d := GLGetProcAddress('glVertexAttrib4d');
   glVertexAttrib4dv := GLGetProcAddress('glVertexAttrib4dv');
   glVertexAttrib4f := GLGetProcAddress('glVertexAttrib4f');
   glVertexAttrib4fv := GLGetProcAddress('glVertexAttrib4fv');
   glVertexAttrib4iv := GLGetProcAddress('glVertexAttrib4iv');
   glVertexAttrib4s := GLGetProcAddress('glVertexAttrib4s');
   glVertexAttrib4sv := GLGetProcAddress('glVertexAttrib4sv');
   glVertexAttrib4ubv := GLGetProcAddress('glVertexAttrib4ubv');
   glVertexAttrib4uiv := GLGetProcAddress('glVertexAttrib4uiv');
   glVertexAttrib4usv := GLGetProcAddress('glVertexAttrib4usv');
   glVertexAttribPointer := GLGetProcAddress('glVertexAttribPointer');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 2.1'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 2.1 core
   //  ###########################################################

   // promoted to core v2.1 from GL_ARB_pixel_buffer_object
   // (no functions or procedures)
   
   // promoted to core v2.1 from GL_EXT_texture_sRGB
   // (no functions or procedures)

   // New commands in OpenGL 2.1
   glUniformMatrix2x3fv := GLGetProcAddress('glUniformMatrix2x3fv');
   glUniformMatrix3x2fv := GLGetProcAddress('glUniformMatrix3x2fv');
   glUniformMatrix2x4fv := GLGetProcAddress('glUniformMatrix2x4fv');
   glUniformMatrix4x2fv := GLGetProcAddress('glUniformMatrix4x2fv');
   glUniformMatrix3x4fv := GLGetProcAddress('glUniformMatrix3x4fv');
   glUniformMatrix4x3fv := GLGetProcAddress('glUniformMatrix4x3fv');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 3.0'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.0 core
   //  ###########################################################

   // promoted to core v3.0 from GL_EXT_gpu_shader4
   glVertexAttribI1i := GLGetProcAddress('glVertexAttribI1i');
   glVertexAttribI2i := GLGetProcAddress('glVertexAttribI2i');
   glVertexAttribI3i := GLGetProcAddress('glVertexAttribI3i');
   glVertexAttribI4i := GLGetProcAddress('glVertexAttribI4i');
   glVertexAttribI1ui := GLGetProcAddress('glVertexAttribI1ui');
   glVertexAttribI2ui := GLGetProcAddress('glVertexAttribI2ui');
   glVertexAttribI3ui := GLGetProcAddress('glVertexAttribI3ui');
   glVertexAttribI4ui := GLGetProcAddress('glVertexAttribI4ui');
   glVertexAttribI1iv := GLGetProcAddress('glVertexAttribI1iv');
   glVertexAttribI2iv := GLGetProcAddress('glVertexAttribI2iv');
   glVertexAttribI3iv := GLGetProcAddress('glVertexAttribI3iv');
   glVertexAttribI4iv := GLGetProcAddress('glVertexAttribI4iv');
   glVertexAttribI1uiv := GLGetProcAddress('glVertexAttribI1uiv');
   glVertexAttribI2uiv := GLGetProcAddress('glVertexAttribI2uiv');
   glVertexAttribI3uiv := GLGetProcAddress('glVertexAttribI3uiv');
   glVertexAttribI4uiv := GLGetProcAddress('glVertexAttribI4uiv');
   glVertexAttribI4bv := GLGetProcAddress('glVertexAttribI4bv');
   glVertexAttribI4sv := GLGetProcAddress('glVertexAttribI4sv');
   glVertexAttribI4ubv := GLGetProcAddress('glVertexAttribI4ubv');
   glVertexAttribI4usv := GLGetProcAddress('glVertexAttribI4usv');
   glVertexAttribIPointer := GLGetProcAddress('glVertexAttribIPointer');
   glGetVertexAttribIiv := GLGetProcAddress('glGetVertexAttribIiv');
   glGetVertexAttribIuiv := GLGetProcAddress('glGetVertexAttribIuiv');
   glUniform1ui := GLGetProcAddress('glUniform1ui');
   glUniform2ui :=  GLGetProcAddress('glUniform2ui');
   glUniform3ui := GLGetProcAddress('glUniform3ui');
   glUniform4ui := GLGetProcAddress('glUniform4ui');
   glUniform1uiv := GLGetProcAddress('glUniform1uiv');
   glUniform2uiv := GLGetProcAddress('glUniform2uiv');
   glUniform3uiv := GLGetProcAddress('glUniform3uiv');
   glUniform4uiv := GLGetProcAddress('glUniform4uiv');
   glGetUniformuiv := GLGetProcAddress('glGetUniformuiv');
   glBindFragDataLocation := GLGetProcAddress('glBindFragDataLocation');
   glGetFragDataLocation := GLGetProcAddress('glGetFragDataLocation');

   // promoted to core v3.0 from GL_NV_conditional_render
   glBeginConditionalRender := GLGetProcAddress('glBeginConditionalRender');
   glEndConditionalRender := GLGetProcAddress('glEndConditionalRender');
   // promoted to core v3.0 from GL_ARB_color_buffer_float
   glClampColor := GLGetProcAddress('glClampColor');
   // promoted to core v3.0 from GL_EXT_texture_integer
   //glClearColorIi := GLGetProcAddress('glClearColorIi');
   //glClearColorIui := GLGetProcAddress('glClearColorIui');
   glTexParameterIiv := GLGetProcAddress('glTexParameterIiv');
   glTexParameterIuiv := GLGetProcAddress('glTexParameterIuiv');
   glGetTexParameterIiv := GLGetProcAddress('glGetTexParameterIiv');
   glGetTexParameterIuiv := GLGetProcAddress('glGetTexParameterIuiv');

   // promoted to core v3.0 from GL_EXT_draw_buffers2
   glColorMaski := GLGetProcAddress('glColorMaski');
   glGetBooleani_v := GLGetProcAddress('glGetBooleani_v');
   glGetIntegeri_v := GLGetProcAddress('glGetIntegeri_v');
   glEnablei := GLGetProcAddress('glEnablei');
   glDisablei := GLGetProcAddress('glDisablei');
   glIsEnabledi := GLGetProcAddress('glIsEnabledi');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRange := GLGetProcAddress('glBindBufferRange');
   glBindBufferBase := GLGetProcAddress('glBindBufferBase');
   glBeginTransformFeedback := GLGetProcAddress('glBeginTransformFeedback');
   glEndTransformFeedback := GLGetProcAddress('glEndTransformFeedback');
   glTransformFeedbackVaryings := GLGetProcAddress('glTransformFeedbackVaryings');
   glGetTransformFeedbackVarying := GLGetProcAddress('glGetTransformFeedbackVarying');

   // New commands in OpenGL 3.0
   glClearBufferiv := GLGetProcAddress('glClearBufferiv');
   glClearBufferuiv := GLGetProcAddress('glClearBufferuiv');
   glClearBufferfv := GLGetProcAddress('glClearBufferfv');
   glClearBufferfi := GLGetProcAddress('glClearBufferfi');
   glGetStringi := GLGetProcAddress('glGetStringi');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 3.1'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.1 core
   //  ###########################################################

   glDrawArraysInstanced := GLGetProcAddress('glDrawArraysInstanced');
   glDrawElementsInstanced := GLGetProcAddress('glDrawElementsInstanced');
   glTexBuffer := GLGetProcAddress('glTexBuffer');
   glPrimitiveRestartIndex := GLGetProcAddress('glPrimitiveRestartIndex');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 3.2'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.2 core
   //  ###########################################################

   glGetInteger64i_v := GLGetProcAddress('glGetInteger64i_v');
   glGetBufferParameteri64v := GLGetProcAddress('glGetBufferParameteri64v');
   glProgramParameteri := GLGetProcAddress('glProgramParameteri');
   glFramebufferTexture := GLGetProcAddress('glFramebufferTexture');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 3.3'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.3 core
   //  ###########################################################

   glVertexAttribDivisor := GLGetProcAddress('glVertexAttribDivisor');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures added with OpenGL 3.3'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //         extensions integrated into OpenGL 3.3 core
   //  ###########################################################

   glBlendEquationi := GLGetProcAddress('glBlendEquationi');
   glBlendEquationSeparatei := GLGetProcAddress('glBlendEquationSeparatei');
   glBlendFunci := GLGetProcAddress('glBlendFunci');
   glBlendFuncSeparatei := GLGetProcAddress('glBlendFuncSeparatei');
   glMinSampleShading := GLGetProcAddress('glMinSampleShading');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                     GLU extensions
   //  ###########################################################

   gluNurbsCallbackDataEXT := GLGetProcAddress('gluNurbsCallbackDataEXT');
   gluNewNurbsTessellatorEXT := GLGetProcAddress('gluNewNurbsTessellatorEXT');
   gluDeleteNurbsTessellatorEXT := GLGetProcAddress('gluDeleteNurbsTessellatorEXT');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS} {$region 'locate functions/procedures for ARB approved extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                  ARB approved extensions
   //  ###########################################################

   // GL_ARB_multitexture (#1)
   glActiveTextureARB := GLGetProcAddress('glActiveTextureARB');
   glClientActiveTextureARB := GLGetProcAddress('glClientActiveTextureARB');
   glMultiTexCoord1dARB := GLGetProcAddress('glMultiTexCoord1dARB');
   glMultiTexCoord1dVARB := GLGetProcAddress('glMultiTexCoord1dVARB');
   glMultiTexCoord1fARB := GLGetProcAddress('glMultiTexCoord1fARB');
   glMultiTexCoord1fVARB := GLGetProcAddress('glMultiTexCoord1fVARB');
   glMultiTexCoord1iARB := GLGetProcAddress('glMultiTexCoord1iARB');
   glMultiTexCoord1iVARB := GLGetProcAddress('glMultiTexCoord1iVARB');
   glMultiTexCoord1sARB := GLGetProcAddress('glMultiTexCoord1sARB');
   glMultiTexCoord1sVARB := GLGetProcAddress('glMultiTexCoord1sVARB');
   glMultiTexCoord2dARB := GLGetProcAddress('glMultiTexCoord2dARB');
   glMultiTexCoord2dvARB := GLGetProcAddress('glMultiTexCoord2dvARB');
   glMultiTexCoord2fARB := GLGetProcAddress('glMultiTexCoord2fARB');
   glMultiTexCoord2fvARB := GLGetProcAddress('glMultiTexCoord2fvARB');
   glMultiTexCoord2iARB := GLGetProcAddress('glMultiTexCoord2iARB');
   glMultiTexCoord2ivARB := GLGetProcAddress('glMultiTexCoord2ivARB');
   glMultiTexCoord2sARB := GLGetProcAddress('glMultiTexCoord2sARB');
   glMultiTexCoord2svARB := GLGetProcAddress('glMultiTexCoord2svARB');
   glMultiTexCoord3dARB := GLGetProcAddress('glMultiTexCoord3dARB');
   glMultiTexCoord3dvARB := GLGetProcAddress('glMultiTexCoord3dvARB');
   glMultiTexCoord3fARB := GLGetProcAddress('glMultiTexCoord3fARB');
   glMultiTexCoord3fvARB := GLGetProcAddress('glMultiTexCoord3fvARB');
   glMultiTexCoord3iARB := GLGetProcAddress('glMultiTexCoord3iARB');
   glMultiTexCoord3ivARB := GLGetProcAddress('glMultiTexCoord3ivARB');
   glMultiTexCoord3sARB := GLGetProcAddress('glMultiTexCoord3sARB');
   glMultiTexCoord3svARB := GLGetProcAddress('glMultiTexCoord3svARB');
   glMultiTexCoord4dARB := GLGetProcAddress('glMultiTexCoord4dARB');
   glMultiTexCoord4dvARB := GLGetProcAddress('glMultiTexCoord4dvARB');
   glMultiTexCoord4fARB := GLGetProcAddress('glMultiTexCoord4fARB');
   glMultiTexCoord4fvARB := GLGetProcAddress('glMultiTexCoord4fvARB');
   glMultiTexCoord4iARB := GLGetProcAddress('glMultiTexCoord4iARB');
   glMultiTexCoord4ivARB := GLGetProcAddress('glMultiTexCoord4ivARB');
   glMultiTexCoord4sARB := GLGetProcAddress('glMultiTexCoord4sARB');
   glMultiTexCoord4svARB := GLGetProcAddress('glMultiTexCoord4svARB');

   // GL_ARB_transpose_matrix (#3)
   glLoadTransposeMatrixfARB := GLGetProcAddress('glLoadTransposeMatrixfARB');
   glLoadTransposeMatrixdARB := GLGetProcAddress('glLoadTransposeMatrixdARB');
   glMultTransposeMatrixfARB := GLGetProcAddress('glMultTransposeMatrixfARB');
   glMultTransposeMatrixdARB := GLGetProcAddress('glMultTransposeMatrixdARB');

   // GL_ARB_multisample (#5)
   glSampleCoverageARB := GLGetProcAddress('glSampleCoverageARB');

   // GL_ARB_texture_compression (#12)
   glCompressedTexImage3DARB := GLGetProcAddress('glCompressedTexImage3DARB');
   glCompressedTexImage2DARB := GLGetProcAddress('glCompressedTexImage2DARB');
   glCompressedTexImage1DARB := GLGetProcAddress('glCompressedTexImage1DARB');
   glCompressedTexSubImage3DARB := GLGetProcAddress('glCompressedTexSubImage3DARB');
   glCompressedTexSubImage2DARB := GLGetProcAddress('glCompressedTexSubImage2DARB');
   glCompressedTexSubImage1DARB := GLGetProcAddress('glCompressedTexSubImage1DARB');
   glGetCompressedTexImageARB := GLGetProcAddress('glGetCompressedTexImageARB');

   // GL_ARB_point_parameter (#14)
   glPointParameterfARB := GLGetProcAddress('glPointParameterfARB');
   glPointParameterfvARB := GLGetProcAddress('glPointParameterfvARB');

   // GL_ARB_vertex_blend (#15) {deprecated?}
   glWeightbvARB := GLGetProcAddress('glWeightbvARB');
   glWeightsvARB := GLGetProcAddress('glWeightsvARB');
   glWeightivARB := GLGetProcAddress('glWeightivARB');
   glWeightfvARB := GLGetProcAddress('glWeightfvARB');
   glWeightdvARB := GLGetProcAddress('glWeightdvARB');
   glWeightubvARB := GLGetProcAddress('glWeightubvARB');
   glWeightusvARB := GLGetProcAddress('glWeightusvARB');
   glWeightuivARB := GLGetProcAddress('glWeightuivARB');
   glWeightPointerARB := GLGetProcAddress('glWeightPointerARB');
   glVertexBlendARB := GLGetProcAddress('glVertexBlendARB');

   // GL_ARB_matrix_palette (#16) {deprecated?}
   glCurrentPaletteMatrixARB := GLGetProcAddress('glCurrentPaletteMatrixARB');
   glMatrixIndexubvARB := GLGetProcAddress('glMatrixIndexubvARB');
   glMatrixIndexusvARB := GLGetProcAddress('glMatrixIndexusvARB');
   glMatrixIndexuivARB := GLGetProcAddress('glMatrixIndexuivARB');
   glMatrixIndexPointerARB := GLGetProcAddress('glMatrixIndexPointerARB');

   // GL_ARB_window_pos (#25)
   glWindowPos2dARB := GLGetProcAddress('glWindowPos2dARB');
   glWindowPos2dvARB := GLGetProcAddress('glWindowPos2dvARB');
   glWindowPos2fARB := GLGetProcAddress('glWindowPos2fARB');
   glWindowPos2fvARB := GLGetProcAddress('glWindowPos2fvARB');
   glWindowPos2iARB := GLGetProcAddress('glWindowPos2iARB');
   glWindowPos2ivARB := GLGetProcAddress('glWindowPos2ivARB');
   glWindowPos2sARB := GLGetProcAddress('glWindowPos2sARB');
   glWindowPos2svARB := GLGetProcAddress('glWindowPos2svARB');
   glWindowPos3dARB := GLGetProcAddress('glWindowPos3dARB');
   glWindowPos3dvARB := GLGetProcAddress('glWindowPos3dvARB');
   glWindowPos3fARB := GLGetProcAddress('glWindowPos3fARB');
   glWindowPos3fvARB := GLGetProcAddress('glWindowPos3fvARB');
   glWindowPos3iARB := GLGetProcAddress('glWindowPos3iARB');
   glWindowPos3ivARB := GLGetProcAddress('glWindowPos3ivARB');
   glWindowPos3sARB := GLGetProcAddress('glWindowPos3sARB');
   glWindowPos3svARB := GLGetProcAddress('glWindowPos3svARB');

   // GL_ARB_vertex_program (#26)
   glVertexAttrib1dARB := GLGetProcAddress('glVertexAttrib1dARB');
   glVertexAttrib1dvARB := GLGetProcAddress('glVertexAttrib1dvARB');
   glVertexAttrib1fARB := GLGetProcAddress('glVertexAttrib1fARB');
   glVertexAttrib1fvARB := GLGetProcAddress('glVertexAttrib1fvARB');
   glVertexAttrib1sARB := GLGetProcAddress('glVertexAttrib1sARB');
   glVertexAttrib1svARB := GLGetProcAddress('glVertexAttrib1svARB');
   glVertexAttrib2dARB := GLGetProcAddress('glVertexAttrib2dARB');
   glVertexAttrib2dvARB := GLGetProcAddress('glVertexAttrib2dvARB');
   glVertexAttrib2fARB := GLGetProcAddress('glVertexAttrib2fARB');
   glVertexAttrib2fvARB := GLGetProcAddress('glVertexAttrib2fvARB');
   glVertexAttrib2sARB := GLGetProcAddress('glVertexAttrib2sARB');
   glVertexAttrib2svARB := GLGetProcAddress('glVertexAttrib2svARB');
   glVertexAttrib3dARB := GLGetProcAddress('glVertexAttrib3dARB');
   glVertexAttrib3dvARB := GLGetProcAddress('glVertexAttrib3dvARB');
   glVertexAttrib3fARB := GLGetProcAddress('glVertexAttrib3fARB');
   glVertexAttrib3fvARB := GLGetProcAddress('glVertexAttrib3fvARB');
   glVertexAttrib3sARB := GLGetProcAddress('glVertexAttrib3sARB');
   glVertexAttrib3svARB := GLGetProcAddress('glVertexAttrib3svARB');
   glVertexAttrib4NbvARB := GLGetProcAddress('glVertexAttrib4NbvARB');
   glVertexAttrib4NivARB := GLGetProcAddress('glVertexAttrib4NivARB');
   glVertexAttrib4NsvARB := GLGetProcAddress('glVertexAttrib4NsvARB');
   glVertexAttrib4NubARB := GLGetProcAddress('glVertexAttrib4NubARB');
   glVertexAttrib4NubvARB := GLGetProcAddress('glVertexAttrib4NubvARB');
   glVertexAttrib4NuivARB := GLGetProcAddress('glVertexAttrib4NuivARB');
   glVertexAttrib4NusvARB := GLGetProcAddress('glVertexAttrib4NusvARB');
   glVertexAttrib4bvARB := GLGetProcAddress('glVertexAttrib4bvARB');
   glVertexAttrib4dARB := GLGetProcAddress('glVertexAttrib4dARB');
   glVertexAttrib4dvARB := GLGetProcAddress('glVertexAttrib4dvARB');
   glVertexAttrib4fARB := GLGetProcAddress('glVertexAttrib4fARB');
   glVertexAttrib4fvARB := GLGetProcAddress('glVertexAttrib4fvARB');
   glVertexAttrib4ivARB := GLGetProcAddress('glVertexAttrib4ivARB');
   glVertexAttrib4sARB := GLGetProcAddress('glVertexAttrib4sARB');
   glVertexAttrib4svARB := GLGetProcAddress('glVertexAttrib4svARB');
   glVertexAttrib4ubvARB := GLGetProcAddress('glVertexAttrib4ubvARB');
   glVertexAttrib4uivARB := GLGetProcAddress('glVertexAttrib4uivARB');
   glVertexAttrib4usvARB := GLGetProcAddress('glVertexAttrib4usvARB');
   glVertexAttribPointerARB := GLGetProcAddress('glVertexAttribPointerARB');
   glEnableVertexAttribArrayARB := GLGetProcAddress('glEnableVertexAttribArrayARB');
   glDisableVertexAttribArrayARB := GLGetProcAddress('glDisableVertexAttribArrayARB');
   glProgramStringARB := GLGetProcAddress('glProgramStringARB');
   glBindProgramARB := GLGetProcAddress('glBindProgramARB');
   glDeleteProgramsARB := GLGetProcAddress('glDeleteProgramsARB');
   glGenProgramsARB := GLGetProcAddress('glGenProgramsARB');
   glProgramEnvParameter4dARB := GLGetProcAddress('glProgramEnvParameter4dARB');
   glProgramEnvParameter4dvARB := GLGetProcAddress('glProgramEnvParameter4dvARB');
   glProgramEnvParameter4fARB := GLGetProcAddress('glProgramEnvParameter4fARB');
   glProgramEnvParameter4fvARB := GLGetProcAddress('glProgramEnvParameter4fvARB');
   glProgramLocalParameter4dARB := GLGetProcAddress('glProgramLocalParameter4dARB');
   glProgramLocalParameter4dvARB := GLGetProcAddress('glProgramLocalParameter4dvARB');
   glProgramLocalParameter4fARB := GLGetProcAddress('glProgramLocalParameter4fARB');
   glProgramLocalParameter4fvARB := GLGetProcAddress('glProgramLocalParameter4fvARB');
   glGetProgramEnvParameterdvARB := GLGetProcAddress('glGetProgramEnvParameterdvARB');
   glGetProgramEnvParameterfvARB := GLGetProcAddress('glGetProgramEnvParameterfvARB');
   glGetProgramLocalParameterdvARB := GLGetProcAddress('glGetProgramLocalParameterdvARB');
   glGetProgramLocalParameterfvARB := GLGetProcAddress('glGetProgramLocalParameterfvARB');
   glGetProgramivARB := GLGetProcAddress('glGetProgramivARB');
   glGetProgramStringARB := GLGetProcAddress('glGetProgramStringARB');
   glGetVertexAttribdvARB := GLGetProcAddress('glGetVertexAttribdvARB');
   glGetVertexAttribfvARB := GLGetProcAddress('glGetVertexAttribfvARB');
   glGetVertexAttribivARB := GLGetProcAddress('glGetVertexAttribivARB');
   glGetVertexAttribPointervARB := GLGetProcAddress('glGetVertexAttribPointervARB');
   glIsProgramARB := GLGetProcAddress('glIsProgramARB');

   // GL_ARB_vertex_buffer_object (#28)
   glBindBufferARB := GLGetProcAddress('glBindBufferARB');
   glDeleteBuffersARB := GLGetProcAddress('glDeleteBuffersARB');
   glGenBuffersARB := GLGetProcAddress('glGenBuffersARB');
   glIsBufferARB := GLGetProcAddress('glIsBufferARB');
   glBufferDataARB := GLGetProcAddress('glBufferDataARB');
   glBufferSubDataARB := GLGetProcAddress('glBufferSubDataARB');
   glGetBufferSubDataARB := GLGetProcAddress('glGetBufferSubDataARB');
   glMapBufferARB := GLGetProcAddress('glMapBufferARB');
   glUnmapBufferARB := GLGetProcAddress('glUnmapBufferARB');
   glGetBufferParameterivARB := GLGetProcAddress('glGetBufferParameterivARB');
   glGetBufferPointervARB := GLGetProcAddress('glGetBufferPointervARB');

   // GL_ARB_occlusion_query (#29)
   glGenQueriesARB := GLGetProcAddress('glGenQueriesARB');
   glDeleteQueriesARB := GLGetProcAddress('glDeleteQueriesARB');
   glIsQueryARB := GLGetProcAddress('glIsQueryARB');
   glBeginQueryARB := GLGetProcAddress('glBeginQueryARB');
   glEndQueryARB := GLGetProcAddress('glEndQueryARB');
   glGetQueryivARB := GLGetProcAddress('glGetQueryivARB');
   glGetQueryObjectivARB := GLGetProcAddress('glGetQueryObjectivARB');
   glGetQueryObjectuivARB := GLGetProcAddress('glGetQueryObjectuivARB');

   // GL_ARB_shader_objects (#30)
   glDeleteObjectARB := GLGetProcAddress('glDeleteObjectARB');
   glGetHandleARB := GLGetProcAddress('glGetHandleARB');
   glDetachObjectARB := GLGetProcAddress('glDetachObjectARB');
   glCreateShaderObjectARB := GLGetProcAddress('glCreateShaderObjectARB');
   glShaderSourceARB := GLGetProcAddress('glShaderSourceARB');
   glCompileShaderARB := GLGetProcAddress('glCompileShaderARB');
   glCreateProgramObjectARB := GLGetProcAddress('glCreateProgramObjectARB');
   glAttachObjectARB := GLGetProcAddress('glAttachObjectARB');
   glLinkProgramARB := GLGetProcAddress('glLinkProgramARB');
   glUseProgramObjectARB := GLGetProcAddress('glUseProgramObjectARB');
   glValidateProgramARB := GLGetProcAddress('glValidateProgramARB');
   glUniform1fARB := GLGetProcAddress('glUniform1fARB');
   glUniform2fARB := GLGetProcAddress('glUniform2fARB');
   glUniform3fARB := GLGetProcAddress('glUniform3fARB');
   glUniform4fARB := GLGetProcAddress('glUniform4fARB');
   glUniform1iARB := GLGetProcAddress('glUniform1iARB');
   glUniform2iARB := GLGetProcAddress('glUniform2iARB');
   glUniform3iARB := GLGetProcAddress('glUniform3iARB');
   glUniform4iARB := GLGetProcAddress('glUniform4iARB');
   glUniform1fvARB := GLGetProcAddress('glUniform1fvARB');
   glUniform2fvARB := GLGetProcAddress('glUniform2fvARB');
   glUniform3fvARB := GLGetProcAddress('glUniform3fvARB');
   glUniform4fvARB := GLGetProcAddress('glUniform4fvARB');
   glUniform1ivARB := GLGetProcAddress('glUniform1ivARB');
   glUniform2ivARB := GLGetProcAddress('glUniform2ivARB');
   glUniform3ivARB := GLGetProcAddress('glUniform3ivARB');
   glUniform4ivARB := GLGetProcAddress('glUniform4ivARB');
   glUniformMatrix2fvARB := GLGetProcAddress('glUniformMatrix2fvARB');
   glUniformMatrix3fvARB := GLGetProcAddress('glUniformMatrix3fvARB');
   glUniformMatrix4fvARB := GLGetProcAddress('glUniformMatrix4fvARB');
   glGetObjectParameterfvARB := GLGetProcAddress('glGetObjectParameterfvARB');
   glGetObjectParameterivARB := GLGetProcAddress('glGetObjectParameterivARB');
   glGetInfoLogARB := GLGetProcAddress('glGetInfoLogARB');
   glGetAttachedObjectsARB := GLGetProcAddress('glGetAttachedObjectsARB');
   glGetUniformLocationARB := GLGetProcAddress('glGetUniformLocationARB');
   glGetActiveUniformARB := GLGetProcAddress('glGetActiveUniformARB');
   glGetUniformfvARB := GLGetProcAddress('glGetUniformfvARB');
   glGetUniformivARB := GLGetProcAddress('glGetUniformivARB');
   glGetShaderSourceARB := GLGetProcAddress('glGetShaderSourceARB');

   // GL_ARB_vertex_shader (#31)
   glBindAttribLocationARB := GLGetProcAddress('glBindAttribLocationARB');
   glGetActiveAttribARB := GLGetProcAddress('glGetActiveAttribARB');
   glGetAttribLocationARB := GLGetProcAddress('glGetAttribLocationARB');

   // GL_ARB_draw_buffers (#37)
   glDrawBuffersARB := GLGetProcAddress('glDrawBuffersARB');

   // GL_ARB_color_buffer_float (#39)
   glClampColorARB := GLGetProcAddress('glClampColorARB');

   // GL_ARB_draw_instanced (ARB #44)
   glDrawArraysInstancedARB := GLGetProcAddress('glDrawArraysInstancedARB');
   glDrawElementsInstancedARB := GLGetProcAddress('glDrawElementsInstancedARB');

   // GL_ARB_framebuffer_object (ARB #45)
   glIsRenderbuffer := GLGetProcAddress('glIsRenderbuffer');
   glBindRenderbuffer := GLGetProcAddress('glBindRenderbuffer');
   glDeleteRenderbuffers := GLGetProcAddress('glDeleteRenderbuffers');
   glGenRenderbuffers := GLGetProcAddress('glGenRenderbuffers');
   glRenderbufferStorage := GLGetProcAddress('glRenderbufferStorage');
   glRenderbufferStorageMultisample := GLGetProcAddress('glRenderbufferStorageMultisample');
   glGetRenderbufferParameteriv := GLGetProcAddress('glGetRenderbufferParameteriv');
   glIsFramebuffer := GLGetProcAddress('glIsFramebuffer');
   glBindFramebuffer := GLGetProcAddress('glBindFramebuffer');
   glDeleteFramebuffers := GLGetProcAddress('glDeleteFramebuffers');
   glGenFramebuffers := GLGetProcAddress('glGenFramebuffers');
   glCheckFramebufferStatus := GLGetProcAddress('glCheckFramebufferStatus');
   glFramebufferTexture1D := GLGetProcAddress('glFramebufferTexture1D');
   glFramebufferTexture2D := GLGetProcAddress('glFramebufferTexture2D');
   glFramebufferTexture3D := GLGetProcAddress('glFramebufferTexture3D');
   glFramebufferTextureLayer := GLGetProcAddress('glFramebufferTextureLayer');
   glFramebufferRenderbuffer := GLGetProcAddress('glFramebufferRenderbuffer');
   glGetFramebufferAttachmentParameteriv := GLGetProcAddress('glGetFramebufferAttachmentParameteriv');
   glBlitFramebuffer := GLGetProcAddress('glBlitFramebuffer');
   glGenerateMipmap := GLGetProcAddress('glGenerateMipmap');

   // GL_ARB_geometry_shader4 (ARB #47)
   glProgramParameteriARB := GLGetProcAddress('glProgramParameteriARB');
   glFramebufferTextureARB := GLGetProcAddress('glFramebufferTextureARB');
   glFramebufferTextureLayerARB := GLGetProcAddress('glFramebufferTextureLayerARB');
   glFramebufferTextureFaceARB := GLGetProcAddress('glFramebufferTextureFaceARB');

   // GL_ARB_instanced_arrays (ARB #49)
   glVertexAttribDivisorARB := GLGetProcAddress('glVertexAttribDivisorARB');

   // GL_ARB_map_buffer_range (ARB #50)
   glMapBufferRange := GLGetProcAddress('glMapBufferRange');
   glFlushMappedBufferRange := GLGetProcAddress('glFlushMappedBufferRange');

   // GL_ARB_texture_buffer_object (ARB #51)
   glTexBufferARB := GLGetProcAddress('glTexBufferARB');

   // GL_ARB_vertex_array_object (ARB #54)
   glBindVertexArray := GLGetProcAddress('glBindVertexArray');
   glDeleteVertexArrays := GLGetProcAddress('glDeleteVertexArrays');
   glGenVertexArrays := GLGetProcAddress('glGenVertexArrays');
   glIsVertexArray := GLGetProcAddress('glIsVertexArray');

   // GL_ARB_uniform_buffer_object (ARB #57)
   glGetUniformIndices := GLGetProcAddress('glGetUniformIndices');
   glGetActiveUniformsiv := GLGetProcAddress('glGetActiveUniformsiv');
   glGetActiveUniformName := GLGetProcAddress('glGetActiveUniformName');
   glGetUniformBlockIndex := GLGetProcAddress('glGetUniformBlockIndex');
   glGetActiveUniformBlockiv := GLGetProcAddress('glGetActiveUniformBlockiv');
   glGetActiveUniformBlockName := GLGetProcAddress('glGetActiveUniformBlockName');
   glUniformBlockBinding := GLGetProcAddress('glUniformBlockBinding');

   // GL_ARB_copy_buffer (ARB #59)
   glCopyBufferSubData := GLGetProcAddress('glCopyBufferSubData');

   // GL_ARB_draw_elements_base_vertex (ARB #62)
   glDrawElementsBaseVertex := GLGetProcAddress('glDrawElementsBaseVertex');
   glDrawRangeElementsBaseVertex := GLGetProcAddress('glDrawRangeElementsBaseVertex');
   glDrawElementsInstancedBaseVertex := GLGetProcAddress('glDrawElementsInstancedBaseVertex');
   glMultiDrawElementsBaseVertex := GLGetProcAddress('glMultiDrawElementsBaseVertex');

   // GL_ARB_provoking_vertex (ARB #64)
   glProvokingVertex := GLGetProcAddress('glProvokingVertex');

   // GL_ARB_sync commands (ARB #66)
   glFenceSync := GLGetProcAddress('glFenceSync');
   glIsSync := GLGetProcAddress('glIsSync');
   glDeleteSync := GLGetProcAddress('glDeleteSync');
   glClientWaitSync := GLGetProcAddress('glClientWaitSync');
   glWaitSync := GLGetProcAddress('glWaitSync');
   glGetInteger64v := GLGetProcAddress('glGetInteger64v');
   glGetSynciv := GLGetProcAddress('glGetSynciv');

   // GL_ARB_texture_multisample (ARB #67)
   glTexImage2DMultisample := GLGetProcAddress('glTexImage2DMultisample');
   glTexImage3DMultisample := GLGetProcAddress('glTexImage3DMultisample');
   glGetMultisamplefv := GLGetProcAddress('glGetMultisamplefv');
   glSampleMaski := GLGetProcAddress('glSampleMaski');

   // GL_ARB_draw_buffers_blend (ARB #69)
   glBlendEquationiARB := GLGetProcAddress('glBlendEquationiARB');
   glBlendEquationSeparateiARB := GLGetProcAddress('glBlendEquationSeparateiARB');
   glBlendFunciARB := GLGetProcAddress('glBlendFunciARB');
   glBlendFuncSeparateiARB := GLGetProcAddress('glBlendFuncSeparateiARB');

   // GL_ARB_sample_shading (ARB #70)
   glMinSampleShadingARB := GLGetProcAddress('glMinSampleShadingARB');

   // GL_ARB_blend_func_extended (ARB #78)
   glBindFragDataLocationIndexed := GLGetProcAddress('glBindFragDataLocationIndexed');
   glGetFragDataIndex := GLGetProcAddress('glGetFragDataIndex');

   // GL_ARB_sampler_objects (ARB #81)
   glGenSamplers := GLGetProcAddress('glGenSamplers');
   glDeleteSamplers := GLGetProcAddress('glDeleteSamplers');
   glIsSampler := GLGetProcAddress('glIsSampler');
   glBindSampler := GLGetProcAddress('glBindSampler');
   glSamplerParameteri := GLGetProcAddress('glSamplerParameteri');
   glSamplerParameteriv := GLGetProcAddress('glSamplerParameteriv');
   glSamplerParameterf := GLGetProcAddress('glSamplerParameterf');
   glSamplerParameterfv := GLGetProcAddress('glSamplerParameterfv');
   glSamplerParameterIiv := GLGetProcAddress('glSamplerParameterIiv');
   glSamplerParameterIuiv := GLGetProcAddress('glSamplerParameterIuiv');
   glGetSamplerParameteriv := GLGetProcAddress('glGetSamplerParameteriv');
   glGetSamplerParameterIiv := GLGetProcAddress('glGetSamplerParameterIiv');
   glGetSamplerParameterfv := GLGetProcAddress('glGetSamplerParameterfv');
   glGetSamplerParameterIfv := GLGetProcAddress('glGetSamplerParameterIfv');

   // GL_ARB_timer_query (ARB #85)
   glQueryCounter := GLGetProcAddress('glQueryCounter');
   glGetQueryObjecti64v := GLGetProcAddress('glGetQueryObjecti64v');
   glGetQueryObjectui64v := GLGetProcAddress('glGetQueryObjectui64v');

   // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
   glVertexP2ui := GLGetProcAddress('glVertexP2ui');
   glVertexP2uiv := GLGetProcAddress('glVertexP2uiv');
   glVertexP3ui := GLGetProcAddress('glVertexP3ui');
   glVertexP3uiv := GLGetProcAddress('glVertexP3uiv');
   glVertexP4ui := GLGetProcAddress('glVertexP4ui');
   glVertexP4uiv := GLGetProcAddress('glVertexP4uiv');
   glTexCoordP1ui := GLGetProcAddress('glTexCoordP1ui');
   glTexCoordP1uiv := GLGetProcAddress('glTexCoordP1uiv');
   glTexCoordP2ui := GLGetProcAddress('glTexCoordP2ui');
   glTexCoordP2uiv := GLGetProcAddress('glTexCoordP2uiv');
   glTexCoordP3ui := GLGetProcAddress('glTexCoordP3ui');
   glTexCoordP3uiv := GLGetProcAddress('glTexCoordP3uiv');
   glTexCoordP4ui := GLGetProcAddress('glTexCoordP4ui');
   glTexCoordP4uiv := GLGetProcAddress('glTexCoordP4uiv');
   glMultiTexCoordP1ui := GLGetProcAddress('glMultiTexCoordP1ui');
   glMultiTexCoordP1uiv := GLGetProcAddress('glMultiTexCoordP1uiv');
   glMultiTexCoordP2ui := GLGetProcAddress('glMultiTexCoordP2ui');
   glMultiTexCoordP2uiv := GLGetProcAddress('glMultiTexCoordP2uiv');
   glMultiTexCoordP3ui := GLGetProcAddress('glMultiTexCoordP3ui');
   glMultiTexCoordP3uiv := GLGetProcAddress('glMultiTexCoordP3uiv');
   glMultiTexCoordP4ui := GLGetProcAddress('glMultiTexCoordP4ui');
   glMultiTexCoordP4uiv := GLGetProcAddress('glMultiTexCoordP4uiv');
   glNormalP3ui := GLGetProcAddress('glNormalP3ui');
   glNormalP3uiv := GLGetProcAddress('glNormalP3uiv');
   glColorP3ui := GLGetProcAddress('glColorP3ui');
   glColorP3uiv := GLGetProcAddress('glColorP3uiv');
   glColorP4ui := GLGetProcAddress('glColorP4ui');
   glColorP4uiv := GLGetProcAddress('glColorP4uiv');
   glSecondaryColorP3ui := GLGetProcAddress('glSecondaryColorP3ui');
   glSecondaryColorP3uiv := GLGetProcAddress('glSecondaryColorP3uiv');
   glVertexAttribP1ui := GLGetProcAddress('glVertexAttribP1ui');
   glVertexAttribP1uiv := GLGetProcAddress('glVertexAttribP1uiv');
   glVertexAttribP2ui := GLGetProcAddress('glVertexAttribP2ui');
   glVertexAttribP2uiv := GLGetProcAddress('glVertexAttribP2uiv');
   glVertexAttribP3ui := GLGetProcAddress('glVertexAttribP3ui');
   glVertexAttribP3uiv := GLGetProcAddress('glVertexAttribP3uiv');
   glVertexAttribP4ui := GLGetProcAddress('glVertexAttribP4ui');
   glVertexAttribP4uiv := GLGetProcAddress('glVertexAttribP4uiv');

   // GL_ARB_draw_indirect (ARB #87)
   glDrawArraysIndirect := GLGetProcAddress('glDrawArraysIndirect');
   glDrawElementsIndirect := GLGetProcAddress('glDrawElementsIndirect');

   // GL_ARB_gpu_shader_fp64 (ARB #89)
   glUniform1d := GLGetProcAddress('glUniform1d');
   glUniform2d := GLGetProcAddress('glUniform2d');
   glUniform3d := GLGetProcAddress('glUniform3d');
   glUniform4d := GLGetProcAddress('glUniform4d');
   glUniform1dv := GLGetProcAddress('glUniform1dv');
   glUniform2dv := GLGetProcAddress('glUniform2dv');
   glUniform3dv := GLGetProcAddress('glUniform3dv');
   glUniform4dv := GLGetProcAddress('glUniform4dv');
   glUniformMatrix2dv := GLGetProcAddress('glUniformMatrix2dv');
   glUniformMatrix3dv := GLGetProcAddress('glUniformMatrix3dv');
   glUniformMatrix4dv := GLGetProcAddress('glUniformMatrix4dv');
   glUniformMatrix2x3dv := GLGetProcAddress('glUniformMatrix2x3dv');
   glUniformMatrix2x4dv := GLGetProcAddress('glUniformMatrix2x4dv');
   glUniformMatrix3x2dv := GLGetProcAddress('glUniformMatrix3x2dv');
   glUniformMatrix3x4dv := GLGetProcAddress('glUniformMatrix3x4dv');
   glUniformMatrix4x2dv := GLGetProcAddress('glUniformMatrix4x2dv');
   glUniformMatrix4x3dv := GLGetProcAddress('glUniformMatrix4x3dv');
   glGetUniformdv := GLGetProcAddress('glGetUniformdv');
   glProgramUniform1dEXT := GLGetProcAddress('glProgramUniform1dEXT');
   glProgramUniform2dEXT := GLGetProcAddress('glProgramUniform2dEXT');
   glProgramUniform3dEXT := GLGetProcAddress('glProgramUniform3dEXT');
   glProgramUniform4dEXT := GLGetProcAddress('glProgramUniform4dEXT');
   glProgramUniform1dvEXT := GLGetProcAddress('glProgramUniform1dvEXT');
   glProgramUniform2dvEXT := GLGetProcAddress('glProgramUniform2dvEXT');
   glProgramUniform3dvEXT := GLGetProcAddress('glProgramUniform3dvEXT');
   glProgramUniform4dvEXT := GLGetProcAddress('glProgramUniform4dvEXT');
   glProgramUniformMatrix2dvEXT := GLGetProcAddress('glProgramUniformMatrix2dvEXT');
   glProgramUniformMatrix3dvEXT := GLGetProcAddress('glProgramUniformMatrix3dvEXT');
   glProgramUniformMatrix4dvEXT := GLGetProcAddress('glProgramUniformMatrix4dvEXT');
   glProgramUniformMatrix2x3dvEXT := GLGetProcAddress('glProgramUniformMatrix2x3dvEXT');
   glProgramUniformMatrix2x4dvEXT := GLGetProcAddress('glProgramUniformMatrix2x4dvEXT');
   glProgramUniformMatrix3x2dvEXT := GLGetProcAddress('glProgramUniformMatrix3x2dvEXT');
   glProgramUniformMatrix3x4dvEXT := GLGetProcAddress('glProgramUniformMatrix3x4dvEXT');
   glProgramUniformMatrix4x2dvEXT := GLGetProcAddress('glProgramUniformMatrix4x2dvEXT');
   glProgramUniformMatrix4x3dvEXT := GLGetProcAddress('glProgramUniformMatrix4x3dvEXT');

   // GL_ARB_shader_subroutine (ARB #90)
   glGetSubroutineUniformLocation := GLGetProcAddress('glGetSubroutineUniformLocation');
   glGetSubroutineIndex := GLGetProcAddress('glGetSubroutineIndex');
   glGetActiveSubroutineUniformiv := GLGetProcAddress('glGetActiveSubroutineUniformiv');
   glGetActiveSubroutineUniformName := GLGetProcAddress('glGetActiveSubroutineUniformName');
   glGetActiveSubroutineName := GLGetProcAddress('glGetActiveSubroutineName');
   glUniformSubroutinesuiv := GLGetProcAddress('glUniformSubroutinesuiv');
   glGetUniformSubroutineuiv := GLGetProcAddress('glGetUniformSubroutineuiv');
   glGetProgramStageiv := GLGetProcAddress('glGetProgramStageiv');

   // GL_ARB_tessellation_shader (ARB #91)
   glPatchParameteri := GLGetProcAddress('glPatchParameteri');
   glPatchParameterfv := GLGetProcAddress('glPatchParameterfv');

   // GL_ARB_transform_feedback2 (ARB #93)
   glBindTransformFeedback := GLGetProcAddress('glBindTransformFeedback');
   glDeleteTransformFeedbacks := GLGetProcAddress('glDeleteTransformFeedbacks');
   glGenTransformFeedbacks := GLGetProcAddress('glGenTransformFeedbacks');
   glIsTransformFeedback := GLGetProcAddress('glIsTransformFeedback');
   glPauseTransformFeedback := GLGetProcAddress('glPauseTransformFeedback');
   glResumeTransformFeedback := GLGetProcAddress('glResumeTransformFeedback');
   glDrawTransformFeedback := GLGetProcAddress('glDrawTransformFeedback');

   // GL_ARB_transform_feedback3 (ARB # 94)
   glDrawTransformFeedbackStream := GLGetProcAddress('glDrawTransformFeedbackStream');
   glBeginQueryIndexed := GLGetProcAddress('glBeginQueryIndexed');
   glEndQueryIndexed := GLGetProcAddress('glEndQueryIndexed');
   glGetQueryIndexediv := GLGetProcAddress('glGetQueryIndexediv');

  // GL_ARB_ES2_compatibility (ARB #95)
   glReleaseShaderCompiler := GLGetProcAddress('glReleaseShaderCompiler');
   glShaderBinary := GLGetProcAddress('glShaderBinary');
   glGetShaderPrecisionFormat := GLGetProcAddress('glGetShaderPrecisionFormat');
   glDepthRangef := GLGetProcAddress('glDepthRangef');
   glClearDepthf := GLGetProcAddress('glClearDepthf');

   // GL_ARB_get_program_binary (ARB #96)
   glGetProgramBinary := GLGetProcAddress('glGetProgramBinary');
   glProgramBinary := GLGetProcAddress('glProgramBinary');
   glProgramParameteri := GLGetProcAddress('glProgramParameteri');

   // GL_ARB_separate_shader_objects (ARB #97)
   glUseProgramStages := GLGetProcAddress('glUseProgramStages');
   glActiveShaderProgram := GLGetProcAddress('glActiveShaderProgram');
   glCreateShaderProgramv := GLGetProcAddress('glCreateShaderProgramv');
   glBindProgramPipeline := GLGetProcAddress('glBindProgramPipeline');
   glDeleteProgramPipelines := GLGetProcAddress('glDeleteProgramPipelines');
   glGenProgramPipelines := GLGetProcAddress('glGenProgramPipelines');
   glIsProgramPipeline := GLGetProcAddress('glIsProgramPipeline');
   glGetProgramPipelineiv := GLGetProcAddress('glGetProgramPipelineiv');
   glProgramUniform1i := GLGetProcAddress('glProgramUniform1i');
   glProgramUniform1iv := GLGetProcAddress('glProgramUniform1iv');
   glProgramUniform1f := GLGetProcAddress('glProgramUniform1f');
   glProgramUniform1fv := GLGetProcAddress('glProgramUniform1fv');
   glProgramUniform1d := GLGetProcAddress('glProgramUniform1d');
   glProgramUniform1dv := GLGetProcAddress('glProgramUniform1dv');
   glProgramUniform1ui := GLGetProcAddress('glProgramUniform1ui');
   glProgramUniform1uiv := GLGetProcAddress('glProgramUniform1uiv');
   glProgramUniform2i := GLGetProcAddress('glProgramUniform2i');
   glProgramUniform2iv := GLGetProcAddress('glProgramUniform2iv');
   glProgramUniform2f := GLGetProcAddress('glProgramUniform2f');
   glProgramUniform2fv := GLGetProcAddress('glProgramUniform2fv');
   glProgramUniform2d := GLGetProcAddress('glProgramUniform2d');
   glProgramUniform2dv := GLGetProcAddress('glProgramUniform2dv');
   glProgramUniform2ui := GLGetProcAddress('glProgramUniform2ui');
   glProgramUniform2uiv := GLGetProcAddress('glProgramUniform2uiv');
   glProgramUniform3i := GLGetProcAddress('glProgramUniform3i');
   glProgramUniform3iv := GLGetProcAddress('glProgramUniform3iv');
   glProgramUniform3f := GLGetProcAddress('glProgramUniform3f');
   glProgramUniform3fv := GLGetProcAddress('glProgramUniform3fv');
   glProgramUniform3d := GLGetProcAddress('glProgramUniform3d');
   glProgramUniform3dv := GLGetProcAddress('glProgramUniform3dv');
   glProgramUniform3ui := GLGetProcAddress('glProgramUniform3ui');
   glProgramUniform3uiv := GLGetProcAddress('glProgramUniform3uiv');
   glProgramUniform4i := GLGetProcAddress('glProgramUniform4i');
   glProgramUniform4iv := GLGetProcAddress('glProgramUniform4iv');
   glProgramUniform4f := GLGetProcAddress('glProgramUniform4f');
   glProgramUniform4fv := GLGetProcAddress('glProgramUniform4fv');
   glProgramUniform4d := GLGetProcAddress('glProgramUniform4d');
   glProgramUniform4dv := GLGetProcAddress('glProgramUniform4dv');
   glProgramUniform4ui := GLGetProcAddress('glProgramUniform4ui');
   glProgramUniform4uiv := GLGetProcAddress('glProgramUniform4uiv');
   glProgramUniformMatrix2fv := GLGetProcAddress('glProgramUniformMatrix2fv');
   glProgramUniformMatrix3fv := GLGetProcAddress('glProgramUniformMatrix3fv');
   glProgramUniformMatrix4fv := GLGetProcAddress('glProgramUniformMatrix4fv');
   glProgramUniformMatrix2dv := GLGetProcAddress('glProgramUniformMatrix2dv');
   glProgramUniformMatrix3dv := GLGetProcAddress('glProgramUniformMatrix3dv');
   glProgramUniformMatrix4dv := GLGetProcAddress('glProgramUniformMatrix4dv');
   glProgramUniformMatrix2x3fv := GLGetProcAddress('glProgramUniformMatrix2x3fv');
   glProgramUniformMatrix3x2fv := GLGetProcAddress('glProgramUniformMatrix3x2fv');
   glProgramUniformMatrix2x4fv := GLGetProcAddress('glProgramUniformMatrix2x4fv');
   glProgramUniformMatrix4x2fv := GLGetProcAddress('glProgramUniformMatrix4x2fv');
   glProgramUniformMatrix3x4fv := GLGetProcAddress('glProgramUniformMatrix3x4fv');
   glProgramUniformMatrix4x3fv := GLGetProcAddress('glProgramUniformMatrix4x3fv');
   glProgramUniformMatrix2x3dv := GLGetProcAddress('glProgramUniformMatrix2x3dv');
   glProgramUniformMatrix3x2dv := GLGetProcAddress('glProgramUniformMatrix3x2dv');
   glProgramUniformMatrix2x4dv := GLGetProcAddress('glProgramUniformMatrix2x4dv');
   glProgramUniformMatrix4x2dv := GLGetProcAddress('glProgramUniformMatrix4x2dv');
   glProgramUniformMatrix3x4dv := GLGetProcAddress('glProgramUniformMatrix3x4dv');
   glProgramUniformMatrix4x3dv := GLGetProcAddress('glProgramUniformMatrix4x3dv');
   glValidateProgramPipeline := GLGetProcAddress('glValidateProgramPipeline');
   glGetProgramPipelineInfoLog := GLGetProcAddress('glGetProgramPipelineInfoLog');

   // GL_ARB_shader_precision (ARB #98)
   // (no entry points)

   // GL_ARB_vertex_attrib_64bit (ARB #99)
   glVertexAttribL1d := GLGetProcAddress('glVertexAttribL1d');
   glVertexAttribL2d := GLGetProcAddress('glVertexAttribL2d');
   glVertexAttribL3d := GLGetProcAddress('glVertexAttribL3d');
   glVertexAttribL4d := GLGetProcAddress('glVertexAttribL4d');
   glVertexAttribL1dv := GLGetProcAddress('glVertexAttribL1dv');
   glVertexAttribL2dv := GLGetProcAddress('glVertexAttribL2dv');
   glVertexAttribL3dv := GLGetProcAddress('glVertexAttribL3dv');
   glVertexAttribL4dv := GLGetProcAddress('glVertexAttribL4dv');
   glVertexAttribLPointer := GLGetProcAddress('glVertexAttribLPointer');
   glGetVertexAttribLdv := GLGetProcAddress('glGetVertexAttribLdv');
   // glVertexArrayVertexAttribLOffsetEXT is only valid if EXT_direct_state_access is available
   glVertexArrayVertexAttribLOffsetEXT := GLGetProcAddress('glVertexArrayVertexAttribLOffsetEXT');

   // GL_ARB_viewport_array (ARB #100)
   glViewportArrayv := GLGetProcAddress('glViewportArrayv');
   glViewportIndexedf := GLGetProcAddress('glViewportIndexedf');
   glViewportIndexedfv := GLGetProcAddress('glViewportIndexedfv');
   glScissorArrayv := GLGetProcAddress('glScissorArrayv');
   glScissorIndexed := GLGetProcAddress('glScissorIndexed');
   glScissorIndexedv := GLGetProcAddress('glScissorIndexedv');
   glDepthRangeArrayv := GLGetProcAddress('glDepthRangeArrayv');
   glDepthRangeIndexed := GLGetProcAddress('glDepthRangeIndexed');
   glGetFloati_v := GLGetProcAddress('glGetFloati_v');
   glGetDoublei_v := GLGetProcAddress('glGetDoublei_v');

   // GL_ARB_debug_output (ARB #104)
   glDebugMessageControlARB := GLGetProcAddress('glDebugMessageControlARB');
   glDebugMessageInsertARB := GLGetProcAddress('glDebugMessageInsertARB');
   glDebugMessageCallbackARB := GLGetProcAddress('glDebugMessageCallbackARB');
   glGetDebugMessageLogARB := GLGetProcAddress('glGetDebugMessageLogARB');

   // GL_ARB_robustness (ARB #105)
   glGetGraphicsResetStatusARB := GLGetProcAddress('glGetGraphicsResetStatusARB');
   glGetnMapdvARB := GLGetProcAddress('glGetnMapdvARB');
   glGetnMapfvARB := GLGetProcAddress('glGetnMapfvARB');
   glGetnMapivARB := GLGetProcAddress('glGetnMapivARB');
   glGetnPixelMapfvARB := GLGetProcAddress('glGetnPixelMapfvARB');
   glGetnPixelMapuivARB := GLGetProcAddress('glGetnPixelMapuivARB');
   glGetnPixelMapusvARB := GLGetProcAddress('glGetnPixelMapusvARB');
   glGetnPolygonStippleARB := GLGetProcAddress('glGetnPolygonStippleARB');
   glGetnColorTableARB := GLGetProcAddress('glGetnColorTableARB');
   glGetnConvolutionFilterARB := GLGetProcAddress('glGetnConvolutionFilterARB');
   glGetnSeparableFilterARB := GLGetProcAddress('glGetnSeparableFilterARB');
   glGetnHistogramARB := GLGetProcAddress('glGetnHistogramARB');
   glGetnMinmaxARB := GLGetProcAddress('glGetnMinmaxARB');
   glGetnTexImageARB := GLGetProcAddress('glGetnTexImageARB');
   glReadnPixelsARB := GLGetProcAddress('glReadnPixelsARB');
   glGetnCompressedTexImageARB := GLGetProcAddress('glGetnCompressedTexImageARB');
   glGetnUniformfvARB := GLGetProcAddress('glGetnUniformfvARB');
   glGetnUniformivARB := GLGetProcAddress('glGetnUniformivARB');
   glGetnUniformuivARB := GLGetProcAddress('glGetnUniformuivARB');
   glGetnUniformdvARB := GLGetProcAddress('glGetnUniformdvARB');

{$IFDEF GLS_REGIONS} {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS}  {$region 'locate functions/procedures for Vendor/EXT extensions'} {$ENDIF}

   //  ###########################################################
   //            locate functions and procedures for
   //                   Vendor/EXT extensions
   //  ###########################################################

   // functions/procedures belonging to unknown extensions
   glSamplePassARB := GLGetProcAddress('glSamplePassARB');
   glArrayElementArrayEXT := GLGetProcAddress('glArrayElementArrayEXT');

   // WIN_swap_hint (extension # not found)
   glAddSwapHintRectWIN := GLGetProcAddress('glAddSwapHintRectWIN');

   // GL_EXT_blend_color (#2)
   glBlendColorEXT := GLGetProcAddress('glBlendColorEXT');

   // GL_EXT_polygon_offset (#3)
   glPolygonOffsetEXT := GLGetProcAddress('glPolygonOffsetEXT');

   // GL_EXT_texture3D (#6)
   glTexImage3DEXT := GLGetProcAddress('glTexImage3DEXT');

   // GL_EXT_subtexture (#9)
   glTexSubImage1dEXT := GLGetProcAddress('glTexSubImage1DEXT');
   glTexSubImage2dEXT := GLGetProcAddress('glTexSubImage2DEXT');
   glTexSubImage3dEXT := GLGetProcAddress('glTexSubImage3DEXT');

   // GL_EXT_copy_texture (#10)
   glCopyTexImage1DEXT := GLGetProcAddress('glCopyTexImage1DEXT');
   glCopyTexImage2DEXT := GLGetProcAddress('glCopyTexImage2DEXT');
   glCopyTexSubImage1DEXT := GLGetProcAddress('glCopyTexSubImage1DEXT');
   glCopyTexSubImage2DEXT := GLGetProcAddress('glCopyTexSubImage2DEXT');
   glCopyTexSubImage3DEXT := GLGetProcAddress('glCopyTexSubImage3DEXT');

   // GL_EXT_texture_object (#20)
   glGenTexturesEXT := GLGetProcAddress('glGenTexturesEXT');
   glDeleteTexturesEXT := GLGetProcAddress('glDeleteTexturesEXT');
   glBindTextureEXT := GLGetProcAddress('glBindTextureEXT');
   glPrioritizeTexturesEXT := GLGetProcAddress('glPrioritizeTexturesEXT');
   glAreTexturesResidentEXT := GLGetProcAddress('glAreTexturesResidentEXT');
   glIsTextureEXT := GLGetProcAddress('glIsTextureEXT');

   // GL_SGIS_multisample (#25)
   glSampleMaskSGIS := GLGetProcAddress('glSampleMaskSGIS');
   glSamplePatternSGIS := GLGetProcAddress('glSamplePatternSGIS');

   // GL_EXT_blend_minmax (#37)
   glBlendEquationEXT := GLGetProcAddress('glBlendEquationEXT');

   // GL_EXT_paletted_texture (#78)
   glColorTableEXT := GLGetProcAddress('glColorTableEXT');
   glColorSubTableEXT := GLGetProcAddress('glColorSubTableEXT');
   glGetColorTableEXT := GLGetProcAddress('glGetColorTableEXT');
   glGetColorTableParameterivEXT := GLGetProcAddress('glGetColorTableParameterivEXT');
   glGetColorTableParameterfvEXT := GLGetProcAddress('glGetColorTableParameterfvEXT');

   // GL_EXT_index_material (#94)
   glIndexMaterialEXT := GLGetProcAddress('glIndexMaterialEXT');

   // GL_EXT_index_func (#95)
   glIndexFuncEXT := GLGetProcAddress('glIndexFuncEXT');

   // EXT_compiled_vertex_array (#97)
   glLockArraysEXT := GLGetProcAddress('glLockArraysEXT');
   glUnlockArraysEXT := GLGetProcAddress('glUnlockArraysEXT');
   
   // GL_EXT_draw_range_elements (#112)
   glDrawRangeElementsEXT := GLGetProcAddress('glDrawRangeElementsEXT');

   // GL_EXT_secondary_color (#145)
   glSecondaryColor3bEXT := GLGetProcAddress('glSecondaryColor3bEXT');
   glSecondaryColor3bvEXT := GLGetProcAddress('glSecondaryColor3bvEXT');
   glSecondaryColor3dEXT := GLGetProcAddress('glSecondaryColor3dEXT');
   glSecondaryColor3dvEXT := GLGetProcAddress('glSecondaryColor3dvEXT');
   glSecondaryColor3fEXT := GLGetProcAddress('glSecondaryColor3fEXT');
   glSecondaryColor3fvEXT := GLGetProcAddress('glSecondaryColor3fvEXT');
   glSecondaryColor3iEXT := GLGetProcAddress('glSecondaryColor3iEXT');
   glSecondaryColor3ivEXT := GLGetProcAddress('glSecondaryColor3ivEXT');
   glSecondaryColor3sEXT := GLGetProcAddress('glSecondaryColor3sEXT');
   glSecondaryColor3svEXT := GLGetProcAddress('glSecondaryColor3svEXT');
   glSecondaryColor3ubEXT := GLGetProcAddress('glSecondaryColor3ubEXT');
   glSecondaryColor3ubvEXT := GLGetProcAddress('glSecondaryColor3ubvEXT');
   glSecondaryColor3uiEXT := GLGetProcAddress('glSecondaryColor3uiEXT');
   glSecondaryColor3uivEXT := GLGetProcAddress('glSecondaryColor3uivEXT');
   glSecondaryColor3usEXT := GLGetProcAddress('glSecondaryColor3usEXT');
   glSecondaryColor3usvEXT := GLGetProcAddress('glSecondaryColor3usvEXT');
   glSecondaryColorPointerEXT := GLGetProcAddress('glSecondaryColorPointerEXT');

   // GL_EXT_multi_draw_arrays (#148)
   glMultiDrawArraysEXT := GLGetProcAddress('glMultiDrawArraysEXT');
   glMultiDrawElementsEXT := GLGetProcAddress('glMultiDrawElementsEXT');

   // GL_EXT_fog_coord (#149)
   glFogCoordfEXT := GLGetProcAddress('glFogCoordfEXT'); 
   glFogCoordfvEXT := GLGetProcAddress('glFogCoordfvEXT'); 
   glFogCoorddEXT := GLGetProcAddress('glFogCoorddEXT');
   glFogCoorddvEXT := GLGetProcAddress('glFogCoorddvEXT'); 
   glFogCoordPointerEXT := GLGetProcAddress('glFogCoordPointerEXT'); 

   // GL_EXT_blend_func_separate (#173)
   glBlendFuncSeparateEXT := GLGetProcAddress('glBlendFuncSeparateEXT');

   // GL_NV_vertex_array_range (#190)
   glFlushVertexArrayRangeNV := GLGetProcAddress('glFlushVertexArrayRangeNV'); 
   glVertexArrayRangeNV := GLGetProcAddress('glVertexArrayRangeNV');
   wglAllocateMemoryNV := GLGetProcAddress('wglAllocateMemoryNV'); 
   wglFreeMemoryNV := GLGetProcAddress('wglFreeMemoryNV'); 

   // GL_NV_register_combiners (#191)
   glCombinerParameterfvNV := GLGetProcAddress('glCombinerParameterfvNV'); 
   glCombinerParameterfNV := GLGetProcAddress('glCombinerParameterfNV');
   glCombinerParameterivNV := GLGetProcAddress('glCombinerParameterivNV'); 
   glCombinerParameteriNV := GLGetProcAddress('glCombinerParameteriNV'); 
   glCombinerInputNV := GLGetProcAddress('glCombinerInputNV');
   glCombinerOutputNV := GLGetProcAddress('glCombinerOutputNV'); 
   glFinalCombinerInputNV := GLGetProcAddress('glFinalCombinerInputNV');
   glGetCombinerInputParameterfvNV := GLGetProcAddress('glGetCombinerInputParameterfvNV');
   glGetCombinerInputParameterivNV := GLGetProcAddress('glGetCombinerInputParameterivNV'); 
   glGetCombinerOutputParameterfvNV := GLGetProcAddress('glGetCombinerOutputParameterfvNV');
   glGetCombinerOutputParameterivNV := GLGetProcAddress('glGetCombinerOutputParameterivNV');
   glGetFinalCombinerInputParameterfvNV := GLGetProcAddress('glGetFinalCombinerInputParameterfvNV'); 
   glGetFinalCombinerInputParameterivNV := GLGetProcAddress('glGetFinalCombinerInputParameterivNV');

   // GL_MESA_resize_buffers (#196)
   glResizeBuffersMESA := GLGetProcAddress('glResizeBuffersMESA');

   // GL_3DFX_tbuffer (#208)
   glTbufferMask3DFX := GLGetProcAddress('glTbufferMask3DFX');

   // GL_EXT_multisample (#209)
   glSampleMaskEXT := GLGetProcAddress('glSampleMaskEXT');
   glSamplePatternEXT := GLGetProcAddress('glSamplePatternEXT');

   // GL_SGIS_texture_color_mask (#214)
   glTextureColorMaskSGIS := GLGetProcAddress('glTextureColorMaskSGIS');

   // GL_NV_fence (#222)
   glGenFencesNV := GLGetProcAddress('glGenFencesNV');
   glDeleteFencesNV := GLGetProcAddress('glDeleteFencesNV');
   glSetFenceNV := GLGetProcAddress('glSetFenceNV');
   glTestFenceNV := GLGetProcAddress('glTestFenceNV');
   glFinishFenceNV := GLGetProcAddress('glFinishFenceNV');
   glIsFenceNV := GLGetProcAddress('glIsFenceNV');
   glGetFenceivNV := GLGetProcAddress('glGetFenceivNV');

   // GL_NV_vertex_program (#233)
   glAreProgramsResidentNV := GLGetProcAddress('glAreProgramsResidentNV');
   glBindProgramNV := GLGetProcAddress('glBindProgramNV');
   glDeleteProgramsNV := GLGetProcAddress('glDeleteProgramsNV');
   glExecuteProgramNV := GLGetProcAddress('glExecuteProgramNV');
   glGenProgramsNV := GLGetProcAddress('glGenProgramsNV');
   glGetProgramParameterdvNV := GLGetProcAddress('glGetProgramParameterdvNV');
   glGetProgramParameterfvNV := GLGetProcAddress('glGetProgramParameterfvNV');
   glGetProgramivNV := GLGetProcAddress('glGetProgramivNV');
   glGetProgramStringNV := GLGetProcAddress('glGetProgramStringNV');
   glGetTrackMatrixivNV := GLGetProcAddress('glGetTrackMatrixivNV');
   glGetVertexAttribdvNV:= GLGetProcAddress('glGetVertexAttribdvNV');
   glGetVertexAttribfvNV:= GLGetProcAddress('glGetVertexAttribfvNV');
   glGetVertexAttribivNV:= GLGetProcAddress('glGetVertexAttribivNV');
   glGetVertexAttribPointervNV := GLGetProcAddress ('glGetVertexAttribPointervNV');
   glIsProgramNV := GLGetProcAddress('glIsProgramNV');
   glLoadProgramNV := GLGetProcAddress('glLoadProgramNV');
   glProgramParameter4dNV := GLGetProcAddress('glProgramParameter4dNV');
   glProgramParameter4dvNV := GLGetProcAddress('glProgramParameter4dvNV');
   glProgramParameter4fNV := GLGetProcAddress('glProgramParameter4fNV');
   glProgramParameter4fvNV := GLGetProcAddress('glProgramParameter4fvNV');
   glProgramParameters4dvNV := GLGetProcAddress ('glProgramParameters4dvNV');
   glProgramParameters4fvNV := GLGetProcAddress ('glProgramParameters4fvNV');
   glRequestResidentProgramsNV := GLGetProcAddress ('glRequestResidentProgramsNV');
   glTrackMatrixNV := GLGetProcAddress('glTrackMatrixNV');
   glVertexAttribPointerNV := GLGetProcAddress('glVertexAttribPointerNV');
   glVertexAttrib1dNV := GLGetProcAddress('glVertexAttrib1dNV');
   glVertexAttrib1dvNV := GLGetProcAddress('glVertexAttrib1dvNV');
   glVertexAttrib1fNV := GLGetProcAddress('glVertexAttrib1fNV');
   glVertexAttrib1fvNV := GLGetProcAddress('glVertexAttrib1fvNV');
   glVertexAttrib1sNV := GLGetProcAddress('glVertexAttrib1sNV');
   glVertexAttrib1svNV := GLGetProcAddress('glVertexAttrib1svNV');
   glVertexAttrib2dNV := GLGetProcAddress('glVertexAttrib2dNV');
   glVertexAttrib2dvNV := GLGetProcAddress('glVertexAttrib2dvNV');
   glVertexAttrib2fNV := GLGetProcAddress('glVertexAttrib2fNV');
   glVertexAttrib2fvNV := GLGetProcAddress('glVertexAttrib2fvNV');
   glVertexAttrib2sNV := GLGetProcAddress('glVertexAttrib2sNV');
   glVertexAttrib2svNV := GLGetProcAddress('glVertexAttrib2svNV');
   glVertexAttrib3dNV := GLGetProcAddress('glVertexAttrib3dNV');
   glVertexAttrib3dvNV := GLGetProcAddress('glVertexAttrib3dvNV');
   glVertexAttrib3fNV := GLGetProcAddress('glVertexAttrib3fNV');
   glVertexAttrib3fvNV := GLGetProcAddress('glVertexAttrib3fvNV');
   glVertexAttrib3sNV := GLGetProcAddress('glVertexAttrib3sNV');
   glVertexAttrib3svNV := GLGetProcAddress('glVertexAttrib3svNV');
   glVertexAttrib4dNV := GLGetProcAddress('glVertexAttrib4dNV');
   glVertexAttrib4dvNV := GLGetProcAddress('glVertexAttrib4dvNV');
   glVertexAttrib4fNV := GLGetProcAddress('glVertexAttrib4fNV');
   glVertexAttrib4fvNV := GLGetProcAddress('glVertexAttrib4fvNV');
   glVertexAttrib4sNV := GLGetProcAddress('glVertexAttrib4sNV');
   glVertexAttrib4svNV := GLGetProcAddress('glVertexAttrib4svNV');
   glVertexAttrib4ubvNV := GLGetProcAddress('glVertexAttrib4ubvNV');
   glVertexAttribs1dvNV := GLGetProcAddress('glVertexAttribs1dvNV');
   glVertexAttribs1fvNV := GLGetProcAddress('glVertexAttribs1fvNV');
   glVertexAttribs1svNV := GLGetProcAddress('glVertexAttribs1svNV');
   glVertexAttribs2dvNV := GLGetProcAddress('glVertexAttribs2dvNV');
   glVertexAttribs2fvNV := GLGetProcAddress('glVertexAttribs2fvNV');
   glVertexAttribs2svNV := GLGetProcAddress('glVertexAttribs2svNV');
   glVertexAttribs3dvNV := GLGetProcAddress('glVertexAttribs3dvNV');
   glVertexAttribs3fvNV := GLGetProcAddress('glVertexAttribs3fvNV');
   glVertexAttribs3svNV := GLGetProcAddress('glVertexAttribs3svNV');
   glVertexAttribs4dvNV := GLGetProcAddress('glVertexAttribs4dvNV');
   glVertexAttribs4fvNV := GLGetProcAddress('glVertexAttribs4fvNV');
   glVertexAttribs4svNV := GLGetProcAddress('glVertexAttribs4svNV');
   glVertexAttribs4ubvNV := GLGetProcAddress('glVertexAttribs4ubvN');

   // GL_NV_occlusion_query (#261)
   glGenOcclusionQueriesNV := GLGetProcAddress('glGenOcclusionQueriesNV');
   glDeleteOcclusionQueriesNV := GLGetProcAddress('glDeleteOcclusionQueriesNV');
   glIsOcclusionQueryNV := GLGetProcAddress('glIsOcclusionQueryNV');
   glBeginOcclusionQueryNV := GLGetProcAddress('glBeginOcclusionQueryNV');
   glEndOcclusionQueryNV := GLGetProcAddress('glEndOcclusionQueryNV');
   glGetOcclusionQueryivNV := GLGetProcAddress('glGetOcclusionQueryivNV');
   glGetOcclusionQueryuivNV := GLGetProcAddress('glGetOcclusionQueryuivNV');

   // GL_NV_point_sprite (#262)
   glPointParameteriNV := GLGetProcAddress('glPointParameteriNV');
   glPointParameterivNV := GLGetProcAddress('glPointParameterivNV');

   // GL_EXT_stencil_two_side (#268)
   glActiveStencilFaceEXT := GLGetProcAddress('glActiveStencilFaceEXT');

   // GL_ATI_draw_buffers (#277)
   glDrawBuffersATI := GLGetProcAddress('glDrawBuffersATI');

   // GL_NV_primitive_restart (#285)
   glPrimitiveRestartNV := GLGetProcAddress('glPrimitiveRestartNV');
   glPrimitiveRestartIndexNV := GLGetProcAddress('glPrimitiveRestartIndexNV');
   glPrimitiveRestartIndex := GLGetProcAddress('glPrimitiveRestartIndex');
   if Addr(glPrimitiveRestartIndex) = nil then
    glPrimitiveRestartIndex := glPrimitiveRestartIndexNV;

   // GL_EXT_depth_bounds_test (#297)
   glDepthBoundsEXT := GLGetProcAddress('glDepthBoundsEXT');

   // GL_EXT_blend_equation_separate (#299)
   glBlendEquationSeparateEXT := GLGetProcAddress('glBlendEquationSeparateEXT');

   // GL_EXT_framebuffer_object (#310)
   glIsRenderbufferEXT := GLGetProcAddress('glIsRenderbufferEXT');
   glBindRenderbufferEXT := GLGetProcAddress('glBindRenderbufferEXT');
   glDeleteRenderbuffersEXT := GLGetProcAddress('glDeleteRenderbuffersEXT');
   glGenRenderbuffersEXT := GLGetProcAddress('glGenRenderbuffersEXT');
   glRenderbufferStorageEXT := GLGetProcAddress('glRenderbufferStorageEXT');
   glGetRenderbufferParameterivEXT := GLGetProcAddress('glGetRenderbufferParameterivEXT');
   glIsFramebufferEXT := GLGetProcAddress('glIsFramebufferEXT');
   glBindFramebufferEXT := GLGetProcAddress('glBindFramebufferEXT');
   glDeleteFramebuffersEXT := GLGetProcAddress('glDeleteFramebuffersEXT');
   glGenFramebuffersEXT := GLGetProcAddress('glGenFramebuffersEXT');
   glCheckFramebufferStatusEXT := GLGetProcAddress('glCheckFramebufferStatusEXT');
   glFramebufferTexture1DEXT := GLGetProcAddress('glFramebufferTexture1DEXT');
   glFramebufferTexture2DEXT := GLGetProcAddress('glFramebufferTexture2DEXT');
   glFramebufferTexture3DEXT := GLGetProcAddress('glFramebufferTexture3DEXT');
   glFramebufferRenderbufferEXT := GLGetProcAddress('glFramebufferRenderbufferEXT');
   glGetFramebufferAttachmentParameterivEXT := GLGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
   glGenerateMipmapEXT := GLGetProcAddress('glGenerateMipmapEXT');

   // GL_GREMEDY_string_marker (EXT #311)
   glStringMarkerGREMEDY := GLGetProcAddress('glStringMarkerGREMEDY');

   // GL_EXT_stencil_clear_tag (EXT #314)
   glStencilClearTagEXT := GLGetProcAddress('glStencilClearTagEXT');

   // GL_EXT_framebuffer_blit (#316)
   glBlitFramebufferEXT := GLGetProcAddress('glBlitFramebufferEXT');

   // GL_EXT_framebuffer_multisample (#317)
   glRenderbufferStorageMultisampleEXT := GLGetProcAddress('glRenderbufferStorageMultisampleEXT');

   // GL_EXT_timer_query (#319)
   glGetQueryObjecti64vEXT := GLGetProcAddress('glGetQueryObjecti64vEXT');
   glGetQueryObjectui64vEXT := GLGetProcAddress('glGetQueryObjectui64vEXT');

   // GL_EXT_gpu_program_parameters (#320)
   glProgramEnvParameters4fvEXT := GLGetProcAddress('glProgramEnvParameters4fvEXT');
   glProgramLocalParameters4fvEXT := GLGetProcAddress('glProgramLocalParameters4fvEXT');

   // GL_NV_geometry_program4 (#323)
   glProgramVertexLimitNV := GLGetProcAddress('glProgramVertexLimitNV');

   // GL_EXT_geometry_shader4 (#324)
   glProgramParameteriEXT := GLGetProcAddress('glProgramParameteriEXT');
   glFramebufferTextureEXT := GLGetProcAddress('glFramebufferTextureEXT');
   glFramebufferTextureLayerEXT := GLGetProcAddress('glFramebufferTextureLayerEXT');
   glFramebufferTextureFaceEXT := GLGetProcAddress('glFramebufferTextureFaceEXT');

   // GL_EXT_gpu_shader4 (#326)
   glVertexAttribI1iEXT := GLGetProcAddress('glVertexAttribI1iEXT');
   glVertexAttribI2iEXT := GLGetProcAddress('glVertexAttribI2iEXT');
   glVertexAttribI3iEXT := GLGetProcAddress('glVertexAttribI3iEXT');
   glVertexAttribI4iEXT := GLGetProcAddress('glVertexAttribI4iEXT');

   glVertexAttribI1uiEXT := GLGetProcAddress('glVertexAttribI1uiEXT');
   glVertexAttribI2uiEXT := GLGetProcAddress('glVertexAttribI2uiEXT');
   glVertexAttribI3uiEXT := GLGetProcAddress('glVertexAttribI3uiEXT');
   glVertexAttribI4uiEXT := GLGetProcAddress('glVertexAttribI4uiEXT');

   glVertexAttribI1ivEXT := GLGetProcAddress('glVertexAttribI1ivEXT');
   glVertexAttribI2ivEXT := GLGetProcAddress('glVertexAttribI2ivEXT');
   glVertexAttribI3ivEXT := GLGetProcAddress('glVertexAttribI3ivEXT');
   glVertexAttribI4ivEXT := GLGetProcAddress('glVertexAttribI4ivEXT');

   glVertexAttribI1uivEXT := GLGetProcAddress('glVertexAttribI1uivEXT');
   glVertexAttribI2uivEXT := GLGetProcAddress('glVertexAttribI2uivEXT');
   glVertexAttribI3uivEXT := GLGetProcAddress('glVertexAttribI3uivEXT');
   glVertexAttribI4uivEXT := GLGetProcAddress('glVertexAttribI4uivEXT');

   glVertexAttribI4bvEXT := GLGetProcAddress('glVertexAttribI4bvEXT');
   glVertexAttribI4svEXT := GLGetProcAddress('glVertexAttribI4svEXT');
   glVertexAttribI4ubvEXT := GLGetProcAddress('glVertexAttribI4ubvEXT');
   glVertexAttribI4usvEXT := GLGetProcAddress('glVertexAttribI4usvEXT');

   glVertexAttribIPointerEXT := GLGetProcAddress('glVertexAttribIPointerEXT');

   glGetVertexAttribIivEXT := GLGetProcAddress('glGetVertexAttribIivEXT');
   glGetVertexAttribIuivEXT := GLGetProcAddress('glGetVertexAttribIuivEXT');

   glUniform1uiEXT := GLGetProcAddress('glUniform1uiEXT');
   glUniform2uiEXT := GLGetProcAddress('glUniform2uiEXT');
   glUniform3uiEXT := GLGetProcAddress('glUniform3uiEXT');
   glUniform4uiEXT := GLGetProcAddress('glUniform4uiEXT');

   glUniform1uivEXT := GLGetProcAddress('glUniform1uivEXT');
   glUniform2uivEXT := GLGetProcAddress('glUniform2uivEXT');
   glUniform3uivEXT := GLGetProcAddress('glUniform3uivEXT');
   glUniform4uivEXT := GLGetProcAddress('glUniform4uivEXT');

   glGetUniformuivEXT := GLGetProcAddress('glGetUniformuivEXT');

   glBindFragDataLocationEXT := GLGetProcAddress('glBindFragDataLocationEXT');
   glGetFragDataLocationEXT := GLGetProcAddress('glGetFragDataLocationEXT');

   // GL_EXT_draw_instanced (#327)
   glDrawArraysInstancedEXT := GLGetProcAddress('glDrawArraysInstancedEXT');
   glDrawElementsInstancedEXT := GLGetProcAddress('glDrawElementsInstancedEXT');

   // GL_EXT_texture_array (#329)
//   glFramebufferTextureLayerEXT:= GLGetProcAddress('glFramebufferTextureLayerEXT');

   // GL_EXT_texture_buffer_object (#330)
   glTexBufferEXT := GLGetProcAddress('glTexBufferEXT');

   // GL_EXT_draw_buffers2 (#340)
   glColorMaskIndexedEXT := GLGetProcAddress('glColorMaskIndexedEXT');
   glGetBooleanIndexedvEXT := GLGetProcAddress('glGetBooleanIndexedvEXT');
   glGetIntegerIndexedvEXT:= GLGetProcAddress('glGetIntegerIndexedvEXT');
   glEnableIndexedEXT:= GLGetProcAddress('glEnableIndexedEXT');
   glDisableIndexedEXT:= GLGetProcAddress('glDisableIndexedEXT');
   glIsEnabledIndexedEXT:= GLGetProcAddress('glIsEnabledIndexedEXT');

   // GL_NV_transform_feedback (#341)
   glBindBufferRangeNV := GLGetProcAddress('glBindBufferRangeNV');
   glBindBufferOffsetNV := GLGetProcAddress('glBindBufferOffsetNV');
   glBindBufferBaseNV := GLGetProcAddress('glBindBufferBaseNV');
   glTransformFeedbackAttribsNV := GLGetProcAddress('glTransformFeedbackAttribsNV');
   glTransformFeedbackVaryingsNV := GLGetProcAddress('glTransformFeedbackVaryingsNV');
   glBeginTransformFeedbackNV := GLGetProcAddress('glBeginTransformFeedbackNV');
   glEndTransformFeedbackNV := GLGetProcAddress('glEndTransformFeedbackNV');
   glGetVaryingLocationNV := GLGetProcAddress('glGetVaryingLocationNV');
   glGetActiveVaryingNV := GLGetProcAddress('glGetActiveVaryingNV');
   glActiveVaryingNV := GLGetProcAddress('glActiveVaryingNV');
   glGetTransformFeedbackVaryingNV := GLGetProcAddress('glGetTransformFeedbackVaryingNV');

   // GL_EXT_bindable_uniform (#342)
   glUniformBufferEXT := GLGetProcAddress('glUniformBufferEXT');
   glGetUniformBufferSizeEXT := GLGetProcAddress('glGetUniformBufferSizeEXT');
   glGetUniformOffsetEXT := GLGetProcAddress('glGetUniformOffsetEXT');

   // GL_EXT_texture_integer (#343)
   glClearColorIiEXT := GLGetProcAddress('glClearColorIiEXT');
   glClearColorIuiEXT := GLGetProcAddress('glClearColorIuiEXT');
   glTexParameterIivEXT := GLGetProcAddress('glTexParameterIivEXT');
   glTexParameterIuivEXT := GLGetProcAddress('glTexParameterIuivEXT');
   glGetTexParameterIivEXT := GLGetProcAddress('glGetTexParameterIivEXT');
   glGetTexParameterIuivEXT := GLGetProcAddress('glGetTexParameterIuivEXT');

   // GL_GREMEDY_frame_terminator (EXT #345)
   glFrameTerminatorGREMEDY := GLGetProcAddress('glFrameTerminatorGREMEDY');

   // GL_NV_conditional_render (#346)
   glBeginConditionalRenderNV := GLGetProcAddress('glBeginConditionalRenderNV');
   glEndConditionalRenderNV := GLGetProcAddress('glEndConditionalRenderNV');

   // GL_EXT_transform_feedback (#352)
   glBindBufferRangeEXT := GLGetProcAddress('glBindBufferRangeEXT');
   glBindBufferOffsetEXT := GLGetProcAddress('glBindBufferOffsetEXT');
   glBindBufferBaseEXT := GLGetProcAddress('glBindBufferBaseEXT');
   glBeginTransformFeedbackEXT := GLGetProcAddress('glBeginTransformFeedbackEXT');
   glEndTransformFeedbackEXT := GLGetProcAddress('glEndTransformFeedbackEXT');
   glTransformFeedbackVaryingsEXT := GLGetProcAddress('glTransformFeedbackVaryingsEXT');
   glGetTransformFeedbackVaryingEXT:= GLGetProcAddress('glGetTransformFeedbackVaryingEXT');

   // GL_AMD_vertex_shader_tesselator (#363)
   glTessellationFactorAMD := GLGetProcAddress('glTessellationFactorAMD');
   glTessellationModeAMD := GLGetProcAddress('glTessellationModeAMD');

   // GL_NV_copy_image (#376)
   glCopyImageSubDataNV := GLGetProcAddress('glCopyImageSubDataNV');

   // GL_NV_shader_buffer_load (#379)
   glMakeBufferResidentNV := GLGetProcAddress('glMakeBufferResidentNV');
   glMakeBufferNonResidentNV := GLGetProcAddress('glMakeBufferNonResidentNV');
   glIsBufferResidentNV := GLGetProcAddress('glIsBufferResidentNV');
   glMakeNamedBufferResidentNV := GLGetProcAddress('glMakeNamedBufferResidentNV');
   glMakeNamedBufferNonResidentNV := GLGetProcAddress('glMakeNamedBufferNonResidentNV');
   glIsNamedBufferResidentNV := GLGetProcAddress('glIsNamedBufferResidentNV');
   glGetBufferParameterui64vNV := GLGetProcAddress('glGetBufferParameterui64vNV');
   glGetNamedBufferParameterui64vNV := GLGetProcAddress('glGetNamedBufferParameterui64vNV');
   glGetIntegerui64vNV := GLGetProcAddress('glGetIntegerui64vNV');
   glUniformui64NV := GLGetProcAddress('glUniformui64NV');
   glUniformui64vNV := GLGetProcAddress('glUniformui64vNV');
   glGetUniformui64vNV := GLGetProcAddress('glGetUniformui64vNV');
   glProgramUniformui64NV := GLGetProcAddress('glProgramUniformui64NV');
   glProgramUniformui64vNV := GLGetProcAddress('glProgramUniformui64vNV');

   // GL_NV_vertex_buffer_unified_memory (#380)
   glBufferAddressRangeNV := GLGetProcAddress('glBufferAddressRangeNV');
   glVertexFormatNV := GLGetProcAddress('glVertexFormatNV');
   glNormalFormatNV := GLGetProcAddress('glNormalFormatNV');
   glColorFormatNV := GLGetProcAddress('glColorFormatNV');
   glIndexFormatNV := GLGetProcAddress('glIndexFormatNV');
   glTexCoordFormatNV := GLGetProcAddress('glTexCoordFormatNV');
   glEdgeFlagFormatNV := GLGetProcAddress('glEdgeFlagFormatNV');
   glSecondaryColorFormatNV := GLGetProcAddress('glSecondaryColorFormatNV');
   glFogCoordFormatNV := GLGetProcAddress('glFogCoordFormatNV');
   glVertexAttribFormatNV := GLGetProcAddress('glVertexAttribFormatNV');
   glVertexAttribIFormatNV := GLGetProcAddress('glVertexAttribIFormatNV');
   glGetIntegerui64i_vNV := GLGetProcAddress('glGetIntegerui64i_vNV');

{$IFDEF GLS_REGIONS}  {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS}  {$region 'locate functions/procedures for Windows OpenGL (WGL) extensions'} {$ENDIF}
   {$IFDEF SUPPORT_WGL}
   ReadWGLExtensions;
   {$ENDIF}
{$IFDEF GLS_REGIONS}  {$endregion} {$ENDIF}

{$IFDEF GLS_REGIONS}  {$region 'locate functions/procedures for GLX extensions'} {$ENDIF}
   {$IFDEF SUPPORT_GLX}
   ReadGLXExtensions;
   {$ENDIF}
{$IFDEF GLS_REGIONS}  {$endregion} {$ENDIF}

end;

{$IFDEF SUPPORT_WGL}
// ReadWGLExtensions
//
procedure ReadWGLExtensions;
begin
   // ARB wgl extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved WGL extensions
   //  ###########################################################

   // WGL_buffer_region (ARB #4)
   wglCreateBufferRegionARB := GLGetProcAddress('wglCreateBufferRegionARB');
   wglDeleteBufferRegionARB := GLGetProcAddress('wglDeleteBufferRegionARB');
   wglSaveBufferRegionARB := GLGetProcAddress('wglSaveBufferRegionARB');
   wglRestoreBufferRegionARB := GLGetProcAddress('wglRestoreBufferRegionARB');

   // WGL_ARB_extensions_string (ARB #8)
   wglGetExtensionsStringARB := GLGetProcAddress('wglGetExtensionsStringARB');

   // WGL_ARB_pixel_format (ARB #9)
   wglGetPixelFormatAttribivARB := GLGetProcAddress('wglGetPixelFormatAttribivARB');
   wglGetPixelFormatAttribfvARB := GLGetProcAddress('wglGetPixelFormatAttribfvARB');
   wglChoosePixelFormatARB := GLGetProcAddress('wglChoosePixelFormatARB');

   // WGL_make_current_read (ARB #10)
   wglMakeContextCurrentARB := GLGetProcAddress('wglMakeContextCurrentARB');
   wglGetCurrentReadDCARB := GLGetProcAddress('wglGetCurrentReadDCARB');

   // WGL_ARB_pbuffer (ARB #11)
   wglCreatePbufferARB := GLGetProcAddress('wglCreatePbufferARB');
   wglGetPbufferDCARB := GLGetProcAddress('wglGetPbufferDCARB');
   wglReleasePbufferDCARB := GLGetProcAddress('wglReleasePbufferDCARB');
   wglDestroyPbufferARB := GLGetProcAddress('wglDestroyPbufferARB');
   wglQueryPbufferARB := GLGetProcAddress('wglQueryPbufferARB');

   // WGL_ARB_render_texture (ARB #20)
   wglBindTexImageARB := GLGetProcAddress('wglBindTexImageARB');
   wglReleaseTexImageARB := GLGetProcAddress('wglReleaseTexImageARB');
   wglSetPbufferAttribARB := GLGetProcAddress('wglSetPbufferAttribARB');

   // WGL_ARB_create_context (ARB #55)
   wglCreateContextAttribsARB := GLGetProcAddress('wglCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
   wglSwapIntervalEXT := GLGetProcAddress('wglSwapIntervalEXT');
   wglGetSwapIntervalEXT := GLGetProcAddress('wglGetSwapIntervalEXT');

   // WGL_NV_gpu_affinity
   wglEnumGpusNV := GLGetProcAddress('wglEnumGpusNV');
   wglEnumGpuDevicesNV := GLGetProcAddress('wglEnumGpuDevicesNV');
   wglCreateAffinityDCNV := GLGetProcAddress('wglCreateAffinityDCNV');
   wglEnumGpusFromAffinityDCNV := GLGetProcAddress('wglEnumGpusFromAffinityDCNV');
   wglDeleteDCNV := GLGetProcAddress('wglDeleteDCNV');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXExtensions
//
procedure ReadGLXExtensions;
begin
   // ARB glx extensions

   //  ###########################################################
   //            locating functions and procedures for
   //                  ARB approved GLX extensions
   //  ###########################################################

   //loading first!
   glXGetProcAddress := GLLibGetProcAddress('glXGetProcAddress');
   glXGetProcAddressARB := GLLibGetProcAddress('glXGetProcAddressARB');

   //GLX 1.3 and later
   glXChooseFBConfig := GLGetProcAddress('glXChooseFBConfig');
   glXGetFBConfigAttrib := GLGetProcAddress('glXGetFBConfigAttrib');
   glXGetFBConfigs := GLGetProcAddress('glXGetFBConfigs');
   glXGetVisualFromFBConfig := GLGetProcAddress('glXGetVisualFromFBConfig');
   glXCreateWindow := GLGetProcAddress('glXCreateWindow');
   glXDestroyWindow := GLGetProcAddress('glXDestroyWindow');
   glXCreatePixmap := GLGetProcAddress('glXCreatePixmap');
   glXDestroyPixmap := GLGetProcAddress('glXDestroyPixmap');
   glXCreatePbuffer := GLGetProcAddress('glXCreatePbuffer');
   glXDestroyPbuffer := GLGetProcAddress('glXDestroyPbuffer');
   glXQueryDrawable := GLGetProcAddress('glXQueryDrawable');
   glXCreateNewContext := GLGetProcAddress('glXCreateNewContext');
   glXMakeContextCurrent := GLGetProcAddress('glXMakeContextCurrent');
   glXGetCurrentReadDrawable := GLGetProcAddress('glXGetCurrentReadDrawable');
   glXQueryContext := GLGetProcAddress('glXQueryContext');
   glXSelectEvent := GLGetProcAddress('glXSelectEvent');
   glXGetSelectedEvent := GLGetProcAddress('glXGetSelectedEvent');
   glXBindTexImageARB := GLGetProcAddress('glXBindTexImageARB');
   glXReleaseTexImageARB := GLGetProcAddress('glXReleaseTexImageARB');
   glxDrawableAttribARB := GLGetProcAddress('glxDrawableAttribARB');

   //GLX 1.4
   // GLX_ARB_create_context (EXT #56)
   glXCreateContextAttribsARB := GLGetProcAddress('glXCreateContextAttribsARB');

   //  ###########################################################
   //            locating functions and procedures for
   //                Vendor/EXT WGL extensions
   //  ###########################################################

   // WGL_EXT_swap_control (EXT #172)
    glXSwapIntervalSGI := GLGetProcAddress('glXSwapIntervalSGI');
    glXGetVideoSyncSGI := GLGetProcAddress('glXGetVideoSyncSGI');
    glXWaitVideoSyncSGI := GLGetProcAddress('glXWaitVideoSyncSGI');
    glXFreeContextEXT := GLGetProcAddress('glXFreeContextEXT');
    glXGetContextIDEXT := GLGetProcAddress('glXGetContextIDEXT');
    glXGetCurrentDisplayEXT := GLGetProcAddress('glXGetCurrentDisplayEXT');
    glXImportContextEXT := GLGetProcAddress('glXImportContextEXT');
    glXQueryContextInfoEXT := GLGetProcAddress('glXQueryContextInfoEXT');
    glXCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
    glXCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
    glXReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
    glXSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

    glXBindTexImageEXT := GLGetProcAddress('glXBindTexImageEXT');
    glXReleaseTexImageEXT := GLGetProcAddress('glXReleaseTexImageEXT');

    //GLX 1.4
    glXMakeCurrentReadSGI := GLGetProcAddress('glXMakeCurrentReadSGI');
    glXGetCurrentReadDrawableSGI := GLGetProcAddress('glXGetCurrentReadDrawableSGI');
    glXGetFBConfigAttribSGIX := GLGetProcAddress('glXGetFBConfigAttribSGIX');
    glXChooseFBConfigSGIX := GLGetProcAddress('glXChooseFBConfigSGIX');
    glXCreateGLXPixmapWithConfigSGIX := GLGetProcAddress('glXCreateGLXPixmapWithConfigSGIX');
    glXCreateContextWithConfigSGIX := GLGetProcAddress('glXCreateContextWithConfigSGIX');
    glXGetVisualFromFBConfigSGIX := GLGetProcAddress('glXGetVisualFromFBConfigSGIX');
    glXGetFBConfigFromVisualSGIX := GLGetProcAddress('glXGetFBConfigFromVisualSGIX');
    glXCreateGLXPbufferSGIX := GLGetProcAddress('glXCreateGLXPbufferSGIX');
    glXDestroyGLXPbufferSGIX := GLGetProcAddress('glXDestroyGLXPbufferSGIX');
    glXQueryGLXPbufferSGIX := GLGetProcAddress('glXQueryGLXPbufferSGIX');
    glXSelectEventSGIX := GLGetProcAddress('glXSelectEventSGIX');
    glXGetSelectedEventSGIX := GLGetProcAddress('glXGetSelectedEventSGIX');
    glXCushionSGI := GLGetProcAddress('glXCushionSGI');
    glXBindChannelToWindowSGIX := GLGetProcAddress('glXBindChannelToWindowSGIX');
    glXChannelRectSGIX := GLGetProcAddress('glXChannelRectSGIX');
    glXQueryChannelRectSGIX := GLGetProcAddress('glXQueryChannelRectSGIX');
    glXQueryChannelDeltasSGIX := GLGetProcAddress('glXQueryChannelDeltasSGIX');
    glXChannelRectSyncSGIX := GLGetProcAddress('glXChannelRectSyncSGIX');
    glXJoinSwapGroupSGIX := GLGetProcAddress('glXJoinSwapGroupSGIX');
    glXBindSwapBarrierSGIX := GLGetProcAddress('glXBindSwapBarrierSGIX');
    glXQueryMaxSwapBarriersSGIX := GLGetProcAddress('glXQueryMaxSwapBarriersSGIX');
    glXQueryHyperpipeNetworkSGIX := GLGetProcAddress('glXQueryHyperpipeNetworkSGIX');


    glXHyperpipeConfigSGIX := GLGetProcAddress('glXHyperpipeConfigSGIX');
    glXQueryHyperpipeConfigSGIX := GLGetProcAddress('glXQueryHyperpipeConfigSGIX');
    glXDestroyHyperpipeConfigSGIX := GLGetProcAddress('glXDestroyHyperpipeConfigSGIX');
    glXBindHyperpipeSGIX := GLGetProcAddress('glXBindHyperpipeSGIX');
    glXQueryHyperpipeBestAttribSGIX := GLGetProcAddress('glXQueryHyperpipeBestAttribSGIX');
    glXHyperpipeAttribSGIX := GLGetProcAddress('glXHyperpipeAttribSGIX');
    glXQueryHyperpipeAttribSGIX := GLGetProcAddress('glXQueryHyperpipeAttribSGIX');
    glXGetAGPOffsetMESA := GLGetProcAddress('glXGetAGPOffsetMESA');
    glXEnumerateVideoDevicesNV := GLGetProcAddress('glXEnumerateVideoDevicesNV');
    glXBindVideoDeviceNV := GLGetProcAddress('glXBindVideoDeviceNV');
    glxGetVideoDeviceNV := GLGetProcAddress('glxGetVideoDeviceNV');
    glXCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
    glXReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
    glXCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
    glXSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

    glXAllocateMemoryNV := GLGetProcAddress('glXAllocateMemoryNV');
    glXFreeMemoryNV := GLGetProcAddress('glXFreeMemoryNV');

    glXReleaseVideoDeviceNV := GLGetProcAddress('glXReleaseVideoDeviceNV');
    glXBindVideoImageNV := GLGetProcAddress('glXBindVideoImageNV');
    glXReleaseVideoImageNV := GLGetProcAddress('glXReleaseVideoImageNV');
    glXSendPbufferToVideoNV := GLGetProcAddress('glXSendPbufferToVideoNV');
    glXGetVideoInfoNV := GLGetProcAddress('glXGetVideoInfoNV');
    glXJoinSwapGroupNV := GLGetProcAddress('glXJoinSwapGroupNV');
    glXBindSwapBarrierNV := GLGetProcAddress('glXBindSwapBarrierNV');
    glXQuerySwapGroupNV := GLGetProcAddress('glXQuerySwapGroupNV');
    glXQueryMaxSwapGroupsNV := GLGetProcAddress('glXQueryMaxSwapGroupsNV');
    glXQueryFrameCountNV := GLGetProcAddress('glXQueryFrameCountNV');
    glXResetFrameCountNV := GLGetProcAddress('glXResetFrameCountNV');
    glXBindVideoCaptureDeviceNV := GLGetProcAddress('glXBindVideoCaptureDeviceNV');
    glXEnumerateVideoCaptureDevicesNV := GLGetProcAddress('glXEnumerateVideoCaptureDevicesNV');
    glxLockVideoCaptureDeviceNV := GLGetProcAddress('glxLockVideoCaptureDeviceNV');
    glXQueryVideoCaptureDeviceNV := GLGetProcAddress('glXQueryVideoCaptureDeviceNV');
    glXReleaseVideoCaptureDeviceNV := GLGetProcAddress('glXReleaseVideoCaptureDeviceNV');
    glXSwapIntervalEXT := GLGetProcAddress('glXSwapIntervalEXT');
    glXCopyImageSubDataNV := GLGetProcAddress('glXCopyImageSubDataNV');
end;
{$ENDIF}

// TrimAndSplitVersionString
//
procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".
var
  Separator: Integer;
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', Buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < Length(Buffer)) and (AnsiChar(Buffer[Separator - 1]) in ['0'..'9']) and
      (AnsiChar(Buffer[Separator + 1]) in ['0'..'9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator);
      // Find last non-numeric character before version number.
      while (Separator > 0) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Dec(Separator);
      // Delete leading characters which do not belong to the version string.
      Delete(Buffer, 1, Separator);
      Separator := Pos('.', Buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= Length(Buffer)) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
        Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(Buffer, Separator, 255);
      // Now translate the numbers.
      Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
      Max := StrToInt(Copy(Buffer, 1, Separator - 1));
      Min := StrToInt(Copy(Buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    Min := 0;
    Max := 0;
  end;
end;

function IsVersionMet(MajorVersion,MinorVersion,actualMajorVersion, actualMinorVersion:Integer): boolean;
begin
  Result:=(actualMajorVersion>MajorVersion)or
          ((actualMajorVersion=MajorVersion)and(actualMinorVersion>=MinorVersion));
end;

// ReadImplementationProperties
//
procedure ReadImplementationProperties;
var
   Buffer : String;
   MajorVersion, MinorVersion: Integer;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // determine OpenGL versions supported
   buffer:=String(glGetString(GL_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GL_VERSION_1_0:=True;
   GL_VERSION_1_1:=IsVersionMet(1,1,majorVersion,minorVersion);
   GL_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GL_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);
   GL_VERSION_1_4:=IsVersionMet(1,4,majorVersion,minorVersion);
   GL_VERSION_1_5:=IsVersionMet(1,5,majorVersion,minorVersion);
   GL_VERSION_2_0:=IsVersionMet(2,0,majorVersion,minorVersion);
   GL_VERSION_2_1:=IsVersionMet(2,1,majorVersion,minorVersion);
   GL_VERSION_3_0:=IsVersionMet(3,0,majorVersion,minorVersion);
   GL_VERSION_3_1:=IsVersionMet(3,1,majorVersion,minorVersion);
   GL_VERSION_3_2:=IsVersionMet(3,2,majorVersion,minorVersion);
   GL_VERSION_3_3:=IsVersionMet(3,3,majorVersion,minorVersion);
   GL_VERSION_4_0:=IsVersionMet(4,0,majorVersion,minorVersion);
   GL_VERSION_4_1:=IsVersionMet(4,1,majorVersion,minorVersion);

   // determine GLU versions met
   buffer:=String(gluGetString(GLU_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GLU_VERSION_1_1:=True; // won't load without at least GLU 1.1
   GLU_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GLU_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);

   // check supported OpenGL extensions
   Buffer := String(glGetString(GL_EXTENSIONS));
   // check ARB approved OpenGL extensions
   GL_ARB_blend_func_extended := CheckExtension('GL_ARB_blend_func_extended');
   GL_ARB_color_buffer_float := CheckExtension('GL_ARB_color_buffer_float');
   GL_ARB_compatibility := CheckExtension('GL_ARB_compatibility');
   GL_ARB_copy_buffer := CheckExtension('GL_ARB_copy_buffer');
   GL_ARB_debug_output := CheckExtension('GL_ARB_debug_output');
   GL_ARB_depth_buffer_float := CheckExtension('GL_ARB_depth_buffer_float');
   GL_ARB_depth_clamp := CheckExtension('GL_ARB_depth_clamp');
   GL_ARB_depth_texture := CheckExtension('GL_ARB_depth_texture');
   GL_ARB_draw_buffers := CheckExtension('GL_ARB_draw_buffers');
   GL_ARB_draw_buffers_blend := CheckExtension('GL_ARB_draw_buffers_blend');
   GL_ARB_draw_elements_base_vertex := CheckExtension('GL_ARB_draw_elements_base_vertex');
   GL_ARB_draw_indirect := CheckExtension('GL_ARB_draw_indirect');
   GL_ARB_draw_instanced := CheckExtension('GL_ARB_draw_instanced');
   GL_ARB_ES2_compatibility := CheckExtension('GL_ARB_ES2_compatibility');
   GL_ARB_explicit_attrib_location := CheckExtension('GL_ARB_explicit_attrib_location');
   GL_ARB_fragment_coord_conventions := CheckExtension('GL_ARB_fragment_coord_conventions');
   GL_ARB_fragment_program := CheckExtension('GL_ARB_fragment_program');
   GL_ARB_fragment_program_shadow := CheckExtension('GL_ARB_fragment_program_shadow');
   GL_ARB_fragment_shader := CheckExtension('GL_ARB_fragment_shader');
   GL_ARB_framebuffer_object := CheckExtension('GL_ARB_framebuffer_object');
   GL_ARB_framebuffer_sRGB := CheckExtension('GL_ARB_framebuffer_sRGB');
   GL_ARB_geometry_shader4 := CheckExtension('GL_ARB_geometry_shader4');
   GL_ARB_get_program_binary := CheckExtension('GL_ARB_get_program_binary');
   GL_ARB_gpu_shader_fp64 := CheckExtension('GL_ARB_gpu_shader_fp64');
   GL_ARB_gpu_shader5 := CheckExtension('GL_ARB_gpu_shader5');
   GL_ARB_half_float_pixel := CheckExtension('GL_ARB_half_float_pixel');
   GL_ARB_half_float_vertex := CheckExtension('GL_ARB_half_float_vertex');
   GL_ARB_imaging := CheckExtension('GL_ARB_imaging');
   GL_ARB_instanced_arrays := CheckExtension('GL_ARB_instanced_arrays');
   GL_ARB_map_buffer_range := CheckExtension('GL_ARB_map_buffer_range');
   GL_ARB_matrix_palette  := CheckExtension('GL_ARB_matrix_palette');
   GL_ARB_multisample := CheckExtension(' GL_ARB_multisample'); // ' ' to avoid collision with WGL variant
   GL_ARB_multitexture := CheckExtension('GL_ARB_multitexture');
   GL_ARB_occlusion_query := CheckExtension('GL_ARB_occlusion_query');
   GL_ARB_occlusion_query2 := CheckExtension('GL_ARB_occlusion_query2');
   GL_ARB_pixel_buffer_object := CheckExtension('GL_ARB_pixel_buffer_object');
   GL_ARB_point_parameters := CheckExtension('GL_ARB_point_parameters');
   GL_ARB_point_sprite := CheckExtension('GL_ARB_point_sprite');
   GL_ARB_provoking_vertex := CheckExtension('GL_ARB_provoking_vertex');
   GL_ARB_robustness := CheckExtension('GL_ARB_robustness');
   GL_ARB_sample_shading := CheckExtension('GL_ARB_sample_shading');
   GL_ARB_sampler_objects := CheckExtension('GL_ARB_sampler_objects');
   GL_ARB_seamless_cube_map := CheckExtension('GL_ARB_seamless_cube_map');
   GL_ARB_separate_shader_objects := CheckExtension('GL_ARB_separate_shader_objects');
   GL_ARB_shader_bit_encoding := CheckExtension('GL_ARB_shader_bit_encoding');
   GL_ARB_shader_precision := CheckExtension('GL_ARB_shader_precision');
   GL_ARB_shader_objects := CheckExtension('GL_ARB_shader_objects');
   GL_ARB_shader_stencil_export := CheckExtension('GL_ARB_shader_stencil_export');
   GL_ARB_shader_subroutine := CheckExtension('GL_ARB_shader_subroutine');
   GL_ARB_shader_texture_lod := CheckExtension('GL_ARB_shader_texture_lod');
   GL_ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
   GL_ARB_shadow := CheckExtension('GL_ARB_shadow');
   GL_ARB_shadow_ambient := CheckExtension('GL_ARB_shadow_ambient');
   GL_ARB_sync := CheckExtension('GL_ARB_sync');
   GL_ARB_tessellation_shader := CheckExtension('GL_ARB_tessellation_shader');
   GL_ARB_texture_border_clamp := CheckExtension('GL_ARB_texture_border_clamp');
   GL_ARB_texture_buffer_object := CheckExtension('GL_ARB_texture_buffer_object');
   GL_ARB_texture_buffer_object_rgb32 := CheckExtension('GL_ARB_texture_buffer_object_rgb32');
   GL_ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
   GL_ARB_texture_compression_rgtc := CheckExtension('GL_ARB_texture_compression_rgtc');
   GL_ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
   GL_ARB_texture_cube_map_array := CheckExtension('GL_ARB_texture_cube_map_array');
   GL_ARB_texture_env_add := CheckExtension('GL_ARB_texture_env_add');
   GL_ARB_texture_env_combine := CheckExtension('GL_ARB_texture_env_combine');
   GL_ARB_texture_env_crossbar := CheckExtension('GL_ARB_texture_env_crossbar');
   GL_ARB_texture_env_dot3 := CheckExtension('GL_ARB_texture_env_dot3');
   GL_ARB_texture_float := CheckExtension('GL_ARB_texture_float');
   GL_ARB_texture_gather := CheckExtension('GL_ARB_texture_gather');
   GL_ARB_texture_mirrored_repeat := CheckExtension('GL_ARB_texture_mirrored_repeat');
   GL_ARB_texture_multisample := CheckExtension('GL_ARB_texture_multisample');
   GL_ARB_texture_non_power_of_two := CheckExtension('GL_ARB_texture_non_power_of_two');
   GL_ARB_texture_query_lod := CheckExtension('GL_ARB_texture_query_lod');
   GL_ARB_texture_rectangle := CheckExtension('GL_ARB_texture_rectangle');
   GL_ARB_texture_rg := CheckExtension('GL_ARB_texture_rg');
   GL_ARB_texture_rgb10_a2ui := CheckExtension('GL_ARB_texture_rgb10_a2ui');
   GL_ARB_texture_swizzle := CheckExtension('GL_ARB_texture_swizzle');
   GL_ARB_timer_query := CheckExtension('GL_ARB_timer_query');
   GL_ARB_transform_feedback2 := CheckExtension('GL_ARB_transform_feedback2');
   GL_ARB_transform_feedback3 := CheckExtension('GL_ARB_transform_feedback3');
   GL_ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
   GL_ARB_uniform_buffer_object := CheckExtension('GL_ARB_uniform_buffer_object');
   GL_ARB_vertex_array_bgra := CheckExtension('GL_ARB_vertex_array_bgra');
   GL_ARB_vertex_array_object := CheckExtension('GL_ARB_vertex_array_object');
   GL_ARB_vertex_attrib_64bit := CheckExtension('GL_ARB_vertex_attrib_64bit');
   GL_ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');
   GL_ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
   GL_ARB_vertex_program := CheckExtension('GL_ARB_vertex_program');
   GL_ARB_vertex_shader := CheckExtension('GL_ARB_vertex_shader');
   GL_ARB_vertex_type_2_10_10_10_rev := CheckExtension('GL_ARB_vertex_type_2_10_10_10_rev');
   GL_ARB_viewport_array := CheckExtension('GL_ARB_viewport_array');
   GL_ARB_window_pos := CheckExtension('GL_ARB_window_pos');
   GL_ARB_texture_compression_bptc := CheckExtension('GL_ARB_texture_compression_bptc');

   // check Vendor/EXT OpenGL extensions
   GL_3DFX_multisample := CheckExtension('GL_3DFX_multisample');
   GL_3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
   GL_3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');
   GL_ATI_draw_buffers := CheckExtension('GL_ATI_draw_buffers');
   GL_ATI_texture_compression_3dc := CheckExtension('GL_ATI_texture_compression_3dc');
   GL_ATI_texture_float := CheckExtension('GL_ATI_texture_float');
   GL_ATI_texture_mirror_once := CheckExtension('GL_ATI_texture_mirror_once');

   GL_S3_s3tc := CheckExtension('GL_S3_s3tc');

   GL_EXT_abgr := CheckExtension('GL_EXT_abgr');
   GL_EXT_bgra := CheckExtension('GL_EXT_bgra');
   GL_EXT_bindable_uniform := CheckExtension('GL_EXT_bindable_uniform');   
   GL_EXT_blend_color := CheckExtension('GL_EXT_blend_color');
   GL_EXT_blend_equation_separate := CheckExtension('GL_EXT_blend_equation_separate');
   GL_EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
   GL_EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
   GL_EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
   GL_EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
   GL_EXT_Cg_shader := CheckExtension('GL_EXT_Cg_shader');
   GL_EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
   GL_EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
   GL_EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
   GL_EXT_depth_bounds_test := CheckExtension('GL_EXT_depth_bounds_test');
   GL_EXT_draw_buffers2 := CheckExtension('GL_EXT_draw_buffers2');
   GL_EXT_draw_instanced := CheckExtension('GL_EXT_draw_instanced');
   GL_EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
   GL_EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
   GL_EXT_framebuffer_blit := CheckExtension('GL_EXT_framebuffer_blit');
   GL_EXT_framebuffer_multisample := CheckExtension('GL_EXT_framebuffer_multisample');
   GL_EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
   GL_EXT_framebuffer_sRGB := CheckExtension('GL_EXT_framebuffer_sRGB');
   GL_EXT_geometry_shader4 := CheckExtension('GL_EXT_geometry_shader4');
   GL_EXT_gpu_program_parameters := CheckExtension('GL_EXT_gpu_program_parameters');
   GL_EXT_gpu_shader4 := CheckExtension('GL_EXT_gpu_shader4');
   GL_EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
   GL_EXT_multisample := CheckExtension('GL_EXT_multisample');
   GL_EXT_packed_depth_stencil := CheckExtension('GL_EXT_packed_depth_stencil');
   GL_EXT_packed_float := CheckExtension('GL_EXT_packed_float');
   GL_EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
   GL_EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
   GL_EXT_pixel_buffer_object := CheckExtension('GL_EXT_pixel_buffer_object');
   GL_EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
   GL_EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
   GL_EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
   GL_EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
   GL_EXT_shadow_funcs := CheckExtension('GL_EXT_shadow_funcs');
   GL_EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
   GL_EXT_stencil_clear_tag := CheckExtension('GL_EXT_stencil_clear_tag');
   GL_EXT_stencil_two_side := CheckExtension('EXT_stencil_two_side');
   GL_EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
   GL_EXT_texture3D := CheckExtension('GL_EXT_texture3D');
   GL_EXT_texture_array := CheckExtension('GL_EXT_texture_array');
   GL_EXT_texture_buffer_object := CheckExtension('GL_EXT_texture_buffer_object');
   GL_EXT_texture_compression_latc := CheckExtension('GL_EXT_texture_compression_latc');
   GL_EXT_texture_compression_rgtc := CheckExtension('GL_EXT_texture_compression_rgtc');
   GL_EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
   GL_EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
   GL_EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
   GL_EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
   GL_EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
   GL_EXT_texture_env_dot3 := CheckExtension('GL_EXT_texture_env_dot3');
   GL_EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic');
   GL_EXT_texture_integer := CheckExtension('GL_EXT_texture_integer');
   GL_EXT_texture_lod := CheckExtension('GL_EXT_texture_lod');
   GL_EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
   GL_EXT_texture_mirror_clamp := CheckExtension('GL_EXT_texture_mirror_clamp');
   GL_EXT_texture_object := CheckExtension('GL_EXT_texture_object');
   GL_EXT_texture_rectangle := CheckExtension('GL_EXT_texture_rectangle');
   GL_EXT_texture_sRGB := CheckExtension('GL_EXT_texture_sRGB');
   GL_EXT_texture_shared_exponent := CheckExtension('GL_EXT_texture_shared_exponent');
   GL_EXT_timer_query := CheckExtension('GL_EXT_timer_query');
   GL_EXT_transform_feedback := CheckExtension('GL_EXT_transform_feedback');
   GL_EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');

   GL_HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');

   GL_IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');

   GL_KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');

   GL_MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');

   GL_NV_blend_square := CheckExtension('GL_NV_blend_square');
   GL_NV_conditional_render := CheckExtension('GL_NV_conditional_render');
   GL_NV_copy_image := CheckExtension('GL_NV_copy_image');
   GL_NV_depth_buffer_float := CheckExtension('GL_NV_depth_buffer_float');
   GL_NV_fence := CheckExtension('GL_NV_fence');
   GL_NV_float_buffer := CheckExtension('GL_NV_float_buffer');
   GL_NV_fog_distance := CheckExtension('GL_NV_fog_distance');
   GL_NV_geometry_program4 := CheckExtension('GL_NV_geometry_program4');
   GL_NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
   GL_NV_multisample_filter_hint  := CheckExtension('GL_NV_multisample_filter_hint');
   GL_NV_occlusion_query := CheckExtension('GL_NV_occlusion_query');
   GL_NV_point_sprite := CheckExtension('GL_NV_point_sprite');
   GL_NV_primitive_restart := CheckExtension('GL_NV_primitive_restart');
   GL_NV_register_combiners := CheckExtension('GL_NV_register_combiners');
   GL_NV_shader_buffer_load := CheckExtension('GL_NV_shader_buffer_load');
   GL_NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
   GL_NV_texture_compression_vtc := CheckExtension('GL_NV_texture_compression_vtc');
   GL_NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
   GL_NV_texture_rectangle := CheckExtension('GL_NV_texture_rectangle');
   GL_NV_texture_shader := CheckExtension('GL_NV_texture_shader');
   GL_NV_texture_shader2 := CheckExtension('GL_NV_texture_shader2');
   GL_NV_texture_shader3 := CheckExtension('GL_NV_texture_shader3');
   GL_NV_transform_feedback := CheckExtension('GL_NV_transform_feedback');
   GL_NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
   GL_NV_vertex_array_range2 := CheckExtension('GL_NV_vertex_array_range2');
   GL_NV_vertex_buffer_unified_memory := CheckExtension('GL_NV_vertex_buffer_unified_memory');
   GL_NV_vertex_program := CheckExtension('GL_NV_vertex_program');

   GL_SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');

   GL_SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
   GL_SGIS_multisample := CheckExtension('GL_SGIS_multisample');
   GL_SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
   GL_SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
   GL_SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
   GL_SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');

   GL_SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
   GL_SGIX_shadow := CheckExtension('GL_SGIX_shadow'); 
   GL_SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');

   GL_AMD_vertex_shader_tessellator := CheckExtension('GL_AMD_vertex_shader_tessellator');

   GL_WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');

   GL_GREMEDY_frame_terminator := CheckExtension('GL_GREMEDY_frame_terminator');
   GL_GREMEDY_string_marker := CheckExtension('GL_GREMEDY_string_marker');

   // check supported GLU extensions
   Buffer := String(gluGetString(GLU_EXTENSIONS));
   GLU_EXT_nurbs_tessellator := CheckExtension('GLU_EXT_nurbs_tessellator');
   GLU_EXT_object_space_tess := CheckExtension('GLU_EXT_object_space_tess');
   GLU_EXT_TEXTURE := CheckExtension('GLU_EXT_TEXTURE');

   {$IFDEF SUPPORT_WGL}
   //check supported WGL extensions
   ReadWGLImplementationProperties;
   {$ENDIF}

   {$IFDEF SUPPORT_GLX}
   //check supported GLX extensions
   ReadGLXImplementationProperties;
   {$ENDIF}
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties
//
procedure ReadWGLImplementationProperties;
var
   Buffer: string;

   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;

begin
   // ARB wgl extensions
   if Assigned(wglGetExtensionsStringARB) then
      Buffer:=String(wglGetExtensionsStringARB(wglGetCurrentDC))
   else Buffer:='';
   WGL_ARB_buffer_region:=CheckExtension('WGL_ARB_buffer_region');
   WGL_ARB_create_context := CheckExtension('WGL_ARB_create_context');
   WGL_ARB_create_context_profile := CheckExtension('WGL_ARB_create_context_profile');
   WGL_ARB_extensions_string:=CheckExtension('WGL_ARB_extensions_string');
   WGL_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
   WGL_ARB_make_current_read:=CheckExtension('WGL_ARB_make_current_read');
   WGL_ARB_multisample:=CheckExtension('WGL_ARB_multisample');
   WGL_ARB_pbuffer:=CheckExtension('WGL_ARB_pbuffer');
   WGL_ARB_pixel_format:=CheckExtension('WGL_ARB_pixel_format');
   WGL_ARB_pixel_format_float:=CheckExtension('WGL_ARB_pixel_format_float');
   WGL_ARB_render_texture:=CheckExtension('WGL_ARB_render_texture');
   // Vendor/EXT wgl extensions
   WGL_ATI_pixel_format_float := CheckExtension('WGL_ATI_pixel_format_float');
   WGL_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
   WGL_EXT_pixel_format_packed_float := CheckExtension('WGL_EXT_pixel_format_packed_float');
   WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
   WGL_NV_gpu_affinity := CheckExtension('WGL_NV_gpu_affinity');
end;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties
//
procedure ReadGLXImplementationProperties;
var
   Buffer: string;
   MajorVersion, MinorVersion: Integer;
   Dpy: PDisplay;
   // Checks if the given Extension string is in Buffer.
   function CheckExtension(const Extension: string): Boolean;
   var
     ExtPos: Integer;
   begin
     // First find the position of the extension string as substring in Buffer.
     ExtPos := Pos(Extension, Buffer);
     Result := ExtPos > 0;
     // Now check that it isn't only a substring of another extension.
     if Result then
       Result := ((ExtPos + Length(Extension) - 1)= Length(Buffer))
                 or (Buffer[ExtPos + Length(Extension)]=' ');
   end;
begin
   Dpy:=glXGetCurrentDisplay();
   buffer:=String(glXQueryServerString(Dpy, XDefaultScreen(Dpy), GLX_VERSION));
   TrimAndSplitVersionString(buffer, majorversion, minorVersion);
   GLX_VERSION_1_1:=IsVersionMet(1,1,majorVersion,minorVersion);
   GLX_VERSION_1_2:=IsVersionMet(1,2,majorVersion,minorVersion);
   GLX_VERSION_1_3:=IsVersionMet(1,3,majorVersion,minorVersion);
   GLX_VERSION_1_4:=IsVersionMet(1,4,majorVersion,minorVersion);

   // This procedure will probably need changing, as totally untested
   // This might only work if GLX functions/procedures are loaded dynamically

   if Assigned(@glXQueryExtensionsString) then
     Buffer := glXQueryExtensionsString(Dpy, 0)  //guess at a valid screen
   else
     Buffer:='';
   GLX_ARB_create_context := CheckExtension('GLX_ARB_create_context');
   GLX_ARB_create_context_profile := CheckExtension('GLX_ARB_create_context_profile');
   GLX_ARB_framebuffer_sRGB := CheckExtension('GLX_ARB_framebuffer_sRGB');
   GLX_EXT_framebuffer_sRGB := CheckExtension('GLX_EXT_framebuffer_sRGB');
   GLX_EXT_fbconfig_packed_float := CheckExtension('GLX_EXT_fbconfig_packed_float');
   GLX_SGI_swap_control := CheckExtension('GLX_SGI_swap_control');
   GLX_ARB_multisample := CheckExtension('GLX_ARB_multisample');

   GLX_SGIS_multisample	 := CheckExtension('GLX_SGIS_multisample');
   GLX_EXT_visual_info	 := CheckExtension('GLX_EXT_visual_info');
   GLX_SGI_video_sync := CheckExtension('GLX_SGI_video_sync');
   GLX_SGI_make_current_read := CheckExtension('GLX_SGI_make_current_read');
   GLX_SGIX_video_source := CheckExtension('GLX_SGIX_video_source');
   GLX_EXT_visual_rating := CheckExtension('GLX_EXT_visual_rating');
   GLX_EXT_import_context := CheckExtension('GLX_EXT_import_context');
   GLX_SGIX_fbconfig := CheckExtension('GLX_SGIX_fbconfig');
   GLX_SGIX_pbuffer := CheckExtension('GLX_SGIX_pbuffer');
   GLX_SGI_cushion := CheckExtension('GLX_SGI_cushion');
   GLX_SGIX_video_resize := CheckExtension('GLX_SGIX_video_resize');
   GLX_SGIX_dmbuffer := CheckExtension('GLX_SGIX_dmbuffer');
   GLX_SGIX_swap_group := CheckExtension('GLX_SGIX_swap_group');
   GLX_SGIX_swap_barrier := CheckExtension('GLX_SGIX_swap_barrier');
   GLX_SGIS_blended_overlay := CheckExtension('GLX_SGIS_blended_overlay');
   GLX_SGIS_shared_multisample	 := CheckExtension('GLX_SGIS_shared_multisample');
   GLX_SUN_get_transparent_index := CheckExtension('GLX_SUN_get_transparent_index');
   GLX_3DFX_multisample	 := CheckExtension('GLX_3DFX_multisample');
   GLX_MESA_copy_sub_buffer := CheckExtension('GLX_MESA_copy_sub_buffer');
   GLX_MESA_pixmap_colormap := CheckExtension('GLX_MESA_pixmap_colormap');
   GLX_MESA_release_buffers := CheckExtension('GLX_MESA_release_buffers');
   GLX_MESA_set_3dfx_mode := CheckExtension('GLX_MESA_set_3dfx_mode');
   GLX_SGIX_visual_select_group	 := CheckExtension('GLX_SGIX_visual_select_group');
   GLX_SGIX_hyperpipe  := CheckExtension('GLX_SGIX_hyperpipe');
end;
{$ENDIF}

// CloseOpenGL
//
procedure CloseOpenGL;
begin
   if GLHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(Cardinal(GLHandle));
      GLHandle:=INVALID_MODULEHANDLE;
   end;

   if GLUHandle<>INVALID_MODULEHANDLE then begin
      FreeLibrary(Cardinal(GLUHandle));
      GLUHandle:=INVALID_MODULEHANDLE;
   end;
end;

// InitOpenGL
//
function InitOpenGL : Boolean;
begin
   if (GLHandle=INVALID_MODULEHANDLE) or (GLUHandle=INVALID_MODULEHANDLE) then
      Result:=InitOpenGLFromLibrary(opengl32, glu32)
   else Result:=True;
end;

// InitOpenGLFromLibrary
//
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
begin
   Result := False;
   CloseOpenGL;

   GLHandle:=LoadLibrary(PChar(GLName));
   GLUHandle:=LoadLibrary(PChar(GLUName));

   {$IFDEF Linux}   // make it work when mesa-dev is not installed and only libGL.so.1 is available
   if (GLHandle=INVALID_MODULEHANDLE) then
         GLHandle:=LoadLibrary(PChar(GLName+'.1'));
     if (GLUHandle=INVALID_MODULEHANDLE) then
         GLUHandle:=LoadLibrary(PChar(GLUName+'.1'));
   {$ENDIF}

   if (GLHandle<>INVALID_MODULEHANDLE) and (GLUHandle<>INVALID_MODULEHANDLE) then
     Result:=True
   else begin
      if GLHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(Cardinal(GLHandle));
      if GLUHandle<>INVALID_MODULEHANDLE then
         FreeLibrary(Cardinal(GLUHandle));
   end;
end;

// IsOpenGLInitialized
//
function IsOpenGLInitialized: Boolean;
begin
   Result:=(GLHandle<>INVALID_MODULEHANDLE);
end;

// compatibility routines

// UnloadOpenGL
//
procedure UnloadOpenGL;
begin
   CloseOpenGL;
end;

// LoadOpenGL
//
function LoadOpenGL: Boolean;
begin
   Result := InitOpenGL;
end;

// LoadOpenGLFromLibrary
//
function LoadOpenGLFromLibrary(GLName, GLUName: String): Boolean;
begin
   Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

// IsOpenGLLoaded
//
function IsOpenGLLoaded: Boolean;
begin
  Result := IsOpenGLInitialized();
end;

// IsMesaGL
//
function IsMesaGL : Boolean;
begin
  Result:=GLGetProcAddress('glResizeBuffersMESA')<>nil;
end;

// IsOpenGLVersionMet
//
function IsOpenGLVersionMet(MajorVersion, MinorVersion: Integer): boolean;
var
  Buffer : String;
  GLMajorVersion, GLMinorVersion: Integer;
begin
  buffer:=String(glGetString(GL_VERSION));
  TrimAndSplitVersionString(buffer, GLMajorVersion, GLMinorVersion);
  Result:=IsVersionMet(MajorVersion,MinorVersion,GLMajorVersion,GLMinorVersion);
end;

initialization

   Set8087CW($133F);

finalization

   CloseOpenGL;

end.

