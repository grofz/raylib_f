!
! Raylib interface for Fortran
!
  module raylib_mod
    use iso_c_binding
    use iso_fortran_env, only : int8, int64, int32
    implicit none

    ! color type is 32 bit integer (rrggbbaa)
    type, bind(c) :: color_t
      integer(int8) :: r, g, b, a
    end type
    interface color_t
      module procedure color_from_array
    end interface
    private color_from_array

    ! "unsigned int" type
    type, bind(c) :: uint32_t
      integer(c_int32_t) :: uint
    end type
    interface uint32_t
      module procedure uint32_from_int64
    end interface
    private uint32_from_int64

    ! TODO: how define the colors and flags?
    ! unfortunately, this is not allowed
    !type(color_t) :: &
    !  LIGHTGRAY = color_from_array([200, 200, 200, 255])


    ! Raylib interfaces binding
    ! using https://www.raylib.com/cheatsheet/cheatsheet.html

    interface

    ! =============
    ! Module: rcore
    ! =============
    ! Window-related functions

      subroutine raylib_init_window(width, height, title) bind(c,name="InitWindow")
        import c_int, c_char, C_NULL_CHAR
        implicit none
        integer(c_int), intent(in), value :: width, height
        character(c_char), intent(in) :: title(*)
      end subroutine

      logical(c_bool) function raylib_window_should_close() bind(c,name="WindowShouldClose")
        import c_bool
        implicit none
      end function

      subroutine raylib_close_window() bind(c,name="CloseWindow")
      end subroutine

      integer(c_int) function raylib_get_render_width() bind(c,name="GetRenderWidth")
        import c_int
        implicit none
      end function

      integer(c_int) function raylib_get_render_height() bind(c,name="GetRenderHeight")
        import c_int
        implicit none
      end function

    ! Custom frame control functions

    ! Cursor related functions

    ! Drawing-related functions

      subroutine raylib_clear_background(color) bind(c,name="ClearBackground")
        import color_t
        implicit none
        type(color_t), intent(in), value :: color
      end subroutine

      subroutine raylib_begin_drawing() bind(c,name="BeginDrawing")
      end subroutine

      subroutine raylib_end_drawing() bind(c,name="EndDrawing")
      end subroutine

    ! VR stereo config functions for VR simulator

    ! Shader management functions

    ! Screen-space-related functions

    ! Timing-related functions

      subroutine raylib_set_target_fps(fps) bind(c,name="SetTargetFPS")
        import c_int
        implicit none
        integer(c_int), intent(in), value :: fps
      end subroutine

      real(c_float) function raylib_get_frame_time() bind(c,name="GetFrameTime")
        import c_float
        implicit none
      end function

    ! Misc. functions

      subroutine raylib_set_config_flags(flags) bind(c,name="SetConfigFlags")
        import uint32_t
        type(uint32_t), intent(in), value :: flags
      end subroutine

    ! Set cutom callbacks

    ! Files management functions

    ! Compression encoding functionality

    ! Input-related functions: keyboard

    ! Input-related functions: gamepads

    ! Input-related functions: mouse

    logical(c_bool) function raylib_is_mouse_button_pressed(button) bind(c,name="IsMouseButtonPressed")
      import c_bool, c_int
      implicit none
      integer(c_int), intent(in), value :: button
    end function

    integer(c_int) function raylib_get_mouse_x() bind(c,name="GetMouseX")
      import c_int
      implicit none
    end function

    integer(c_int) function raylib_get_mouse_y() bind(c,name="GetMouseY")
      import c_int
      implicit none
    end function

    ! Input-related functions: touch

    ! Gestures and touch handling functions

    ! Camera system functions

    ! ===============
    ! Module: rshapes
    ! ===============

    ! Basic shapes drawing functions

    subroutine raylib_draw_rectangle(posx, posy, width, height, color) bind(c,name="DrawRectangle")
      import c_int, color_t
      implicit none
      integer(c_int), intent(in), value :: posx, posy, width, height
      type(color_t), intent(in), value :: color
    end subroutine

    ! Basic shapes collision detection functions

    ! =================
    ! Module: rtextures
    ! =================

    ! =============
    ! Module: rtext
    ! =============

    ! Font loading functions

    ! Text drawing functions

      subroutine raylib_draw_text(text, posx, posy, fontsize, color) bind(c,name="DrawText")
        import c_int, color_t, c_char
        implicit none
        integer(c_int), intent(in), value :: posx, posy, fontsize
        type(color_t), intent(in), value :: color
        character(c_char), intent(in) :: text(*)
      end subroutine

    ! Text font info functions

    ! Text codepoints management functions

    ! Text strings management functions

    ! ===============
    ! Module: rmodels
    ! ===============

    ! ==============
    ! Module: raudio
    ! ==============

    end interface

  contains

    pure elemental integer(int8) function uintconvert(s) result(u)
      integer, intent(in) :: s
!
! Convert unsigned integer (0-255) to 8-bit integer
!
      integer :: bs, pos, s0

      bs = bit_size(u)
      if (s >= 2**bs) error stop 'uintconvert - input overflowing'
      if (s < 0) error stop 'uintconvert - input is negative'
      s0 = s
      u = int(z'00', kind=int8)

      do pos = bs-1, 0, -1
        if (s0 < 2**pos) cycle
        u = ibset(u, pos)
        s0 = s0 - 2**pos
      end do
    end function


    pure elemental integer(c_int32_t) function uintconvert32(s) result(u)
      integer(int64), intent(in) :: s
!
! Convert unsigned integer (0 ... 4 294 967 295) to a 32-bit integer
!
      integer :: bs, pos
      integer(int64) :: s0

      bs = bit_size(u)
      if (s >= 2_int64**bs) error stop 'uintconvert32 - input overflowing'
      if (s < 0_int64) error stop 'uintconvert32 - input is negative'
      s0 = s

      do pos = bs-1, 0, -1
        if (s0 < 2_int64**pos) then
          u = ibclr(u, pos)
        else
          u = ibset(u, pos)
          s0 = s0 - 2_int64**pos
        end if
      end do
    end function


    pure type(color_t) function color_from_array(arr) result(new)
      integer, intent(in) :: arr(4)
!
! color_t constructor
!
      new%r = uintconvert(arr(1))
      new%g = uintconvert(arr(2))
      new%b = uintconvert(arr(3))
      new%a = uintconvert(arr(4))
    end function


    pure type(uint32_t) function uint32_from_int64(u) result(new)
      integer(int64), intent(in) :: u
      new%uint = uintconvert32(u)
    end function

  end module raylib_mod
