
  module examples_mod
    implicit none
  contains

  subroutine example01
    use raylib_mod
    implicit none

    integer(c_int) :: screenWidth=800, screenHeight=450
    integer(c_int) :: gap = 20
    type(color_t) :: background_color, rectangle_color
    logical :: should_exit

    call raylib_set_config_flags(uint32_t(int(z'00000004',int64))) ! FLAG_WINDOW_RESIZABLE
    call raylib_set_config_flags(uint32_t(int(z'00000020',int64))) ! FLAG_MSAA_4X_HINT
    call raylib_init_window(screenWidth, screenHeight, "raylib example"//C_NULL_CHAR)
    call raylib_set_target_fps(60_c_int)

    background_color = color_t([245,245,245,255])
    should_exit = .false.

    do
      if (raylib_window_should_close() .or. should_exit) exit
      screenWidth = raylib_get_render_width()
      screenHeight = raylib_get_render_height()
      gap = min(screenWidth,screenHeight) / 4
      call raylib_begin_drawing()

      call raylib_clear_background(background_color)
      if (raylib_get_mouse_x() >= gap .and. raylib_get_mouse_y() >= gap .and. &
          raylib_get_mouse_x() <= screenWidth-gap .and. raylib_get_mouse_y() <= screenHeight-gap) then
        rectangle_color = color_t([255,100,194,125])
        if (raylib_is_mouse_button_pressed(1)) should_exit = .true.
      else
        rectangle_color = color_t([255,100,194,255])
      end if
      call raylib_draw_rectangle(gap,gap,screenWidth-2*gap, screenHeight-2*gap,rectangle_color)
      call raylib_draw_text('Press right mouse button to close!'//C_NULL_CHAR, gap, screenHeight/2, 20, color_t([200,200,200,255]))

      call raylib_end_drawing()

    end do

    call raylib_close_window()
  end subroutine

  end module examples_mod
