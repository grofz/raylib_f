
  module examples_mod
    implicit none
  contains

  subroutine example01
    use raylib_mod
    implicit none

    integer(c_int), parameter :: screenWidth=800, screenHeight=450
    type(color_t) :: background_color


    call raylib_init_window(screenWidth, screenHeight, "raylib example"//C_NULL_CHAR)
    call raylib_set_target_fps(60_c_int)

    background_color = color_t([245,245,245,255])
print *, 'background_color =', background_color

    do
      if (raylib_window_should_close()) exit
      call raylib_begin_drawing()

      call raylib_clear_background(background_color)
      call raylib_draw_text('Congrats! Your first window!'//C_NULL_CHAR, 190, 200, 20, color_t([200,200,200,255]))

      call raylib_end_drawing()
    end do

    call raylib_close_window()
  end subroutine

  end module examples_mod
