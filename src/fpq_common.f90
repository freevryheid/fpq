module fpq_common
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: copy, c_strlen, c_f_str_ptr

  interface

    function c_strlen(str) bind(c, name='strlen')
      import :: c_ptr, c_size_t
      implicit none
      type(c_ptr), intent(in), value :: str
      integer(c_size_t) :: c_strlen
    end function c_strlen

  end interface

  contains

    !! # Common procedures used by fpq modules.

    pure function copy(a)
      character, intent(in)  :: a(:)
      character(len=size(a)) :: copy
      integer(kind=8) :: i
      do i = 1, size(a)
        copy(i:i) = a(i)
      end do
    end function copy

    subroutine c_f_str_ptr(c_str, f_str)
      !! ## Extract fortran string from a c pointer.
      type(c_ptr), intent(in) :: c_str
      character(len=:), allocatable, intent(out) :: f_str
      character(kind=c_char), pointer :: ptrs(:)
      integer(kind=8) :: sz
      if (.not. c_associated(c_str)) return
      sz = c_strlen(c_str)
      if (sz < 0) return
      call c_f_pointer(c_str, ptrs, [ sz ])
      allocate (character(len=sz) :: f_str)
      f_str = copy(ptrs)
    end subroutine c_f_str_ptr

    ! function cstr(str) result(r)
    !   !! Returns NULL terminated string.
    !   character(len=*), intent(in) :: str
    !     !! Fortran string.
    !   character(len=:), allocatable :: r
    !     !! NULL terminated string.
    !   r = str // c_null_char
    ! end function cstr

end module fpq_common

