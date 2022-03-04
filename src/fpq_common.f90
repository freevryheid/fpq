module fpq_common

  use, intrinsic :: iso_c_binding
  implicit none
  private

  type pq
    type(c_ptr) :: ptr
  end type pq

  public :: pq
  public :: c_str
  public :: c_f_str_ptr

  interface

    function c_strlen(str) bind(c, name='strlen')
      import :: c_ptr, c_size_t
      implicit none
      type(c_ptr), intent(in), value :: str
      integer(c_size_t) :: c_strlen
    end function c_strlen

  end interface

  contains

    !! Common procedures used by fpq modules.

    pure function copy(a)
      character, intent(in)  :: a(:)
      character(len=size(a)) :: copy
      integer(kind=8) :: i
      do i = 1, size(a)
        copy(i:i) = a(i)
      end do
    end function copy

    subroutine c_f_str_ptr(cstr, fstr)
      !! Extract fortran string from a c pointer.
      type(c_ptr), intent(in) :: cstr
      character(len=:), allocatable, intent(out) :: fstr
      character(kind=c_char), pointer :: ptrs(:)
      integer(kind=8) :: sz
      if (.not. c_associated(cstr)) return
      sz = c_strlen(cstr)
      if (sz < 0) return
      call c_f_pointer(cstr, ptrs, [ sz ])
      allocate (character(len=sz) :: fstr)
      fstr = copy(ptrs)
    end subroutine c_f_str_ptr

    function c_str(fstr) result(cstr)
      !! Converts fortran string to null terminated c string.
      character(len=*), intent(in) :: fstr
        !! Fortran string.
      character(kind=c_char, len=:), allocatable :: cstr
        !! NULL terminated string.
      cstr = trim(fstr) // c_null_char
    end function c_str

end module fpq_common

