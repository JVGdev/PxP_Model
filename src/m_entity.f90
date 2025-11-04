module m_entity
        implicit none
        private 

        type, abstract, public :: entity
                private
                        integer :: x__, y__
						real :: matter
                contains
                        procedure :: get_x, set_x, get_y, set_y, get_matter, sub_matter, add_matter, set_matter
        end type

        contains
                integer function get_x(self) result(x)
                        class(entity), intent(in) :: self
                        x = self%x__
                end function get_x

                subroutine set_x(self, x)
                        class(entity), intent(out) :: self
                        integer, intent(in) :: x
                        self%x__ = merge(x, 1, x > 1)
                end subroutine set_x

                integer function get_y(self) result(y)
                        class(entity), intent(in) :: self
                        y = self%y__
                end function get_y

                subroutine set_y(self, y)
                        class(entity), intent(out) :: self
                        integer, intent(in) :: y
                        self%y__ = merge(y, 1, y > 1)
                end subroutine set_y

                real function get_matter(self) result(matter)
                        class(entity), intent(in) :: self
                        matter = self%matter
                end function get_matter
                
				subroutine set_matter(self, matter)
                        class(entity), intent(out) :: self
                        real, intent(in) :: matter
                        self%matter = matter
                end subroutine set_matter

                subroutine add_matter(self, matter)
                        class(entity), intent(out) :: self
                        real, intent(in) :: matter
                        self%matter = self%matter + matter
                end subroutine add_matter

                subroutine sub_matter(self, matter)
                        class(entity), intent(out) :: self
                        real, intent(in) :: matter
                        self%matter = self%matter - matter
                end subroutine sub_matter
end module m_entity
