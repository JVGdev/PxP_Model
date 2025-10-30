module m_predator
        use m_animal
        use m_brain

        implicit none
        private
                character, parameter :: art = 'K' 

        type, extends(animal), public :: predator
                private
                        real :: attack
                contains
                        procedure :: get_attack, set_attack

        end type

        interface predator
                module procedure no_args
                module procedure req_args
                module procedure all_args
        end interface predator

        contains
                type(predator) function no_args()
                        call no_args%set_x(1)
                        call no_args%set_y(1)
                        call no_args%set_energy(100.0)
                        call no_args%set_vision(4)
                        call no_args%set_movement(1)
                        call no_args%set_health(100)
                        call no_args%set_attack(2.0)
                        call no_args%set_passive_cost(15.0)
                        
                        call no_args%set_brain(brain(art, 0.0, 0.0, 0.0, 0.0))
                end function no_args
               
                type(predator) function req_args(x, y)
                        integer, intent(in) :: x, y
                        req_args = no_args()
                        call req_args%set_x(x)
                        call req_args%set_y(y)
                end function req_args

                type(predator) function all_args(x, y, predator_val, prey_val, plant_val, stop_val)
                        integer, intent(in) :: x, y
                        real, intent(in) :: predator_val, prey_val, plant_val, stop_val
                        call all_args%set_x(x)
                        call all_args%set_y(y)
                        call all_args%set_energy(100.0)
                        call all_args%set_vision(4)
                        call all_args%set_movement(1)
                        call all_args%set_health(100)
                        call all_args%set_attack(2.0)
                        call all_args%set_passive_cost(5.0)
                        
                        call all_args%set_brain(brain(art, predator_val, prey_val, plant_val, stop_val))
                end function all_args
        
                real function get_attack(self) result(res)
                        class(predator), intent(in) :: self
                        res = self%attack
                end function
                
                subroutine set_attack(self, attack)
                        class(predator), intent(out) :: self
                        real, intent(in) :: attack
                        self%attack = merge(attack, 0.0, attack > 0)
                end subroutine

                subroutine move(self, step)
                        class(predator), intent(inout) :: self
                        integer, dimension(2), intent(in) :: step
                        call self%set_x(step(1))
                        call self%set_y(step(2))
                        call self%sub_energy(self%get_passive_cost())
                end subroutine move
end module m_predator
