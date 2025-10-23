module m_pseudo_vector
        use m_plant
        use m_prey
        use m_predator

        implicit none

        interface add_element
                module procedure add_plant__, add_prey__, add_predator__
        end interface add_element
        
        contains
                subroutine add_plant__(list, element)
                        type(plant), dimension(:), allocatable, intent(inout) :: list
                        type(plant), dimension(:), allocatable :: temp
                        type(plant), intent(in) :: element
                        
                        include "add_element.inc"
                end subroutine add_plant__

                subroutine add_prey__(list, element)
                        type(prey), dimension(:), allocatable, intent(inout) :: list
                        type(prey), dimension(:), allocatable :: temp
                        type(prey), intent(in) :: element
                        
                        include "add_element.inc"
                end subroutine add_prey__

                subroutine add_predator__(list, element)
                        type(predator), dimension(:), allocatable, intent(inout) :: list
                        type(predator), dimension(:), allocatable :: temp
                        type(predator), intent(in) :: element
                        
                        include "add_element.inc"
                end subroutine add_predator__

end module m_pseudo_vector
