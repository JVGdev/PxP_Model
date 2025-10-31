module m_env_helper
  	use m_entity
	use m_animal
	
	implicit none
    private

    public get_entity, genetic_select
    
    contains

		function get_entity(list, x, y, max) result (index)
        	class(entity), dimension(:), intent(in) :: list
			integer, intent(in) :: x, y, max
			integer :: index, i
			index = 0
			
          	do i = max, 1, -1
          		if(list(i)%get_x() == x .and. list(i)%get_y() == y) then
                  	index = i
					exit
              	end if
          	end do        
		end function get_entity
		
		subroutine recombine(vessel, parents, size)
    		real, dimension(:), intent(inout) :: vessel
    		real, dimension(:), intent(in) :: parents
			integer, intent(in) :: size
			integer :: i, p
			real :: r

			do i=1, size-1
				call random_number(r)
				p = floor(r*2) + 1
				vessel(i) = parents(i+((p-1)*size))
			end do

		end subroutine recombine

		function genetic_select(list, size) result (offspring)
			real, dimension(4) :: offspring
      		class(animal), dimension(:), intent(in) :: list
			integer, intent(in) :: size
      		real :: m, c
    		real, dimension(4) :: new_vals, old_vals_p1, old_vals_p2
        	class(animal), allocatable ::  parent1, parent2
    		integer :: gene
			offspring = 0.0    	

			parent1 = list(1)
    		parent2 = list(2)
      		old_vals_p1 = parent1%get_brain_vals()
      		old_vals_p2 = parent2%get_brain_vals()
			call recombine(offspring, [ old_vals_p1, old_vals_p2 ], 4)	    	

			! Selecionando Gene Random
      		call random_number(m)
      		gene = floor(m*3) + 1
    		
      		call random_number(m)
			call random_number(c)
      		offspring(gene) = offspring(gene) + (((m * 100.0)/10) * merge(-1, 1, c > 0.5))
	
		end function genetic_select  

end module m_env_helper
