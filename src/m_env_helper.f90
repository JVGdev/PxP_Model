module m_env_helper
  	use m_entity
	use m_animal
	
	implicit none
    private

    public get_entity, genetic_select, kill
    
    contains

		function get_entity(list_indexes, list_entities, x, y, max) result (index)
        	integer, dimension(:), intent(in) :: list_indexes
        	class(entity), dimension(:), intent(in) :: list_entities
			integer, intent(in) :: x, y, max
			integer :: i
			integer, dimension(2) :: index 
			index = 0
			
          	do i = max, 1, -1
				index(1) = list_indexes(i)
          		if(list_entities(index(1))%get_x() == x .and. list_entities(index(1))%get_y() == y) then
                  	index(2) = index(1)
					index(1) = i
					exit
              	end if
          	end do        
		end function get_entity
	
		subroutine kill(list_i, list_top, index)
			integer, allocatable, intent(inout) :: list_i(:)
			integer, intent(inout) :: list_top
			integer, intent(in) :: index
          		
          	list_i(index) = list_i(list_top)
          	list_top = list_top - 1  

		end subroutine kill
		
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

		function genetic_select(list_i, list_o, size) result (offspring)
			real, dimension(4) :: offspring
      		class(animal), dimension(:), intent(in) :: list_o
      		integer, dimension(:), intent(in) :: list_i
			integer, intent(in) :: size
      		real :: m, c
    		real, dimension(4) :: new_vals, old_vals_p1, old_vals_p2
    		integer :: gene
			offspring = 0.0    	

      		old_vals_p1 = list_o(list_i(1))%get_brain_vals()
      		old_vals_p2 = list_o(list_i(2))%get_brain_vals()
			call recombine(offspring, [ old_vals_p1, old_vals_p2 ], 4)	    	

			! Selecionando Gene Random
      		call random_number(m)
      		gene = floor(m*3) + 1
    		
      		call random_number(m)
			call random_number(c)
      		offspring(gene) = offspring(gene) + (((m * 100.0)/10) * merge(-1, 1, c > 0.5))
	
		end function genetic_select  

end module m_env_helper
