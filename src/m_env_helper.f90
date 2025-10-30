module m_env_helper
  	use m_entity
	
	implicit none
    private

    public get_entity
    
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

end module m_env_helper
