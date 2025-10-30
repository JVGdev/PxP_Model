module m_brain

        implicit none
        private

        public :: brain

        type :: brain 
                private
                        character :: host
                        real :: prey_val, predator_val, plant_val, stop_val
                        integer :: last_x, last_y
                contains
                        procedure :: next_step, set_host, get_host, calc_entities, get_plant_val, get_predator_val, get_prey_val, get_stop_val
        end type brain
        
        interface brain
                module procedure req_args
        end interface brain


        contains
                type(brain) function req_args(host, prey_val, predator_val, plant_val, stop_val)
                        character, intent(in) :: host 
                        real, intent(in) :: prey_val, predator_val, plant_val, stop_val 
                        call req_args%set_host(host)
                        req_args%prey_val = prey_val
                        req_args%predator_val = predator_val
                        req_args%plant_val = plant_val
                        req_args%stop_val = stop_val
                        req_args%last_x = 1
                        req_args%last_y = 1
                end function req_args
                
                function get_host(self) result(res)
                        class(brain), intent(in) :: self
                        character :: res
                        res = self%host
                end function get_host

                subroutine set_host(self, host)
                        class(brain), intent(inout) :: self
                        character, intent(in) :: host
                        self%host = host
                end subroutine set_host
                
                function get_predator_val(self) result(res)
                        class(brain), intent(in) :: self
                        real :: res
                        res = self%predator_val
                end function get_predator_val
                
                function get_prey_val(self) result(res)
                        class(brain), intent(in) :: self
                        real :: res
                        res = self%prey_val
                end function get_prey_val
                
                function get_plant_val(self) result(res)
                        class(brain), intent(in) :: self
                        real :: res
                        res = self%plant_val
                end function get_plant_val
                
                function get_stop_val(self) result(res)
                        class(brain), intent(in) :: self
                        real :: res
                        res = self%stop_val
                end function get_stop_val
                
	subroutine next_step(self, view, n, m, x,y, nxt) 
  		class(brain), intent(inout) :: self
        character, dimension(:,:), intent(in) :: view
      	integer, dimension(:), intent(inout) :: nxt
    	integer, intent(in) :: x, y, n, m
      	real, dimension(n,m) :: preview 
    	integer :: bi, bj, ai, aj, i, j
    	preview = 0.0

    	ai = x
      	aj = y
                        
      	preview = self%calc_entities(view, n, m, ai, aj)
    	preview(self%last_x, self%last_y) =  0.0
    	preview(x, y) = 0.0
    	self%last_x = x
      	self%last_y = y

      	bi = merge(x+1, x, x < n)
      	ai = merge(x-1, x, x > 1)
    	bj = merge(y+1, y, y < m)      ! RAW e WAR
    	aj = merge(y-1, y, y > 1)
                        
    	nxt = maxloc(preview(ai:bi, aj:bj))
      	nxt(1) = nxt(1) + (ai-1) 
        nxt(2) = nxt(2) + (aj-1)
                                
		!do i=1, n
        !  	do j=1, m
		!	  	write (*, "(F10.3)", advance="no") preview(i, j)
  		!	end do
        !   print *
    	!end do		
                        
      	!read *, bi
  	end subroutine next_step

    function calc_entities(self, view, n, m, ai, aj)
        class(brain), intent(in) :: self
        character, dimension(:,:), intent(in) :: view
        integer, intent(in) :: n, m, ai, aj
        real, dimension(n,m) :: calc_entities
    	integer :: i, j, valx, valy, bi, bj
        character :: act
    	calc_entities = 0.0
                        
        do i=1, n
  	    	do j=1, m
            	act = view(i, j)
                if(ai == i .and. aj == j) then
                else if(act == 'x') then
                	call wave(calc_entities, i, j, n, m, self%get_plant_val()) 
              	else if(act == 'K') then
          			call wave(calc_entities, i, j, n, m, self%get_predator_val()) 
              	else if(act == 'P') then
            		call wave(calc_entities, i, j, n, m, self%get_prey_val()) 
          		end if
        	end do
        end do
	  	
    end function
                
	recursive subroutine wave(matrix, x, y, n, m, weight)
    	integer, intent(in) :: x, y, n, m
        real, intent(in) :: weight
        real, dimension(:,:) :: matrix
  		real, allocatable :: wlist(:)
        integer :: times, max_cord, i, j, i_window_m, i_window_p, j_window_m, j_window_p
        real :: nweight
        
		max_cord = merge(n, m, n > m)

		allocate(wlist(max_cord))
        wlist = 0.0      

	  	times = 1
        nweight = weight
        do while(nweight >= 1 .and. times <= max_cord)
          	wlist(times) = nweight
			nweight = nweight / 2
			times = times + 1
        end do
		
		do i=0, times
        	do j=0, times
                i_window_m = merge(x-i, 1, x-i > 1)
                i_window_p = merge(x+i, n, x+i < n)
                j_window_m = merge(y-j, 1, y-j > 1)
                j_window_p = merge(y+j, m, y+j < m)
				matrix(i_window_m, j_window_m) = matrix(i_window_m, j_window_m) + wlist((x+y) - (i+j))
				matrix(i_window_p, j_window_p) = matrix(i_window_p, j_window_p) + wlist((x+y) - (i+j))
  			end do
        end do		
      	deallocate(wlist)

  	end subroutine wave
                
end module m_brain
