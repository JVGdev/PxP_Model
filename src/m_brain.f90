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
                        bj = merge(y+1, y, y < n)      ! RAW e WAR
                        aj = merge(y-1, y, y > 1)
                        
                        nxt = maxloc(preview(ai:bi, aj:bj))
                        nxt(1) = nxt(1) + (ai-1) 
                        nxt(2) = nxt(2) + (aj-1)
                                
                end subroutine next_step

                function calc_entities(self, view, n, m, ai, aj)
                        class(brain), intent(in) :: self
                        character, dimension(:,:), intent(in) :: view
                        integer, intent(in) :: n, m, ai, aj
                        real, dimension(n,n) :: calc_entities
                        integer :: i, j, valx, valy, bi, bj
                        character :: act
                        
                        do i=1, n
                                do j=1, m
                                        act = view(i, j)
                                        if(ai == i .and. aj == j) then
                                        else if(act == 'x') then
                                               call wave(calc_entities, i, j, n, m, 'a', self%get_plant_val()) 
                                        else if(act == 'K') then
                                               call wave(calc_entities, i, j, n, m, 'a', self%get_predator_val()) 
                                        else if(act == 'P') then
                                               call wave(calc_entities, i, j, n, m, 'a', self%get_prey_val()) 
                                        end if

                                end do
                        end do
                        
                end function
                
                recursive subroutine wave(matrix, x, y, n, m, constraint, weight)
                        integer, intent(in) :: x, y, n, m
                        character, intent(in) :: constraint
                        real, intent(in) :: weight
                        real, dimension(:,:) :: matrix
                        real :: nweight
                        nweight = weight / 4

                        !O que ela faz de fato
                        matrix(x, y) = matrix(x, y) + weight
                        
                        ! Saída
                        
                        if(nweight <= 1) then
                                return
                        end if


                        ! Recursão
                        if(constraint /= 'r' .and. x <  n) then
                                call wave(matrix, x+1, y, n, m, 'l', nweight)
                        end if
                        if(constraint /= 'l' .and. x > 1) then
                                call wave(matrix, x-1, y, n, m, 'r', nweight)
                        end if
                        if(constraint /= 'u' .and. y < m) then
                                call wave(matrix, x, y+1, n, m, 'd', nweight)
                        end if
                        if(constraint /= 'd' .and. y > 1) then
                                call wave(matrix, x, y-1, n, m, 'u', nweight)
                        end if
                end subroutine
                
end module m_brain
