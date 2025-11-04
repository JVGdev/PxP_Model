module m_plant
        use m_entity
        implicit none
        private

        public :: plant

        type, extends(entity), public :: plant
                private
                        real :: energy, spread_chance, food
                contains 
                        procedure :: get_energy
                        procedure :: set_energy
                        procedure :: sub_energy
                        procedure :: add_energy
                        procedure :: get_food
                        procedure :: set_food
                        procedure :: get_spread_chance
                        procedure :: set_spread_chance
                        procedure :: spread_cords
        end type plant

        interface plant
                module procedure no_args
                module procedure req_args
        end interface plant

        contains
                type(plant) function no_args()
                        call no_args%set_x(1)
                        call no_args%set_y(1)
                        call no_args%set_matter(100.0)
                        call no_args%set_energy(100.0)
                        call no_args%set_food(4.0)
                        call no_args%set_spread_chance(0.10)
                end function no_args
                
                type(plant) function req_args(x, y) 
                        integer, intent(in) :: x, y
                        req_args = no_args()
                        call req_args%set_x(x)
                        call req_args%set_y(y)
                end function req_args

                real function get_energy(self)
                        class(plant), intent(in) :: self
                        get_energy = self%energy
                end function get_energy
                subroutine set_energy(self, energy)
                        class(plant), intent(out) :: self
                        real :: energy
                        self%energy = energy
                end subroutine set_energy     
                subroutine sub_energy(self, cost)        
                        class(plant), intent(out) :: self      
                        real, intent(in) :: cost
                        self%energy = (self%energy - cost) 
                end subroutine
                subroutine add_energy(self, profit)        
                        class(plant), intent(out) :: self      
                        real, intent(in) :: profit
                        self%energy = (self%energy + profit) 
                end subroutine

                real function get_food(self)
                        class(plant), intent(in) :: self
                        get_food = self%food
                end function get_food
                subroutine set_food(self,food)
                        class(plant), intent(out) :: self
                        real :: food
                        self%food = food
                end subroutine set_food

                real function get_spread_chance(self)
                        class(plant), intent(in) :: self
                        get_spread_chance = self%spread_chance
                end function get_spread_chance
                subroutine set_spread_chance(self, spread_chance)
                        class(plant), intent(out) :: self
                        real :: spread_chance
                        self%spread_chance = spread_chance
                end subroutine set_spread_chance 

				function spread_cords(self, view, vn, empty) result (cords)
					class(plant), intent(inout) :: self
					character, dimension(:, :), intent(in)  :: view
					integer, intent(in) :: vn
  					character, intent(in) :: empty
					integer, dimension(2) :: cords
					integer :: x, y
      				real :: r
                    
					x = self%get_x()
					y = self%get_y()
    				
              		call random_number(r)
              		if(r > self%get_spread_chance()) then
              			call random_number(r)
                  		select case(floor(4*r) + 1)
                    		case(1)
                        		if(x + 1 < vn .and. view(x+1, y) == empty) then
                            		x = x + 1
								else
									x = 0
                          		end if       
                    		case(2)
                          		if (x - 1 > 1 .and. view(x-1, y) == empty) then
                            		x = x - 1
								else
									x = 0
                          		end if       
                      		case(3) 
                        		if(y + 1 < vn .and. view(x, y+1) == empty) then
                          			y = y + 1
                          		else 
                            		y = 0
                          		end if       
                  			case(4)
                          		if(y - 1 > 1 .and. view(x, y-1) == empty) then
                            		y = y - 1
                          		else 
                            		y = 0
                          		end if    
                  		end select   
            		else
						cords = 0
						return
					end if
					cords(1) = x
					cords(2) = y
				end function spread_cords
end module m_plant

