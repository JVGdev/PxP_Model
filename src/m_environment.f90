module m_environment
	use m_plant
    use m_prey
    use m_predator
	use m_env_helper
    
	implicit none
    private
    	character, parameter :: empty = '.', predatorc = 'K', preyc = 'P', plantc = 'x'
      	integer, parameter :: max_animals = 15, dis = 20
                 
    type, public :: environment
    	character, dimension(:,:), allocatable :: plains 
    	type(plant), allocatable :: plants(:)
        type(prey), allocatable :: preys(:)
        type(predator), allocatable :: predators(:)
        integer :: preys_alive = 0, predators_alive = 0, plants_alive = 0
        
        contains
        	procedure :: populate, extinction, pass_round, print, repopulate, print_sim_vals
  	end type environment
        
        
  	interface environment
    	module procedure no_args
  	end interface environment

	contains
    	type(environment) function no_args()
      	allocate(no_args%plains(dis,dis))
        no_args%plains(:,:) = empty
        allocate(no_args%preys(max_animals+2))
        allocate(no_args%predators(max_animals+2))
        allocate(no_args%plants(max_animals*4))
	end function no_args
                
  	subroutine populate(self)
    	class(environment), intent(inout) :: self
      	type(prey) :: prey1
      	type(predator) :: predator1
      	type(plant) :: plant1
      	integer :: x, y, i, offset
      	logical :: e

      	do i=1,max_animals
                                
        	prey1 = prey(1, 1)
          	e = .true.
          	do while(e)
            	e = add_prey(self, prey1)
          	end do
                        
          	predator1 = predator(dis, dis)
            e = .true.
        	do while(e)        
            	e = add_predator(self, predator1)
          	end do
                                
    	end do
        
		do i = 1, max_animals/2
      		plant1 = plant(dis/2, dis/2)
        	e = .true.
          	do while(e)        
            	e = add_plant(self, plant1)
        	end do
      	end do
	end subroutine populate

	!!! FAZER METEORO FUTURAMENTE
  	subroutine extinction(self)
  		class(environment), intent(inout) :: self
      	self%plains = empty
                        
      	self%preys_alive = 0
    	self%predators_alive = 0
    	self%plants_alive = 0
  	end subroutine extinction
                
  	logical function pass_round(self) result(res)
    	class(environment), intent(inout) :: self
      	logical :: e
      	type(plant) :: plant1
      	integer, dimension(2) :: cords
      	integer :: i, j, index, lsize, ex, ey
      	real :: r
      	character :: nchar
      	res = .false.

    	pred: do i=self%predators_alive, 1, -1
        	ex = self%predators(i)%get_x()
        	ey = self%predators(i)%get_y()
                                
        	if(self%predators(i)%get_energy() <= 0.5) then 
          		self%plains(ex, ey) = empty
          		self%predators(i) = self%predators(self%predators_alive)
          		self%predators_alive = self%predators_alive - 1  
            	cycle pred
          	end if
                                
          	cords = self%predators(i)%think_move(self%plains, dis, dis, ex, ey)
          	20 continue
        	nchar = self%plains(cords(1), cords(2))
                                
        	if(nchar == preyc) then
            	self%plains(ex, ey) = empty
            	self%plains(cords(1), cords(2)) = predatorc
                                        
              	call self%predators(i)%move(cords)                                 
                
				index = get_entity(self%preys, cords(1), cords(2), self%preys_alive)
             	if(index /= 0) then
					self%preys(index) = self%preys(self%preys_alive)
              		self%preys_alive = self%preys_alive - 1  
                                        
              		call self%predators(i)%add_energy(75.0)
				end if
    		else if (nchar == empty) then
        		self%plains(ex, ey) = empty
          		self%plains(cords(1), cords(2)) = predatorc
    	    	call self%predators(i)%move(cords)                                 
	
    		else if (nchar == plantc) then
        		self%plains(ex, ey) = empty
        	  	self%plains(cords(1), cords(2)) = predatorc
        	  	call self%predators(i)%move(cords)                                 
                                        
				index = get_entity(self%plants, cords(1), cords(2), self%plants_alive)
             	if(index /= 0) then
              	  	self%plants(index) = self%plants(self%plants_alive)
              	  	self%plants_alive = self%plants_alive - 1  
				end if
          		
				call self%predators(i)%sub_energy(20.0)
   		 	else if (nchar == predatorc) then
        		call self%predators(i)%sub_energy(25.0)
      		end if
                                
		end do pred
          				
      	prey: do i=self%preys_alive, 1, -1        
    		ex = self%preys(i)%get_x()
      		ey = self%preys(i)%get_y()
                                 
      		if(self%preys(i)%get_energy() <= 0.5) then 
        		self%plains(ex, ey) = empty
          		self%preys(i) = self%preys(self%preys_alive)
              	self%preys_alive = self%preys_alive - 1
                cycle prey
          	end if
                                
                                
        	cords = self%preys(i)%think_move(self%plains, dis, dis, ex, ey)
                                
          	nchar = self%plains(cords(1), cords(2))
                                
          	if (nchar == empty) then
            	self%plains(ex, ey) = empty
            	self%plains(cords(1), cords(2)) = preyc
              	call self%preys(i)%move(cords)                                 

          	else if (nchar == plantc) then
            	self%plains(ex, ey) = empty
              	self%plains(cords(1), cords(2)) = preyc
              	call self%preys(i)%move(cords)                                 
                                        
				index = get_entity(self%plants, cords(1), cords(2), self%plants_alive)
             	if(index /= 0) then
                	self%plants(index) = self%plants(self%plants_alive)
                 	self%plants_alive = self%plants_alive - 1  
                    exit
				end if
                                        
              	call self%preys(i)%add_energy(50.0)
        	else 
            	self%plains(ex, ey) = empty
                                        
				index = get_entity(self%predators, cords(1), cords(2), self%predators_alive)
             	if(index /= 0) then
                  	call self%predators(index)%add_energy(75.0)
				end if
                                        
              	self%preys(i) = self%preys(self%preys_alive)
              	self%preys_alive = self%preys_alive - 1  
                cycle prey  
		  	end if
  		end do prey
                        
      	plnt: do i=self%plants_alive, 1, -1
    		ex = self%plants(i)%get_x()
      		ey = self%plants(i)%get_y()
            
			!!! Gasto de Energia
			call self%plants(i)%sub_energy(10.0)
      		if(self%plants(i)%get_energy() <= 0.5) then 
        		self%plains(ex, ey) = empty
          		self%plants(i) = self%plants(self%plants_alive)
              	self%plants_alive = self%plants_alive - 1
                cycle plnt
          	end if
			!!!
			
			!!! Spread de plantas
        	if (self%plants_alive < max_animals*4) then
                
				cords = self%plants(i)%spread_cords(self%plains, dis, empty)                
				
				if(cords(1) /= 0 .and. cords(2) /= 0) then
					self%plains(cords(1), cords(2)) = plantc
              		plant1 = plant(cords(1), cords(2))
              		e = add_plant(self, plant1)
				end if
        	else
            	exit
          	end if
			!!!
    	end do plnt
                        
	  	if(self%preys_alive <= 1 .and. self%predators_alive <= 1) then
        	res = .true.
      	end if

	end function pass_round
                

	subroutine repopulate(self)
    	class(environment), intent(inout) :: self
        type(prey) ::  chosen_prey, prey1
      	type(predator) :: chosen_predator, predator1
    	type(plant) :: plant1
      	real :: n, c, m, r
    	real, dimension(4) :: new_vals, old_vals_predator, old_vals_prey
    	integer :: i, j, x, y
    	logical :: e
      	chosen_prey = self%preys(max_animals)
    	chosen_predator = self%predators(max_animals)
      	old_vals_predator = chosen_predator%get_brain_vals()
      	old_vals_prey = chosen_prey%get_brain_vals()
                        
    	call self%extinction()

		do i=1,max_animals
                        
  			!Recriando Presas
      		new_vals = old_vals_prey
    		call random_number(c)
      		call random_number(m)
      		j = floor(m*3) + 1
    		call random_number(new_vals(j))
      		new_vals(j) = ((floor(new_vals(j) * 2.0)) + old_vals_prey(j)) * merge(-1, 1, c > 0.5)

      		prey1 = prey(1, 1, new_vals(1), new_vals(2), new_vals(3), new_vals(4))

      		e = .true.
      		do while(e)                                          
              	e = add_prey(self, prey1)
          	end do
                        
            ! Recriando Predadores
          	new_vals = old_vals_predator
          	call random_number(c)
          	call random_number(m)
          	j = floor(m*3) + 1
            call random_number(new_vals(j))
          	new_vals(j) = ((floor(new_vals(j) * 2.0)) + old_vals_predator(j)) * merge(-1, 1, c > 0.5)
                                
        	predator1 = predator(dis, dis, new_vals(1), new_vals(2), new_vals(3), new_vals(4))
                                
          	e = .true.
        	do while(e)        
            	e = add_predator(self, predator1)
          	end do
                                
		end do

		! Recriando Plantas
      	do i = 1, max_animals/2
      		plant1 = plant(dis/2, dis/2)
          	
			e = .true.
            do while(e)        
          		e = add_plant(self, plant1)
          	end do
    	end do

	end subroutine repopulate

  	logical function add_plant(self, eplant) result(e)
    	class(environment), intent(inout) :: self
      	class(plant), intent(inout) :: eplant
      	integer :: x, y
      	real :: r
	  	e = .false.

      	call random_number(r)
      	x = floor(dis * r) + 1
      	call random_number(r)
      	y = floor(dis * r) + 1
                                        
      	call eplant%set_x(x)
      	call eplant%set_y(y)

      	if(self%plains(x, y) /= empty) then
        	e = .true.
          	return  
        end if
      	self%plains(x,y) = plantc
                        
      	self%plants_alive = self%plants_alive + 1
    	self%plants(self%plants_alive) = eplant
             
	end function add_plant         

  	logical function add_prey(self, eprey) result(e)
		class(environment), intent(inout) :: self
      	class(prey), intent(inout) :: eprey
      	integer :: x, y
      	real :: r
      	e = .false.

      	call random_number(r)
	  	x = floor(dis * r) + 1
      	call random_number(r)
      	y = floor(dis * r) + 1
                                        
      	call eprey%set_x(x)
      	call eprey%set_y(y)
                        
      	if(self%plains(x, y) /= empty) then
          	e = .true.
      		return  
      	end if

      	self%plains(x,y) = preyc
                       
      	self%preys_alive = self%preys_alive + 1
		self%preys(self%preys_alive) = eprey

	end function add_prey         

	logical function add_predator(self, epredator) result(e)
    	class(environment), intent(inout) :: self
      	class(predator), intent(inout) :: epredator
      	integer :: x, y
	  	real :: r
      	e = .false.

      	call random_number(r)
      	x = floor(dis * r) + 1
      	call random_number(r)
      	y = floor(dis * r) + 1
                                                
      	call epredator%set_x(x)
      	call epredator%set_y(y)
                        
      	if(self%plains(x, y) /= empty) then
        	e = .true.
          	return  
        end if

      	self%plains(x,y) = predatorc
                        
      	self%predators_alive = self%predators_alive + 1
      	self%predators(self%predators_alive) = epredator;
  	end function add_predator       

  	subroutine print(self)
      	class(environment) :: self
    	integer :: i, j
    	write(*, "(*(A))") ((self%plains(j, i), " " , j=1, dis), new_line("A"), i=1, dis)
    end subroutine print
	
	subroutine print_sim_vals(self)
		class(environment), intent(in) :: self
		real, dimension(4) :: vals

  		vals = self%preys(1)%get_brain_vals()
      	print "(A)", "Melhor presa: "
    	write (*, "(A)", advance = "no") "Peso predador: "
    	write (*, "(F6.1)") vals(1)
      	write (*, "(A)", advance = "no") "Peso presa: "
      	write (*, "(F6.1)") vals(2)
      	write (*, "(A)", advance = "no") "Peso planta: "
      	write (*, "(F6.1)") vals(3)
      	write (*, "(A)", advance = "no") "Peso não se mover: "
      	write (*, "(F6.1)") vals(4)
      	print *
      	print *

      	vals = self%predators(1)%get_brain_vals()

      	print "(A)", "Melhor predador: "
      	write (*, "(A)", advance = "no") "Peso predador: "
      	write (*, "(F6.1)") vals(1)
      	write (*, "(A)", advance = "no") "Peso presa: "
      	write (*, "(F6.1)") vals(2)
      	write (*, "(A)", advance = "no") "Peso planta: "
      	write (*, "(F6.1)") vals(3)
      	write (*, "(A)", advance = "no") "Peso não se mover: "
      	write (*, "(F6.1)") vals(4)
      	print *
      	print *
    end subroutine print_sim_vals
end module m_environment
