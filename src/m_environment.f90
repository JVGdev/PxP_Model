module m_environment
	use m_plant
    use m_prey
    use m_predator
	use m_env_helper
    
	implicit none
    private
        character, parameter :: empty = ' ', predatorc = 'K', preyc = 'P', plantc = '.'
      	integer, parameter :: max_animals = 20, dis = 20
                 
    type, public :: environment
    	character, dimension(:,:), allocatable :: plains 
    	type(plant), allocatable :: o_plants(:)
        type(prey), allocatable :: o_preys(:)
        type(predator), allocatable :: o_predators(:)
    	
		integer, allocatable :: plants(:)
        integer, allocatable :: preys(:)
        integer, allocatable :: predators(:)
        integer :: preys_alive = 0, predators_alive = 0, plants_alive = 0
        
        contains
        	procedure :: extinction, pass_round, print, repopulate, print_sim_vals
  	end type environment
        
        
  	interface environment
    	module procedure no_args
  	end interface environment

	contains
    	type(environment) function no_args()
      	integer :: i
      	allocate(no_args%plains(dis,dis))
        no_args%plains(:,:) = empty
        !Actual entity storage
		allocate(no_args%o_preys(max_animals))
        allocate(no_args%o_predators(max_animals))
        allocate(no_args%o_plants(max_animals*2))
        
		!Indexes helpers
		no_args%preys = (/(i, i=1, max_animals, 1)/)
        no_args%predators = (/(i, i=1, max_animals, 1)/)
        no_args%plants = (/(i, i=1, max_animals*2, 1)/)
	end function no_args
                
	!!! FAZER METEORO FUTURAMENTE
  	subroutine extinction(self)
  		class(environment), intent(inout) :: self
      	integer :: i
		self%plains = empty
                        
      	self%preys_alive = 0
    	self%predators_alive = 0
    	self%plants_alive = 0
		
		self%preys = (/(i, i=1, max_animals, 1)/)
        self%predators = (/(i, i=1, max_animals, 1)/)
        self%plants = (/(i, i=1, max_animals*2, 1)/)
  	end subroutine extinction
                
  	logical function pass_round(self) result(res)
    	class(environment), intent(inout) :: self
      	logical :: e
      	class(plant), allocatable :: plant1
      	integer, dimension(2) :: cords, index
      	integer :: i, j, p, lsize, ex, ey, ix, xi, jy, yj
      	real :: r
      	character :: nchar
      	res = .false.

    	pred: do i=self%predators_alive, 1, -1
  			p = self%predators(i)
        	ex = self%o_predators(p)%get_x()
        	ey = self%o_predators(p)%get_y()
                                
        	if(self%o_predators(p)%get_energy() <= 0.5) then 
          		self%plains(ex, ey) = empty
        		call kill(self%predators, self%predators_alive, i)    	
				cycle pred
          	end if
                                
			ix = merge(ex-8, 1, ex-8 >= 1)
			xi = merge(ex+8, 1, ex+8 <= dis)
			jy = merge(ey-8, 1, ey-8 >= 1)
			yj = merge(ey+8, 1, ex+8 <= dis)

          	cords = self%o_predators(p)%think_move(self%plains(ix:xi, jy:yj), dis, dis, ex, ey)
        	nchar = self%plains(cords(1), cords(2))
                                
        	if(nchar == preyc) then
            	self%plains(ex, ey) = empty
            	self%plains(cords(1), cords(2)) = predatorc
                                        
              	call self%o_predators(p)%move(cords)                                 
                
				!!Otimizar urgente
				index = get_entity(self%preys, self%o_preys, cords(1), cords(2), self%preys_alive)
             	if(index(2) /= 0) then
              		call self%o_predators(p)%add_energy(self%o_preys(index(2))%get_matter())
					
        			call kill(self%preys, self%preys_alive, index(1))    	
				end if
    		else if (nchar == empty) then
        		self%plains(ex, ey) = empty
          		self%plains(cords(1), cords(2)) = predatorc
    	    	call self%o_predators(p)%move(cords)                                 
	
    		else if (nchar == plantc) then
        		self%plains(ex, ey) = empty
        	  	self%plains(cords(1), cords(2)) = predatorc
        	  	call self%o_predators(p)%move(cords)                                 
                                        
				index = get_entity(self%plants, self%o_plants, cords(1), cords(2), self%plants_alive)
             	if(index(2) /= 0) then
        			call self%o_predators(p)%sub_energy(self%o_plants(index(2))%get_matter()/10)
              	  	
        			call kill(self%plants, self%plants_alive, index(1))    	
				end if
      
   		 	else if (nchar == predatorc) then
        		call self%o_predators(p)%sub_energy(25.0)
      		end if
                                
		end do pred
          				
      	
		prey: do i=self%preys_alive, 1, -1        
    		p = self%preys(i)
			ex = self%o_preys(p)%get_x()
      		ey = self%o_preys(p)%get_y()
                                 
      		if(self%o_preys(p)%get_energy() <= 0.5) then 
        		self%plains(ex, ey) = empty
        		call kill(self%preys, self%preys_alive, i)    	
                cycle prey
          	end if
                                
			ix = merge(ex-8, 1, ex-8 >= 1)
			xi = merge(ex+8, 1, ex+8 <= dis)
			jy = merge(ey-8, 1, ey-8 >= 1)
			yj = merge(ey+8, 1, ex+8 <= dis)

          	cords = self%o_preys(p)%think_move(self%plains(ix:xi, jy:yj), dis, dis, ex, ey)
        	nchar = self%plains(cords(1), cords(2))
                                
                                
          	if (nchar == empty) then
            	self%plains(ex, ey) = empty
            	self%plains(cords(1), cords(2)) = preyc
              	call self%o_preys(p)%move(cords)                                 

          	else if (nchar == plantc) then
            	self%plains(ex, ey) = empty
              	self%plains(cords(1), cords(2)) = preyc
              	call self%o_preys(p)%move(cords)                                 
                                        
				index = get_entity(self%plants, self%o_plants, cords(1), cords(2), self%plants_alive)
             	if(index(2) /= 0) then
					call kill(self%plants, self%plants_alive, index(1))
				end if
                                        
              	call self%o_preys(p)%add_energy(200.0)
        	else 
            	self%plains(ex, ey) = empty
                                        
				index = get_entity(self%predators, self%o_predators, cords(1), cords(2), self%predators_alive)
             	if(index(2) /= 0) then
                  	call self%o_predators(index(2))%add_energy(200.0)
				end if
                                        
				call kill(self%preys, self%preys_alive, i)
                cycle prey  
		  	end if
  		end do prey
                        
      	plnt: do i=self%plants_alive, 1, -1
    		p = self%plants(i)
			ex = self%o_plants(p)%get_x()
      		ey = self%o_plants(p)%get_y()
            
			!!! Gasto de Energia
			call self%o_plants(p)%sub_energy(10.0)
      		if(self%o_plants(p)%get_energy() <= 0.5) then 
        		self%plains(ex, ey) = empty
				call kill(self%plants, self%plants_alive, i)
                cycle plnt
          	end if
			!!!
			
			!!! Spread de plantas
        	if (self%plants_alive < max_animals*2) then
                
				cords = self%o_plants(p)%spread_cords(self%plains, dis, empty)                
				
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
        class(prey), allocatable :: prey1
      	class(predator), allocatable :: predator1
    	class(plant), allocatable :: plant1
    	real, dimension(4) :: new_vals
		integer :: i
    	logical :: e
                        
    	call self%extinction()

		do i=1,max_animals
                        
  			!Recriando Presas
			new_vals = genetic_select(self%preys, self%o_preys, max_animals)
      		
			prey1 = prey(1, 1, new_vals(1), new_vals(2), new_vals(3), new_vals(4))

      		e = .true.
      		do while(e)                                          
              	e = add_prey(self, prey1)
          	end do
                        
            ! Recriando Predadores
			new_vals = genetic_select(self%predators, self%o_predators, max_animals)
                                
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

	!!! Fazer funções de add funcionar com nova lista de indexes

  	logical function add_plant(self, eplant) result(e)
    	class(environment), intent(inout) :: self
      	class(plant), allocatable, intent(inout) :: eplant
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
    	self%o_plants(self%plants(self%plants_alive)) = eplant
		
             
	end function add_plant         

  	logical function add_prey(self, eprey) result(e)
		class(environment), intent(inout) :: self
      	class(prey), allocatable, intent(inout) :: eprey
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
		self%o_preys(self%preys(self%preys_alive)) = eprey

	end function add_prey         

	logical function add_predator(self, epredator) result(e)
    	class(environment), intent(inout) :: self
      	class(predator), allocatable, intent(inout) :: epredator
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
      	self%o_predators(self%predators(self%predators_alive)) = epredator;
  	end function add_predator       

  	subroutine print(self)
      	class(environment) :: self
    	integer :: i, j
		
		write(*, "(I3)") self%predators
		print *, ""
		write(*, "(I3)") self%preys
		print *, ""
		write(*, "(I3)") self%plants
		print *, ""
    	write(*, "(*(A))", advance="no") (" ―", i=1, dis)
		print *, ""
		do i = 1, dis
			write (*, "(A)", advance="no") " | "
			do j = 1, dis
    			write(*, "(*(A))", advance="no") self%plains(i, j)
				if(j /= dis) then
					write (*, "(A)", advance="no") ' '
				end if
			end do
			print *, "|" 
		end do
    	write(*, "(*(A))", advance="no") (" ―", i=1, dis)
		print *, ""
    end subroutine print
	
	subroutine print_sim_vals(self)
		class(environment), intent(in) :: self
		real, dimension(4) :: vals

  		vals = self%o_preys(self%preys(1))%get_brain_vals()
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

      	vals = self%o_predators(self%predators(1))%get_brain_vals()

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
