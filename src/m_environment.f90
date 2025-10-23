module m_environment
        use m_plant
        use m_prey
        use m_predator
        implicit none
        private
                character, parameter :: empty = '.', predatorc = 'K', preyc = 'P', plantc = 'x'
                integer, parameter :: max_animals = 10, dis = 20
                 
       type, public :: environment
                character, dimension(:,:), allocatable :: plains 
                type(plant), allocatable :: plants(:)
                type(prey), allocatable :: preys(:)
                type(predator), allocatable :: predators(:)
                integer :: preys_alive = 0, predators_alive = 0, plants_alive = 0
        
                contains
                        procedure :: populate, extinction, pass_round, print, repopulate
        end type environment
        
        interface environment
                module procedure no_args
        end interface environment

        contains
                type(environment) function no_args()
                        allocate(no_args%plains(dis,dis))
                        no_args%plains(:,:) = empty
                        allocate(no_args%preys(max_animals))
                        allocate(no_args%predators(max_animals))
                        allocate(no_args%plants(max_animals))
                end function no_args
                
                subroutine populate(self)
                        class(environment), intent(inout) :: self
                        type(prey) :: prey1
                        type(predator) :: predator1
                        type(plant) :: plant1
                        integer :: x, y, i, offset
                        logical :: e
                        real :: r

                        do i=1,max_animals
                                
                                prey1 = prey(1, 1)
                                
                                e = .true.
                                do while(e)
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                        
                                        call prey1%set_x(x)
                                        call prey1%set_y(y)
                                        
                                        e = add_prey(self, prey1)
                                end do
                        
                                predator1 = predator(dis, dis)
                                e = .true.
                                do while(e)        
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                                
                                        call predator1%set_x(x)
                                        call predator1%set_y(y)
                                        e = add_predator(self, predator1)
                                end do
                                
                        end do
                        do i = 1, max_animals/2
                                plant1 = plant(dis/2, dis/2)
                                e = .true.
                                do while(e)        
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                        
                                        call plant1%set_x(x)
                                        call plant1%set_y(y)
                                        e = add_plant(self, plant1)
                                end do
                        end do
                endsubroutine populate

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
                        integer :: i, j, lsize, ex, ey
                        real :: r
                        character :: nchar
                        res = .false.


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
                                
                                10 continue 
                                nchar = self%plains(cords(1), cords(2))
                                
                                if (nchar == empty) then
                                        self%plains(ex, ey) = empty
                                        self%plains(cords(1), cords(2)) = preyc
                                        call self%preys(i)%move(cords)                                 

                                else if (nchar == plantc) then
                                        self%plains(ex, ey) = empty
                                        self%plains(cords(1), cords(2)) = preyc
                                        call self%preys(i)%move(cords)                                 
                                        
                                        do j = self%plants_alive, 1, -1
                                                if(self%plants(j)%get_x() == cords(1) .and. self%plants(j)%get_y() == cords(2)) then
                                                        self%plants(j) = self%plants(self%plants_alive)
                                                        self%plants_alive = self%plants_alive - 1  
                                                        exit
                                                end if
                                        end do        
                                        
                                        call self%preys(i)%add_energy(50.0)
                                else 
                                       call self%preys(i)%sub_energy(15.0)
                                       cords = cords +1
                                       
                                       go to 10
                                end if
                        end do prey
                        
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
                                        
                                        do j = self%preys_alive, 1, -1
                                                if(self%preys(j)%get_x() == cords(1) .and. self%preys(j)%get_y() == cords(2)) then
                                                        self%preys(j) = self%preys(self%preys_alive)
                                                        self%preys_alive = self%preys_alive - 1  
                                                        exit
                                                end if
                                        end do        
                                        
                                        call self%predators(i)%add_energy(50.0)

                                else if (nchar == empty) then
                                        self%plains(ex, ey) = empty
                                        self%plains(cords(1), cords(2)) = predatorc
                                        call self%predators(i)%move(cords)                                 

                                else if (nchar == plantc) then
                                        self%plains(ex, ey) = empty
                                        self%plains(cords(1), cords(2)) = predatorc
                                        call self%predators(i)%move(cords)                                 
                                        
                                        do j=1, self%plants_alive
                                                if(self%plants(j)%get_x() == cords(1) .and. self%plants(j)%get_y() == cords(2)) then
                                                        self%plants(j) = self%plants(self%plants_alive)
                                                        self%plants_alive = self%plants_alive - 1  
                                                        exit 
                                                end if
                                        end do        
                                        
                                        call self%predators(i)%sub_energy(10.0)
                                else if (nchar == predatorc) then
                                        call self%predators(i)%sub_energy(20.0)
                                else
                                        call self%predators(i)%sub_energy(15.0)
                                        cords = cords + 1

                                        go to 20
                                end if
                                
                        end do pred
                        
                        plnt: do i=self%plants_alive, 1, -1
                                if (self%plants_alive < max_animals*4) then
                                        ex = self%plants(i)%get_x()
                                        ey = self%plants(i)%get_y()
                                        
                                        call random_number(r)
                                        if(r > self%plants(i)%get_spread_chance()) then
                                                call random_number(r)
                                                select case(floor(4*r) + 1)
                                                        case(1)
                                                                if(ex + 1 < dis .and. self%plains(ex+1, ey) == empty) then
                                                                        ex = ex +1
                                                                else 
                                                                        cycle plnt
                                                                end if       
                                                        case(2)
                                                                if (ex - 1 > 1 .and. self%plains(ex-1, ey) == empty) then
                                                                        ey = ex - 1
                                                                else 
                                                                        cycle plnt
                                                                end if       
                                                        case(3) 
                                                                if(ey + 1 < dis .and. self%plains(ex, ey+1) == empty) then
                                                                        ey = ey + 1
                                                                else 
                                                                        cycle plnt
                                                                end if       
                                                        case(4)
                                                                if(ey - 1 > 1 .and. self%plains(ex, ey-1) == empty) then
                                                                        ey = ey - 1
                                                                else 
                                                                        cycle plnt
                                                                end if    

                                                end select   
                                        end if
                                        
                                        self%plains(ex, ey) = plantc
                                        plant1 = plant(ex, ey)
                                        e = add_plant(self, plant1)
                                else
                                        exit
                                end if
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
                        chosen_prey = self%preys(1)
                        chosen_predator = self%predators(1)
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
                                new_vals(j) = ((floor(new_vals(j) * 10.0)) + old_vals_prey(j)) * merge(-1, 1, c > 0.5)

                                prey1 = prey(1, 1, new_vals(1), new_vals(2), new_vals(3), new_vals(4))

                                e = .true.
                                do while(e)        
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                        
                                        call prey1%set_x(x)
                                        call prey1%set_y(y)
                                        
                                        e = add_prey(self, prey1)
                                end do
                        
                                new_vals = old_vals_predator
                                call random_number(c)
                                call random_number(m)
                                j = floor(m*3) + 1
                                call random_number(new_vals(j))
                                new_vals(j) = ((floor(new_vals(j) * 10.0)) + old_vals_predator(j)) * merge(-1, 1, c > 0.5)
                                
                                predator1 = predator(dis, dis, new_vals(1), new_vals(2), new_vals(3), new_vals(4))
                                
                                e = .true.
                                do while(e)        
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                                
                                        call predator1%set_x(x)
                                        call predator1%set_y(y)
                                        e = add_predator(self, predator1)
                                end do
                                
                        end do

                        do i = 1, max_animals/2
                                plant1 = plant(dis/2, dis/2)
                                e = .true.
                                do while(e)        
                                        call random_number(r)
                                        x = floor(dis * r) + 1
                                        call random_number(r)
                                        y = floor(dis * r) + 1
                                        
                                        call plant1%set_x(x)
                                        call plant1%set_y(y)
                                        e = add_plant(self, plant1)
                                end do
                        end do

                end subroutine repopulate

                logical function add_plant(self, eplant) result(e)
                        class(environment), intent(inout) :: self
                        class(plant), intent(in) :: eplant
                        integer :: x, y
                        e = .false.

                        x = eplant%get_x()
                        y = eplant%get_y()

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
                        class(prey), intent(in) :: eprey
                        integer :: x, y
                        e = .false.
                        
                        x = eprey%get_x()
                        y = eprey%get_y()
                        

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
                        class(predator), intent(in) :: epredator
                        integer :: x, y
                        e = .false.

                        x = epredator%get_x()
                        y = epredator%get_y()
                        
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
        
end module m_environment
