! type(*), dimension(:), allocatable, intent(inout) :: list
! type(*), dimension(:), allocatable :: temp
! type(*), intent(in) :: element

integer :: lsize
                        
if(allocated(list)) then
        lsize = size(list)
        allocate(temp(lsize + 1))
        temp(1:lsize) = list
        temp(lsize + 1) = element
        call move_alloc(temp, list)
else
	allocate(list(1))
	list(1) = element
end if



