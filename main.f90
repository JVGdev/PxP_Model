program main
  	use m_environment

  	implicit none 
  	type(environment) :: environment1
  	integer :: n, i, k, time_thresold
  	character :: opt
  	opt = '1'
    time_thresold = 300
	k = 0

	environment1 = environment()
  	call environment1%populate()
        
	do while(opt /= '0')
        
        !print *, achar(27)//"[1J"
        call environment1%print()
        
        print "(A)", "Digite 1 para passar um turno | Digite 2 Para passar uma geração | Digite 3 para passar N gerações | Digite 0 para finalizar"
        read (*, "(A)") opt
        
        k = 0

		select case(opt)
          	case('1')
    			print "(A)", "Turno Passou." 
  				if(environment1%pass_round() .or. k > time_thresold) then
                        	
	  				call environment1%print_sim_vals()                                

          			print "(A)", "Digite qualquer outra tecla para continuar: "
            		read (*, *) 
        	
					call environment1%repopulate()
      			end if       
    			k = k + 1
  			case('2')
      			do while((.not. environment1%pass_round()))
        			if (k>time_thresold) then
        				exit
          			end if
    				k = k + 1
      			end do
                                
				call environment1%print_sim_vals()                                
                        
      			print "(A)", "Digite qualquer outra tecla para continuar: "
      			read (*, *) 
                                
    			call environment1%repopulate()
			case('3')
            	print "(A)", "Quantas gerações deseja passar:"
              	read (*, *) n
              	
				do i=1, n
                  	do while(.not. environment1%pass_round())
                    	if (k>time_thresold) then
                        	exit
                      	end if
                  		
  						k = k + 1
                  	end do
                  	
					call environment1%repopulate()
                	print *, "passou geração", i
              	end do
                        
              	print *, n, "Gerações acabaram, melhores indivíduos da última geração simulada foram:"
                        
			  	call environment1%print_sim_vals()                                

              	print "(A)", "Digite qualquer outra tecla para continuar: "
          		read (*, *) 
        	case('0')
      	end select
  	end do

end program main
        
