program main
        use m_environment

        implicit none 
        type(environment) :: environment1
        real, dimension(4) :: vals
        integer :: n, i, k
        character :: opt
        opt = '1'
        k = 0

        environment1 = environment()
        call environment1%populate()
        
        do while(opt /= '0')
        
        print *, achar(27)//"[1J"
        call environment1%print()
        
        print "(A)", "Digite 1 para passar um turno | Digite 2 Para passar uma geração | Digite 3 para passar N gerações | Digite 0 para finalizar"
        read (*, "(A)") opt
        
        k = 0

        select case(opt)
                case('1')
                        print "(A)", "Turno Passou." 
                        if(environment1%pass_round() .or. k > 100) then
                                vals = environment1%preys(1)%get_brain_vals()
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

                                vals = environment1%predators(1)%get_brain_vals()

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

                                print "(A)", "Digite qualquer outra tecla para continuar: "
                                read (*, *) 
                                call environment1%repopulate()
                        end if       
                        k = k + 1
                case('2')
                        do while((.not. environment1%pass_round()))
                                if (k>100) then
                                        exit
                                end if
                                k = k + 1
                        end do
                                
                        vals = environment1%preys(1)%get_brain_vals()
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

                        vals = environment1%predators(1)%get_brain_vals()

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
                        
                        print "(A)", "Digite qualquer outra tecla para continuar: "
                        read (*, *) 
                                
                        call environment1%repopulate()
                case('3')
                        print "(A)", "Quantas gerações deseja passar:"
                        read (*, *) n
                        do i=1, n
                                do while(.not. environment1%pass_round())
                                        if (k>100) then
                                                exit
                                        end if
                                        k = k + 1
                                end do
                                call environment1%repopulate()
                                print *, "passou geração", i
                        end do
                        
                        print *, n, "Gerações acabaram, melhores indivíduos da última geração simulada foram:"
                        
                        vals = environment1%preys(1)%get_brain_vals()
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

                        vals = environment1%predators(1)%get_brain_vals()

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

                        print "(A)", "Digite qualquer outra tecla para continuar: "
                        read (*, *) 
                case('0')
                end select
        end do

end program main
        
