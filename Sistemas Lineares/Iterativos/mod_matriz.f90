module mod_sis_iterativos

        implicit none 

        contains

        subroutine imprimir(matriz, nl, nc)
        
                implicit none 

                integer, intent(in) :: nl, nc
                real, dimension(nl,nc), intent(in) :: matriz

                integer :: i
                
                print *, '-------Matriz--------'
                do i = 1, nl
                        print *, matriz(i,:)
                end do

        end subroutine

        subroutine jacobi(matriz, nl, nc, chute, erro)

                implicit none 

                integer, intent(in) :: nl, nc
                real, dimension(nl, nc), intent(in) :: matriz
                real, dimension(nl), intent(inout) :: chute
                real, intent(inout) :: erro

                real, dimension(nl) :: solucao
                integer :: i, j

                do i = 1, nl
                        solucao(i) = matriz(i,nc)
                        do j = 1, nc-1
                                if (i .ne. j) then
                                        solucao(i) = solucao(i) - chute(j) * matriz(i,j)
                                end if
                        end do
                        solucao(i) = solucao(i)/matriz(i,i)
                end do
                
                erro = norm_max(chute, solucao, nl)
                chute = solucao
        end subroutine
        
        subroutine gauss_seidel(matriz, nl, nc, chute, erro)

                implicit none 

                integer, intent(in) :: nl, nc
                real, dimension(nl, nc), intent(in) :: matriz
                real, dimension(nl), intent(inout) :: chute
                real, intent(inout) :: erro
                
                real, dimension(nl) :: ini
                real :: solucao
                integer :: i, j
		
		ini = chute
                do i = 1, nl
                        solucao = matriz(i,nc)
                        do j = 1, nc-1
                                if (i .ne. j) then
                                        solucao = solucao - chute(j) * matriz(i,j)
                                end if
                        end do
                        chute(i) = solucao/matriz(i,i)
                end do
                
                erro = norm_max(ini, chute, nl)
        end subroutine

        real function norm_max(v1, v2, n)

                implicit none 
                
                integer, intent(in) :: n
                real, dimension(n), intent(in) :: v1, v2

                norm_max = (maxval(v2-v1)/maxval(v2))
                return
        end function
end module 
