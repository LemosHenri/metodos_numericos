module mod_sis_exatos

        implicit none 

        contains

        subroutine imprimir(matriz, nl, nc)
        
                implicit none 

                integer, intent(in) :: nl, nc
                integer :: i
                real, dimension(nl,nc), intent(in) :: matriz

                print *, '-------Matriz--------'
                do i = 1, nl
                        print *, matriz(i,:)
                end do

        end subroutine

        subroutine eliminacao_gauss(matriz, nl, nc)

                implicit none 
                integer, intent(in) :: nl, nc
                real, dimension(nl,nc), intent(inout) :: matriz 

                integer :: i, k

                do k = 1, nl-1
                        do i = k+1, nl 
                                matriz(i,:) = matriz(i,:) - matriz(k,:)*(matriz(i,k)/matriz(k,k))
                        end do 
                end do
        end subroutine 

        subroutine substituicao_retroativa(matriz, nl, nc, sol)

                implicit none 
                integer, intent(in) :: nl, nc 
                real, dimension(nl,nc), intent(in) :: matriz 
                real, dimension(:), intent(inout), allocatable :: sol
                integer :: i, j
                real :: soma

                allocate(sol(nl))

                do i = nl,1,-1
                        soma = 0
                        do j = i+1, nc-1 
                                soma = soma + matriz(i,j)*sol(j)
                        end do
                        sol(i) = (matriz(i,nc)-soma)/matriz(i,i)
                end do

        end subroutine

        subroutine decomp_lu(matriz, nl, nc, l, u)

                implicit none 

                integer :: nl, nc
                real, dimension(nl, nc), intent(in) :: matriz
                real, dimension(:, :), intent(inout), allocatable :: l, u
                integer :: i, j, k

                do i = 1, nl
                        l(i, i) = 1
                end do

                do i = 1,nl
                        do j = 1, nc
                                if (i <= j) then
                                        u(i,j) = matriz(i,j)
                                        do k = 1, i-1
                                                u(i,j) = u(i,j) - l(i,k) * u(k,j)
                                        end do
                                else
                                        l(i,j) = matriz(i,j)
                                        do k = 1, j-1
                                                l(i,j) = l(i,j) - l(i,k) * u(k,j)
                                        end do
                                        l(i,j) = l(i,j)/u(j,j)
                                end if
                        end do
                end do
        end subroutine

        real function erro_max(matriz, nl, nc, sol)
                implicit none
        
                integer, intent(in) :: nl, nc
                real, dimension(nl,nc), intent(in) :: matriz 
                real, dimension(nl), intent(in) :: sol 

                real, dimension(nl) :: x, b

                x = matmul(matriz(1:nl,1:nc-1),sol)
                b = matriz(:,nc)

                ! b - Mx = erro
                
                erro_max = maxval(b - x)
                return 
        end function 

end module 
