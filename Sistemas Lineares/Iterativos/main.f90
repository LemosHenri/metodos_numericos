program main

        use mod_sis_iterativos 

        implicit none 
        integer :: nl, nc
        integer :: i, j, it
        real, dimension(:,:), allocatable :: matriz
        real, dimension(:), allocatable :: solucao, x
        real :: erro, tolerance

        !Leitura da matriz contida no arquivo matriz.dat
        open(unit=1,file='matriz.dat',status='old')

        read (1,*) nl, nc
        allocate(matriz(nl,nc), x(nl), solucao(nl))
        
        do i = 1, nl
                read (1,*) (matriz(i,j), j=1, nc)
        end do

        close(1)
        !Fechamento do arquivo e encerramento da leitura da matriz

        !verificação do conteúdo da matriz
        !call imprimir(matriz,nl,nc)

        erro = 1
        tolerance = 1e-10
        x = (/1, 1, 1, 1, 1, 1/)
        !it = 0
        
        print *, 'Iteração ---------- Erro --------- Solução'
        do while (erro > tolerance)
                print *, it, erro, x
                call gauss_seidel(matriz, nl, nc, x, erro)
                it = it + 1
        end do
        
end program

