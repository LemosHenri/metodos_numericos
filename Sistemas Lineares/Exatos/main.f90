program main

        use mod_sis_exatos

        implicit none 
        integer :: nl, nc
        integer :: i, j
        real, dimension(:,:), allocatable :: matriz
        real, dimension(:), allocatable :: sol

        !Leitura da matriz contida no arquivo matriz.dat
        open(unit=1,file='matriz.dat',status='old')

        read (1,*) nl, nc
        allocate(matriz(nl,nc))
        
        do i = 1, nl
                read (1,*) (matriz(i,j), j=1, nc)
        end do

        close(1)
        !Fechamento do arquivo e encerramento da leitura da matriz

        !verificação do conteúdo da matriz
        !call imprimir(matriz,nl,nc)

        call eliminacao_gauss(matriz,nl,nc)
        !call imprimir(matriz,nl,nc)

        call substituicao_retroativa(matriz, nl, nc, sol)
        print *, '------Solução-------'
        print *, sol

        print *, '------Erro-------'
        print *, erro_max(matriz, nl, nc, sol)

end program

