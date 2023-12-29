module nmp_module
    use, intrinsic :: iso_fortran_env, only : int64
    implicit none
    integer, parameter :: m = ! Define the value of m
    integer, parameter :: r = ! Define the value of r
    integer, parameter :: x = ! Define the value of x
    integer, parameter :: y = ! Define the value of y
    integer, parameter :: z = ! Define the value of z
    real(8), allocatable :: trained_W(:, :), trained_B(:, :)
contains

    subroutine get_unique_unit_number(unit)
        implicit none
        integer, intent(out) :: unit

        ! Find a free unit number (example implementation)
        unit = 10  ! Starting unit number
        do while (.true.)
            inquire(unit = unit, opened = opened)
            if (.not. opened) exit
            unit = unit + 1
        end do
    end subroutine get_unique_unit_number

    subroutine load_trained_matrices(filename, array, m, r)
        ! Load 'trained_W.bin' and 'trained_B.bin'
        ! Implement file reading here
        implicit none
        character(len = *), intent(in) :: filename
        real, intent(out) :: array(:, :)
        integer, intent(in) :: m, r
        integer :: iostat, file_unit

        ! Get a unique file unit number
        call get_unique_unit_number(file_unit)

        ! Open the binary file
        open(unit = file_unit, file = filename, form = 'unformatted', access = 'direct', recl = m * r * 4, iostat = iostat)
        if (iostat /= 0) then
            print *, "Error opening file: ", filename
            stop
        endif

        ! Read the data
        read(unit = file_unit, rec = 1, iostat = iostat) array
        if (iostat /= 0) then
            print *, "Error reading file: ", filename
            stop
        endif

        ! Close the file
        close(unit = file_unit)
    end subroutine load_trained_matrices

    function project_to_latent_space(V) result(H_mc)
        real(8), intent(in) :: V(m, x, y, z)
        real(8) :: V_flattened(m, x * y * z), H(r, x * y * z), H_mc(r, x, y, z)
        integer(int64) :: s_com
        external :: dgemm
        integer :: lda, ldb, ldc
        real(8) :: alpha, beta

        ! Flatten V
        V_flattened = reshape(V, [m, x * y * z])

        ! Set parameters for GEMM
        alpha = 1.0d0
        beta = 0.0d0
        lda = m
        ldb = r
        ldc = r

        ! Call GEMM for matrix multiplication B @ V
        call dgemm('N', 'N', r, x * y * z, m, alpha, trained_B, lda, V_flattened, ldb, beta, H, ldc)

        ! Compute scaling factor
        s_com = sum(V_flattened) / sum(H)

        ! Scale and unstack H
        H_mc = reshape(s_com * H, [r, x, y, z])
    end function project_to_latent_space

    function reconstruct_to_source_space(H_advected) result(V_reconstructed_mc)
        real(8), intent(in) :: H_advected(r, x, y, z)
        real(8) :: H_advected_flattened(r, x * y * z), V_reconstructed(m, x * y * z)
        real(8) :: V_reconstructed_mc(m, x, y, z)
        integer(int64) :: s_rec
        external :: dgemm
        integer :: lda, ldb, ldc
        real(8) :: alpha, beta

        ! Flatten H_advected
        H_advected_flattened = reshape(H_advected, [r, x * y * z])

        ! Set parameters for GEMM
        alpha = 1.0d0
        beta = 0.0d0
        lda = m
        ldb = r
        ldc = m

        ! Call GEMM for matrix multiplication W @ H_advected
        call dgemm('N', 'N', m, x * y * z, r, alpha, trained_W, lda, H_advected_flattened, ldb, beta, V_reconstructed, ldc)

        ! Compute scaling factor
        s_rec = sum(H_advected_flattened) / sum(V_reconstructed)

        ! Scale and unstack V_reconstructed
        V_reconstructed_mc = reshape(s_rec * V_reconstructed, [m, x, y, z])
    end function reconstruct_to_source_space

end module nmp_module

program main
    use nmp_module
    implicit none
    ! Define and initialize V, H_advected, etc. as needed
    ! Call functions project_to_latent_space and reconstruct_to_source_space

    ! Example usage:
    ! H_mc = project_to_latent_space(V)
    ! V_reconstructed_mc = reconstruct_to_source_space(H_advected)

end program main
