module nmp_module
    use, intrinsic :: iso_fortran_env, only : int64
    implicit none
    real(32), allocatable :: trained_W(:, :), trained_B(:, :)
    integer, parameter :: m = size(trained_B, 1)
    integer, parameter :: r = size(trained_B, 2)
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

        ! Check if array is already allocated with correct dimensions
        if (allocated(array) .and. size(array, 1) == m .and. size(array, 2) == r) then
            ! The array is already correctly allocated, no need to reload
            return
        endif

        ! If not correctly allocated, deallocate if necessary and reallocate
        if (allocated(array)) then
            deallocate(array)
        endif
        allocate(array(m, r))

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
        real(8), intent(in) :: V(:, :, :, :)
        integer, parameter :: m = size(array, 1)
        integer, parameter :: x = size(array, 2)
        integer, parameter :: y = size(array, 3)
        integer, parameter :: z = size(array, 4)
        real(8) :: V_flattened(m, x * y * z), H(r, x * y * z), H_mc(r, x, y, z)
        integer(int64) :: s_com
        external :: dgemm
        integer :: lda, ldb, ldc
        real(8) :: alpha, beta

        ! Ensure V_flattened is allocated with the correct dimensions
        if (.not. allocated(V_flattened) .or. size(V_flattened, 1) /= m .or. size(V_flattened, 2) /= x * y * z) then
            if (allocated(V_flattened)) deallocate(V_flattened)
            allocate(V_flattened(m, x * y * z))
        endif

        ! Ensure H is allocated with the correct dimensions
        if (.not. allocated(H) .or. size(H, 1) /= r .or. size(H, 2) /= x * y * z) then
            if (allocated(H)) deallocate(H)
            allocate(H(r, x * y * z))
        endif

        ! Flatten V
        V_flattened = reshape(V, [m, x * y * z])

        ! Set parameters for GEMM
        alpha = 1.0_wp
        beta = 0.0_wp
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
        real(8), intent(in) :: H_advected(:, :, :, :)
        integer, parameter :: r = size(array, 1)
        integer, parameter :: x = size(array, 2)
        integer, parameter :: y = size(array, 3)
        integer, parameter :: z = size(array, 4)
        real(8) :: H_advected_flattened(r, x * y * z), V_reconstructed(m, x * y * z)
        real(8) :: V_reconstructed_mc(m, x, y, z)
        integer(int64) :: s_rec
        external :: dgemm
        integer :: lda, ldb, ldc
        real(8) :: alpha, beta

        ! Ensure H_advected_flattened is allocated with the correct dimensions
        if (.not. allocated(H_advected_flattened) .or. size(H_advected_flattened, 1) /= r .or. size(H_advected_flattened, 2) /= x * y * z) then
            if (allocated(H_advected_flattened)) deallocate(H_advected_flattened)
            allocate(H_advected_flattened(r, x * y * z))
        endif

        ! Ensure V_reconstructed is allocated with the correct dimensions
        if (.not. allocated(V_reconstructed) .or. size(V_reconstructed, 1) /= m .or. size(V_reconstructed, 2) /= x * y * z) then
            if (allocated(V_reconstructed)) deallocate(V_reconstructed)
            allocate(V_reconstructed(m, x * y * z))
        endif

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
    character (len = *) :: path_trained_B, path_trained_W
    integer :: m, r
    real(32), allocatable :: trained_B(:, :), trained_W(:, :), V(:, :, :), H_mc(:, :, :), V_reconstructed_mc(:, :, :), H_advected

    trained_B = load_trained_matrices(path_trained_B, trained_B, m, r)
    trained_W = load_trained_matrices(path_trained_W, trained_W, m, r)

    H_mc = project_to_latent_space(V)
    V_reconstructed_mc = reconstruct_to_source_space(H_advected)
    ! Define and initialize V, H_advected, etc. as needed
    ! Call functions project_to_latent_space and reconstruct_to_source_space

    ! Example usage:
    ! H_mc = project_to_latent_space(V)
    ! V_reconstructed_mc = reconstruct_to_source_space(H_advected)

end program main
