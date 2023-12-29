module nmp_module
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  integer, parameter :: m = ! Define the value of m
  integer, parameter :: r = ! Define the value of r
  integer, parameter :: x = ! Define the value of x
  integer, parameter :: y = ! Define the value of y
  integer, parameter :: z = ! Define the value of z
  real(8), allocatable :: trained_W(:,:), trained_B(:,:)
contains

  subroutine load_trained_matrices()
    ! Load 'trained_W.bin' and 'trained_B.bin'
    ! Implement file reading here
  end subroutine load_trained_matrices

  function project_to_latent_space(V) result(H_mc)
    real(8), intent(in) :: V(m,x,y,z)
    real(8) :: V_flattened(m,x*y*z), H(r,x*y*z), H_mc(r,x,y,z)
    integer(int64) :: s_com
    external :: dgemm
    integer :: lda, ldb, ldc
    real(8) :: alpha, beta

    ! Flatten V
    V_flattened = reshape(V, [m, x*y*z])

    ! Set parameters for GEMM
    alpha = 1.0d0
    beta = 0.0d0
    lda = m
    ldb = r
    ldc = r

    ! Call GEMM for matrix multiplication B @ V
    call dgemm('N', 'N', r, x*y*z, m, alpha, trained_B, lda, V_flattened, ldb, beta, H, ldc)

    ! Compute scaling factor
    s_com = sum(V_flattened) / sum(H)

    ! Scale and unstack H
    H_mc = reshape(s_com * H, [r, x, y, z])
  end function project_to_latent_space

  function reconstruct_to_source_space(H_advected) result(V_reconstructed_mc)
    real(8), intent(in) :: H_advected(r,x,y,z)
    real(8) :: H_advected_flattened(r,x*y*z), V_reconstructed(m,x*y*z)
    real(8) :: V_reconstructed_mc(m,x,y,z)
    integer(int64) :: s_rec
    external :: dgemm
    integer :: lda, ldb, ldc
    real(8) :: alpha, beta

    ! Flatten H_advected
    H_advected_flattened = reshape(H_advected, [r, x*y*z])

    ! Set parameters for GEMM
    alpha = 1.0d0
    beta = 0.0d0
    lda = m
    ldb = r
    ldc = m

    ! Call GEMM for matrix multiplication W @ H_advected
    call dgemm('N', 'N', m, x*y*z, r, alpha, trained_W, lda, H_advected_flattened, ldb, beta, V_reconstructed, ldc)

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

!
!
!
!
!

subroutine project_to_latent_space(V, H_mc)
    ! Use assumed-shape arrays for flexibility
    real, intent(in) :: V(:,:,:,:)  ! Input array
    real, intent(out) :: H_mc(:,:,:,:)  ! Output array
    real :: V_flattened(size(V,1), size(V,2)*size(V,3)*size(V,4))
    real :: H(size(B    ,1), size(V_flattened,2))
    real :: s_com
    integer :: i, j

    ! Flatten V
    V_flattened = reshape(V, shape(V_flattened))

    ! Matrix multiplication: B @ V
    call gemm('N', 'N', size(B,1), size(V_flattened,2), size(B,2), 1.0, B, size(B,1), V_flattened, size(V_flattened,1), 0.0, H, size(H,1))

    ! Compute scaling factor
    s_com = sum(V_flattened) / sum(H)

    ! Scale and reshape H
    H = s_com * H
    H_mc = reshape(H, shape(V))
end subroutine

subroutine reconstruct_to_source_space(H_advected, V_reconstructed_mc)
    ! Use assumed-shape arrays for flexibility
    real, intent(in) :: H_advected(:,:,:,:)  ! Input array
    real, intent(out) :: V_reconstructed_mc(:,:,:,:)  ! Output array
    real :: H_advected_flattened(size(H_advected,1), size(H_advected,2)*size(H_advected,3)*size(H_advected,4))
    real :: V_reconstructed(size(W,1), size(H_advected_flattened,2))
    real :: s_rec
    integer :: i, j

    ! Flatten H_advected
    H_advected_flattened = reshape(H_advected, shape(H_advected_flattened))

    ! Matrix multiplication: W @ H
    call gemm('N', 'N', size(W,1), size(H_advected_flattened,2), size(W,2), 1.0, W, size(W,1), H_advected_flattened, size(H_advected_flattened,1), 0.0, V_reconstructed, size(V_reconstructed,1))

    ! Compute scaling factor
    s_rec = sum(H_advected_flattened) / sum(V_reconstructed)

    ! Scale and reshape V_reconstructed
    V_reconstructed = s_rec * V_reconstructed
    V_reconstructed_mc = reshape(V_reconstructed, shape(H_advected))
end subroutine

program nmp_update
    implicit none

    ! Declare variables
    real, allocatable :: V(:,:,:,:), H_mc(:,:,:,:)
    real, allocatable :: H_advected(:,:,:,:), V_reconstructed_mc(:,:,:,:)
    integer :: m, r, x, y, z, n

    ! Initialize dimensions (example values, should be set according to your data)
    m = 10; r = 5; x = 4; y = 3; z = 2
    n = x * y * z

    ! Allocate and initialize matrices (example initialization)
    allocate(V(m, x, y, z))
    allocate(H_mc(r, x, y, z))
    allocate(H_advected(r, x, y, z))
    allocate(V_reconstructed_mc(m, x, y, z))
    ! Initialize V, H_advected, and any other necessary data...

    ! Call subroutines
    call project_to_latent_space(V, H_mc)
    call reconstruct_to_source_space(H_advected, V_reconstructed_mc)

    ! H_mc and V_reconstructed_mc now hold the results

    ! Deallocate arrays and perform any cleanup
    deallocate(V, H_mc, H_advected, V_reconstructed_mc)

end program nmp_update


subroutine read_binary_file(filename, array, m, r)
    implicit none
    character(len=*), intent(in) :: filename
    real, intent(out) :: array(:,:)
    integer, intent(in) :: m, r
    integer :: iostat, file_unit

    ! Get a unique file unit number
    call get_unique_unit_number(file_unit)

    ! Open the binary file
    open(unit=file_unit, file=filename, form='unformatted', access='direct', recl=m*r*4, iostat=iostat)
    if (iostat /= 0) then
        print *, "Error opening file: ", filename
        stop
    endif

    ! Read the data
    read(unit=file_unit, rec=1, iostat=iostat) array
    if (iostat /= 0) then
        print *, "Error reading file: ", filename
        stop
    endif

    ! Close the file
    close(unit=file_unit)
end subroutine read_binary_file

subroutine get_unique_unit_number(unit)
    implicit none
    integer, intent(out) :: unit

    ! Find a free unit number (example implementation)
    unit = 10  ! Starting unit number
    do while (.true.)
        inquire(unit=unit, opened=opened)
        if (.not. opened) exit
        unit = unit + 1
    end do
end subroutine get_unique_unit_number