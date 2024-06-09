! Acceleration of advection scheme by dimensionality reduction for tracers

MODULE mo_advection_ssai

    USE mo_util_ftorch_api,     ONLY: ftorch_net
    USE ftorch

    IMPLICIT NONE

    PRIVATE

    TYPE :: ssai
        ! Class for SuperSpecies AI scheme and asscoiated subroutines

        private
        ! Initialise attribute for the class object
        type(ftorch_net)        :: encode_net
        type(ftorch_net)        :: decode_net
        INTEGER                 :: nt, nst      ! Number of tracers and super tracers

        CONTAINS
        ! Pass submethods from the class attributes to self
        PROCEDURE :: encode_net%load_model  =>  load_encoder
        PROCEDURE :: encode_net%emulate     =>  projection
        PROCEDURE :: decode_net%load_model  =>  load_decoder
        PROCEDURE :: decode_net%emulate     =>  reconstruction

        ! Define self attributes from subroutines in this module
        PROCEDURE :: preprocessWW
    END TYPE ssai

    PUBLIC :: ssai

    CHARACTER(len=*), PARAMETER :: modname = 'mo_advection_ssai'

    !-------------------------------------------------------------------------
    
    CONTAINS
    

    !-------------------------------------------------------------------------
    ! Subroutine of the module
    SUBROUTINE compression_ratio
        ! Load the compression ratio

        USE mo_util,            ONLY: config        ! Place holder for the config loaded from host model
        USE mo_art_util,        ONLY: art_config    ! Place holder for config loaded from ART

        INTEGER, INTENT(INOUT)  ::  nt      ! Nubmer of Tracer being advected
        INTEGER, INTENT(INOUT)  ::  nst     ! Number of super tracer

        nst = art_config%n_super    ! Loade the number of superspecies

    END SUBROUTINE compression_ratio



END MODULE mo_advection_ssai