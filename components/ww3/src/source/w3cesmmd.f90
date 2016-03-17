!/ ------------------------------------------------------------------- /
      MODULE W3CESMMD

!/ ------------------------------------------------------------------- /
!/
      private
      character(len=256),public :: casename
      ! QL, 150823, flag for restart
      logical,public :: rstwr ! true => write restart at end of day
      ! QL, 150925, runtype now used by W3SRCE
      character(len=16),public :: runtype
!/
!/ End of module CESMMD ---------------------------------------------- /
!/
      END MODULE W3CESMMD
