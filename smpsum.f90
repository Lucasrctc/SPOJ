PROGRAM SMPSUM
    IMPLICIT NONE
    INTEGER :: a, b, s, i

    READ(*,*) a, b
    DO i=a, b
        s = s + i*i
    END DO
    WRITE(*,*) s
END PROGRAM SMPSUM
