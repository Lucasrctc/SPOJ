PROGRAM FCTRL
    IMPLICIT NONE
    INTEGER :: n = 5, ans, i
    INTEGER, DIMENSION(:), ALLOCATABLE :: v

    READ(*,*) n
    ALLOCATE(v(1:n))
    READ(*,*) (v(i), i = 1, n)
    DO i = 1, n
        ans = 0
        DO WHILE(v(i) .GT. 0)
            v(i) = v(i) / 5
            ans = ans + v(i)
        END DO
        WRITE(*,*) ans
    END DO
END PROGRAM FCTRL
