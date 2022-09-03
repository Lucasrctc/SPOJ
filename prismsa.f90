PROGRAM TEST
    IMPLICIT NONE
    REAL :: V, a, h, S
    REAL :: sq3
    INTEGER :: n = 5, i
    !INTEGER, DIMENSION(5) :: inpdata

    !inpdata(1) = 10
    !inpdata(2) = 5
    !inpdata(3) = 100
    !inpdata(4) = 245
    !inpdata(5) = 5421

    INTEGER, DIMENSION(:), ALLOCATABLE :: inpdata
    READ(*,*) n
    ALLOCATE(inpdata(1:n))
    DO i = 1, n
        READ(*,*) inpdata(i)
    END DO
    sq3 = SQRT(3.0)
    DO i = 1, n
        a = (4*inpdata(i))**(1.0/3.0)
        h = 4*inpdata(i)/(sq3*a**2)
        WRITE(*,*) a**2*sq3/2 +3*a*h
    END DO
END PROGRAM TEST
