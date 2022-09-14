PROGRAM SMPSEQ6
    IMPLICIT NONE
    INTEGER :: m, n, anssize, i, j, prev
    INTEGER, DIMENSION(:), ALLOCATABLE :: s, q, ans

    READ(*,*) n, m
    ALLOCATE(s(1:n))
    READ(*,*) (s(i), i = 1, n)
    ALLOCATE(q(1:n))
    READ(*,*) (q(i), i = 1, n)
    ALLOCATE(ans(1:n))
    anssize = 0
    prev = q(1)
    j = 1
    DO i = 1, n
        DO j = -m, m
            IF (s(i) .EQ. q(MAX(1, MIN(n, i+j)))) THEN
                anssize = anssize + 1
                ans(anssize) = i
            END IF
        END DO
    END DO
    WRITE(*,*) (ans(i), i=1, anssize)
END PROGRAM SMPSEQ6
