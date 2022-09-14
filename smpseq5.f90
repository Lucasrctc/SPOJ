PROGRAM SMPSEQ5
    IMPLICIT NONE
    INTEGER :: m, n, anssize, i, j, prev
    INTEGER, DIMENSION(:), ALLOCATABLE :: s, q, ans

    READ(*,*) n
    ALLOCATE(s(1:n))
    READ(*,*) (s(i), i = 1, n)
    READ(*,*) m
    ALLOCATE(q(1:m))
    READ(*,*) (q(i), i = 1, m)
    anssize = 0
    prev = q(1)
    j = 1
    ALLOCATE(ans(1:n))
    DO i = 1, MIN(n, m)
        IF (s(i) .EQ. q(i)) THEN
            anssize = anssize + 1
            ans(anssize) = i
        END IF
    END DO
    WRITE(*,*) (ans(i), i=1, anssize)
END PROGRAM SMPSEQ5
