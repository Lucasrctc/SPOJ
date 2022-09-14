PROGRAM SMPSEQ3
    IMPLICIT NONE
    INTEGER :: m, n, anssize, i, j, prev
    INTEGER, DIMENSION(:), ALLOCATABLE :: s, q, ans

    READ(*,*) n
    ALLOCATE(s(1:n))
    READ(*,*) (s(i), i = 1, n)
    READ(*,*) m
    ALLOCATE(q(1:m + 1))
    READ(*,*) (q(i), i = 1, m)
    q(m + 1) = s(n) + 1
    anssize = 1
    prev = q(1)
    j = 1
    ALLOCATE(ans(1:n))
    DO i = 1, n
        DO WHILE(s(i) .GE. q(j))
            prev = q(j)
            j = j + 1
        END DO
        IF (s(i) .NE. prev) THEN
            ans(anssize) = s(i)
            anssize = anssize + 1
        END IF
    END DO
    WRITE(*,*) (ans(i), i=1, anssize - 1)
END PROGRAM SMPSEQ3
