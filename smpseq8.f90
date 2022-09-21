PROGRAM SMPSEQ8
    IMPLICIT NONE
    INTEGER :: m, n, anssize, i, j, prev, sq, ss
    INTEGER, DIMENSION(:), ALLOCATABLE :: s, q, ans

    READ(*,*) n
    ALLOCATE(s(1:n))
    READ(*,*) (s(i), i = 1, n)
    READ(*,*) m
    ALLOCATE(q(1:m))
    READ(*,*) (q(i), i = 1, m)
    ss = 0
    DO i = 1, n
        ss = ss + s(i)
    END DO
    sq = 0
    DO j = 1, m
        sq = sq + q(j)
    END DO
    IF (ss>sq) THEN
        WRITE(*,*) (s(i), i=1, n)
    ELSE
        WRITE(*,*) (q(i), i=1, m)
    END IF
END PROGRAM SMPSEQ8
