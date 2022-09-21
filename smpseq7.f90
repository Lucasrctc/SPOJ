PROGRAM SMPSEQ7
    IMPLICIT NONE
    INTEGER :: m, n, anssize, i, j, prev
    INTEGER, DIMENSION(:), ALLOCATABLE :: s, q
    LOGICAL :: change = .FALSE., ans = .TRUE.
    READ(*,*) n
    ALLOCATE(s(1:n))
    READ(*,*) (s(i), i = 1, n)
    DO i = 1, n - 1
        IF (s(i) .le. s(i+1)) THEN
            change = .TRUE.
            EXIT
        END IF
    END DO
    DO j = i + 1, n - 1
        IF (s(j) .ge. s(j+1)) THEN
            ans = .FALSE.
            EXIT
        END IF
    END DO
    IF (ans .eqv. .TRUE.) THEN
        WRITE(*,*) "Yes"
    ELSE
        WRITE(*,*) "No"
    END IF
END PROGRAM SMPSEQ7
