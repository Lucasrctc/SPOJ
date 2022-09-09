PROGRAM ALCATRAZ1
    IMPLICIT NONE
    INTEGER :: n = 5, ans, i, j
    CHARACTER(50) :: digitlist

    READ(*,*) n
    DO i = 1, n
        ans = 0
        READ(*,*) digitlist
        DO j = 1, LEN(digitlist)
            IF (digitlist(j:j) .ne. ' ') THEN
                ans = ans + ICHAR(digitlist(j:j)) - ICHAR('0')
            END IF
        END DO
        WRITE(*,*) ans
    END DO
END PROGRAM ALCATRAZ1
