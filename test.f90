PROGRAM TEST
    IMPLICIT NONE
    LOGICAL :: ans = .FALSE.
    INTEGER :: n

    DO WHILE(ans .EQV. .FALSE.)
        READ(*,*) n
	IF (n==42) THEN
	    ans = .TRUE.
	ELSE
	    WRITE(*,*) n
	END IF
    END DO
END PROGRAM TEST
