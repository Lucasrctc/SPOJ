PROGRAM LASTDIG
    IMPLICIT NONE
    INTEGER :: n, ans, i, a, b, c, temp

    READ(*,*) n
    DO i = 1, n
        ans = 0
        READ(*,*) a, b
        a = MOD(a, 10)
        temp = a
        c = 1
        temp = MOD(temp*a, 10)
        DO WHILE(temp .ne. a)
            temp = MOD(temp*a, 10)
            c = c + 1
        END DO
        temp = MOD(b, c)
        ans = mod(a**temp, 10)
        IF (c .eq. 1) THEN
            ans = a
        END IF
        IF (temp .eq. 0) THEN
            ans = mod(a**c, 10)
        END IF
        IF (b .eq. 0) THEN
            ans = 1
        END IF
        WRITE(*, *) 'a: ', a,'b: ', b,'c: ', c,'b%c: ', temp,'ans: ', ans, 'tag: ', ans - MOD(a**b, 10)
        !WRITE(*, *) ans 
    END DO
END PROGRAM LASTDIG
