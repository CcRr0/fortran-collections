MODULE linked_list
    IMPLICIT NONE
    PUBLIC :: APPEND, PREPEND, REMOVE_HEAD, REMOVE_TAIL, DEALLOCATE_LIST

    TYPE :: NODE
        CLASS(*), ALLOCATABLE :: VALUE
        TYPE(NODE), POINTER :: NEXT => NULL(), PREV => NULL()
    END TYPE NODE

    TYPE :: LIST
        TYPE(NODE), POINTER :: HEAD => NULL(), TAIL => NULL()
        INTEGER :: SIZE = 0
    END TYPE LIST

CONTAINS
    SUBROUTINE APPEND(LI, VALUE)
        TYPE(LIST), INTENT(INOUT) :: LI
        CLASS(*), INTENT(IN) :: VALUE

        TYPE(NODE), POINTER :: ND

        ALLOCATE (ND)
        ALLOCATE (ND%VALUE, SOURCE=VALUE)

        IF (LI%SIZE == 0) THEN
            LI%HEAD => ND
            LI%TAIL => ND
        ELSE
            LI%TAIL%NEXT => ND
            ND%PREV => LI%TAIL
            LI%TAIL => ND
        END IF

        LI%SIZE = LI%SIZE + 1

    END SUBROUTINE APPEND

    SUBROUTINE PREPEND(LI, VALUE)
        TYPE(LIST), INTENT(INOUT) :: LI
        CLASS(*), INTENT(IN) :: VALUE

        TYPE(NODE), POINTER :: ND

        ALLOCATE (ND)
        ALLOCATE (ND%VALUE, SOURCE=VALUE)

        IF (LI%SIZE == 0) THEN
            LI%HEAD => ND
            LI%TAIL => ND
        ELSE
            LI%HEAD%PREV => ND
            ND%NEXT => LI%HEAD
            LI%HEAD => ND
        END IF

        LI%SIZE = LI%SIZE + 1

    END SUBROUTINE PREPEND

    SUBROUTINE REMOVE_HEAD(LI)
        TYPE(LIST), INTENT(INOUT) :: LI

        TYPE(NODE), POINTER :: TEMP

        TEMP => LI%HEAD
        LI%HEAD => LI%HEAD%NEXT

        IF (ASSOCIATED(LI%HEAD)) THEN
            LI%HEAD%PREV => NULL()
        ELSE
            LI%TAIL => NULL()
        END IF

        LI%SIZE = LI%SIZE - 1

        DEALLOCATE (TEMP%VALUE)
        DEALLOCATE (TEMP)

    END SUBROUTINE REMOVE_HEAD

    SUBROUTINE REMOVE_TAIL(LI)
        TYPE(LIST), INTENT(INOUT) :: LI

        TYPE(NODE), POINTER :: TEMP

        TEMP => LI%TAIL
        LI%TAIL => LI%TAIL%PREV

        IF (ASSOCIATED(LI%TAIL)) THEN
            LI%TAIL%NEXT => NULL()
        ELSE
            LI%HEAD => NULL()
        END IF

        LI%SIZE = LI%SIZE - 1

        DEALLOCATE (TEMP%VALUE)
        DEALLOCATE (TEMP)

    END SUBROUTINE REMOVE_TAIL

    SUBROUTINE DEALLOCATE_LIST(LI)
        TYPE(LIST), INTENT(INOUT) :: LI

        TYPE(NODE), POINTER :: ND

        DO WHILE (ASSOCIATED(LI%HEAD))
            ND => LI%HEAD
            LI%HEAD => LI%HEAD%NEXT

            DEALLOCATE (ND%VALUE)
            DEALLOCATE (ND)
        END DO

        LI%TAIL => NULL()
        LI%SIZE = 0

    END SUBROUTINE DEALLOCATE_LIST

END MODULE linked_list
