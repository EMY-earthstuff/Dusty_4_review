FUNCTION beta_val(  Model, Node, InputArray) RESULT(beta)
  ! provides you with most Elmer functionality
  USE DefUtils
  ! saves you from stupid errors
  IMPLICIT NONE
  ! the external variables
  !----------------------------------------------------------------------------
  TYPE(Model_t) :: Model     ! the access point to everything about the model
  INTEGER :: Node            ! the current Node number
  REAL(KIND=dp) :: InputArray(4) ! Contains the arguments passed to the function
  REAL(KIND=dp) :: beta      ! the result
  REAL(KIND=dp) :: coordxs, coordys, bf_ids
  !----------------------------------------------------------------------------
  ! internal variables
  !----------------------------------------------------------------------------
  REAL(KIND=dp) :: Bq, Bsmax, m_surge, b_surge, m_q, b_q, Bq_NA, Bq_SA, Bq_MT, Bs, &
       inittime, time, elevation, cutoff
  LOGICAL :: FirstTime=.TRUE.
  ! Remember this value
  SAVE FirstTime, inittime


  ! lets hard-code our values (if we have time we can later make them being read from SIF)
!575e-6
  Bq_NA = 0.1_dp
  Bq_SA = 0.001_dp
  Bq_MT = 0.001_dp
  Bs = 0.001_dp
  Bsmax = 0.0000645_dp
  m_surge = -49.95_dp
  b_surge = 1148.95_dp
  m_q = -0.495_dp
  b_q = 10.495_dp
  
  ! copy input (should match the arguments!)
  time = InputArray(1)
  Coordxs = InputArray(2)
  Coordys = InputArray(3)
  bf_ids = InputArray(4)
  !WRITE (Message, '(A,E10.2,A,E10.2)')  "time=", time, "Coordxs=", Coordxs, "Coordys=", Coordys
  CALL INFO("beta_val", Message, Level=9)

  ! store the initial time, to be sure to have relative times
  IF (FirstTime) THEN
     inittime = time
     FirstTime = .FALSE.
  END IF


  ! get the time dependent beta values
  IF (bf_ids == 0.0) THEN
     beta = 0.01
  ELSE IF (bf_ids == 2.0) THEN
     beta = Bq_MT
  !ELSE IF (bf_ids == 4.0) THEN !f
  !   beta = Bq_MT               !f
  ELSE 
    IF (time < 40.1) THEN
      
      !beta = Bq_NA
      IF (bf_ids == 1.0) THEN
        beta = Bq_MT
      ELSE IF (bf_ids == 4.0) THEN !f
        beta = Bq_MT                !f
      ELSE
        beta = Bq_NA
      END IF

!3a surge
!20.1 21.9 or 20.1 22.9
!    ELSE IF (100.1 <= time .AND. time <= 102.9) THEN
!      beta = Bsmax
!62.1 63.9 or 83.1 ad 85.9
!    ELSE IF (143.1 <= time .AND. time <= 145.9) THEN
!      beta = Bsmax
!    ELSE IF (186.1 <= time .AND. time <= 188.9) THEN
!      beta = Bsmax

!1+2a surge
    ELSE IF (40.1 <= time .AND. time < 41.0) THEN

      IF (bf_ids == 1.0) THEN
        beta = Bq_MT
      ELSE IF (bf_ids == 4.0) THEN !f
        beta = Bq_MT                !f
      ELSE
        beta = Bsmax
      END IF

    ELSE IF (83.1 <= time .AND. time < 84.0) THEN

      IF (bf_ids == 1.0) THEN
        beta = Bq_MT
      ELSE IF (bf_ids == 4.0) THEN !f
        beta = Bq_MT                !f
      ELSE
        beta = Bsmax
      END IF

    ELSE IF (126.1 <= time .AND. time <= 127.0) THEN

      IF (bf_ids == 1.0) THEN
        beta = Bq_MT
      ELSE IF (bf_ids == 4.0) THEN !f
        beta = Bq_MT                !f
      ELSE
        beta = Bsmax
      END IF

!MAIN SURGE
    ELSE IF (41.0 <= time .AND. time <= 42.9) THEN
      beta = Bsmax
    ELSE IF (84.0 <= time .AND. time <= 85.9) THEN
      beta = Bsmax
    ELSE IF (127.0 <= time .AND. time <= 128.9) THEN
      beta = Bsmax


!2a surge
!    ELSE IF (100.1 <= time .AND. time <= 101.9) THEN
!      beta = Bsmax
!    ELSE IF (142.1 <= time .AND. time <= 143.9) THEN
!      beta = Bsmax
!    ELSE IF (184.1 <= time .AND. time <= 185.9) THEN
!      beta = Bsmax


    ELSE 
      !beta = Bq_NA


      IF (bf_ids == 1.0) THEN
        beta = Bq_MT
      ELSE IF (bf_ids == 4.0) THEN !f
        beta = Bq_MT                !f
      ELSE
        beta = Bq_NA
      END IF


    END IF
  END IF

  ! set the x and y dependent beta values


  RETURN

END FUNCTION beta_val
