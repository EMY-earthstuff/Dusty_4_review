FUNCTION MB_funk(  Model, Node, InputArray) RESULT(b_l)
  ! provides you with most Elmer functionality
  USE DefUtils
  ! saves you from stupid errors
  IMPLICIT NONE
  ! the external variables
  !----------------------------------------------------------------------------
  TYPE(Model_t) :: Model     ! the access point to everything about the model
  INTEGER :: Node            ! the current Node number
  REAL(KIND=dp) :: InputArray(3) ! Contains the arguments passed to the function
  REAL(KIND=dp) :: b_l      ! the result
  REAL(KIND=dp) :: MB_in, bf_id
  !----------------------------------------------------------------------------
  ! internal variables
  !----------------------------------------------------------------------------
  REAL(KIND=dp) :: res_factor, main_factor, b, m, time, inittime
  LOGICAL :: FirstTime=.TRUE.
 !  Remember this value
  SAVE FirstTime, inittime

  ! lets hard-code our values (if we have time we can later make them being read from SIF)
! 106SS = 1.4 & 105SS = 1.0   &   506 = 1.1
  res_factor = 2.32_dp
  main_factor = 0.75_dp
  m = 0.00015_dp
  b = 1.8_dp
  
  ! copy input (should match the arguments!)
  
  time = InputArray(1)
  MB_in = InputArray(2)
  bf_id = InputArray(3)
  !WRITE (Message, '(A,E10.2,A,E10.2)')  "time=", time, "Coordxs=", Coordxs, "Coordys=", Coordys
  CALL INFO("MB", Message, Level=9)

  ! store the initial time, to be sure to have relative times
  IF (FirstTime) THEN
    inittime = time
     FirstTime = .FALSE.
  END IF


  ! get the time dependent beta values
    ! 2.0/1.0 for 575e-6 case
  IF (time < 40.1) THEN
     IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.5) + (main_factor*0.5) + (res_factor*0.5)
     ELSE
       b_l = (MB_in*0.5) + (main_factor*0.5)
     END IF

!!DURING SURGES
  ELSE IF (40.1 <= time .AND. time <= 42.9) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.05) + (main_factor*0.05) + (res_factor*0.05)
    ELSE
       b_l = (MB_in*0.05) + (main_factor*0.05)
    END IF

  ELSE IF (83.1 <= time .AND. time <= 85.9) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.05) + (main_factor*0.05) + (res_factor*0.05)
    ELSE
       b_l = (MB_in*0.05) + (main_factor*0.05)
    END IF

  ELSE IF (126.1 <= time .AND. time <= 128.9) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.05) + (main_factor*0.05) + (res_factor*0.05)
    ELSE
       b_l = (MB_in*0.05) + (main_factor*0.05)
    END IF

!!!POST SURGE TERMINUS ADVANCE
  ELSE IF (42.9 < time .AND. time <= 53.0) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.125) + (main_factor*0.125) + (res_factor*0.125)
    ELSE
       b_l = (MB_in*0.125) + (main_factor*0.125)  
    END IF
  ELSE IF (85.9 < time .AND. time <= 96.0) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.125) + (main_factor*0.125) + (res_factor*0.125)
    ELSE
       b_l = (MB_in*0.125) + (main_factor*0.125)  
    END IF
  ELSE IF (128.9 < time .AND. time <= 139.0) THEN

    IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.125) + (main_factor*0.125) + (res_factor*0.125)
    ELSE
       b_l = (MB_in*0.125) + (main_factor*0.125)  
    END IF
!  ELSE IF (100.1 <= time .AND. time <= 101.9) THEN
!     b_l = MB_in*0.05
!  ELSE IF (142.1 <= time .AND. time <= 143.9) THEN
!     b_l = MB_in*0.05
!  ELSE IF (184.1 <= time .AND. time <= 185.9) THEN
!     b_l = MB_in*0.05


  ELSE 
     IF (bf_id == 3.0) THEN
       b_l = (MB_in*0.5) + (main_factor * 0.5)  + (res_factor*0.5)
     ELSE
       b_l = (MB_in*0.5) + (main_factor * 0.5)
     END IF
  END IF

  ! set the x and y dependent beta values


  RETURN

END FUNCTION MB_funk
