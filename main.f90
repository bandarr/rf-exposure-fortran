program rfe

    implicit none 

    type t_frequencyvalues
        real :: freq 
        real :: swr
        real :: gaindbi
    end type

    type t_cablevalues
        real :: k1
        real :: k2
    end type

    integer :: xmtr_power = 1000
    integer :: feedline_length = 73
    real    :: duty_cycle = .5
    real    :: per_30 = .5
    integer :: j 
    real    :: yarg

    type(t_frequencyvalues) :: current_freq_values

    type(t_cablevalues) cable_values
    
    type(t_frequencyvalues), dimension(6) :: all_frequency_values
    
    cable_values = t_cablevalues(0.122290, 0.000260)

    all_frequency_values = (/t_frequencyvalues(7.3, 2.25, 1.5), & 
        t_frequencyvalues(14.35, 1.35, 1.5), &
        t_frequencyvalues(18.1, 3.7, 1.5), &
        t_frequencyvalues(21.45, 4.45, 1.5), &
        t_frequencyvalues(24.99, 4.1, 1.5), &
        t_frequencyvalues(29.7, 2.18, 4.5)/)



    do j = 1, size(all_frequency_values)
        current_freq_values = all_frequency_values(j)             
        yarg = calc_uncontrolled_safe_distance( current_freq_values, &
                    cable_values, xmtr_power, feedline_length, duty_cycle, per_30 )
    
        write (*,"(f0.2)") yarg           
    end do

    contains 

        real function calc_reflection_coefficient(frequency_values) result(rval)
            type(t_frequencyvalues), intent(in) :: frequency_values
            rval = abs((frequency_values%swr - 1)/(frequency_values%swr + 1))
        end function calc_reflection_coefficient

        real function calc_feedline_loss_for_matched_load_at_frequency(feedline_length,&
         feedline_loss_per_100ft_at_frequency) result(rval)
            integer, intent(in) :: feedline_length
            real, intent(in)    :: feedline_loss_per_100ft_at_frequency
            rval = (feedline_length/100.0) * feedline_loss_per_100ft_at_frequency
        end function calc_feedline_loss_for_matched_load_at_frequency
    
        real function calc_feedline_loss_for_matched_load_at_frequency_pct(feedline_loss_for_matched_load) result (rval)
            real, intent(in) ::  feedline_loss_for_matched_load
            rval = 10**( (0 - feedline_loss_for_matched_load)/10.0)
        end function calc_feedline_loss_for_matched_load_at_frequency_pct
        
        real function calc_feedline_loss_per_100ft_at_frequency(frequency_values, cable_values) result(rval)
            type(t_frequencyvalues), intent(in) :: frequency_values
            type(t_cablevalues), intent(in) :: cable_values
            rval = cable_values%k1 * sqrt(frequency_values%freq + cable_values%k2 * frequency_values%freq)
        end function calc_feedline_loss_per_100ft_at_frequency

        real function calc_feedline_loss_for_swr (feedline_loss_for_matched_load_percentage, gamma_squared) result(rval)
            real, intent(in) :: feedline_loss_for_matched_load_percentage, gamma_squared
            rval = -10 * log10(feedline_loss_for_matched_load_percentage * &
                ((1 - gamma_squared)/(1 - feedline_loss_for_matched_load_percentage**2 * gamma_squared)) )
        end function calc_feedline_loss_for_swr

        real function calc_feedline_loss_for_swr_percentage(feedline_loss_for_swr) result(rval)
            real, intent(in) :: feedline_loss_for_swr
            rval = ( 100 - 100/( 10**( feedline_loss_for_swr/10 ) ) )/100.0
        end function calc_feedline_loss_for_swr_percentage

        real function calc_uncontrolled_safe_distance(freq_values, cable_values, transmitter_power, &
            feedline_length, duty_cycle, uncontrolled_percentage_30_minutes) result(rval)
            type(t_frequencyvalues), intent(in) :: freq_values
            type(t_cablevalues), intent(in)     :: cable_values
            integer, intent(in)                 :: transmitter_power
            integer, intent(in)                 :: feedline_length
            real, intent(in)           :: duty_cycle
            real, intent(in)           :: uncontrolled_percentage_30_minutes

            real :: gamma, gamma_squared 
            real :: feedline_loss_per_100ft_at_frequency, feedline_loss_for_matched_load_at_frequency, & 
                feedline_loss_for_matched_load_at_frequency_percentage    
            real :: feedline_loss_for_swr, feedline_loss_for_swr_percentage, power_loss_at_swr
            real :: uncontrolled_average_pep, peak_envelope_power_at_antenna, gain_decimal, mpe_s

            gamma = calc_reflection_coefficient(freq_values)

            feedline_loss_per_100ft_at_frequency = calc_feedline_loss_per_100ft_at_frequency(freq_values, cable_values)
    
            feedline_loss_for_matched_load_at_frequency = &
                calc_feedline_loss_for_matched_load_at_frequency(feedline_length, feedline_loss_per_100ft_at_frequency)

            feedline_loss_for_matched_load_at_frequency_percentage = & 
                calc_feedline_loss_for_matched_load_at_frequency_pct(feedline_loss_for_matched_load_at_frequency)
    
            gamma_squared = abs(gamma)**2.0
    
            feedline_loss_for_swr = &
                calc_feedline_loss_for_swr(feedline_loss_for_matched_load_at_frequency_percentage, gamma_squared)
    
            feedline_loss_for_swr_percentage = calc_feedline_loss_for_swr_percentage( feedline_loss_for_swr )
    
            power_loss_at_swr = feedline_loss_for_swr_percentage * transmitter_power
    
            peak_envelope_power_at_antenna = transmitter_power - power_loss_at_swr
    
            uncontrolled_average_pep = peak_envelope_power_at_antenna * duty_cycle * uncontrolled_percentage_30_minutes
    
            mpe_s = 180/( freq_values%freq**2 )
    
            gain_decimal = 10**( freq_values%gaindbi/10.0 )

            rval = sqrt( ( 0.219 * uncontrolled_average_pep * gain_decimal )/mpe_s )

        end function calc_uncontrolled_safe_distance

end program rfe