program rfe

    implicit none 

    type frequencyvalues
        real freq 
        real swr
        real gaindbi
    end type

    type cablevalues
        real :: k1
        real :: k2
    end type

    integer :: xmtr_power = 1000
    integer :: feed_line_length = 73
    real    :: duty_cycle = .5
    real    :: per_30 = .5
    integer :: j 
    real    :: yarg

    type(frequencyvalues) :: current_freq_values

    type(cablevalues) cable_values
    
    type(frequencyvalues), dimension(6) :: all_frequency_values
    
    cable_values = cablevalues(0.122290, 0.000260)

    all_frequency_values = (/frequencyvalues(freq=7.3, swr=2.25, gaindbi=1.5), & 
        frequencyvalues(14.35, 1.35, 1.5), &
        frequencyvalues(21.45, 4.45, 1.5), &
        frequencyvalues(21.45, 4.45, 1.5), &
        frequencyvalues(24.99, 4.1, 1.5), &
        frequencyvalues(29.7, 2.18, 4.5)/)

    do j = 1, size(all_frequency_values)
        current_freq_values = all_frequency_values(j)
        yarg = calculate_uncontrolled_safe_distance( current_freq_values, &
                    cable_values, xmtr_power, feed_line_length, duty_cycle, per_30 )
    
        write (*,"(f8.2)") yarg           
    end do

    contains 

        real function calculate_reflection_coefficient( frequency_values )
            type(frequencyvalues) frequency_values 
            calculate_reflection_coefficient = abs((frequency_values%swr - 1)/(frequency_values%swr + 1))
        end function calculate_reflection_coefficient

        real function calculate_feed_line_loss_for_matched_load_at_frequency(feedline_length, feedline_loss_per_100ft_at_frequency)
            integer feedline_length
            real feedline_loss_per_100ft_at_frequency
            calculate_feed_line_loss_for_matched_load_at_frequency = (feedline_length/100) * feedline_loss_per_100ft_at_frequency
        end function calculate_feed_line_loss_for_matched_load_at_frequency
    
        real function calculate_feed_line_loss_for_matched_load_at_frequency_pct(feedline_loss_for_matched_load)
            real feedline_loss_for_matched_load
            calculate_feed_line_loss_for_matched_load_at_frequency_pct = 10**( -feedline_loss_for_matched_load/10)
        end function calculate_feed_line_loss_for_matched_load_at_frequency_pct
        
        real function calculate_feed_line_loss_per_100ft_at_frequency(frequency_values, cable_values)
            type(frequencyvalues) frequency_values
            type(cablevalues) cable_values
            calculate_feed_line_loss_per_100ft_at_frequency = &
                cable_values%k1 * sqrt(frequency_values%freq + cable_values%k2 * frequency_values%freq)
        end function calculate_feed_line_loss_per_100ft_at_frequency

        real function calculate_feed_line_loss_for_swr (feedline_loss_for_matched_load_percentage, gamma_squared)
            real feedline_loss_for_matched_load_percentage, gamma_squared
            calculate_feed_line_loss_for_swr = -10 * log10(feedline_loss_for_matched_load_percentage * &
                ((1 - gamma_squared)/(1 - feedline_loss_for_matched_load_percentage**2 * gamma_squared)) )
        end function calculate_feed_line_loss_for_swr

        real function calculate_feed_line_loss_for_swr_percentage(feedline_loss_for_swr)
            real feedline_loss_for_swr
            calculate_feed_line_loss_for_swr_percentage = ( 100 - 100/( 10**( feedline_loss_for_swr/10 ) ) )/100
        end function calculate_feed_line_loss_for_swr_percentage

        real function calculate_uncontrolled_safe_distance(freq_values, cable_values, transmitter_power, &
            feedline_length, duty_cycle, uncontrolled_percentage_30_minutes)
            type(frequencyvalues) freq_values
            type(cablevalues) cable_values
            integer transmitter_power
            integer feedline_length
            real duty_cycle
            real uncontrolled_percentage_30_minutes

            real gamma, gamma_squared 
            real feed_line_loss_per_100ft_at_frequency, feed_line_loss_for_matched_load_at_frequency, & 
                feed_line_loss_for_matched_load_at_frequency_percentage    
            real feed_line_loss_for_swr, feed_line_loss_for_swr_percentage, power_loss_at_swr
            real uncontrolled_average_pep, peak_envelope_power_at_antenna, gain_decimal, mpe_s

            gamma = calculate_reflection_coefficient(freq_values)

            feed_line_loss_per_100ft_at_frequency = & 
                calculate_feed_line_loss_per_100ft_at_frequency(freq_values, cable_values)
    
            feed_line_loss_for_matched_load_at_frequency = &
                calculate_feed_line_loss_for_matched_load_at_frequency(feedline_length, feed_line_loss_per_100ft_at_frequency)
    
            feed_line_loss_for_matched_load_at_frequency_percentage = & 
                calculate_feed_line_loss_for_matched_load_at_frequency_pct(feed_line_loss_for_matched_load_at_frequency)
    
            gamma_squared = abs(gamma)**2
    
            feed_line_loss_for_swr = &
                calculate_feed_line_loss_for_swr(feed_line_loss_for_matched_load_at_frequency_percentage, gamma_squared)
    
            feed_line_loss_for_swr_percentage = & 
                calculate_feed_line_loss_for_swr_percentage( feed_line_loss_for_swr )
    
            power_loss_at_swr = feed_line_loss_for_swr_percentage * transmitter_power
    
            peak_envelope_power_at_antenna = transmitter_power - power_loss_at_swr
    
            uncontrolled_average_pep = peak_envelope_power_at_antenna * duty_cycle * uncontrolled_percentage_30_minutes
    
            mpe_s = 180/( freq_values%freq**2 )
    
            gain_decimal = 10**( freq_values%gaindbi/10 )

            calculate_uncontrolled_safe_distance = sqrt( ( 0.219 * uncontrolled_average_pep * gain_decimal )/mpe_s )

        end function calculate_uncontrolled_safe_distance

end program rfe