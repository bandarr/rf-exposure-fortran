
program rfe

    implicit none 

    type frequencyvalues
        real :: freq 
        real :: swr
        real :: gaindbi
    end type

    type cablevalues
        real :: k1
        real :: k2
    end type

    contains 

        function calculate_reflection_coefficient( freq_values )
            real calculate_reflection_coefficient   
            calculate_reflection_coefficient = abs((freq_values.swr - 1)/(freq_values%swr + 1))
        end function calculate_reflection_coefficient

    real    :: xmtr_power = 1000
    integer :: feed_line_length = 73
    real    :: duty_cycle = .5
    real    :: per_30 = .5

    type(cablevalues) cable_values
    cable_values = cablevalues(0.122290, 0.000260)

    type(frequencyvalues), dimension(6) :: freq_values

end program rfe



real function CalculateFeedlineLossForMatchedLoadAtFrequency(feedline_length, feedline_loss_per_100ft_at_frequency)

end function CalculateFeedlineLossForMatchedLoadAtFrequency

real function CalculateFeedlineLossPer100ftAtFrequency(frequency_values, cable_values)

end function CalculateFeedlineLossPer100ftAtFrequency

real function CalculateFeedlineLossForSWR (feedline_loss_for_matched_load_percentage, gamma_squared)

end function CalculateFeedlineLossForSWR

real function CalculateFeedlineLossForSWRPercentage(feedline_loss_for_swr)

end function CalculateFeedlineLossForSWRPercentage

real function CalculateUncontrolledSafeDistance(freq_values, cable_values, transmitter_power, feedline_length, uncontrolled_percentage_30_minutes)
real:: uncontrolled_safe_distance

CalculateUncontrolledSafeDistance = uncontrolled_safe_distance
end function CalculateUncontrolledSafeDistance


