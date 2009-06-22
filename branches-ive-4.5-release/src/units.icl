c
c
      integer MAX_UNITS, UNIT_LEN
      parameter ( MAX_UNITS = 4, UNIT_LEN = 80) 
      character *(UNIT_LEN)  domain_units, domain_display_units,
     &                       data_units, data_display_units
      real*8 data_intercept, data_slope, domain_intercept, domain_slope
      dimension domain_units(MAX_UNITS),domain_display_units(MAX_UNITS),
     &          domain_intercept(MAX_UNITS), domain_slope(MAX_UNITS)
c
      common / units_comm / data_intercept, data_slope, 
     &                      domain_intercept, domain_slope
      common / units_comm_char / domain_units, domain_display_units, 
     &                      data_units, data_display_units
