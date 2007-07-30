c
c
      integer MAX_MESSAGE_LENGTH
      parameter ( MAX_MESSAGE_LENGTH = 256 )
      character *(MAX_MESSAGE_LENGTH) message
      common / com_message / message
      integer warn_timeout
      common / com_imessage/ warn_timeout
c
c
