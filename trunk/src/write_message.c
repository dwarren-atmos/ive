/*--------------------------------------------------------------
 *$Id: write_message.c,v 1.1 2001/10/15 23:11:45 davidive Exp $
 *$Log: write_message.c,v $
 *Revision 1.1  2001/10/15 23:11:45  davidive
 *write_message is now in C instead of FORTRAN
 *
 *-------------
 *These routine write messages in a widget using the string in 
 *com_message_.message
 *--------------------------------------------------------------*/
#include <message.h>

void write_info_(){
  char *beg, *end;

  beg=com_message_.message;
  while((*beg==' '||*beg=='\0') && beg <= com_message_.message +(long)256)
    beg++;
  end=com_message_.message+256;  
  while((*end==' '||*end=='\0') && end >= com_message_.message)
    end--;
  if(end < com_message_.message + (long)256)
    *end='\0';
  else
    com_message_.message[255]='\0';
  (void)make_info_widget_ (com_message_.message);

}

void write_message_(){
  char *beg, *end;

  beg=com_message_.message;
  while((*beg==' '||*beg=='\0') && beg <= com_message_.message +(long)256)
    beg++;
  end=com_message_.message+256;  
  while((*end==' '||*end=='\0') && end >= com_message_.message)
    end--;
  if(end < com_message_.message + (long)256)
    *end='\0';
  else
    com_message_.message[255]='\0';
  (void)make_help_widget_ (com_message_.message);

}
