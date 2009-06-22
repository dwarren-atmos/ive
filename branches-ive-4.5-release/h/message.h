#define MAX_MESSAGE_LENGTH 256

struct {
  char message[MAX_MESSAGE_LENGTH];
}com_message_;

struct {
  int warn_timeout[1];
} com_imessage_;
