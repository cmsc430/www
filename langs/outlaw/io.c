#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <wchar.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

#define port_buffer_bytes 8

void utf8_encode_string(val_str_t *, char *);

val_t read_byte(void)
{
  char c = getc(in);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int((unsigned char)c);  
}

val_t peek_byte(void)
{
  char c = getc(in);
  ungetc(c, in);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int(c);
  
}

val_t write_byte(val_t c)
{
  putc((char) val_unwrap_int(c), out);
  return val_wrap_void();
}

val_t read_char(void)
{
  wchar_t c = getwc(in);
  return (c == WEOF) ? val_wrap_eof() : val_wrap_char(c);
}

val_t peek_char(void)
{
  wchar_t c = getwc(in);
  ungetwc(c, in);
  return (c == WEOF) ? val_wrap_eof() : val_wrap_char(c);
}

val_t write_char(val_t c)
{
  putwc((wchar_t) val_unwrap_char(c), out);
  return val_wrap_void();
}

val_t open_input_file(val_t in) {
  FILE *f;
  char *buf;
  val_str_t* fn = val_unwrap_str(in);
  buf = calloc((fn->len*4)+1, 1);
  if (!buf)
    error_handler();
  utf8_encode_string(fn, buf);

  f = fopen(buf, "rb");
  if (!f)
    error_handler();

  free(buf);

  val_symb_t* s;
  s = calloc(6+2, sizeof(val_char_t));
  s->len = 4;
  memcpy(s->codepoints, (val_char_t[]){'p', 'o', 'r', 't'}, 4 * 4);

  val_port_t *p;
  p = calloc(1, sizeof(struct val_port_t));
  p->symbol = val_wrap_symb(s);
  p->fp = f;
  
  return val_wrap_port(p);
}

static int
populate_buffer(val_port_t *p)
{
  if (p->offset < p->len)
    return 1;

  p->len = fread(p->buf, 1, port_buffer_bytes, p->fp);
  p->offset = 0;

  return p->len > 0;
}

val_t read_byte_port(val_t port)
{
  int has_bytes;
  char c;
  val_port_t *p = val_unwrap_port(port);
  
  if (p->closed)
    error_handler();

  has_bytes = populate_buffer(p);
  if (!has_bytes)
    return val_wrap_eof();

  c = p->buf[p->offset];
  p->offset++;

  return val_wrap_int((unsigned char)c);
}

val_t peek_byte_port(val_t port, val_t skip)
{
  int has_bytes;
  char c;
  val_port_t *p = val_unwrap_port(port);

  int64_t sk = val_unwrap_int(skip);
  
  if (p->closed)
    error_handler();

  has_bytes = populate_buffer(p);
  if (!has_bytes)
    return val_wrap_eof();

  c = p->buf[p->offset+sk]; // FIXME: unsafe

  return val_wrap_int((unsigned char)c);
}

