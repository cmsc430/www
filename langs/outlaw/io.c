#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

#define port_buffer_bytes 8

void utf8_encode_string(val_str_t *, char *);
int utf8_encode_char(val_char_t, char *);

val_t read_byte(void)
{
  char c = getc(in);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int((unsigned char)c);
}

val_t peek_byte(void* fake_port, int offset)
{
  char cs[3];
  if ((offset < 0) || (offset > 3)) { exit(-1); }
  int i;
  char c;
  for (i = 0; i < offset; i++) {
    cs[i] = getc(in);
  }
  c = getc(in);
  ungetc(c, in);
  for (i = 0; i < offset; i++) {
    ungetc(cs[offset-i-1], in);
  }
  return (c == EOF) ? val_wrap_eof() : val_wrap_int((unsigned char)c);
}

val_t write_byte(val_t c)
{
  putc((char) val_unwrap_int(c), out);
  return val_wrap_void();
}

val_t print_codepoint_out(val_t c)
{
  char buffer[5] = {0};
  utf8_encode_char(val_unwrap_char(c), buffer);
  fprintf(out, "%s", buffer);
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
