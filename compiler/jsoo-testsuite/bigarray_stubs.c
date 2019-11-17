#include <string.h>
#include <caml/bigarray.h>

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Long_val(v_pos);
}

CAMLprim value caml_bigstring_blit_string_to_ba(
  value v_str, value v_src_pos, value v_bstr, value v_dst_pos, value v_len)
{
  unsigned char *str = Bytes_val(v_str) + Long_val(v_src_pos);
  char *bstr = get_bstr(v_bstr, v_dst_pos);
  memcpy(bstr, str, Long_val(v_len));
  return Val_unit;
}

CAMLprim value caml_bigstring_blit_bytes_to_ba(
  value v_str, value v_src_pos, value v_bstr, value v_dst_pos, value v_len)
{
  unsigned char *str = Bytes_val(v_str) + Long_val(v_src_pos);
  char *bstr = get_bstr(v_bstr, v_dst_pos);
  memcpy(bstr, str, Long_val(v_len));
  return Val_unit;
}

CAMLprim value caml_bigstring_blit_ba_to_bytes(
  value v_bstr, value v_src_pos, value v_str, value v_dst_pos, value v_len)
{
  char *bstr = get_bstr(v_bstr, v_src_pos);
  unsigned char *str = Bytes_val(v_str) + Long_val(v_dst_pos);
  memcpy(str, bstr, Long_val(v_len));
  return Val_unit;
}

CAMLprim value caml_bigstring_blit_ba_to_ba(
  value v_src, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = Long_val(v_len);
  memmove(dst, src, len);
  return Val_unit;
}
