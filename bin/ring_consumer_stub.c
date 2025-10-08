// ring_consumer_stub.c
#define _POSIX_C_SOURCE 200809L
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// declarations from ring_mmap (C ABI)
struct RingHandleC;
extern struct RingHandleC* ring_open(const char* path);
extern void ring_close(struct RingHandleC* ch);
extern uint64_t ring_get_head(struct RingHandleC* ch);
extern uint64_t ring_get_tail(struct RingHandleC* ch);
extern uint64_t ring_get_buf_size(struct RingHandleC* ch);
extern void* ring_get_buffer_ptr(struct RingHandleC* ch);
extern void ring_set_tail(struct RingHandleC* ch, uint64_t new_tail);

static void finalize_ring_handle(value v) {
  struct RingHandleC* h = (struct RingHandleC*)Nativeint_val(v);
  if (h) ring_close(h);
}

// OCaml wrapper: caml_ring_open : string -> nativeint
CAMLprim value caml_ring_open(value v_path) {
  CAMLparam1(v_path);
  const char *path = String_val(v_path);
  struct RingHandleC* h = ring_open(path);
  if (!h) caml_failwith("ring_open failed");
  value v = caml_copy_nativeint((intnat)h);
  // We don't attach a custom block with finalizer here; the user must call caml_ring_close OR use caml_ring_close below
  CAMLreturn(v);
}

CAMLprim value caml_ring_close(value v_handle) {
  CAMLparam1(v_handle);
  struct RingHandleC* h = (struct RingHandleC*)Nativeint_val(v_handle);
  if (h) ring_close(h);
  CAMLreturn0;
}

/*
 * caml_ring_read_next : nativeint -> (int * string) option
 *
 * Non-blocking: if no message available return None.
 * If a full frame is available, returns Some (msg_type, payload_string)
 *
 * Frame layout in buffer:
 * [uint32_t len][uint8_t type][payload bytes...], len = 1 + payload_len
 * WRAP_MARKER = 0xFFFFFFFF indicates that reader should wrap to start.
 */
CAMLprim value caml_ring_read_next(value v_handle) {
  CAMLparam1(v_handle);
  CAMLlocal2(v_some, v_pair);

  struct RingHandleC* h = (struct RingHandleC*)Nativeint_val(v_handle);
  if (!h) CAMLreturn0; // None

  uint64_t head = ring_get_head(h); // absolute offsets
  uint64_t tail = ring_get_tail(h);
  uint64_t buf_size = ring_get_buf_size(h);
  void *buf_base = ring_get_buffer_ptr(h);
  if (!buf_base) CAMLreturn0;

  if (tail >= head) CAMLreturn0; // no data

  uint64_t pos = tail % buf_size;
  uint8_t *base = (uint8_t*)buf_base;

  // read 4-byte length carefully (may cross boundary)
  uint32_t len_field = 0;
  if (pos + 4 <= buf_size) {
    memcpy(&len_field, base + pos, 4);
  } else {
    size_t part = buf_size - pos;
    memcpy(((uint8_t*)&len_field), base + pos, part);
    memcpy(((uint8_t*)&len_field) + part, base, 4 - part);
  }

  const uint32_t WRAP_MARKER = 0xFFFFFFFFu;
  if (len_field == WRAP_MARKER) {
    // advance tail to buffer end and continue (wrap)
    uint64_t new_tail = tail + (buf_size - pos);
    ring_set_tail(h, new_tail);
    CAMLreturn0; // caller will call again
  }
  uint32_t msg_len = len_field; // type + payload
  uint64_t total_needed = (uint64_t)4 + (uint64_t)msg_len;
  if (tail + total_needed > head) {
    // producer hasn't finished writing this frame yet
    CAMLreturn0;
  }

  // read message bytes into a temporary buffer
  uint8_t *tmp = (uint8_t*)malloc(msg_len);
  if (!tmp) caml_failwith("malloc failed in ring read");

  uint64_t payload_pos = (pos + 4) % buf_size;
  if (payload_pos + msg_len <= buf_size) {
    memcpy(tmp, base + payload_pos, msg_len);
  } else {
    size_t first = buf_size - payload_pos;
    memcpy(tmp, base + payload_pos, first);
    memcpy(tmp + first, base, msg_len - first);
  }

  // first byte is msg_type
  int msg_type = tmp[0];
  // payload is tmp+1 with length msg_len-1
  size_t payload_len = (msg_len >= 1 ? (size_t)(msg_len - 1) : 0);

  // create OCaml string
  value v_payload = caml_alloc_string(payload_len);
  if (payload_len > 0) {
    memcpy(String_val(v_payload), tmp + 1, payload_len);
  }

  free(tmp);

  // advance tail
  uint64_t new_tail = tail + total_needed;
  ring_set_tail(h, new_tail);

  // pack Some (msg_type, payload)
  v_pair = caml_alloc(2, 0);
  Store_field(v_pair, 0, Val_int(msg_type));
  Store_field(v_pair, 1, v_payload);
  v_some = caml_alloc(1, 0);
  Store_field(v_some, 0, v_pair);

  CAMLreturn(v_some);
}
