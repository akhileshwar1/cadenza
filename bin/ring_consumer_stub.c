// c_stubs/ring_consumer_stub.c
#define _POSIX_C_SOURCE 200809L
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// Declarations from your ring_mmap C ABI (must exist in ring_mmap.h)
struct RingHandleC;
extern struct RingHandleC* ring_open(const char* path);
extern void ring_close(struct RingHandleC* ch);
extern uint64_t ring_get_head(struct RingHandleC* ch);
extern uint64_t ring_get_tail(struct RingHandleC* ch);
extern uint64_t ring_get_buf_size(struct RingHandleC* ch);
extern void* ring_get_buffer_ptr(struct RingHandleC* ch);
extern void ring_set_tail(struct RingHandleC* ch, uint64_t new_tail);

/* ------------------------------------------------------------------
Utility: create OCaml option values:

- None: represented by the integer 0 (Val_int(0))
- Some(v): block of tag 0 with 1 field: caml_alloc(1, 0); Store_field(...)
Note: In the OCaml runtime, constant constructors (like None) are
represented as small integers (Val_int), which is correct here.
------------------------------------------------------------------ */

static inline value ocaml_none(void) {
  return Val_int(0); /* None */
}

static inline value ocaml_some(value v) {
  value box = caml_alloc(1, 0); /* constructor tag = 0 (Some), 1 field */
  Store_field(box, 0, v);
  return box;
}

/* ------------------------------------------------------------------ */
/* caml_ring_open : string -> nativeint  (raises if open fails)       */
/* ------------------------------------------------------------------ */
CAMLprim value caml_ring_open(value v_path) {
  CAMLparam1(v_path);
  const char *path = String_val(v_path);
  struct RingHandleC* h = ring_open(path);
  if (!h) caml_failwith("ring_open failed");
  /* Represent pointer as nativeint for OCaml side */
  value v = caml_copy_nativeint((intnat)h);
  CAMLreturn(v);
}

/* ------------------------------------------------------------------ */
/* caml_ring_close : nativeint -> unit                               */
/* ------------------------------------------------------------------ */
CAMLprim value caml_ring_close(value v_handle) {
  CAMLparam1(v_handle);
  struct RingHandleC* h = (struct RingHandleC*)Nativeint_val(v_handle);
  if (h) ring_close(h);
  CAMLreturn(Val_unit);
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
  CAMLlocal3(v_pair, v_payload, v_some);

  struct RingHandleC* h = (struct RingHandleC*)Nativeint_val(v_handle);
  if (!h) CAMLreturn(ocaml_none()); /* None */

  uint64_t head = ring_get_head(h); /* absolute offsets written by producer */
  uint64_t tail = ring_get_tail(h);
  uint64_t buf_size = ring_get_buf_size(h);
  void *buf_base = ring_get_buffer_ptr(h);
  if (!buf_base) CAMLreturn(ocaml_none());

  if (tail >= head) CAMLreturn(ocaml_none()); /* no data available */

  uint64_t pos = tail % buf_size;
  uint8_t *base = (uint8_t*)buf_base;

  /* Read 4-byte length (may wrap) */
  uint32_t len_field = 0;
  if (pos + 4 <= buf_size) {
    memcpy(&len_field, base + pos, 4);
  } else {
    size_t part = (size_t)(buf_size - pos);
    memcpy(((uint8_t*)&len_field), base + pos, part);
    memcpy(((uint8_t*)&len_field) + part, base, 4 - part);
  }

  const uint32_t WRAP_MARKER = 0xFFFFFFFFu;
  if (len_field == WRAP_MARKER) {
    /* advance tail to buffer end and return None for this call */
    uint64_t new_tail = tail + (buf_size - pos);
    ring_set_tail(h, new_tail);
    CAMLreturn(ocaml_none());
  }
  uint32_t msg_len = len_field; /* 1 + payload_len */
  uint64_t total_needed = (uint64_t)4 + (uint64_t)msg_len;
  if (tail + total_needed > head) {
    /* producer hasn't finished writing this frame yet */
    CAMLreturn(ocaml_none());
  }

  /* copy message bytes into a temp buffer */
  uint8_t *tmp = (uint8_t*)malloc((size_t)msg_len);
  if (!tmp) caml_failwith("malloc failed in ring read");

  uint64_t payload_pos = (pos + 4) % buf_size;
  if (payload_pos + msg_len <= buf_size) {
    memcpy(tmp, base + payload_pos, msg_len);
  } else {
    size_t first = (size_t)(buf_size - payload_pos);
    memcpy(tmp, base + payload_pos, first);
    memcpy(tmp + first, base, (size_t)(msg_len - first));
  }

  /* first byte is msg_type */
  int msg_type = (int)tmp[0];
  size_t payload_len = (msg_len >= 1 ? (size_t)(msg_len - 1) : 0);

  /* create OCaml string for payload */
  v_payload = caml_alloc_string(payload_len);
  if (payload_len > 0) {
    /* String_val returns const char*, cast away const for destination */
    memcpy((char*)String_val(v_payload), tmp + 1, payload_len);
  }

  free(tmp);

  /* advance tail */
  uint64_t new_tail = tail + total_needed;
  ring_set_tail(h, new_tail);

  /* pack Some (msg_type, payload) as OCaml value
     tuple (msg_type, payload) is an OCaml block of 2 fields */
  v_pair = caml_alloc(2, 0);
  Store_field(v_pair, 0, Val_int(msg_type));
  Store_field(v_pair, 1, v_payload);

  v_some = ocaml_some(v_pair);
  CAMLreturn(v_some);
}
