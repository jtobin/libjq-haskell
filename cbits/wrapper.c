#include "jq.h"
#include "jv.h"
#include "wrapper.h"

void h_jq_start(jq_state *jq, jv *input, int flags) {
    jq_start(jq, *input, flags);
  };

// symbol not found (jq_next) when i call this
void h_jq_next(jv *val, jq_state *jq) {
    *val = jq_next(jq);
  };

// symbol not found (jv_parser_next) when i call this
void h_jv_parser_next(jv *val, jv_parser *jvp) {
    *val = jv_parser_next(jvp);
  };

