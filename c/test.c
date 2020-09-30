#include <signal/signal_protocol.h>
#include <signal/key_helper.h>
#include <stdio.h>
#include "crypto_provider_openssl.h"
#include <pthread.h>
#include <sqlite3.h>
#include <stdlib.h>


typedef union {
    int dont_care;
} state_value;


typedef enum {
    Empty
} state_type;


typedef struct {
    state_type type;
    state_value value;
    int fatal;
} program_state;


typedef union {
    int dont_care;
    char* get_env;
} output_value;


typedef enum {
    GetEnv,
} output_type;


typedef struct {
    output_type type;
    output_value value;
} io_output;


typedef struct {
    char* var;
    char* value;
} got_env_t;


typedef union {
    int dont_care;
    got_env_t got_env;
} input_value;


typedef enum {
    Start,
    None,
    GotEnv,
} input_type;


typedef struct {
    input_type type;
    input_value value;
} io_input;


void init_state(program_state* init) {
    init->type = Empty;
    state_value v;
    v.dont_care = 0;
    init->value = v;
}


void init_input(io_input* init) {
    init->type = Start;
    input_value v;
    v.dont_care = 0;
    init->value = v;
}

void io_do_nothing(
    io_input* const input,
    output_value const output) {

    input->type = None;
    input->value.dont_care = 0;
}

void io_get_env(
    io_input* const input,
    output_value const output) {

    input->type = GotEnv;

    got_env_t got;
    got.var = output.get_env;
    got.value = getenv(output.get_env);

    (input->value).got_env = got;
}


const int max_in_out = 20;


void io(io_input* const input, io_output const output) {
    switch(output.type) {
    case GetEnv:
        io_get_env(input, output.value);
    }
}


void init_output(io_output* output) {
}


void updateOnStart(
    program_state const* const old_state,
    program_state* const new_state,
    input_value const input,
    io_output* const output) {

    output->type = GetEnv;
    output_value v;
    v.get_env = "XDG_DATA_HOME";
    output->value = v;
}


void update(
    program_state const* const old_state,
    program_state* const new_state,
    io_input const input,
    io_output* const output) {

    switch (input.type) {
    case Start:
        updateOnStart(old_state, new_state, input.value, output);

    case None:
        return;

    case GotEnv:
        // todo
        return;
    }
}


int main() {
    program_state state;
    init_state(&state);

    io_input input;
    init_input(&input);

    io_output output;
    init_output(&output);

    while (!state.fatal) {
        program_state new_state;
        update(&state, &new_state, input, &output);
        io(&input, output);
    }
}
