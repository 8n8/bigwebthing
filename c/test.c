#include <stdio.h>
#include <stdlib.h>

#define PATH_SIZE 256
#define DATA_DIR "bigwebthing"


typedef union {
    int dont_care;
    char got_data_dir[PATH_SIZE];
} state_value;


typedef enum {
    Empty,
    GettingHomeDir,
    GotDataDir,
    GettingXDG_DATA_HOME,
} state_type;


typedef struct {
    state_type type;
    state_value value;
    int fatal;
} program_state;


typedef union {
    int dont_care;
    char get_env[PATH_SIZE];
} output_value;


typedef enum {
    GetEnv,
    WaitForMessage,
} output_type;


typedef struct {
    output_type type;
    output_value value;
} io_output;


typedef struct {
    char name[PATH_SIZE];
    char value[PATH_SIZE];
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


void io_get_env(io_input* const input, output_value const output) {
    got_env_t got;
    snprintf(got.name, PATH_SIZE, "%s", output.get_env);
    snprintf(got.value, PATH_SIZE, "%s", getenv(output.get_env));

    input->type = GotEnv;
    input->value.got_env = got;
}


void io(io_input* const input, io_output const output) {
    switch(output.type) {
    case GetEnv:
        io_get_env(input, output.value);

    case WaitForMessage:
        // todo
        return;
    }
}


void init_output(io_output* output) {
    // todo
}


void update_on_start(
    program_state const* const old_state,
    program_state* const new_state,
    input_value const input,
    io_output* const output) {

    output->type = GetEnv;
    output_value v;
    snprintf(v.get_env, PATH_SIZE, "%s", "XDG_DATA_HOME");
    output->value = v;

    new_state->type = GettingXDG_DATA_HOME;
}


void doNothing(io_output* const output) {
    output->type = WaitForMessage;
}


int eqPaths(char const* const p1, char const* const p2) {
    for (int i = 0; i < PATH_SIZE; i++) {
        if (p1[i] != p2[i]) {
            return 0;
        }
    }
    return 1;
}


void update_on_got_env(
    program_state const* const old_state,
    program_state* const new_state,
    input_value const input,
    io_output* const output) {

    if (old_state->type != GettingXDG_DATA_HOME) {
        doNothing(output);
        return;
    }

    if (eqPaths(input.got_env.name, "XDG_DATA_HOME")) {
        doNothing(output);
        return;
    }

    char const* const data_home = input.got_env.value;
    if (data_home == NULL) {
        new_state->type = GettingHomeDir;
        output->type = GetEnv;
        output_value v;
        snprintf(v.get_env, PATH_SIZE, "%s", "HOME");
        output->value = v;
        return;
    }

    snprintf(
        new_state->value.got_data_dir,
        PATH_SIZE,
        "%s/%s",
        data_home,
        DATA_DIR);
    new_state->type = GotDataDir;
    doNothing(output);
}


void update(
    program_state const* const old_state,
    program_state* const new_state,
    io_input const input,
    io_output* const output) {

    switch (input.type) {
    case Start:
        update_on_start(old_state, new_state, input.value, output);

    case None:
        return;

    case GotEnv:
        update_on_got_env(old_state, new_state, input.value, output);
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
