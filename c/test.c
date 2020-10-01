#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define PATH_SIZE 256
#define DATA_DIR "bigwebthing"
#define PRINT_SIZE 2000


typedef struct {
    pthread_mutex_t* tcp_mutex;
    char data_dir[PATH_SIZE];
} ReadyT;


typedef union {
    int dont_care;
    char got_data_dir[PATH_SIZE];
    ReadyT ready;
} state_value;


typedef enum {
    Empty,
    GettingHomeDir,
    GotDataDir,
    GettingXDG_DATA_HOME,
    Ready,
} state_type;


typedef struct {
    state_type type;
    state_value value;
    int fatal;
} program_state;


typedef union {
    int dont_care;
    char get_env[PATH_SIZE];
    char puts[PRINT_SIZE];
    pthread_mutex_t* tcp_mutex;
} output_value;


typedef enum {
    GetEnv,
    UnlockTcpMutex,
    Puts,
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


void io_puts(io_input* const input, output_value const output) {
    puts(output.puts);
    input->type = None;
}


void io_unlock_tcp_mutex(
    io_input* const input,
    output_value const output) {

    pthread_mutex_t* mutex = output.tcp_mutex;
    pthread_mutex_lock(mutex);
}


void io(io_input* const input, io_output const output) {
    switch(output.type) {
    case GetEnv:
        io_get_env(input, output.value);

    case UnlockTcpMutex:
        io_unlock_tcp_mutex(input, output.value);

    case Puts:
        io_puts(input, output.value);
    }
}


void update_on_start(
    program_state* const state,
    input_value const input,
    io_output* const output) {

    output->type = GetEnv;
    output_value v;
    snprintf(v.get_env, PATH_SIZE, "%s", "XDG_DATA_HOME");
    output->value = v;

    state->type = GettingXDG_DATA_HOME;
}


void do_nothing(
    program_state const* const state,
    io_output* const output) {

    switch(state->type) {
    case Empty:
        return;

    case GettingHomeDir:
        return;

    case GotDataDir:
        return;

    case GettingXDG_DATA_HOME:
        return;

    case Ready:
        output->type = UnlockTcpMutex;
        ReadyT ready = state->value.ready;
        output_value v;
        v.tcp_mutex = ready.tcp_mutex;
        output->value = v;
    }
}


int eq_paths(char const* const p1, char const* const p2) {
    for (int i = 0; i < PATH_SIZE; i++) {
        if (p1[i] != p2[i]) {
            return 0;
        }
    }
    return 1;
}


void update_on_got_xdg(
    program_state* const state,
    input_value const input,
    io_output* const output) {

    if (!eq_paths(input.got_env.name, "XDG_DATA_HOME")) {
        do_nothing(state, output);
        return;
    }

    char const* const data_home = input.got_env.value;
    if (data_home == NULL) {
        state->type = GettingHomeDir;
        output->type = GetEnv;
        output_value v;
        snprintf(v.get_env, PATH_SIZE, "%s", "HOME");
        output->value = v;
        return;
    }

    snprintf(
        state->value.got_data_dir,
        PATH_SIZE,
        "%s/%s",
        data_home,
        DATA_DIR);
    state->type = GotDataDir;
    do_nothing(state, output);
}


void fatal_error(
    program_state* const new_state,
    io_output* const output,
    char const* const msg) {

    new_state->fatal = 1;
    output->type = Puts;
    snprintf(output->value.puts, PRINT_SIZE, "%s", msg);
}


void update_on_got_home(
    program_state* const state,
    input_value const input,
    io_output* const output) {

    if (!eq_paths(input.got_env.name, "HOME")) {
        do_nothing(state, output);
        return;
    }

    char const* const home_dir = input.got_env.value;
    if (home_dir == NULL) {
        fatal_error(
            state,
            output,
            "HOME environment variable is NULL");
        return;
    }

    snprintf(
        state->value.got_data_dir,
        PATH_SIZE,
        "%s/.local/share/%s",
        home_dir,
        DATA_DIR);
    state->type = GotDataDir;
    do_nothing(state, output);
}


void update_on_got_env(
    program_state* const state,
    input_value const input,
    io_output* const output) {

    switch(state->type) {
    case Empty:
        return;

    case GettingXDG_DATA_HOME:
        update_on_got_xdg(state, input, output);
        return;

    case GettingHomeDir:
        update_on_got_home(state, input, output);
        return;

    case GotDataDir:
        return;

    case Ready:
        do_nothing(state, output);
    }
}


void update(
    program_state* const state,
    io_input const input,
    io_output* const output) {

    switch (input.type) {
    case Start:
        update_on_start(state, input.value, output);

    case None:
        return;

    case GotEnv:
        update_on_got_env(state, input.value, output);
    }
}


int main() {
    program_state state;
    init_state(&state);

    io_input input;
    init_input(&input);

    io_output output;

    while (!state.fatal) {
        update(&state, input, &output);
        io(&input, output);
    }
}
