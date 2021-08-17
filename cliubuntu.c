#include <stdlib.h>
#include <stdio.h>


struct cmd {
	int open_db;
};

struct model {
};

struct msg {
	int start;
	FILE* db;
	int opened_db;
};

// Mustn't have any side effects. If a side effect is needed, then
// put the information for it in the return value and let the io
// function do it.
static struct cmd pure(
	const struct model* model,
	const struct msg* msg) {

	struct cmd cmd;

	if (msg->start) {
		cmd.open_db = 1;	
		return cmd;
	}

	if (msg->opened_db) {
		if (!msg->db) {
		}
	}
}

// This function
static struct msg io(
	int* quit,
	struct model* model,
	struct cmd* cmd) {

	if (cmd->open_db) {
		cmd->open_db = 0;
		FILE* db = fopen("db", "rb");
		struct msg msg;
		msg.db = db;
		msg.opened_db = 1;
		return msg;
	}
}

static void init_cmd(struct cmd* cmd) {
}

static void init_model(struct model* model) {
}

static void init_msg(struct msg* msg) {
	msg->start = 1;
}

int main() {
	struct model model;
	init_model(&model);

	struct cmd cmd;
	init_cmd(&cmd);

	struct msg msg;
	init_msg(&msg);

	int quit = 0;

	while (!quit) {
		msg = io(&quit, &model, &cmd);
		cmd = pure(&model, &msg);
	}
}
