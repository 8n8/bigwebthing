#define GLEW_STATIC
#include <GL/glew.h>
#include <SDL.h>
#include <SDL_opengl.h>

const char* vertexSource =
	"glsl(\n"
	"	#version 150 core\n"
	"\n"
	"	in vec2 position;\n"
	"	void main() {\n"
	"		gl_Position = vec4(position, 0.0, 1.0);\n"
	"	}\n"
	")glsl";

const char* fragmentSource =
	"glsl(\n"
	"	#version 150 core\n"
	"\n"
	"	out vec4 outColor;\n"
	"\n"
	"	void main() {\n"
	"		outColor = vec4(1.0, 1.0, 1.0, 1.0);\n"
	"	}\n"
	")glsl;";

char* prettyError(int err) {
	switch (err) {
	case GL_INVALID_ENUM:
		return "invalid enum";

	case GL_INVALID_VALUE:
		return "invalid value";

	case GL_INVALID_OPERATION:
		return "invalid operation";

	case GL_INVALID_FRAMEBUFFER_OPERATION:
		return "invalid framebuffer operation";

	case GL_OUT_OF_MEMORY:
		return "out of memory";
	}

	return "error code not known";
}

int main(int argc, char *argv[]) {
	SDL_Init(SDL_INIT_VIDEO);

	SDL_GL_SetAttribute(
		SDL_GL_CONTEXT_PROFILE_MASK,
		SDL_GL_CONTEXT_PROFILE_CORE);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
	SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

	SDL_Window* window = SDL_CreateWindow(
		"OpenGL",
		100,
		100,
		800,
		600,
		SDL_WINDOW_OPENGL);

	SDL_GLContext context = SDL_GL_CreateContext(window);

	glewExperimental = GL_TRUE;
	glewInit();

	GLuint vao;
	glGenVertexArrays(1, &vao);
	glBindVertexArray(vao);
	
	GLuint vbo;
	glGenBuffers(1, &vbo);

	float vertices[] = {
		0.0f, 0.5f,
		0.5f, -0.5f,
		-0.5f, -0.5f
	};

	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(
		GL_ARRAY_BUFFER,
		sizeof(vertices),
		vertices, GL_STATIC_DRAW);

	GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vertexShader, 1, &vertexSource, NULL);
	glCompileShader(vertexShader);
	GLint err;
	glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &err);
	if (err) {
		printf("bad vertex shader %d", err);
		return err;
	}

	GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fragmentShader, 1, &fragmentSource, NULL);
	glCompileShader(fragmentShader);
	glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &err);
	if (err) {
		printf("bad fragment shader %d", err);
		return err;
	}

	GLuint shaderProgram = glCreateProgram();
	glAttachShader(shaderProgram, vertexShader);
	glAttachShader(shaderProgram, fragmentShader);
	glBindFragDataLocation(shaderProgram, 0, "outColor");

	glLinkProgram(shaderProgram);
	glUseProgram(shaderProgram);
	GLenum glErr = glGetError();

	GLint posAttrib = glGetAttribLocation(
		shaderProgram,
		"position");
	glVertexAttribPointer(posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
	glEnableVertexAttribArray(posAttrib);

	if (glErr != GL_NO_ERROR) {
		printf("%s\n", prettyError(glErr));
		return glErr;
	}

	SDL_Event windowEvent;
	while (1) {
		if (SDL_PollEvent(&windowEvent)) {
			if (windowEvent.type == SDL_QUIT) break;
		}

		glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
		glClear(GL_COLOR_BUFFER_BIT);

		glDrawArrays(GL_TRIANGLES, 0, 3);

		SDL_GL_SwapWindow(window);
	}

	glDeleteProgram(shaderProgram);
	glDeleteShader(fragmentShader);
	glDeleteShader(vertexShader);

	glDeleteBuffers(1, &vbo);
	glDrawArrays(GL_TRIANGLES, 0, 3);

	SDL_GL_DeleteContext(context);

	SDL_Quit();
	return 0;
}
