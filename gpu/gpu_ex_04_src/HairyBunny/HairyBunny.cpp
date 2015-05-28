
// *** Hairy Bunny mit Geometry Shader ***

#include <GL/glew.h>
#include <stdlib.h>
#include <math.h>
#include <GL/freeglut.h>
#include <iostream>
#include <string>
#include <fstream>

using namespace std;

// Window size
int width = 512;       
int height = 512;

// camera movement
float center[3];

#define PI 3.141592f

#define ROTATE 1
#define MOVE 2

float thetaStart = PI / 2.0f - 0.5f;
float phiStart = PI / 2.0f;
float rStart = 3.0f;

float theta = thetaStart;
float phi = phiStart;
float r = rStart;

float oldX, oldY;
int motionState;

float viewPosition[3];
float viewDirection[3];

unsigned int geometryShader = 1;

GLuint vaoBunny;
GLuint iboBunny;
GLuint progSimple;
GLuint progHair;
GLuint progNormalHair;
GLuint progFluffyHair;
GLuint progDuplicate;
GLuint uboCamera;
GLuint uboIndex;

extern float bunnyData[];
extern unsigned int bunnyStride;
extern unsigned int bunnySize;
extern unsigned int bunnyIndices[];
extern unsigned int bunnyIndicesStride;
extern unsigned int bunnyIndicesSize;

// Print information about the compiling step
void printShaderInfoLog(GLuint shader)
{
    GLint infologLength = 0;
    GLsizei charsWritten  = 0;
    char *infoLog;

	glGetShaderiv(shader, GL_INFO_LOG_LENGTH,&infologLength);		
	infoLog = (char *)malloc(infologLength);
	glGetShaderInfoLog(shader, infologLength, &charsWritten, infoLog);
	printf("%s\n",infoLog);
	free(infoLog);
}

// Print information about the linking step
void printProgramInfoLog(GLuint program)
{
	GLint infoLogLength = 0;
	GLsizei charsWritten  = 0;
	char *infoLog;

	glGetProgramiv(program, GL_INFO_LOG_LENGTH,&infoLogLength);
	infoLog = (char *)malloc(infoLogLength);
	glGetProgramInfoLog(program, infoLogLength, &charsWritten, infoLog);
	printf("%s\n",infoLog);
	free(infoLog);
}

// Reads a file and returns the content as a string
string readFile(string fileName)
{
	string fileContent;
	string line;

	ifstream file(fileName.c_str());
	if (file.is_open()) {
		while (!file.eof()){
			getline (file,line);
			line += "\n";
			fileContent += line;					
		}
		file.close();
	}
	else
		cout << "ERROR: Unable to open file " << fileName << endl;

	return fileContent;
}

GLuint loadShader(GLenum shaderType, string shaderFileName)
{
	// Create empty shader object
	GLuint shaderHandle = glCreateShader(shaderType);
	// Read vertex shader source 
	cout << "Loading: " + shaderFileName << endl;
	string shaderSource = readFile(shaderFileName);
	const char* sourcePtr = shaderSource.c_str();

	// Attach shader code
	glShaderSource(shaderHandle, 1, &sourcePtr, nullptr);
	// Compile shader	
	glCompileShader(shaderHandle);
	printShaderInfoLog(shaderHandle);

	return shaderHandle;
}

GLuint loadShaderProgram(GLuint vertexShader, GLuint fragmentShader)
{
	// Create shader program
	GLuint shaderProgramHandle = glCreateProgram();

	// Attach shaders to program	
	glAttachShader(shaderProgramHandle, vertexShader);
	glAttachShader(shaderProgramHandle, fragmentShader);

	// Link program
	glLinkProgram(shaderProgramHandle);
	printProgramInfoLog(shaderProgramHandle);

	return shaderProgramHandle;
}


void calcViewerCamera(float theta, float phi, float r)
{
    float x = r * sin(theta) * cos(phi);
    float y = r * cos(theta);
    float z = r * sin(theta) * sin(phi);
 
	viewPosition[0] = center[0] + x;
	viewPosition[1] = center[1] + y;
	viewPosition[2] = center[2] + z;
	viewDirection[0] = -x;
	viewDirection[1] = -y;
	viewDirection[2] = -z;

	glLoadIdentity();
	gluLookAt(viewPosition[0], viewPosition[1], viewPosition[2],
				viewPosition[0] + viewDirection[0], viewPosition[1] + viewDirection[1], viewPosition[2] + viewDirection[2], 
				0, 1, 0);

	// TODO: Updaten der View-Matrix. Die View-Matrix beginnt ab dem 17ten float des UBOs.
	float view[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, view);
	glBufferSubData(GL_UNIFORM_BUFFER, 16 * sizeof(float), 16 * sizeof(float), view);
}

void mouseMotion(int x, int y)
{
	float deltaX = x - oldX;
	float deltaY = y - oldY;
	
	if (motionState == ROTATE) {
		theta -= 0.001f * deltaY;

		if (theta < 0.001f) theta = 0.001f;
		else if (theta > PI - 0.001f) theta = PI - 0.001f;

		phi += 0.001f * deltaX;	
		if (phi < 0) phi += 2*PI;
		else if (phi > 2*PI) phi -= 2*PI;
		calcViewerCamera(theta, phi, r);
	}
	else if (motionState == MOVE) {
		r += 0.03f * deltaY;
		if (r < 0.1f) r = 0.1f;
		calcViewerCamera(theta, phi, r);
	}

	oldX = (float)x;
	oldY = (float)y;

	glutPostRedisplay();
}

void mouse(int button, int state, int x, int y)
{
	oldX = (float)x;
	oldY = (float)y;

	if (button == GLUT_LEFT_BUTTON) {
		if (state == GLUT_DOWN) {
			motionState = ROTATE;
		}
	}
	else if (button == GLUT_RIGHT_BUTTON) {
		if (state == GLUT_DOWN) {
			motionState = MOVE;
		}
	}
}


//------------------------------------------------------------------------
// Rendering loop.
//------------------------------------------------------------------------
void display(void)
{
	// clear frame.
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

	switch (geometryShader) 
	{
		case 1:
			// Den Block-Index des Uniform-Blocks suchen, das im Shader 'progDuplicate' den Namen "GlobalMatrices" trägt.
			uboIndex = glGetUniformBlockIndex(progDuplicate, "GlobalMatrices");	
			// Binden Sie diesen Blockindex an den Binding Point 0.
			glUniformBlockBinding(progDuplicate, uboIndex, 0);
			break;
		case 2:
			// Den Block-Index des Uniform-Blocks suchen, das im Shader 'progNormalHair' den Namen "GlobalMatrices" trägt.
			uboIndex = glGetUniformBlockIndex(progNormalHair, "GlobalMatrices");
			// Binden Sie diesen Blockindex an den Binding Point 0.
			glUniformBlockBinding(progNormalHair, uboIndex, 0);
			break;
		case 3:
			// Den Block-Index des Uniform-Blocks suchen, das im Shader 'progFluffyHair' den Namen "GlobalMatrices" trägt.
			uboIndex = glGetUniformBlockIndex(progFluffyHair, "GlobalMatrices");
			// Binden Sie diesen Blockindex an den Binding Point 0.
			glUniformBlockBinding(progFluffyHair, uboIndex, 0);
			break;
	}

	// TODO: Binden Sie das gesamte UBO an den Binding Point 0. Offset = 0 und Size = Größe der Daten im UBO.
	glBindBufferRange(GL_UNIFORM_BUFFER, 0, uboCamera, 0, 32 * sizeof(float));

	// Bind VAO and IBO
	glBindVertexArray(vaoBunny);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iboBunny);

	// Draw solid bunny
	glUseProgram(progSimple);
	glDrawElements(GL_TRIANGLES, bunnyIndicesSize / bunnyIndicesStride, GL_UNSIGNED_INT, 0);
	glUseProgram(0);


	// Draw hair
	switch (geometryShader) 
	{
		case 1: 
			glUseProgram(progDuplicate);
			break;
		case 2:
			glUseProgram(progNormalHair);
			break;
		case 3:
			glUseProgram(progFluffyHair);
			break;
	}
	glDrawElements(GL_TRIANGLES, bunnyIndicesSize / bunnyIndicesStride, GL_UNSIGNED_INT, 0);
	glUseProgram(0);
	
	// Unbind VAO and IBO
	glBindVertexArray(0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

	// Flush command buffer and swap buffers.
    glFlush();
    glutSwapBuffers();
}


void initBunny()
{
	// Create vertex buffer object
	GLuint vbo;	
	glGenBuffers(1, &vbo);	
	glBindBuffer(GL_ARRAY_BUFFER, vbo);
	glBufferData(GL_ARRAY_BUFFER, bunnySize, bunnyData, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	// Create index buffer object
	glGenBuffers(1, &iboBunny);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, iboBunny);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, bunnyIndicesSize, bunnyIndices, GL_STATIC_DRAW);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

	// Create vertex array object
	glGenVertexArrays(1, &vaoBunny);
	glBindVertexArray(vaoBunny);
		glBindBuffer(GL_ARRAY_BUFFER, vbo);
		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, bunnyStride, 0);
		glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, bunnyStride, (char*)12);
		glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, bunnyStride, (char*)24);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glEnableVertexAttribArray(2);
	glBindVertexArray(0);
}

void initGL()
{
	// Initialize light source
	GLfloat light_pos[] = {10, 10, 10, 1};
	GLfloat light_col[] = { 1,  1,  1, 1};

	glLightfv(GL_LIGHT0, GL_POSITION, light_pos);
	glLightfv(GL_LIGHT0, GL_DIFFUSE,  light_col);
	glLightfv(GL_LIGHT0, GL_SPECULAR, light_col);

	// Enable lighting
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);

	// Initialize material
	GLfloat Bunny_diffuse[]  = {0.75f, 0.375f, 0.075f, 1};
	GLfloat Bunny_specular[] = {0.8f, 0.8f, 0.8f, 1};

	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, Bunny_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, Bunny_specular);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 45.2776f);

	// TODO: Uniform Buffer Object für die Camera Matrizen anlegen.
	glGenBuffers(1, &uboCamera);

	// TODO: Das UBO binden (target = GL_UNIFORM_BUFFER)
	glBindBuffer(GL_UNIFORM_BUFFER, uboCamera);
		
	// TODO: Speicherplatz allokieren mit glBufferData. Reservieren Sie Platz für 2 4x4 Matrizen mit float-Einträgen. Data = NULL und Usage = GL_STREAM_DRAW
	glBufferData(GL_UNIFORM_BUFFER, 32 * sizeof(float), nullptr, GL_STREAM_DRAW);

	// Initialize camera
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45, 1, 0.1, 100);
	
	// TODO: query projection matrix and update the vbo.
		// Getten Sie sich die Projektionsmatrix und kopieren Sie sie auf die ersten 16 float Werte des UBOs. Beachten Sie, das das UBO dazu gebunden sein muss!
		// Verwenden Sie dazu die Befehle glGetFloatv und glBufferSubData
	float projection[16];
	glGetFloatv(GL_PROJECTION_MATRIX, projection);
	glBufferSubData(GL_UNIFORM_BUFFER, 0, 16 * sizeof(float), projection);
	
	// Viewmatrix initialisieren
	glMatrixMode(GL_MODELVIEW);
	calcViewerCamera(theta, phi, r);

	// Enable depth buffer
	glEnable(GL_DEPTH_TEST);
}

void initGLSL()
{
	GLuint vertexShaderHair = loadShader(GL_VERTEX_SHADER, "hair.vert");
	GLuint fragmentShaderHair = loadShader(GL_FRAGMENT_SHADER, "hair.frag");
	GLuint fragmentShaderFluffyHair = loadShader(GL_FRAGMENT_SHADER, "fluffy_hair.frag");

	GLuint vertexShaderSimple = loadShader(GL_VERTEX_SHADER, "simple.vert");
	GLuint fragmentShaderSimple = loadShader(GL_FRAGMENT_SHADER, "simple.frag");

	GLuint geometryShaderDuplicate = loadShader(GL_GEOMETRY_SHADER, "duplicate.geom");
	GLuint geometryShaderNormalHair = loadShader(GL_GEOMETRY_SHADER, "normal_hair.geom");
	GLuint geometryShaderFluffyHair = loadShader(GL_GEOMETRY_SHADER, "fluffy_hair.geom");

	// Create shader program
	progDuplicate = glCreateProgram();

	// Attach shader
	glAttachShader(progDuplicate, vertexShaderHair);
	glAttachShader(progDuplicate, fragmentShaderHair);
	glAttachShader(progDuplicate, geometryShaderDuplicate);

	// Link program
	cout << "Linking: progDuplicate" << endl;
	glLinkProgram(progDuplicate);
	printProgramInfoLog(progDuplicate);

	// *********************************************************************

	// Create shader program
	progNormalHair = glCreateProgram();

	// Attach shader
	glAttachShader(progNormalHair, vertexShaderHair);
	glAttachShader(progNormalHair, fragmentShaderHair);
	glAttachShader(progNormalHair, geometryShaderNormalHair);

	// Link program
	cout << "Linking: progNormalHair" << endl;
	glLinkProgram(progNormalHair);
	printProgramInfoLog(progNormalHair);

	// *********************************************************************

	// Create shader program
	progFluffyHair = glCreateProgram();

	// Attach shader
	glAttachShader(progFluffyHair, vertexShaderHair);
	glAttachShader(progFluffyHair, fragmentShaderFluffyHair);
	glAttachShader(progFluffyHair, geometryShaderFluffyHair);

	// Link program
	cout << "Linking: progFluffyHair" << endl;
	glLinkProgram(progFluffyHair);
	printProgramInfoLog(progFluffyHair);

	// *********************************************************************

	// Create shader program
	progSimple = glCreateProgram();	

	// Attach shader
	glAttachShader(progSimple, vertexShaderSimple);
	glAttachShader(progSimple, fragmentShaderSimple);
	
	// Link program
	cout << "Linking: progSimple" << endl;
	glLinkProgram(progSimple);
	printProgramInfoLog(progSimple);

	// *********************************************************************

}

void keyboard(unsigned char key, int x, int y)
{
	// set parameters
	switch (key)
	{
		case '1':
			geometryShader = 1;
			break;
		case '2':
			geometryShader = 2;
			break;
		case '3':
			geometryShader = 3;
			break;
	}
}


//------------------------------------------------------------------------
//   It's the main application function. Note the clean code you can
//   obtain using he GLUT library. No calls to dark windows API
//   functions with many obscure parameters list. =)
//------------------------------------------------------------------------
int main(int argc, char** argv)
{
     // Initialize GLUT
   glutInit(&argc, argv);
   glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH | GLUT_MULTISAMPLE);
   glutInitWindowSize(width, height);
   glutCreateWindow("Hairy Bunny");

   // Init glew so that the GLSL functionality will be available
   if(glewInit() != GLEW_OK)
	   cout << "GLEW init failed!" << endl;

	// OpenGL/GLSL initializations
	initGL();
	initBunny();
	initGLSL();

	// Register callback functions   
	glutKeyboardFunc(keyboard);
	glutMotionFunc(mouseMotion);
	glutMouseFunc(mouse);
	glutDisplayFunc(display);
	
	// Enter main loop
	glutMainLoop();

	return 0;
}
