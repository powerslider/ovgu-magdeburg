#include <GL/freeglut.h>

int width = 600;
int height = 600;


void drawQuad(float x, float y, float z)
{
	glBegin(GL_QUADS);
	glVertex3f(x, y, z);
	glVertex3f(x + 1, y, z);
	glVertex3f(x + 1, y + 1, z);
	glVertex3f(x, y + 1, z);
	glEnd();
}


void display(void)	
{
	glClear(GL_COLOR_BUFFER_BIT);

	glLoadIdentity();
	gluLookAt(0, 0, 1, 0, 0, 0, 0, 1, 0);

	// *** Farben mit Alpha Kanal setzen
	glColor3f(1, 0, 0);
	drawQuad(1, 1, -2);

	glColor3f(0, 1, 0);
	drawQuad(0.25, 0.75, -1);
	
	glColor3f(0, 0, 1);
	drawQuad(0.5, 0.25, 0);

	glFlush();
}

void keyboard(unsigned char key, int x, int y)
{
	// TODO: Bei Taste 1 Additive Alpha-Blending setzen.
	if (key == '1')
	{
		glClearColor(0, 0, 0, 0);
		glClear(GL_COLOR_BUFFER_BIT);

		glColor3f(1, 0, 0);
		drawQuad(1, 1, -2);

		glColor3f(0, 1, 0);
		drawQuad(0.25, 0.75, -1);

		glColor3f(0, 0, 1);
		drawQuad(0.5, 0.25, 0);

		glBlendFunc(GL_ONE, GL_ONE);
		
		glFlush();
	}

	// TODO: Bei Taste 2 
	if (key == '2')
	{
		glClearColor(0, 0, 0, 0);
		glClear(GL_COLOR_BUFFER_BIT);
		
		glColor4f(1, 0, 0, 0.7);
		drawQuad(1, 1, -2);
		
		glColor4f(0, 1, 0, 0.7);
		drawQuad(0.25, 0.75, -1);
		
		glColor4f(0, 0, 1, 0.7);
		drawQuad(0.5, 0.25, 0);
		
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		
		glFlush();
	}

	// TODO: Bei Taste 3 Texturekoordinaten mit Hilfe des Matrix-Stacks um den Faktor 2 skalieren.
	if (key == '3')
	{
		glClearColor(1, 1, 1, 1);
		glClear(GL_COLOR_BUFFER_BIT);
		
		glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);
		glColor3f(1, 1, 0);
		drawQuad(1, 1, -2);

		glColor3f(0, 1, 1);
		drawQuad(0.25, 0.75, -1);

		glColor3f(1, 0, 1);
		drawQuad(0.5, 0.25, 0);
		
		glFlush();
	}

}


int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGBA | GLUT_SINGLE);
	glutInitWindowSize(width, height);
	glutCreateWindow("Blending");

	glutDisplayFunc(display);
	
	glutKeyboardFunc(keyboard);

	glDisable(GL_DEPTH_TEST);

	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, 2, 0, 2, 0, 100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	// *** Blending Funktion setzen
	glEnable(GL_BLEND);

	glutMainLoop();
	return 0;
}
