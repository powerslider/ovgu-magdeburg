// geometry shader for growing hair

#version 150

#define OUT_VERTS 6
#define NORMAL_LENGTH 0.06

layout(triangles) in;
layout(line_strip, max_vertices = OUT_VERTS) out;

in vec3 normal[3];

float grav = 0.001f;

layout(std140) uniform GlobalMatrices
{
	mat4 Projection;
	mat4 View;
};

void main(void)
{
	//Pass-thru!
	gl_Position = vec4(0);
	vec4 currentPosition = gl_in[0].gl_Position;
	vec4 currentNormal = vec4(normal[0] * NORMAL_LENGTH, 0);
				
	gl_Position = Projection * View * gl_Position;
		
	EmitVertex();
		
	for(int j = 1; j < OUT_VERTS; j++){
		currentPosition += currentNormal / OUT_VERTS;
			
		//add gravity to y axis
		currentPosition.y -= j * grav;
			
		// transform to eye coordinates
		gl_Position = Projection * View * currentPosition;
		EmitVertex();
	}
	EndPrimitive();
}
