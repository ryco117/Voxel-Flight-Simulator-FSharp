#version 450
layout (location = 0) in vec2 coord;

layout (location = 0) out vec4 fragColor;

const vec3 cameraPosition = vec3(0.0, 0.0, -4.0);

const float pi = 3.14159265358;
const float e = 2.718281828;
const int maxIterations = 50;
const float epsilon = 0.0001;
const vec3 dirX = vec3(1.0, 0.0, 0.0);
const vec3 dirY = vec3(0.0, 1.0, 0.0);
const vec3 dirZ = vec3(0.0, 0.0, 1.0);

// Phong lighting
const float ambientStrength = 0.35;
const vec3 lightDir = normalize(vec3(0.5, 0.6, 0.8));
const vec3 lightColor = vec3(0.9, 0.9, 0.9);
const vec3 ambientLight = ambientStrength * lightColor;

mat3 buildRot3(vec3 u, float theta)
{
	float c = cos(theta);
	float cC = 1.0 - c;
	float s = sin(theta);
	float sC = 1.0 - s;
	return mat3(
		c+u.x*u.x*cC, u.y*u.x*cC+u.z*s, u.z*u.x*cC-u.y*s,
		u.x*u.y*cC-u.z*s, c+u.y*u.y*cC, u.z*u.y*cC+u.x*s,
		u.x*u.z*cC+u.y*s, u.y*u.z*cC-u.x*s, c+u.z*u.z*cC
	);
}

float bound(float x, float b) {
	return mod(x + b, 2.0*b) - b;
}

vec3 gradient;
vec4 orbitTrap;
const float reScale = 0.4;
const float scale = 2.05;
const vec3 center = vec3(sqrt(0.5), sqrt(0.3), sqrt(0.2));
float distanceEstimator(vec3 t, const int iterations)
{
	orbitTrap = vec4(1.0, 1.0, 1.0, 1.0);

	//*
	vec3 s = reScale*t;
	//const vec3 center = vec3(1.0);
	float r2 = dot(s, s);
	float DEfactor = 1.0;
	//const mat3 rotato1 = buildRot3(dirX, 3.0);
	//const mat3 rotato2;

	for(int i = 0; i < iterations; i++) {
		//s *= rotato1;

		if(s.x+s.y<0.0){float x1=-s.y;s.y=-s.x;s.x=x1;}
		if(s.x+s.z<0.0){float x1=-s.z;s.z=-s.x;s.x=x1;}
		if(s.y+s.z<0.0){float y1=-s.z;s.z=-s.y;s.y=y1;}

		//s *= rotato2;

		s = scale*s - (scale - 1.0)*center;
		r2 = dot(s, s);

		orbitTrap.x = min(orbitTrap.x, length(s - dirX)/2.0);
		orbitTrap.y = min(orbitTrap.y, length(s - dirY)/2.0);
		orbitTrap.z = min(orbitTrap.z, length(s - dirZ)/2.0);

		DEfactor *= scale;
	}
	return (sqrt(r2) - 2.0) / DEfactor / reScale;
	/*/
	return length(vec3(bound(t.x, 3.0), bound(t.y, 3.0), bound(t.z, 4.0))) - 0.4;//*/
}

const float maxBrightness = 1.35;
const float maxBrightnessR2 = maxBrightness*maxBrightness;
vec4 scaleColor(float si, vec3 col) {
	col *= pow(1.0 - si/float(maxIterations), 0.9);
	if(dot(col, col) > maxBrightnessR2) {
		col = maxBrightness*normalize(col);
	}
	return vec4(col, 1.0);
}

vec4 phongLighting(vec3 c, float shadow) {
	vec3 diffuse = max(dot(normalize(gradient), -lightDir), 0.0) * lightColor;
	return vec4((ambientLight + diffuse * shadow) * c, 1.0);
}

const float maxDistance = 20.0;
const float fov = pi/1.9 / 2.0;
const float minTravel = 0.2;
const float hitDistance = epsilon;
const float maxShadow = 4.0;
void main(void)
{
	float dy = cos(0.6*coord.y*fov);
	vec3 direction = vec3(sin(coord.x*fov)*dy, -sin(0.6*coord.y*fov), cos(coord.x*fov)*dy);
	vec3 pos = cameraPosition + minTravel*direction;
	float travel = minTravel;
	for(int i = 0; i < maxIterations; i++) {
		int deMaxIter = int(6.0 * pow(1.0 - travel/maxDistance, 0.8) + 3.0);
		float dist = distanceEstimator(pos, deMaxIter);

		//float ratio = travel / maxDistance;
		//float adjustedHitDistance = hitDistance*((1.0 - ratio) + 20.0*ratio);
		float adjustedHitDistance = hitDistance;
		if(dist <= adjustedHitDistance) {
			if(travel <= 4.0) {
				vec4 color = orbitTrap;
				const float shadowStart = dist;
				vec3 newPos = pos - shadowStart*lightDir;
				//vec3 newPos = pos;
				float shadowDist = 1.0;
				float shadowTravel = shadowStart;
				for(int j = 0; j < maxIterations / 2 && shadowTravel < maxShadow; j++) {
					float dist = max(distanceEstimator(newPos, deMaxIter), 0.0);
					shadowDist = min(shadowDist, dist);

					if (dist <= hitDistance) {
						break;
					}

					newPos -= dist*lightDir;
					shadowTravel += dist;
				}
				//gradient = (((vec3(distanceEstimator(pos + epsilon*dirX, deMaxIter), distanceEstimator(pos + epsilon*dirY, deMaxIter), distanceEstimator(pos + epsilon*dirZ, deMaxIter))-dist)/epsilon)-((vec3(distanceEstimator(pos - epsilon*dirX, deMaxIter), distanceEstimator(pos - epsilon*dirY, deMaxIter), distanceEstimator(pos - epsilon*dirZ, deMaxIter))-dist)/epsilon))/2.0;
				gradient = (vec3(distanceEstimator(pos + epsilon*dirX, deMaxIter), distanceEstimator(pos + epsilon*dirY, deMaxIter), distanceEstimator(pos + epsilon*dirZ, deMaxIter))-dist)/epsilon;
				//fragColor = phongLighting(color.xyz, clamp((shadowDist - hitDistance)/(shadowStart - hitDistance), 0.0, 1.0));
				fragColor = phongLighting(color.xyz, 1.0);
			} else {
				vec4 color = orbitTrap;
				gradient = (vec3(distanceEstimator(pos + epsilon*dirX, deMaxIter), distanceEstimator(pos + epsilon*dirY, deMaxIter), distanceEstimator(pos + epsilon*dirZ, deMaxIter))-dist)/epsilon;
				fragColor = scaleColor(i, phongLighting(color.xyz, 1.0).xyz);
			}
			return;
		}

		pos += (0.95*dist)*direction;
		travel += dist;
		if(travel >= maxDistance) break;
	}
	fragColor = vec4(0.0, 0.0, 0.0, 1.0);
	return;
}