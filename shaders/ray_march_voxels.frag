#version 450
layout (location = 0) in vec2 coord;

layout (location = 0) out vec4 fragColor;

layout (push_constant) uniform Push {
	float time;
} push;

struct Voxel {
	vec4 averageColour;
	uint ftl;
	uint ftr;
	uint fbl;
	uint fbr;
	uint btl;
	uint btr;
	uint bbl;
	uint bbr;
	uint flags;
};
const uint emptyVoxel = 0xFFFFFFFF;

layout(set = 0, binding = 0) readonly buffer VoxelOctree {   
	Voxel voxels[];
} voxelOctree;

const float pi = 3.14159265358;
const float e = 2.718281828;
const int maxIterations = 100;
const float epsilon = 0.00005;
const vec3 dirX = vec3(1.0, 0.0, 0.0);
const vec3 dirY = vec3(0.0, 1.0, 0.0);
const vec3 dirZ = vec3(0.0, 0.0, 1.0);
const vec3 negDirX = vec3(-1.0, 0.0, 0.0);
const vec3 negDirY = vec3(0.0, -1.0, 0.0);
const vec3 negDirZ = vec3(0.0, 0.0, -1.0);

const vec3 cameraPosition = vec3(0.0, 0.0, -2.5);

// Fog?
const vec4 fogColour = vec4(0.3, 0.3, 0.3, 1.0);

// Phong lighting
const float ambientStrength = 0.25;
const vec3 lightDir = normalize(vec3(-1.0, 1.0, -0.2));
const vec3 lightColor = vec3(0.9, 0.9, 0.9);
const vec3 ambientLight = ambientStrength * lightColor;

const vec3 ftlCell = vec3(-0.5, 0.5, -0.5);
const vec3 ftrCell = vec3(0.5, 0.5, -0.5);
const vec3 fblCell = vec3(-0.5, -0.5, -0.5);
const vec3 fbrCell = vec3(0.5, -0.5, -0.5);
const vec3 btlCell = vec3(-0.5, 0.5, 0.5);
const vec3 btrCell = vec3(0.5, 0.5, 0.5);
const vec3 bblCell = vec3(-0.5, -0.5, 0.5);
const vec3 bbrCell = vec3(0.5, -0.5, 0.5);

vec3 cubeNorm(vec3 t) {
	vec3 s = abs(t);
	if(s.x >= s.y && s.x >= s.z) {
		return vec3(sign(t.x), 0.0, 0.0);
	} else if(s.y >= s.x && s.y >= s.z) {
		return vec3(0.0, sign(t.y), 0.0);
	} else {
		return vec3(0.0, 0.0, sign(t.z));
	}
}

vec3 projectToOutsideDistance(vec3 t) {
	vec3 s = abs(t);
	if(s.x >= s.y && s.x >= s.z) {
		return vec3(sign(t.x) * (1.0 + epsilon), t.y, t.z) - t;
	} else if(s.y >= s.x && s.y >= s.z) {
		return vec3(t.x, sign(t.y) * (1.0 + epsilon), t.z) - t;
	} else {
		return vec3(t.x, t.y, sign(t.z) * (1.0 + epsilon)) - t;
	}
}

bool insideCube(vec3 t) {
	t = abs(t);
	return t.x <= 1.0 && t.y <= 1.0 && t.z <= 1.0;
}

bool projectToRootVoxel(inout vec3 p, vec3 d) {
	if(insideCube(p)) return true;

	vec3 s;
	float t;
	if(abs(p.x) >= 1.0) {
		t = (sign(p.x) - p.x)/d.x;
		if(t >= 0.0) {
			s = p + (t + epsilon)*d;
			if(abs(s.y) <= 1.0 && abs(s.z) <= 1.0) {
				p = s;
				return true;
			}
		}
	}
	if(abs(p.y) >= 1.0) {
		t = (sign(p.y) - p.y)/d.y;
		if(t >= 0.0) {
			s = p + (t + epsilon)*d;
			if(abs(s.x) <= 1.0 && abs(s.z) <= 1.0) {
				p = s;
				return true;
			}
		}
	}
	if(abs(p.z) >= 1.0) {
		t = (sign(p.z) - p.z)/d.z;
		if(t >= 0.0) {
			s = p + (t + epsilon)*d;
			if(abs(s.y) <= 1.0 && abs(s.x) <= 1.0) {
				p = s;
				return true;
			}
		}
	}
	return false;
}

float escapeCubeDistance(vec3 p, vec3 d) {
	vec3 s;
	float t = (sign(d.x) - p.x)/d.x;
	s = p + t*d;
	if(abs(s.y) <= 1.0 && abs(s.z) <= 1.0) {
		return t + epsilon;
	}

	t = (sign(d.y) - p.y)/d.y;
	s = p + t*d;
	if(abs(s.x) <= 1.0 && abs(s.z) <= 1.0) {
		return t + epsilon;
	}

	return (sign(d.z) - p.z)/d.z + epsilon;
}

uint voxelIndex(inout vec3 p, inout float scale, int scaleDepth) {
	uint index = 0;

	// Determine smallest scale voxel cell this point exists in
	int i = 0;
	do {
		if(index == emptyVoxel) return index;

		scale *= 2.0;
		if(p.x > 0.0) {
			if(p.y > 0.0) {
				if(p.z > 0.0) {
					index = voxelOctree.voxels[index].btr;
					p -= btrCell;
				} else {
					index = voxelOctree.voxels[index].ftr;
					p -= ftrCell;
				}
			} else {
				if(p.z > 0.0) {
					index = voxelOctree.voxels[index].bbr;
					p -= bbrCell;
				} else {
					index = voxelOctree.voxels[index].fbr;
					p -= fbrCell;
				}
			}
		} else {
			if(p.y > 0.0) {
				if(p.z > 0.0) {
					index = voxelOctree.voxels[index].btl;
					p -= btlCell;
				} else {
					index = voxelOctree.voxels[index].ftl;
					p -= ftlCell;
				}
			} else {
				if(p.z > 0.0) {
					index = voxelOctree.voxels[index].bbl;
					p -= bblCell;
				} else {
					index = voxelOctree.voxels[index].fbl;
					p -= fblCell;
				}
			}
		}
		p *= 2.0;
	} while(++i < scaleDepth && voxelOctree.voxels[index].flags == 0);
	return index;
}

const float maxDistance = 50.0;
float castShadowRay(vec3 p, vec3 d, int maxDepth) {
	if(!projectToRootVoxel(p, d)) return 1.0;

	float travelDist = 0.0;
	int i = 0;
	do {
		vec3 s = p;
		float scale = 1.0;
		uint index = voxelIndex(s, scale, maxDepth);

		// Is empty or filled?
		if(index == emptyVoxel) {
			float t = escapeCubeDistance(s, d) / scale;
			p += t * d;
			travelDist += t;
		} else {
			return 0.0;
		}
	} while(insideCube(p) && ++i < maxIterations && travelDist < maxDistance);
	return 1.0;
}

const float maxBrightness = 1.3;
const float maxBrightnessR2 = maxBrightness*maxBrightness;
vec4 scaleColor(float si, vec3 col) {
	col *= pow(1.0 - si/float(maxIterations), 0.9);
	if(dot(col, col) > maxBrightnessR2) {
		col = maxBrightness*normalize(col);
	}
	return vec4(col, 1.0);
}

vec3 gradient;
vec3 phongLighting(vec3 c, float shadow) {
	vec3 diffuse = max(dot(normalize(gradient), lightDir), 0.0) * lightColor;
	return (ambientLight + diffuse * shadow) * c;
}

vec4 castVoxelRay(vec3 p, vec3 d) {
	gradient = vec3(0.0);
	if(!projectToRootVoxel(p, d)) return fogColour;

	float travelDist = 0.0;
	int i = 0;
	do {
		vec3 s = p;
		float scale = 1.0;
		int maxDepth = min(7 - int(0.25*log(travelDist + epsilon)), 12);
		uint index = voxelIndex(s, scale, maxDepth);
		//uint index = voxelIndex(s, scale, 9);

		// Is empty or filled?
		if(index == emptyVoxel) {
			float t = escapeCubeDistance(s, d) / scale;
			p += t * d;
			travelDist += t;
		} else {
			gradient = cubeNorm(s);

			p += projectToOutsideDistance(s) / scale;
			return vec4(phongLighting(voxelOctree.voxels[index].averageColour.xyz, castShadowRay(p, lightDir, maxDepth)), 1.0);
		}
	} while(insideCube(p) && ++i < maxIterations && travelDist < maxDistance);
	return fogColour;
}

const float fov = pi/1.9 / 2.0;
void main(void) {
	float dy = cos(0.6*coord.y*fov);
	vec3 direction = vec3(sin(coord.x*fov)*dy, -sin(0.6*coord.y*fov), cos(coord.x*fov)*dy);
	vec3 pos = cameraPosition;

	float delta = push.time / 8.0;
	float sdelta = sin(delta);
	float cdelta = cos(delta);
	direction.xz = vec2(cdelta*direction.x + sdelta*direction.z, cdelta*direction.z - sdelta*direction.x);
	pos.xz = vec2(cdelta*pos.x + sdelta*pos.z, cdelta*pos.z - sdelta*pos.x);
	fragColor = castVoxelRay(pos, direction);
}