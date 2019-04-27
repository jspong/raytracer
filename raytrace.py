from __future__ import division
from __future__ import print_function

import abc
import collections
import math
import random

class Vec3(collections.namedtuple('Vec3', 'x y z')):

  def __add__(self, other):
    return Vec3(self.x + other.x, self.y + other.y, self.z + other.z)

  def __sub__(self, other):
    return self + (-1 * other)

  def __mul__(self, scale):
    if isinstance(scale, Vec3):
      return Vec3(self.x * scale.x, self.y * scale.y, self.z * scale.z)
    else:
      return Vec3(self.x * scale, self.y * scale, self.z * scale)

  def __rmul__(self, scale):
    return self * scale

  def __neg__(self):
    return self * -1

  def __truediv__(self, scale):
    return self * (1 / scale)

  @property
  def length(self):
    return math.sqrt(self.squared_length)

  @property
  def squared_length(self):
    return self.x * self.x + self.y * self.y + self.z * self.z

  def as_unit(self):
    return self / self.length

  def __str__(self):
    return '%d %d %d' % (self.x, self.y, self.z)

  @classmethod
  def right(cls):
    return cls(1.0, 0.0, 0.0)

  @classmethod
  def up(cls):
    return cls(0, 1.0, 0.0)

  @classmethod
  def forward(cls):
    return cls(0, 0, -1)

  @classmethod
  def zero(cls):
    return cls(0.0, 0.0, 0.0)

  @classmethod
  def ones(cls):
    return cls(1.0, 1.0, 1.0)

  @classmethod
  def rand(cls):
    return cls(random.random(), random.random(), random.random())

class Color(Vec3):

  @classmethod
  def white(cls):
    return cls.ones()

  @classmethod
  def red(cls):
    return cls(1, 0, 0)

def dot(a, b):
  return a.x * b.x + a.y * b.y + a.z * b.z

def cross(a, b):
  return Vec3(a.y * b.z - a.z * b.y,
              a.z * b.x - a.x * b.z,
              a.x * b.y - a.y * b.x)

class Ray(collections.namedtuple('Ray', 'origin direction')):

  def point_at(self, t):
    return self.origin + t * self.direction


def lerp(a, b, t):
  return (1 - t) * a + t * b


def color(ray, hitable, depth=0):
  record = hitable.hit(ray, 0.001, 100000)
  if record is None:
    t = 0.5 * (ray.direction.as_unit().y + 1.0)
    return lerp(Color.white(), Vec3(0.5, 0.7, 1.0), t)
  else:
    if depth > 50:
      return Vec3.zero()
    scattered, attenuation = record.material.scatter(ray, record)
    if scattered is None:
      return Vec3.zero()
    return attenuation * color(scattered, hitable, depth + 1)


HitRecord = collections.namedtuple('HitRecord', 't pos normal material')

class Hitable(object):
 
  __metaclass__ = abc.ABCMeta

  @abc.abstractmethod
  def hit(self, ray, t_min, t_max):
    "Returns None or a HitRecord if the ray hits between t_min and t_max"


class Sphere(Hitable):

  def __init__(self, center, radius, material):
    self._center = center
    self._radius = radius
    self._material = material
  
  def hit(self, ray, t_min, t_max):
    oc = ray.origin - self._center
    a = dot(ray.direction, ray.direction)
    b = dot(oc, ray.direction)
    c = dot(oc, oc) - self._radius * self._radius
    discriminant = b * b - a * c
    if discriminant > 0:
      t = (-b - math.sqrt(discriminant)) / a
      if t < t_max and t > t_min:
        point = ray.point_at(t)
        return HitRecord(t, point, (point - self._center) / self._radius, self._material)
    return None

class HitableList(Hitable):

  def __init__(self, objects=()):
    self._objects = list(objects)

  def hit(self, ray, t_min, t_max):
    record = None
    closest = t_max
    for hitable in self._objects:
      this_record = hitable.hit(ray, t_min, closest)
      if this_record is None:
        continue
      closest = this_record.t
      record = this_record
    return record
 
class Camera(object):
  def __init__(self, lower_left, horizontal, vertical, origin):
    self._lower_left = lower_left
    self._horizontal = horizontal
    self._vertical = vertical
    self._origin = origin

  def get_ray(self, u, v):
    return Ray(self._origin, self._lower_left + u * self._horizontal + v * self._vertical - self._origin)


def random_in_unit_sphere():
  while True:
    p = 2 * Vec3.rand() - Vec3.ones()
    if p.squared_length < 1:
      return p


class Material(object):

  __metaclass__ = abc.ABCMeta

  @abc.abstractmethod
  def scatter(self, ray, hit_record):
    "Return the attenuation and scattered ray, or None if it is absorbed"


class Lambertian(Material):

  def __init__(self, albedo):
    self._albedo = albedo

  def scatter(self, ray, hit_record):
    target = hit_record.pos + hit_record.normal + random_in_unit_sphere()
    scattered = Ray(hit_record.pos, target - hit_record.pos)
    return scattered, self._albedo

def reflect(vector, normal):
  return vector - 2 * dot(vector, normal) * normal


class Metal(Material):
  def __init__(self, albedo):
    self._albedo = albedo

  def scatter(self, ray, hit_record):
    reflected = reflect(ray.direction.as_unit(), hit_record.normal)
    scattered = Ray(hit_record.pos, reflected)
    if dot(scattered.direction, hit_record.normal) > 0:
      return scattered, self._albedo
    return None, None


def refract(vector, normal, ni_over_nt):
  uv = vector.as_unit()
  dt = dot(uv, normal)
  discriminant = 1.0 - ni_over_nt * ni_over_nt * (1 - dt * dt)
  if discriminant > 0:
    return ni_over_nt * (uv - normal * dt) - normal * math.sqrt(discriminant)
  else:
    return None

def schlick(cosine, ref_idx):
  r0 = (1 - ref_idx) / (1 + ref_idx)
  r0 *= r0
  return r0 + (1-r0) * ((1 - cosine) ** 5)

class Dialectric(Material):
  def __init__(self, refraction):
    self._refraction = refraction

  def scatter(self, ray, hit_record):
    if dot(ray.direction, hit_record.normal) > 0:
      outward_normal = -hit_record.normal
      ni_over_nt = self._refraction
      cosine = self._refraction * dot(ray.direction, hit_record.normal) / ray.direction.length
    else:
      outward_normal = hit_record.normal
      ni_over_nt = 1 / self._refraction
      cosine = -dot(ray.direction, hit_record.normal) / ray.direction.length
    refracted = refract(ray.direction, outward_normal, ni_over_nt)
    if refracted:
      reflect_prob = schlick(cosine, self._refraction)
    else:
      reflect_prob = 1.0
    if random.random() < reflect_prob:
      reflected = reflect(ray.direction, hit_record.normal)
      return Ray(hit_record.pos, reflected), Color.white()
    else:
      return Ray(hit_record.pos, refracted), Color.white()
    

def main():
  width, height = 400, 200
  samples = 10
  print("P3")
  print(width, height)
  print(255)

  camera = Camera(Vec3(-2.0, -1.0, -1.0), 4 * Vec3.right(), 2 * Vec3.up(), Vec3.zero())

  objects = HitableList([
     Sphere(Vec3(0,0,-1), 0.5, Lambertian(Vec3(0.8, 0.3, 0.3))),
     Sphere(Vec3(0, -100.5, -1), 100, Lambertian(Vec3(0.8, 0.8, 0.0))),
     Sphere(Vec3(1, 0, -1), 0.5, Metal(Vec3(0.8, 0.6, 0.2))),
     Sphere(Vec3(-1, 0, -1), 0.5, Dialectric(1.5)),
     #Sphere(Vec3(-1, 0, -1), -0.45, Dialectric(1.5))
  ])

  for y in range(height-1, -1, -1):
    for x in range(width):
      c = Vec3.zero()
      for _ in range(samples):
        u = (x + random.random()) / width
        v = (y + random.random()) / height
        r = camera.get_ray(u, v)
        c = c + color(r, objects)
      c = c / samples
      c = Vec3(math.sqrt(c.x), math.sqrt(c.y), math.sqrt(c.z))
      print(255.9 * c)


if __name__ == '__main__':
  main()
