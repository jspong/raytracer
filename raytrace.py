from __future__ import division
from __future__ import print_function

import abc
import collections
import math

class Vec3(collections.namedtuple('Vec3', 'x y z')):

  def __add__(self, other):
    return Vec3(self.x + other.x, self.y + other.y, self.z + other.z)

  def __sub__(self, other):
    return self + (-1 * other)

  def __mul__(self, scale):
    return Vec3(self.x * scale, self.y * scale, self.z * scale)

  def __rmul__(self, scale):
    return self * scale

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


def color(ray, hitable):
  record = hitable.hit(ray, 0, 100000)
  if record is None:
    t = 0.5 * (ray.direction.as_unit().y + 1.0)
    return lerp(Color.white(), Vec3(0.5, 0.7, 1.0), t)
  else:
    return 0.5 * (record.normal + Vec3.ones())


HitRecord = collections.namedtuple('HitRecord', 't pos normal')

class Hitable(object):
 
  __metaclass__ = abc.ABCMeta

  @abc.abstractmethod
  def hit(self, ray, t_min, t_max):
    "Returns None or a HitRecord if the ray hits between t_min and t_max"


class Sphere(Hitable):

  def __init__(self, center, radius):
    self._center = center
    self._radius = radius
  
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
        return HitRecord(t, point, (point - self._center) / self._radius)
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


def main():
  width, height = 200, 100
  print("P3")
  print(width, height)
  print(255)

  camera = Camera(Vec3(-2.0, -1.0, -1.0), 4 * Vec3.right(), 2 * Vec3.up(), Vec3.zero())

  objects = HitableList([Sphere(Vec3(0,0,-1), 0.5),
                         Sphere(Vec3(0, -100.5, -1),100)])
  for y in range(height-1, -1, -1):
    for x in range(width):
      u = x / width
      v = y / height
      r = camera.get_ray(u, v)
      print(255.9 * color(r, objects))


if __name__ == '__main__':
  main()
