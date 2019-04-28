from __future__ import division
from __future__ import print_function

import abc
import argparse
import collections
import functools
import itertools
import math
import multiprocessing
import random
import sys

import numpy
import numpy.linalg

def unit(v):
  return v / numpy.sqrt(numpy.sum(v ** 2))

def dot(a, b):
  return numpy.dot(a,b)

def cross(a, b):
  return numpy.cross(a,b)

class Ray(collections.namedtuple('Ray', 'origin direction')):

  def point_at(self, t):
    return self.origin + t * self.direction


def lerp(a, b, t):
  return (1 - t) * a + t * b

def color(ray, hitable, depth=0):
  record = hitable.hit(ray, 0.001, 100000)
  if record is None:
    t = 0.5 * (unit(ray.direction)[0] + 1.0)
    return lerp(numpy.ones(3), numpy.array([0.5, 0.7, 1.0]), t)
  else:
    if depth > 50:
      return numpy.zeros(3)
    scattered, attenuation = record.material.scatter(ray, record)
    if scattered is None:
      return numpy.zeros(3)
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
    b = 2 * dot(oc, ray.direction)
    c = dot(oc, oc) - self._radius * self._radius
    roots = numpy.roots([a, b, c])
    real = roots.real[abs(roots.imag)<1e-5]
    for t in numpy.flip(real):
      if t < t_max and t > t_min:
        point = ray.point_at(t)
        normal = (point - self._center) / self._radius
        return HitRecord(t, point, normal, self._material)
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
  def __init__(self, look_from, look_at, up, vfov, aspect, aperture, focus_dist):
    theta = vfov * math.pi / 180.
    half_height = math.tan(theta / 2)
    half_width = aspect * half_height
    w = unit(look_from - look_at)
    u = unit(cross(up, w))
    v = cross(w, u)
    self._origin = look_from
    self._radius = aperture / 2
    self._lower_left = look_from - half_width * focus_dist * u - half_height * focus_dist * v - focus_dist * w
    self._horizontal = 2 * half_width * focus_dist * u
    self._vertical = 2 * half_height * focus_dist * v
    self._w = w
    self._u = u
    self._v = v

  def get_ray(self, u, v):
    rd = self._radius * random_in_unit_disk()
    offset  = self._u * rd[0] + self._v * rd[1]
    return Ray(self._origin + offset, self._lower_left + u * self._horizontal + v * self._vertical - self._origin - offset)


def random_in_unit_disk():
  while True:
    p = 2 * numpy.array([random.random(), random.random(), 0]) - numpy.array([1, 1, 0])
    if dot(p, p) < 1.0:
      return p

def random_in_unit_sphere():
  while True:
    p = 2 * numpy.random.random(3) - numpy.ones(3)
    if numpy.sum(p ** 2) < 1:
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
  def __init__(self, albedo, fuzz):
    self._albedo = albedo
    self._fuzz = fuzz

  def scatter(self, ray, hit_record):
    reflected = reflect(unit(ray.direction), hit_record.normal)
    scattered = Ray(hit_record.pos, reflected + self._fuzz * random_in_unit_sphere())
    if dot(scattered.direction, hit_record.normal) > 0:
      return scattered, self._albedo
    return None, None


def refract(vector, normal, ni_over_nt):
  uv = unit(vector)
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
      cosine = self._refraction * dot(ray.direction, hit_record.normal) / length(ray.direction)
    else:
      outward_normal = hit_record.normal
      ni_over_nt = 1 / self._refraction
      cosine = -dot(ray.direction, hit_record.normal) / length(ray.direction)
    refracted = refract(ray.direction, outward_normal, ni_over_nt)
    if refracted is None:
      reflect_prob = 1.0
    else:
      reflect_prob = schlick(cosine, self._refraction)
    if random.random() < reflect_prob:
      reflected = reflect(ray.direction, hit_record.normal)
      return Ray(hit_record.pos, reflected), numpy.ones(3)
    else:
      return Ray(hit_record.pos, refracted), numpy.ones(3)
    

def render((y, x), width, height, camera, objects, samples):
  c = numpy.array([0., 0., 0.])
  for _ in range(samples):
    u = (x + random.random()) / width
    v = (y + random.random()) / height
    r = camera.get_ray(u, v)
    c += color(r, objects)
  c /= samples
  c = numpy.sqrt(c)
  return 255.9 * c


def length(v):
  return numpy.sqrt(numpy.sum(v**2))


def parse_args():
  parser = argparse.ArgumentParser()
  parser.add_argument('--width', type=int, default=640)
  parser.add_argument('--height', type=int, default=480)
  parser.add_argument('--samples', type=int, default=10)
  parser.add_argument('--workers', type=int, default=8)
  return parser.parse_args()


def main():
  args = parse_args()
  width, height = args.width, args.height
  print("P3")
  print(width, height)
  print(255)

  look_from = numpy.array([-3., 3., 2.])
  look_at = numpy.array([0., 0., -1.])
  camera = Camera(look_from, look_at, numpy.array([0., 1.0, 0.]), 30, width / height, 2.0, length(look_from - look_at))

  objects = HitableList([
     Sphere(numpy.array([0.0, 0.0, -1.0]), 0.5, Lambertian(numpy.array([0.3, 0.6, 0.3]))),
     Sphere(numpy.array([0.0, -100.5, -1.0]), 100.0, Lambertian(numpy.array([0.8, 0.8, 0.0]))),
     Sphere(numpy.array([1.0, 0.0, -1.0]), 0.5, Metal(numpy.array([0.8, 0.6, 0.2]), 0.3)),
     Sphere(numpy.array([-1.0, 0.0, -1.0]), 0.5, Dialectric(1.5)),
     Sphere(numpy.array([-1.0, 0.0, -1.0]), -0.45, Dialectric(1.5)),
  ])

  pool = multiprocessing.Pool(args.workers)
  pixels = itertools.product(range(height-1, -1, -1), range(width))

  image = pool.map(
      functools.partial(render, width=width, height=height,
                                camera=camera, objects=objects,
                                samples=args.samples),
      pixels)
  for pixel in image:
    print(int(pixel[0]), int(pixel[1]), int(pixel[2]))


if __name__ == '__main__':
  main()
