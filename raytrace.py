from __future__ import print_function

def main():
  width, height = 200, 100
  print("P3")
  print(width, height)
  print(255)
  for y in range(height-1, -1, -1):
    for x in range(width):
      r, g, b = float(x) / width, float(y) / height, 0.2
      r *= 255.99
      g *= 255.99
      b *= 255.99
      print(int(r), int(g), int(b))


if __name__ == '__main__':
  main()
