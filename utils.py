#!/usr/bin/python

# This file is part of AutoMateHome.

# AutoMateHome is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# AutoMateHome is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with AutoMateHome.  If not, see <http://www.gnu.org/licenses/>.

"""Common utilities"""

import threading
import datetime
import json
import sys

def str_2_bytes(s):
  """Return a list bytes from S, with no decoding"""
  return [ord(c) for c in s]

def bytes_2_str(l, trimTo8Bits=False):
  """Convert the ints in the iterable L to a string. If TRIMTO8BITS is
  True, truncate the numbers before conversion, otherwise throw an
  error for numbers > 8 bits (the default)"""
  if trimTo8Bits:
    return "".join([chr(a & 0xFF) for a in l])
  return "".join([chr(a) for a in l])

def printed_bytes_to_bytes(data):
  """Convenience function to convert a string of printed length-2 hex
  numbers to a list of bytes"""
  data = data.replace(" ", "")
  assert(0 == (len(data) & 1))
  result = []
  for i in range(0, len(data), 2):
    d = data[i:i+2]
    val = int(d, 16)
    result.append(val)
  return result

def bytes_to_printed_bytes(data):
  "Translate the given number list to string format, printing the numbers in hex"
  return "[" + ", ".join(["0x%02x" % i for i in data]) + "]"

def merge_bytes(data):
  "Interpret the list of bytes as a number with the most significant byte first"
  sum = 0;
  multiplier = 1
  for i in range(len(data)-1, -1, -1):
    sum += data[i] * multiplier
    multiplier *= 0x100
  return sum

def mergeDict(src, target, *keyfilter):
  """Return an updated TARGET dictionary (or a new dictionary if not
  provided) using the contents of SRC. If KEYFILTER is provided, use
  only keys in that list."""
  if target is None:
    target = dict()
  if len(keyfilter) > 0:
    for key in keyfilter:
      target[key] = src[key]
  else:
    target.update(src)
  return target

def mergeDictIfNotDefault(src, target, key, defaultValue):
  if src[key] != defaultValue:
    target[key] = src[key]
    return True
  return False

def to_binary_string(n):
  l = []
  while (n != 0):
    l.insert(0, n & 0x1)
    n = n >> 1
  return "".join([str(i) for i in l])

def x10TimeToTime(double_hour, mins_to_120):
  if double_hour > 12:
    raise Exception("nonsensical time spec: %(double_hour)d, %(mins_to_120)d" % locals())
  return datetime.time(2*double_hour + (mins_to_120/60), mins_to_120 % 60)

def x10TimeToString(double_hour, mins_to_120, isTimerInitiator=True):
  if isTimerInitiator and (double_hour > 12):
    if double_hour == 14:
      time = "Sunrise"
    elif double_hour == 15:
      time = "Sunset"
    else:
      raise Exception("unknown double_hour: %(double_hour)d" % locals())
    mins = mins_to_120 % 64
    if mins_to_120 & 0x40:
      mins *= -1
    return (time, mins)
  return x10TimeToTime(double_hour, mins_to_120).strftime("%H:%M")

def x10YearDayToString(year_day, year=None):
  if year is None:
    year=datetime.date.today().year
  timeStr = "%(year_day)d %(year)d" % locals()
  date = datetime.datetime.strptime(timeStr, "%j %Y").date()
  return date.isoformat()


def formatWeekMask(m):
  mask = "SMTWTFS"
  return "".join([(1 & (m >> i)) and mask[i] or "." for i in range(0,len(mask))])

def formatTable(dataTable, hasHeader = False, nspaces=1):
  maxlengths = []
  buf = ""
  data = []
  for row in dataTable:
    data.append([cell or "" for cell in row])

  spaces = " " * nspaces

  def getMaxLen(cell):
    maxLen = 0
    for l in str(cell).split("\n"):
      maxLen = max(maxLen, len(l))
    return maxLen

  for row in data:
    for i,cell in enumerate(row):
      while len(maxlengths) <= i:
        maxlengths.append(0)
      maxlengths[i] = max(maxlengths[i], getMaxLen(cell))
  for i,row in enumerate(data):
    if hasHeader and i == 1:
      buf += spaces.join(["-" * l for l in maxlengths]) + "\n"
    buf += spaces.join([str(cell).ljust(maxlengths[i]) for i,cell in enumerate(row)]) + "\n"
  
  return buf

class Latch:
  "Threading helper class - single shot latch"

  def __init__(self):
    self.isopen = False
    self.condition = threading.Condition()

  def waitForIt(self, timeoutInSecs=None):
    self.condition.acquire()
    self.condition.wait(timeoutInSecs)
    self.condition.release()
    if not self.isopen:
      raise Exception("timed out waiting for latch to open")

  def open(self):    
    self.condition.acquire()
    self.isopen = True
    self.condition.notifyAll()
    self.condition.release()

class JSONEncoder(json.JSONEncoder):
  def default(self, obj):
    if hasattr(obj, "toJSON"):
      return obj.toJSON()
    return json.JSONEncoder.default(self, obj)


class XMLBuilder:
  def __init__(self, treebuilder):
    self.builder = treebuilder
    self.stack = []
  def start(self, tag, attribs = {}):
    for k in attribs.keys():
      attribs[k] = str(attribs[k])
    self.builder.start(tag, attribs)
    self.stack.append(tag)
    return self
  def end(self):
    closeTag = self.stack.pop()
    self.builder.end(closeTag)
    return self
  def data(self, dat):
    self.builder.data(str(dat))
    return self
  def close(self):
    while len(self.stack) > 0:
      self.end()
    return self.builder.close()
  def tostring(self):
    t = self.close()
    mod = sys.modules[t.__module__]
    return mod.tostring(t)

if __name__ == "__main__":

  import unittest

  class Tester(unittest.TestCase):
    def testEncodeDecode(self):
      s = "dfsjgpoisdtr"
      b = str_2_bytes(s)
      self.assertEqual(len(s), len(b))
      self.assertEqual(ord(s[-2]), b[-2])
      s1 = bytes_2_str(b)
      self.assertEqual(s, s1)

    def testPrettyPrinting(self):
      input = "0000002300790862004000000000"
      bytes = printed_bytes_to_bytes(input)
      self.assertEqual(len(input)/2, len(bytes))
      out = bytes_to_printed_bytes(bytes)
      self.assertEqual(input, out.replace(", ", "").replace("0x", "")[1:-1])

  unittest.main()
