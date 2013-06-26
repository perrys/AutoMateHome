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
  """Copy the key,value pair specified by KEY into TARGET from SRC if
  the value is not equal to DEFAULTVALUE"""
  if src[key] != defaultValue:
    target[key] = src[key]
    return True
  return False

def to_binary_string(n):
  """Simple binary string formatter. I believe that this is supported
  by python format language in later versions of the standard
  library."""
  l = []
  while (n != 0):
    l.insert(0, n & 0x1)
    n = n >> 1
  return "0b" + "".join([str(i) for i in l])

def convertKeyToHexString(d, *keyfilter):
  """Change the numbers in the dictionary D with keys in the list
  KEYFILTER to length-4 zero padded hexadecimal strings."""
  for k in keyfilter:
    d[k] = "0x%04x" % d[k]

def x10TimeToTime(double_hour, mins_to_120):
  """Convert the X10 format date to a datetime.time
  instance. DOUBLE_HOUR represents the hour of the day/2, MINS_TO_120
  is the number of minutes between these 2-hour markers."""
  if double_hour > 12:
    raise Exception("nonsensical time spec: %(double_hour)d, %(mins_to_120)d" % locals())
  return datetime.time(2*double_hour + (mins_to_120/60), mins_to_120 % 60)

def x10TimeToString(double_hour, mins_to_120, isTimerInitiator=True):
  """Convert the X10 format date to a string, as sepcified above, but
  also check for special values in the CM15 timer initiator bytecode
  which specify the variable times of sunrise and sunset."""
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
  """Format the given year day (1-366) as ISO-8601 date. Note that
  dates after Feb 28th will be different for the same year day in leap
  years. If YEAR is given it is used in the calculation, otherwise the
  system clock's year is used."""
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
    self.isAcknowledged = False
    self.condition = threading.Condition()

  def waitForIt(self, timeoutInSecs=None):
    self.condition.acquire()
    if not self.isopen:
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

class MemoryBuffer:

  def __init__(self, copy=None, debug=False, capacity=0x2000):
    self.__debug = debug
    if copy is not None:
      self.__rom = list(copy.__rom)
    else:
      self.__rom = [None for i in range(0, capacity)]

  def __len__(self):
    return len(self.__rom)

  def __getitem__(self, idx):
    l = self.__rom[idx]
    if self.__debug:
      if isinstance(idx, slice):
        for b in l:
          if b is None:
            raise Exception("attempt to read null byte in %s" %idx)
        for i in range(idx.start, idx.stop, idx.step or 1):
          self.__rom[i] = None
      else:
        if self.__rom[idx] is None:
          raise Exception("attempt to read null byte at 0x%04x" % idx)
        self.__rom[idx] = None
    return l

  def __setitem__(self, idx, val):
    self.__rom[idx] = val

  def peek(self, idx):
    return self.__rom[idx]

  def dump(self, fh):
    for i in range(0, len(self.__rom), 16):
      fh.write("0x%04x " % i)
      for j in range(0, 4):
        for k in range(0,4):
          b = self.__rom[i + j*4 + k]
          s = ".."
          if b is not None:
            s = "%02x" % b
          fh.write(s)
        fh.write(" ")
      fh.write("\n")

  def describeUnread(self):
    l = []
    for i in range(0, len(self)):
      if self.__rom[i] is None:
        if len(l) > 0 and l[-1][-1] is None:
          l[-1][-1] = i-1
      else:
        if len(l) == 0 or l[-1][-1] is not None:
          l.append([i, None])
    if len(l) > 0 and l[-1][-1] is None:
      l[-1][-1] = i
    return l

  def used(self):
    emptyCount = 0
    totalLength = len(self.__rom)
    for b in self.__rom:
      if b is None:
        emptyCount += 1
    return totalLength-emptyCount




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

    def testYearDayConversion(self):
      leap_year = 2004
      self.assertEqual("2004-01-01", x10YearDayToString(1, leap_year))
      self.assertEqual("2004-02-29", x10YearDayToString(60, leap_year))
      self.assertEqual("2005-03-01", x10YearDayToString(60, leap_year+1))
      self.assertEqual("2004-12-31", x10YearDayToString(366, leap_year))
      self.assertEqual("2004-12-31", x10YearDayToString(367, leap_year))


  unittest.main()
