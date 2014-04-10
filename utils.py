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

def unmerge_bytes(data, expectedLength=0):
  """Reverse of merge_bytes() - break down the given number into an
  8-bit number list, with the most significant byte returned first in
  the list"""
  bytes = []
  while data > 0:
    bytes.insert(0, data & 0xFF)
    data = data >> 8
  while len(bytes) < expectedLength:
    bytes.insert(0, 0)
  if expectedLength > 0 and len(bytes) != expectedLength:
    raise Exception("%(data)d is too big to fit into %(expectedLength)d bytes" % locals())
  return bytes

def merge_dict(src, target, *keyfilter):
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

def merge_dict_if_not_default(src, target, key, defaultValue):
  """Copy the key,value pair specified by KEY into TARGET from SRC if
  the value is not equal to DEFAULTVALUE"""
  if key in src and src[key] != defaultValue:
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

def convert_key_to_hex_string(d, *keyfilter):
  """Change the numbers in the dictionary D with keys in the list
  KEYFILTER to length-4 zero padded hexadecimal strings."""
  for k in keyfilter:
    d[k] = "0x%04x" % d[k]

def x10_time_to_time(double_hour, mins_to_120):
  """Convert the X10 format date to a datetime.time
  instance. DOUBLE_HOUR represents the hour of the day/2, MINS_TO_120
  is the number of minutes between these 2-hour markers."""
  if double_hour > 12:
    raise Exception("nonsensical time spec: %(double_hour)d, %(mins_to_120)d" % locals())
  return datetime.time(2*double_hour + (mins_to_120/60), mins_to_120 % 60)

def time_to_x10_time(time):
  """Convert the datetime.time instance to x10 representation"""
  mins = 60 * (time.hour % 2) + time.minute
  hour = time.hour / 2
  return (hour, mins)

def timespec_to_x10_time(timespec, isTimerInitiator=False):
  """Convert HH:MM string to x10 representation"""
  if isTimerInitiator and 2 == len(timespec):
    assert(isinstance(timespec[0], str), isinstance(timespec[0], unicode))
    if timespec[0].lower() == "sunrise":
      double_hour = 14
    elif timespec[0].lower() == "sunset":
      double_hour = 15
    mins = timespec[1]
    if mins < 0:
      mins = abs(mins) & 0x40
    return double_hour, mins
  else:
    assert(5 == len(timespec))
    return time_to_x10_time(datetime.datetime.strptime(timespec, "%H:%M").time())

def x10_time_to_string(double_hour, mins_to_120, isTimerInitiator=True):
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
    mins = mins_to_120 % 0x40
    if mins_to_120 & 0x40:
      mins *= -1
    return (time, mins) # tuple of the special time and its offset, e.g. "15 mins before sunset"
  # ordinary ISO time string:                          
  return x10_time_to_time(double_hour, mins_to_120).strftime("%H:%M")

def x10_year_day_to_string(year_day, year=None):
  """Format the given year day (1-366) as ISO-8601 date. Note that
  dates after Feb 28th will be different for the same year day in leap
  years. If YEAR is given it is used in the calculation, otherwise the
  system clock's year is used."""
  if year is None:
    year=datetime.date.today().year
  timeStr = "%(year_day)d %(year)d" % locals()
  date = datetime.datetime.strptime(timeStr, "%j %Y").date()
  return date.isoformat()

def datestring_to_x10_year_day(dateStr):
  date = datetime.datetime.strptime(dateStr, "%Y-%m-%d").date()
  return int(date.stftime("%j"))

def alignToBoundary(address, alignment):
  """Return a number rounded to the nearest boundary specified by
  ALIGNMENT. If ALIGNMENT is positive round up, otherwise round
  down."""
  remainder = address % (-1 * alignment)
  return address - remainder
  

def mask_to_numbers(mask):
  i = 0;
  result = []
  while mask > 0:
    if 1 & mask:
      result.insert(0, i)
    i += 1
    mask = mask >> 1
  return result

def numbers_to_mask(numbers):
  sum = 0
  for f in numbers:
    sum |= (1 << f)
  return sum

def week_mask_to_string(m):
  mask = "SMTWTFS"
  return "".join([(1 & (m >> i)) and mask[i] or "." for i in range(0,len(mask))])

def string_to_week_mask(m):
  if len(m) != 7:
    raise Exception("invalid week day mask: \"%s\"" % m)
  mask = 0
  for i in range(0,7):
    if m[i] != ".":
      mask |= (1 << i)
  return mask

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

  def setFromByteArray(self, idx, vals):
    self.__rom[idx:idx+len(vals)] = vals
    return idx + len(vals)

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
      self.assertEqual("2004-01-01", x10_year_day_to_string(1, leap_year))
      self.assertEqual("2004-02-29", x10_year_day_to_string(60, leap_year))
      self.assertEqual("2005-03-01", x10_year_day_to_string(60, leap_year+1))
      self.assertEqual("2004-12-31", x10_year_day_to_string(366, leap_year))
      self.assertRaises(Exception, lambda: x10_year_day_to_string(367, leap_year))

    def testMaskToNumbers(self):
      mask = 0x156 # 101010110
      result = mask_to_numbers(mask)
      self.assertEqual([8, 6, 4, 2, 1], result)
      mask1 = numbers_to_mask(result)
      self.assertEqual(mask, mask1)

    def testWeekMask(self):
      mask = 0x56 # 1010110
      s = week_mask_to_string(mask)
      self.assertEqual(".MT.T.S", s)
      self.assertEqual(mask, string_to_week_mask(s))

    def testMergeBytes(self):
      self.assertEqual(0x1234abcd, merge_bytes([0x12, 0x34, 0xab, 0xcd]))

    def testUnMergeBytes(self):
      self.assertEqual([0x12, 0x34, 0xab, 0xcd], unmerge_bytes(0x1234abcd))

    def testAligment(self):
      self.assertEqual(16, alignToBoundary(17, -4))
      self.assertEqual(21, alignToBoundary(18, 7))

  unittest.main()
