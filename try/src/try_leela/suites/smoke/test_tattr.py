# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestTAttr(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_tattr_must_return_null_when_there_is_no_attr(self):
        with self.driver.session("smoke/test_tattr") as session:
            a_guid = helpers.make(session)
            self.assertEqual([], session.execute_fetch("attr get %s \"cpuidle\" [0:1]" % (a_guid,)))

    def test_tattr_must_return_null_when_there_is_no_guid(self):
        with self.driver.session("smoke/test_tattr") as session:
            guid = str(uuid.uuid1())
            self.assertEqual([], session.execute_fetch("attr get %s \"cpuidle\" [0:1]" % (guid,)))

    def test_tattr_types(self):
        with self.driver.session("smoke/test_tattr") as session:
            a_guid = helpers.make(session)
            for (attr_name, attr_time, attr_txvalue, attr_rxvalue) in [
                    ("string", 0, helpers.string_value("value"), "value"),
                    ("int32",  1, helpers.int32_value(1), 1),
                    ("int64",  2, helpers.int64_value(1), 1),
                    ("uint32", 3, helpers.uint32_value(1), 1),
                    ("uint64", 4, helpers.uint64_value(1), 1),
                    ("double", 5, helpers.double_value(1), 1.0)]:
                helpers.tattr_put(session, a_guid, attr_name, attr_time, attr_txvalue)
                self.assertEqual([["t-attr", [a_guid, attr_name, [[attr_time, attr_rxvalue]]]], ["t-attr", [a_guid, attr_name, []]]], session.execute_fetch("attr get %s \"%s\" [0:6]" % (a_guid, attr_name)))

    def test_tattr_with_ttl(self):
        with self.driver.session("smoke/test_tattr") as session:
            ttl    = 5
            a_guid = helpers.make(session)
            helpers.tattr_put(session, a_guid, "cpuidle", 0, helpers.int32_value(1), ttl)
            self.assertEqual([[0.0, 1]], session.execute_fetch("attr get %s \"cpuidle\" [0:1]" % (a_guid,))[0][1][-1])
            helpers.sleep(ttl * 2)
            self.assertEqual([], session.execute_fetch("attr get %s \"cpuidle\" [0:1]" % (a_guid,))[0][1][-1])
