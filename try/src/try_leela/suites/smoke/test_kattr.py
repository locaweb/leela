# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestKAttr(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_kattr_must_return_null_when_there_is_no_attr(self):
        with self.driver.session("smoke/test_kattr") as session:
            a_guid = helpers.make(session)
            self.assertEqual([["k-attr", [a_guid, "foobar", None]]], session.execute_fetch("attr get %s \"foobar\"" % (a_guid,)))

    def test_kattr_must_return_null_when_there_is_no_guid(self):
        with self.driver.session("smoke/test_kattr") as session:
            guid = str(uuid.uuid1())
            self.assertEqual([["k-attr", [guid, "foobar", None]]], session.execute_fetch("attr get %s \"foobar\"" % (guid,)))

    def test_kattr_types(self):
        with self.driver.session("smoke/test_kattr") as session:
            a_guid = helpers.make(session)
            for (attr_name, attr_txvalue, attr_rxvalue) in [("string", helpers.string_value("value"), "value"),
                                                            ("int32", helpers.int32_value(1), 1),
                                                            ("int64", helpers.int64_value(1), 1),
                                                            ("uint32", helpers.uint32_value(1), 1),
                                                            ("uint64", helpers.uint64_value(1), 1),
                                                            ("double", helpers.double_value(1), 1.0)]:
                helpers.kattr_put(session, a_guid, attr_name, attr_txvalue)
                self.assertEqual([["k-attr", [a_guid, attr_name, attr_rxvalue]]], session.execute_fetch("attr get %s \"%s\"" % (a_guid, attr_name)))

    def test_kattr_with_ttl(self):
        with self.driver.session("smoke/test_kattr") as session:
            ttl    = 5
            a_guid = helpers.make(session)
            helpers.kattr_put(session, a_guid, "foobar", helpers.string_value("foobar"), ttl)
            self.assertEqual("foobar", session.execute_fetch("attr get %s \"foobar\"" % (a_guid,))[0][1][-1])
            helpers.sleep(ttl * 2)
            self.assertIsNone(session.execute_fetch("attr get %s \"foobar\"" % (a_guid,))[0][1][-1])
