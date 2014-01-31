# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestNAttr(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_nattr_must_return_an_empty_list_when_there_is_no_attrs(self):
        with self.driver.session("smoke/test_nattr") as session:
            a_guid = helpers.make(session)
            self.assertEqual([["n-attr", [a_guid, []]]], session.execute_fetch("attr list %s \"*\"" % (a_guid,)))

    def test_nattr_must_return_an_empty_list_when_there_is_no_guid(self):
        with self.driver.session("smoke/test_nattr") as session:
            guid = str(uuid.uuid1())
            self.assertEqual([["n-attr", [guid, []]]], session.execute_fetch("attr list %s \"*\"" % (guid,)))

    def test_nattr_by_prefix(self):
        with self.driver.session("smoke/test_nattr") as session:
            a_guid = helpers.make(session)
            helpers.kattr_put(session, a_guid, "foobar", helpers.string_value("foobar"))
            helpers.kattr_put(session, a_guid, "foobaz", helpers.string_value("foobar"))
            helpers.kattr_put(session, a_guid, "zoobar", helpers.string_value("foobar"))
            answer = session.execute_fetch("attr list %s \"foo*\"" % (a_guid,))
            if ("foobar" == answer[0][1][1][0]):
                self.assertEqual([["n-attr", [a_guid, ["foobar", "foobaz"]]]], answer)
            else:
                self.assertEqual([["n-attr", [a_guid, ["foobaz", "foobar"]]]], answer)

    def test_nattr_by_suffix(self):
        with self.driver.session("smoke/test_nattr") as session:
            a_guid = helpers.make(session)
            helpers.kattr_put(session, a_guid, "a_foobar", helpers.string_value("foobar"))
            helpers.kattr_put(session, a_guid, "b_foobar", helpers.string_value("foobar"))
            helpers.kattr_put(session, a_guid, "foobaz", helpers.string_value("foobar"))
            answer = session.execute_fetch("attr list %s \"*bar\"" % (a_guid,))
            if ("a_foobar" == answer[0][1][1][0]):
                self.assertEqual([["n-attr", [a_guid, ["a_foobar", "b_foobar"]]]], answer)
            else:
                self.assertEqual([["n-attr", [a_guid, ["b_foobar", "a_foobar"]]]], answer)
