# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestPath(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_path_without_a_know_guid_produces_no_results(self):
        with self.driver.session("smoke/test_path") as session:
            guid = str(uuid.uuid1())
            self.assertEqual([], session.execute_fetch("path %s -[*]> ()" % (guid,)))

    def test_path_using_prefix(self):
        with self.driver.session("smoke/test_path") as session:
            a_guid = helpers.make(session)
            b_guid = helpers.make(session)
            c_guid = helpers.make(session)
            helpers.link(session, a_guid, "foobar", b_guid)
            helpers.link(session, a_guid, "foobaz", c_guid)
            answer = session.execute_fetch("path %s -[foo*]> ()" % (a_guid,))
            if (answer[0][1][0][-1] == b_guid):
                self.assertEqual([["path",[["foobar", b_guid]]],
                                  ["path",[["foobaz", c_guid]]]], answer)
            else:
                self.assertEqual([["path",[["foobaz", c_guid]]],
                                  ["path",[["foobar", b_guid]]]], answer)

    def test_path_using_suffix(self):
        with self.driver.session("smoke/test_path") as session:
            a_guid = helpers.make(session)
            b_guid = helpers.make(session)
            c_guid = helpers.make(session)
            helpers.link(session, a_guid, "foobar", b_guid)
            helpers.link(session, a_guid, "goobar", c_guid)
            answer = session.execute_fetch("path %s -[*bar]> ()" % (a_guid,))
            if (answer[0][1][0][-1] == b_guid):
                self.assertEqual([["path",[["foobar", b_guid]]],
                                  ["path",[["goobar", c_guid]]]], answer)
            else:
                self.assertEqual([["path",[["goobar", c_guid]]],
                                  ["path",[["foobar", b_guid]]]], answer)

    def test_path_without_arguments(self):
        with self.driver.session("smoke/test_path") as session:
            a_guid  = helpers.make(session)
            b_guid  = helpers.make(session)
            c_guid  = helpers.make(session)
            helpers.link(session, a_guid, "foobar", b_guid)
            helpers.link(session, a_guid, "foobaz", b_guid)
            answer0 = session.execute_fetch("path %s -[*]> ()" % (a_guid,))
            answer1 = session.execute_fetch("path %s" % (a_guid,))
            self.assertEqual(answer0, answer1)

    def test_path_with_restriction(self):
        with self.driver.session("smoke/test_path") as session:
            a_guid  = helpers.make(session)
            b_guid  = helpers.make(session)
            c_guid  = helpers.make(session)
            d_guid  = helpers.make(session)
            helpers.link(session, a_guid, "a", b_guid)
            helpers.link(session, a_guid, "a", c_guid)
            helpers.link(session, b_guid, "x", d_guid)
            helpers.link(session, c_guid, "x", d_guid)
            answer0 = session.execute_fetch("path %s -[a]> %s -[x]> ()" % (a_guid, b_guid))
            answer1 = session.execute_fetch("path %s -[a]> %s -[x]> ()" % (a_guid, c_guid))
            self.assertEqual([["path", [["a", b_guid], ["x", d_guid]]]], answer0)
            self.assertEqual([["path", [["a", c_guid], ["x", d_guid]]]], answer1)
