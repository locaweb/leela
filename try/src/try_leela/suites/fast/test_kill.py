# -*- coding: utf-8 -*-

import unittest
from try_leela import env
from try_leela import helpers

class TestKill(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_kill_without_right_node(self):
        with self.driver.session("fast/test_kill") as session:
            a_guid = helpers.make(session)
            b_guid = helpers.make(session)
            c_guid = helpers.make(session)
            helpers.link(session, a_guid, "a", b_guid)
            helpers.link(session, a_guid, "c", c_guid)
            self.assertEqual(2, session.execute_fmap(len, "path %s -[*]> ()" % (a_guid,)))
            helpers.kill(session, a_guid, "a")
            self.assertEqual([["path", [["c", c_guid]]]], session.execute_fetch("path %s -[*]> ()" % (a_guid,)))

    def test_kill_with_right_node(self):
        with self.driver.session("fast/test_kill") as session:
            a_guid = helpers.make(session)
            b_guid = helpers.make(session)
            c_guid = helpers.make(session)
            helpers.link(session, a_guid, "foobar", b_guid)
            helpers.link(session, a_guid, "foobar", c_guid)
            self.assertEqual(2, session.execute_fmap(len, "path %s -[foobar]> ()" % (a_guid,)))
            helpers.kill(session, a_guid, "foobar", b_guid)
            self.assertEqual([["path", [["foobar", c_guid]]]], session.execute_fetch("path %s -[foobar]> ()" % (a_guid,)))
