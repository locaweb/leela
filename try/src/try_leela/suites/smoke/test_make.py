# -*- coding: utf-8 -*-

import unittest
from try_leela import env
from try_leela import helpers

class TestMake(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_make_returns_name(self):
        with self.driver.session("fast/test_make") as session:
            session.execute("make (%(rnd_name.0)s)")
            self.assertEqual("name", session.message()[0])

    def test_linking_two_vertexes(self):
        with self.driver.session("fast/test_make") as session:
            a_guid = helpers.make(session)
            b_guid = helpers.make(session)
            helpers.link(session, a_guid, "foobar", b_guid)
            self.assertEqual([["path", [["foobar", b_guid]]]], session.execute_fetch("path %s -[foobar]> ()" % (a_guid,)))
