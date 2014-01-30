# -*- coding: utf-8 -*-

import unittest
from try_leela import env

class TestMake(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_make_returns_name(self):
        with self.driver.session() as session:
            session.execute("fast/test_make", "make (%(rnd_name.0)s)")
            self.assertEqual("name", session.message()[0])

    def test_multiple_make_returns_multiple_names(self):
        with self.driver.session() as session:
            session.execute("fast/test_make",
                            "make (%(rnd_name.0)s)",
                            "make (%(rnd_name.1)s)")
            self.assertEqual("name", session.message()[0])
            self.assertEqual("name", session.message()[0])

    def test_linking_two_vertexes(self):
        with self.driver.session() as session:
            session.execute("fast/test_make",
                            "make (%(rnd_name.0)s)",
                            "make (%(rnd_name.1)s)")
            a_guid = session.message()[1][-1]
            b_guid = session.message()[1][-1]
            session.execute("fast/test_make",
                            "make %s -[foobar]> %s" % (a_guid, b_guid))
            session.execute("fast/test_make",
                            "path %s -[foobar]> ()" % (a_guid,))
            self.assertEqual(["path", [["foobar", b_guid]]], session.message())
