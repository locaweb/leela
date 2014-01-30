# -*- coding: utf-8 -*-

import unittest
from try_leela import env

class TestKill(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_kill_must_unlink_two_nodes(self):
        with self.driver.session() as session:
            session.execute("fast/test_kill",
                            "make (%(rnd_name.0)s)",
                            "make (%(rnd_name.1)s)")
            a_guid = session.message()[1][-1]
            b_guid = session.message()[1][-1]
            session.execute("fast/test_kill",
                            "make %s -[foobar]> %s" % (a_guid, b_guid))
            session.execute("fast/test_kill",
                            "kill %s -[foobar]> %s" % (a_guid, b_guid))
            session.execute("fast/test_kill",
                            "path %s -[foobar]> ()" % (a_guid,))
            self.assertIsNone(session.message())
