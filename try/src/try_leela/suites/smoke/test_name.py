# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestName(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_name_without_know_guid_must_produce_404(self):
        with self.driver.session("smoke/test_name") as session:
            guid = str(uuid.uuid1())
            self.assertEqual([["fail", 404, "not found"]], session.execute_fetch("name %s" % (guid,)))

    def test_name_with_recently_created_guid(self):
        with self.driver.session("smoke/test_name") as session:
            name0 = session.execute_fetch("make (%(rnd_name.0)s)")
            name1 = session.execute_fetch("name %s" % (name0[0][1][-1],))
            self.assertEqual(name0, name1)
