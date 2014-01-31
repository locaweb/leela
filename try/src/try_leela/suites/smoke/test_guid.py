# -*- coding: utf-8 -*-

import uuid
import unittest
from try_leela import env
from try_leela import helpers

class TestGUID(unittest.TestCase):

    def setUp(self):
        self.driver = env.driver()

    def test_guid_without_know_name_must_produce_404(self):
        with self.driver.session("smoke/test_guid") as session:
            self.assertEqual([["fail", 404, "not found"]], session.execute_fetch("guid (%(rnd_name.0)s)"))

    def test_guid_with_recently_created_guid(self):
        with self.driver.session("smoke/test_guid") as session:
            name0 = session.execute_fetch("make (%(rnd_name.0)s)")
            name1 = session.execute_fetch("guid (%s)" % (name0[0][1][-2],))
            self.assertEqual(name0, name1)
