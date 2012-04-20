#! /usr/bin/env python

from distutils.core import setup
from rlglue.versions import get_codec_version
import rlglue



setup(name="RL_Glue_PythonCodec",
      version=get_codec_version(),
      description="RL-Glue Python Codec",
      author="Brian Tanner",
      author_email=" brian@tannerpages.com",
      url="http://glue.rl-community.org/Home/Extensions/python-codec",
      packages=['rlglue','rlglue.agent','rlglue.environment','rlglue.network','rlglue.utils'],
	  license='Apache License Version 2.0',
     )

