#!/usr/bin/env python

from os import system

def make_flags(probabilities, probabilities_only, expectation_only, model_enabled, model2_enabled, normalized_reward):
    return {'probabilities':probabilities,
            'probabilities-only':probabilities_only,
            'expectation-only':expectation_only,
            'model-enabled':model_enabled,
            'model2-enabled':model2_enabled,
            'normalized-reward':normalized_reward}

learning_agents = (
                   ('gp-norm',      make_flags('true',   'true',  'false', 'false', 'false', 'true' ), 'learn --only'),
                   ('p-0-norm',     make_flags('0 true', 'true',  'false', 'false', 'false', 'true' ), 'learn --only'),
                   ('p-num-norm',   make_flags('true',   'true',  'false', 'false', 'false', 'true' ), 'learn --only'),
                   ('pm-0-norm',    make_flags('0 true', 'true',  'false', 'true',  'false', 'true' ), 'learn --only'),
                   ('pm-num-norm',  make_flags('true',   'true',  'false', 'true',  'false', 'true' ), 'learn --only'),
                   ('ph-0-norm',    make_flags('0 true', 'false', 'false', 'false', 'false', 'true' ), 'learn --only'),
                   ('ph-num-norm',  make_flags('true',   'false', 'false', 'false', 'false', 'true' ), 'learn --only'),
                   ('pmh-0-norm',   make_flags('0 true', 'false', 'false', 'true',  'false', 'true' ), 'learn --only'),
                   ('pmh-num-norm', make_flags('true',   'false', 'false', 'true',  'false', 'true' ), 'learn --only'),
                  )

competitor_agents = (
                     ('comp-p-num',     make_flags('true',  'true',  'false', 'false', 'false', 'false'), 'learn --disable'),
                     ('comp-pm-num',    make_flags('true',  'true',  'false', 'true',  'false', 'false'), 'learn --disable'),
                     ('comp-ph-num',    make_flags('true',  'false', 'false', 'false', 'false', 'false'), 'learn --disable'),
                     ('comp-pmh-num',   make_flags('true',  'false', 'false', 'true',  'false', 'false'), 'learn --disable'),
                     ('comp-emh-num',   make_flags('false', 'false', 'false', 'true',  'false', 'false'), 'learn --disable'),
                     ('comp-p-norm',    make_flags('true',  'true',  'false', 'false', 'false', 'true' ), 'learn --disable'),
                     ('comp-pm-norm',   make_flags('true',  'true',  'false', 'true',  'false', 'true' ), 'learn --disable'),
                     ('comp-ph-norm',   make_flags('true',  'false', 'false', 'false', 'false', 'true' ), 'learn --disable'),
                     ('comp-pmh-norm',  make_flags('true',  'false', 'false', 'true',  'false', 'true' ), 'learn --disable'),
                     ('comp-emh-norm',  make_flags('false', 'false', 'false', 'true',  'false', 'true' ), 'learn --disable'),
                    )

rule_template = """sp {%(name)s*elaborate*flags-dynamic
   (state <s> ^name dice)
-->
   (<s> %(flags)s)}
%(extra)s
source dice-model-bot-new.soar
"""

ini_template = """[%(name)s]
source="%(source)s"
write="%(write)s"
qna="%(qna)s"
gp="%(gp)s"
"""

class Config:
    def __init__(self, name, flags, extra):
        self.name = name
        self.flags = flags
        self.extra = extra
    def filename(self):
        return self.name + '.soar'
    def make_flags(self):
        return '\n        '.join('^' + key + ' ' + self.flags[key] for key in self.flags)
    def __str__(self):
       return rule_template % {'name' : self.name, 'flags' : self.make_flags(), 'extra' : self.extra}

def ini_item(config):
    source = 'agents/' + config.filename()
    write = 'write/write_' + config.filename() if not config.name[0] == 'c' else ''
    qna = 'dice-qna.ini'
    gp = 'agents/gp/gp.soar' if 'gp' in config.name else ''
    return ini_template % {'name':config.name,
                           'source':source,
                           'write':write,
                           'qna':qna,
                           'gp':gp}

def ini(configs):
    return '\n'.join(ini_item(config) for config in configs)

def main():
    learning_configs = [Config('dice-' + agent[0], agent[1], agent[2]) for agent in learning_agents] + \
                       [Config('dice-' + agent[0] + '-exp', agent[1], agent[2] + '\nrl --set learning-rate 1\nrl --set decay-mode exp') for agent in learning_agents] + \
                       [Config('dice-' + agent[0] + '-log', agent[1], agent[2] + '\nrl --set learning-rate 1\nrl --set decay-mode log') for agent in learning_agents]
    competitor_configs = [Config(agent[0], agent[1], agent[2]) for agent in competitor_agents]
    configs = learning_configs + competitor_configs
    for config in configs:
        with open(config.filename(), 'w') as f:
            f.write(str(config))
    with open('config.ini', 'w') as f:
        f.write(ini(configs))

if __name__ == '__main__':
    main()
