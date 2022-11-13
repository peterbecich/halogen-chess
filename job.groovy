multibranchPipelineJob('configuration-as-code') {
  branchSources {
    git {
      id = 'Halogen Chess'
      remote('https://github.com/peterbecich/halogen-chess.git')
    }
  }
}
