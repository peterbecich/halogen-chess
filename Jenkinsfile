pipeline {
    agent any
    // https://www.jenkins.io/blog/2017/09/25/declarative-1/
    // https://www.jenkins.io/doc/book/pipeline/docker/#dockerfile
    stages {
        stage('Build') {
            parallel {
                stage('build front-end') {
                    steps {
                        echo 'Building...'
                        sh 'spago build'
                    }
                }
                stage('build back-end') {
                    steps {
                        echo 'Building...'
                        sh 'which cabal'
                        sh 'ls'
                        sh 'cabal update'
                        sh 'cabal build'
                    }
                }
                // stage('build Docker image') {
                //     agent {
                //         dockerfile true
                //         // docker {
                //         //     args '-v $HOME/.cabal:/root/.cabal'
                //         // }
                //     }
                //     steps {
                //         echo 'Built.'
                //     }
                // }
            }
        }
    }
}
