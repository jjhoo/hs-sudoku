node {
    checkout scm
    def customImage = docker.build("build-hs-sudoku:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")
    customImage.inside('-v $HOME/.cabal:/home/jenkins/.cabal') {
        stage('Cabal update') {
            sh 'cabal update'
        }
        stage('Build') {
            sh 'cabal build'
        }
        stage('Test') {
            sh 'cabal test'
        }
    }
}
