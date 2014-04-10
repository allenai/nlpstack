angular.module('nlpviz', ['ui.bootstrap']);
var NlpvizCtrl = function($scope, $http) {
  $scope.model = { }

  $scope.visualizeDependencies = function() {
    $scope.working = true;
    $http.post("/viz/dependencies/nlptools/base64", $scope.model.dependencies)
      .success(function(data, status, headers, config) {
        $scope.working = false;
        $scope.errorMessage = undefined;
        $scope.response = {}
        $scope.response.base64 = data;
      })
      .error(function(data, status, headers, config) {
        $scope.working = false;
        $scope.errorMessage = data;
        $scope.response = undefined;
      });
  }
}
