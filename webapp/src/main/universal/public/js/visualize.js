angular.module('visualize', ['ui.bootstrap']);
var VisualizeCtrl = function($scope, $http) {
  $scope.model = { }

  $scope.showExample = function() {
    $scope.model.dependencies = "nsubj(ran-2, Michael-1)\nroot(ROOT-0, ran-2)\n" +
        "prt(ran-2, down-3)\ndet(hill-5, the-4)\ndobj(ran-2, hill-5)";
    $scope.visualizeDependencies();
  }

  $scope.visualizeDependencies = function() {
    $scope.working = true;
    $http.post("/viz/dependencies/base64", $scope.model.dependencies)
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
