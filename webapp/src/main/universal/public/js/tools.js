angular.module('tools', ['ui.bootstrap']);
var NlpToolsCtrl = function($scope, $http) {
  $scope.model = { }

  $scope.working = true;
  $http.get("/api/tools", $scope.model.dependencies)
    .success(function(data, status, headers, config) {
      $scope.working = false;
      $scope.model.tools = data;
    })
    .error(function(data, status, headers, config) {
      $scope.working = false;
      $scope.errorMessage = data;
    });

  $scope.runTool = function(name) {
    $scope.working = true;
    $http.post("/api/tools/" + name, $scope.model[name])
      .success(function(data, status, headers, config) {
        $scope.working = false;
        $scope.errorMessage = undefined;

        $scope.response = {};
        $scope.response[name] = data;
      })
      .error(function(data, status, headers, config) {
        $scope.working = false;
        $scope.response = undefined;

        $scope.errorMessage = data;
      });
  }
}

