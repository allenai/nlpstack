angular.module('tools', ['ui.bootstrap']);
var NlpToolsCtrl = function($scope, $http) {
  $scope.model = { }

  $scope.working = true;
  $http.get("/api/tools")
    .success(function(data, status, headers, config) {
      $scope.working = false;
      $scope.model.tools = data;

      data.forEach(function (tool) {
        $scope.model.toolInfo = {};
        $http.get("/api/tools/" + tool).success(function(data, status, headers, config) {
          $scope.model.toolInfo[tool] = data;
        });
      });
    })
    .error(function(data, status, headers, config) {
      $scope.working = false;
      $scope.errorMessage = data;
    });

  $scope.runTool = function(tool) {
    $scope.working = true;
    $http.post("/api/tools/" + tool, $scope.model[tool])
      .success(function(data, status, headers, config) {
        $scope.working = false;
        $scope.errorMessage = undefined;

        $scope.response = {};
        $scope.response[tool] = data;
      })
      .error(function(data, status, headers, config) {
        $scope.working = false;
        $scope.response = undefined;

        $scope.errorMessage = data;
      });
  }

  $scope.showExample = function(tool) {
    $scope.model[tool] = $scope.model.toolInfo[tool].example;
  }
}

