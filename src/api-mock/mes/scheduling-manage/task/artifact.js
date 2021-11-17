const getAssembleSummary = {
  url: '/api/mes/building/scheduling/assemble/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'schedulingQuantity': 15,
          'totalSchedulingWeight': 100.00000000,
          'taskQuantity': 10,
          'totalTaskWeight': 100.00000000,
          'completeQuantity': 10,
          'totalCompleteWeight': 100.00000000,
          'date': '2021-11-11'
        }]
      }
    }
  }
}

const getArtifactSummary = {
  url: '/api/mes/building/scheduling/artifact/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'schedulingQuantity': 20,
          'totalSchedulingNetWeight': null,
          'totalSchedulingGrossWeight': null,
          'taskQuantity': 20,
          'totalTaskNetWeight': 82716.40000000,
          'totalTaskGrossWeight': 82716.40000000,
          'completeQuantity': 0,
          'totalCompleteNetWeight': 0E-8,
          'totalCompleteGrossWeight': 0E-8,
          'date': '2021-11-11'
        }]
      }
    }
  }
}

export default [
  getAssembleSummary,
  getArtifactSummary
]
