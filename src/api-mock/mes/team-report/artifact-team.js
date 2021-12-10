import {
  artifactListInfo
} from '../common-mock-data/product-type-data'

const getArtifact = {
  url: '/api/mes/building/team/report/artifact',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content|1-20': [{
          'factory': {
            'id': 1,
            'name': '工厂' + '@cword(2,15)'
          },
          'workshop': {
            'id': 1,
            'name': '车间' + '@cword(2,15)'
          },
          'productionLine': {
            'id': 1,
            'name': '生产线' + '@cword(2,15)'
          },
          'processType|0-1': false,
          'completeStatus|1-15': [{
            'id|+1': 1,
            'taskMete|1-1000.1-8': 1,
            'completeMete|1-1000.1-8': 1,
            'completeRate|1-99.1-8': 1,
            'processName|1': ['组立', '打底焊', '埋弧焊', '电渣焊', '总装', '清磨']
          }]
        }]
      }
    }
  }
}

const getArtifactDetail = {
  url: '/api/mes/building/team/report/artifact/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'content|1-20': [{
          ...artifactListInfo,
          'taskQuantity|1-1000': 1,
          'process|1-10': [{
            'id|1-10': 1,
            'name|1': ['组立', '打底焊', '埋弧焊', '电渣焊', '总装', '清磨'],
            'taskQuantity|1-1000': 1,
            'completeQuantity|1-1000': 1
          }]
        }],
        'process|10': [{
          'id|+1': 1,
          'name|1': ['组立', '打底焊', '埋弧焊', '电渣焊', '总装', '清磨'],
          'taskQuantity|1-1000': 1,
          'completeQuantity|1-1000': 1
        }]
      }
    }
  }
}

const getArtifactProcessDetail = {
  url: '/api/mes/building/team/report/artifact/detail/process',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'content|1-20': [{
          ...artifactListInfo,
          'taskQuantity|1-1000': 1,
          'completeQuantity|1-1000': 1,
          'unCompleteQuantity|1-1000': 1,
          'completeMete|1-1000.1-8': 1
        }]
      }
    }
  }
}

export default [
  getArtifact,
  getArtifactDetail,
  getArtifactProcessDetail
]
