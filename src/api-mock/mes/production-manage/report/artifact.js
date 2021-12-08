import { artifactListInfo } from '../../common-mock-data/product-type-data'

const getArtifact = {
  url: '/api/mes/building/production_statements/artifact/page',
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
        'content|1-100': [{
          ...artifactListInfo,
          date: '@datetime'
        }]
      }
    }
  }
}

const getArtifactSummary = {
  url: '/api/mes/building/production_statements/artifact/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'quantity|1-1000': 1,
        'totalGrossWeight|1-10000.1-8': 1,
        'totalNetWeight|1-10000.1-8': 1
      }
    }
  }
}

export default [
  getArtifact,
  getArtifactSummary
]
