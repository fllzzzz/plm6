import { artifactListInfo } from '../common-mock-data/product-type-data'

const getArtifact = {
  url: '/api/mes/building/print/artifact',
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
          'printedQuantity|1-100': 1
        }]
      }
    }
  }
}

export default [
  getArtifact
]
