import {
  artifactListInfo
} from '../../common-mock-data/product-type-data'

const artifactDetail = {
  url: '/api/mes/building/kanban/product/detail/artifact',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        ...artifactListInfo,
        'processSummaryDetailsList': [{
          'name': '组立1',
          'completeQuantity': 8,
          'inspectionQuantity': 8
        },
        {
          'name': '组立2',
          'completeQuantity': 1,
          'inspectionQuantity': 1
        },
        {
          'name': '组立3',
          'completeQuantity': 1,
          'inspectionQuantity': 0
        }
        ]
      }
    }
  }
}

const assembleDetail = {
  url: '/api/mes/building/kanban/product/detail/assemble',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'areaId': 3,
        'id': 1,
        'monomerId': 3,
        'processSummaryDetailsList|1-5': [{
          'completeQuantity': 0,
          'inspectionQuantity': 0,
          'name': '@cword(2,5)'
        }],
        'producedQuantity|1-100': 1,
        'projectId': 38,
        'quantity|1-100': 1,
        'remark': '',
        'serialNumber': '@word()',
        'usedQuantity|1-100': 1
      }
    }
  }
}

export default [
  artifactDetail,
  assembleDetail
]
