import {
  artifactListInfo,
  enclosureListInfo
} from '../../common-mock-data/product-type-data'

const artifact = {
  url: '/api/mes/building/kanban/form/artifact/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasNextPage': false,
        'hasPreviousPage': false,
        'totalElements': 1,
        'content|1-100': [{
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
        }]
      }
    }
  }
}

const enclosure = {
  url: '/api/mes/building/kanban/form/enclosure/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasNextPage': false,
        'hasPreviousPage': false,
        'totalElements': 1,
        'content|1-100': [{
          ...enclosureListInfo,
          'processSummaryDetailsList': [{
            'name': '组立1',
            'completeQuantity': 1,
            'inspectionQuantity': 1
          },
          {
            'name': '组立2',
            'completeQuantity': 1,
            'inspectionQuantity': 0
          }
          ]
        }]
      }
    }
  }
}

export default [
  artifact,
  enclosure
]
