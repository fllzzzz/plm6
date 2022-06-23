import {
  enclosureListInfo
} from '../../common-mock-data/product-type-data'

const enclosureDetail = {
  url: '/api/mes/building/kanban/product/detail/enclosure',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        ...enclosureListInfo,
        'processSummaryDetailsList': [{
          'name': '部件1',
          'completeQuantity': 1,
          'inspectionQuantity': 1
        },
        {
          'name': '部件2',
          'completeQuantity': 1,
          'inspectionQuantity': 0
        }
        ]
      }
    }
  }
}

export default [
  enclosureDetail
]
