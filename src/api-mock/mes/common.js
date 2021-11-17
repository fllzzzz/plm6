const getFactorySimple = {
  url: '/api/mes/building/factory',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-100': [{
          'id|+1': 3,
          'name': '@cword(2,5)',
          'shortName': '@cword(2)',
          'tagColor': '@color'
        }]
      },
      'message': '成功'
    }
  }
}

const getWorkshopSimple = {
  url: '/api/mes/building/workshop',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-100': [{
          'factoryId|1-10': 1,
          'id|+1': 1,
          'name': '@cword(2,5)',
          'shortName': '@cword(2)'
        }]
      },
      'message': '成功'
    }
  }
}

const getProcessSimple = {
  url: '/api/mes/building/process',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-10': [{
          'id|+1': 1,
          'name': '@cword(2,5)',
          'productType': 16,
          'sequenceType': 2,
          'type|1-2': false
        },
        {
          'id|+1': 6,
          'name': '@cword(2,5)',
          'productType': 1,
          'sequenceType': 1,
          'type|1-2': false
        },
        {
          'id|+1': 11,
          'name': '@cword(2,5)',
          'productType': 2,
          'sequenceType': 2,
          'type|1-2': true
        },
        {
          'id|+1': 16,
          'name': '@cword(2,5)',
          'productType': 4,
          'sequenceType': 4,
          'type|1-2': true
        }
        ],
        'totalElements': 17
      },
      'message': '成功'
    }
  }
}

const getAllFactoryWorkshopLines = {
  url: '/api/mes/building/factory/production_line_group',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 3,
        'content|1-10': [{
          'id|+1': 1,
          'name': '@cword(2,5)',
          'shortName': '@cword(2)',
          'tagColor': 'rgba(250, 212, 0, 1)',
          'workshopList|1-10': [{
            'id|+1': 1,
            'name': '@cword(2,5)',
            'shortName': '@cword(2)',
            'factoryId|1-10': 1,
            'productionLineList|1-5': [{
              'id|+1': 1,
              'name': '@cword(2,5)',
              'shortName': '@cword(2)',
              'factoryId|1-10': 1,
              'workshopId|1-10': 1
            }]
          }]
        }]
      }
    }
  }
}

const getAllPackage = {
  url: '/api/mes/building/package/list',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 4,
        'content|1-100': [{
          'id|+1': 1,
          'project': null,
          'packageNumber': '@guid',
          'productType': 2
        }]
      }
    }
  }
}

export default [
  getFactorySimple,
  getWorkshopSimple,
  getProcessSimple,
  getAllFactoryWorkshopLines,
  getAllPackage
]
