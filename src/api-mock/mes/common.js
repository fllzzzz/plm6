const getFactorySimple = {
  url: '/api/mes/building/factory',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [{
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'id': 1,
          'name': '一号工厂',
          'shortName': '一工',
          'tagColor': '#409eff',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'id': 2,
          'name': '二号工厂',
          'remark': 'remark',
          'shortName': '二工',
          'tagColor': 'rgba(250, 212, 0, 1)',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'id': 3,
          'name': '三号工厂',
          'shortName': '三工',
          'tagColor': 'rgba(250, 212, 0, 1)',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'id': 4,
          'name': '四号工厂',
          'shortName': '四工',
          'tagColor': 'rgba(250, 212, 0, 1)',
          'userId': 1,
          'version': 2
        }
        ],
        'totalElements': 2
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
        'content': [{
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'createTime': '2021-10-08T10:13:55',
          'factoryId': 3,
          'id': 4,
          'name': '一号车间',
          'remark': '',
          'shortName': '一车',
          'sort': 1,
          'updateTime': '2021-10-08T10:13:55',
          'userId': 1,
          'version': 2
        }],
        'totalElements': 2
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
        'content': [{
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:19:55',
          'id': 1,
          'inspectType': 2,
          'name': '组立1',
          'productType': 16,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:19:55',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:20:04',
          'id': 2,
          'inspectType': 2,
          'name': '组立2',
          'productType': 16,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:20:04',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:20:12',
          'id': 3,
          'inspectType': 2,
          'name': '组立3',
          'productType': 16,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:20:12',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:20:28',
          'id': 4,
          'inspectType': 2,
          'name': '组立4',
          'productType': 16,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:20:28',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:20:37',
          'id': 5,
          'inspectType': 2,
          'name': '组立5',
          'productType': 16,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:20:37',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:20:58',
          'id': 6,
          'inspectType': 2,
          'name': '零件1',
          'productType': 1,
          'reportType': 2,
          'sequenceType': 1,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:20:58',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:21:05',
          'id': 7,
          'inspectType': 2,
          'name': '零件2',
          'productType': 1,
          'reportType': 2,
          'sequenceType': 1,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:21:05',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:21:11',
          'id': 8,
          'inspectType': 2,
          'name': '零件3',
          'productType': 1,
          'reportType': 2,
          'sequenceType': 1,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:21:11',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:21:18',
          'id': 9,
          'inspectType': 2,
          'name': '零件4',
          'productType': 1,
          'reportType': 2,
          'sequenceType': 1,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:21:18',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:21:38',
          'id': 10,
          'inspectType': 2,
          'name': '零件5',
          'productType': 1,
          'reportType': 2,
          'sequenceType': 1,
          'sort': 1,
          'type': false,
          'updateTime': '2021-11-01T10:21:38',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:22:01',
          'id': 11,
          'inspectType': 2,
          'name': '构件1',
          'productType': 2,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-01T10:22:01',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:22:08',
          'id': 12,
          'inspectType': 2,
          'name': '构件2',
          'productType': 2,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-01T10:22:08',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:22:15',
          'id': 13,
          'inspectType': 2,
          'name': '构件3',
          'productType': 2,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-01T10:22:15',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:22:20',
          'id': 14,
          'inspectType': 2,
          'name': '构件4',
          'productType': 2,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-01T10:22:20',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-01T10:22:27',
          'id': 15,
          'inspectType': 2,
          'name': '构件5',
          'productType': 2,
          'reportType': 2,
          'sequenceType': 2,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-01T10:22:27',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-02T13:29:36',
          'id': 16,
          'inspectType': 2,
          'name': '围护1',
          'productType': 4,
          'reportType': 2,
          'sequenceType': 4,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-02T14:49:47',
          'userId': 1,
          'version': 1
        },
        {
          'boolDeleteEnum': false,
          'createTime': '2021-11-02T16:30:43',
          'id': 17,
          'inspectType': 2,
          'name': '围护2',
          'productType': 4,
          'reportType': 2,
          'sequenceType': 4,
          'sort': 1,
          'type': true,
          'updateTime': '2021-11-02T16:30:43',
          'userId': 1,
          'version': 1
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
        'content': [{
          'createTime': 1633658842000,
          'id': 3,
          'name': '一号工厂',
          'shortName': '一工',
          'tagColor': 'rgba(250, 212, 0, 1)',
          'boolEnabledEnum': true,
          'sort': 1,
          'remark': '',
          'userId': 1,
          'workshopList': [{
            'createTime': 1636339682000,
            'id': 1,
            'name': '一号车间',
            'shortName': '一号车间',
            'sort': null,
            'userId': 1,
            'boolEnabledEnum': true,
            'factoryId': 3,
            'remark': '',
            'productionLineList': [{
              'createTime': 1636339870000,
              'id': 1,
              'name': '一号生产线',
              'shortName': '一号线',
              'sort': 1,
              'boolEnabledEnum': true,
              'remark': '',
              'factoryId': 3,
              'workshopId': 1,
              'userId': 1
            },
            {
              'createTime': 1636339870000,
              'id': 2,
              'name': '二号生产线',
              'shortName': '二号线',
              'sort': 1,
              'boolEnabledEnum': true,
              'remark': '',
              'factoryId': 3,
              'workshopId': 1,
              'userId': 1
            }
            ]
          }]
        }, {
          'createTime': 1635907717000,
          'id': 4,
          'name': '二号工厂',
          'shortName': '二厂',
          'tagColor': 'rgba(0, 206, 209, 1)',
          'boolEnabledEnum': false,
          'sort': 1,
          'remark': 're',
          'userId': 1,
          'workshopList': []
        }, {
          'createTime': 1636340584000,
          'id': 5,
          'name': '信息',
          'shortName': '下',
          'tagColor': '#1682e6',
          'boolEnabledEnum': false,
          'sort': 1,
          'remark': '',
          'userId': 1,
          'workshopList': []
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
        'content': [{
          'id': 9,
          'project': null,
          'packageNumber': '2021-11-09-04',
          'productType': 2,
          'status': false,
          'remark': '',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636437879000
        }, {
          'id': 11,
          'project': null,
          'packageNumber': '2021-11-09-05',
          'productType': 2,
          'status': false,
          'remark': '2222',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636440562000
        }, {
          'id': 12,
          'project': null,
          'packageNumber': '2021-11-09-06',
          'productType': 2,
          'status': false,
          'remark': '1213',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636446532000
        }, {
          'id': 17,
          'project': null,
          'packageNumber': '2021-11-10-04',
          'productType': 4,
          'status': false,
          'remark': '123213123213',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636512111000
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
