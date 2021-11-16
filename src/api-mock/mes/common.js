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
          'createTime': '2021-10-08T10:07:22',
          'id': 3,
          'name': '一号工厂',
          'remark': 'remark',
          'shortName': '一工',
          'sort': 1,
          'tagColor': '#409eff',
          'updateTime': '2021-10-08T10:10:36',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'createTime': '2021-10-08T10:07:22',
          'id': 4,
          'name': '二号工厂',
          'remark': 'remark',
          'shortName': '二工',
          'sort': 1,
          'tagColor': 'rgba(250, 212, 0, 1)',
          'updateTime': '2021-10-08T10:10:36',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'createTime': '2021-10-08T10:07:22',
          'id': 5,
          'name': '三号工厂',
          'remark': 'remark',
          'shortName': '三工',
          'sort': 1,
          'tagColor': 'rgba(250, 212, 0, 1)',
          'updateTime': '2021-10-08T10:10:36',
          'userId': 1,
          'version': 2
        },
        {
          'boolDeleteEnum': false,
          'boolEnabledEnum': true,
          'createTime': '2021-10-08T10:07:22',
          'id': 5,
          'name': '四号工厂',
          'remark': 'remark',
          'shortName': '四工',
          'sort': 1,
          'tagColor': 'rgba(250, 212, 0, 1)',
          'updateTime': '2021-10-08T10:10:36',
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
            }]
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

const getRegionalTree = {
  url: '/api/regionalCascade',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 3,
        'content': [{
          'children': [{
            'children': [
              {
                'children': [],
                'id': 1,
                'name': '东城区'
              },
              {
                'children': [],
                'id': 2,
                'name': '西城区'
              },
              {
                'children': [],
                'id': 3,
                'name': '朝阳区'
              },
              {
                'children': [],
                'id': 4,
                'name': '丰台区'
              },
              {
                'children': [],
                'id': 5,
                'name': '石景山区'
              },
              {
                'children': [],
                'id': 6,
                'name': '海淀区'
              },
              {
                'children': [],
                'id': 7,
                'name': '门头沟区'
              },
              {
                'children': [],
                'id': 8,
                'name': '房山区'
              },
              {
                'children': [],
                'id': 9,
                'name': '通州区'
              },
              {
                'children': [],
                'id': 10,
                'name': '顺义区'
              },
              {
                'children': [],
                'id': 11,
                'name': '昌平区'
              },
              {
                'children': [],
                'id': 12,
                'name': '大兴区'
              },
              {
                'children': [],
                'id': 13,
                'name': '怀柔区'
              },
              {
                'children': [],
                'id': 14,
                'name': '平谷区'
              },
              {
                'children': [],
                'id': 15,
                'name': '密云县'
              },
              {
                'children': [],
                'id': 16,
                'name': '延庆县'
              }
            ],
            'id': 1,
            'name': '北京市'
          },
          {
            'children': [
              {
                'children': [
                  {
                    'children': null,
                    'id': 1,
                    'name': '长安区'
                  },
                  {
                    'children': null,
                    'id': 2,
                    'name': '桥东区'
                  },
                  {
                    'children': null,
                    'id': 3,
                    'name': '桥西区'
                  },
                  {
                    'children': null,
                    'id': 4,
                    'name': '新华区'
                  },
                  {
                    'children': null,
                    'id': 5,
                    'name': '井陉矿区'
                  },
                  {
                    'children': null,
                    'id': 6,
                    'name': '裕华区'
                  },
                  {
                    'children': null,
                    'id': 7,
                    'name': '井陉县'
                  },
                  {
                    'children': null,
                    'id': 8,
                    'name': '正定县'
                  },
                  {
                    'children': null,
                    'id': 9,
                    'name': '栾城县'
                  },
                  {
                    'children': null,
                    'id': 10,
                    'name': '行唐县'
                  },
                  {
                    'children': null,
                    'id': 11,
                    'name': '灵寿县'
                  },
                  {
                    'children': null,
                    'id': 12,
                    'name': '高邑县'
                  },
                  {
                    'children': null,
                    'id': 13,
                    'name': '深泽县'
                  },
                  {
                    'children': null,
                    'id': 14,
                    'name': '赞皇县'
                  },
                  {
                    'children': null,
                    'id': 15,
                    'name': '无极县'
                  },
                  {
                    'children': null,
                    'id': 16,
                    'name': '平山县'
                  },
                  {
                    'children': null,
                    'id': 17,
                    'name': '元氏县'
                  },
                  {
                    'children': null,
                    'id': 18,
                    'name': '赵县'
                  },
                  {
                    'children': null,
                    'id': 19,
                    'name': '辛集市'
                  },
                  {
                    'children': null,
                    'id': 20,
                    'name': '藁城市'
                  },
                  {
                    'children': null,
                    'id': 21,
                    'name': '晋州市'
                  },
                  {
                    'children': null,
                    'id': 22,
                    'name': '新乐市'
                  },
                  {
                    'children': null,
                    'id': 23,
                    'name': '鹿泉市'
                  }
                ],
                'id': 33,
                'name': '石家庄市'
              },
              {
                'children': [
                  {
                    'children': null,
                    'id': 24,
                    'name': '路南区'
                  },
                  {
                    'children': null,
                    'id': 25,
                    'name': '路北区'
                  },
                  {
                    'children': null,
                    'id': 26,
                    'name': '古冶区'
                  },
                  {
                    'children': null,
                    'id': 27,
                    'name': '开平区'
                  },
                  {
                    'children': null,
                    'id': 28,
                    'name': '丰南区'
                  },
                  {
                    'children': null,
                    'id': 29,
                    'name': '丰润区'
                  },
                  {
                    'children': null,
                    'id': 30,
                    'name': '滦县'
                  },
                  {
                    'children': null,
                    'id': 31,
                    'name': '滦南县'
                  },
                  {
                    'children': null,
                    'id': 32,
                    'name': '乐亭县'
                  },
                  {
                    'children': null,
                    'id': 33,
                    'name': '迁西县'
                  },
                  {
                    'children': null,
                    'id': 34,
                    'name': '玉田县'
                  },
                  {
                    'children': null,
                    'id': 35,
                    'name': '曹妃甸区'
                  },
                  {
                    'children': null,
                    'id': 36,
                    'name': '遵化市'
                  },
                  {
                    'children': null,
                    'id': 37,
                    'name': '迁安市'
                  }
                ],
                'id': 34,
                'name': '唐山市'
              }
            ],
            'id': 3,
            'name': '河北省'
          }],
          'id': 1,
          'name': '中国'
        }]
      }
    }
  }
}

export default [
  getRegionalTree,
  getFactorySimple,
  getWorkshopSimple,
  getProcessSimple,
  getAllFactoryWorkshopLines
]
