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
          'serialNumber': '@guid',
          'productType': 2
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
  getAllFactoryWorkshopLines,
  getAllPackage
]
