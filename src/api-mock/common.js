import { validatorPhone } from '@/utils/validate/pattern'
import { measureTypeEnum } from '@enum-ms/wms'
import { supplierClassEnum, supplierIsHideEnum, supplierTypeEnum } from '@enum-ms/supplier'
import { matClsEnum } from '@enum-ms/classification'
// 根据id, 获取末级物料分类
const getFinalMatClsById = {
  url: RegExp('/api/config/classification/final-material/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        // {
        //   'id|1-100': 1,
        //   name: '角铁',
        //   fullName: '角铁',
        //   measureUnit: '根', // 计量单位
        //   accountingUnit: '千克', // 核算单位
        //   accountingPrecision: 2, // 核算单位小数精度
        //   measurePrecision: 0, // 计量单位小数精度
        //   outboundUnit: measureTypeEnum.MEASURE.V, // 出库方式
        //   basicClass: matClsEnum.SECTION_STEEL.V,
        //   specConfig: [
        //     {
        //       id: 1,
        //       name: 'GB-06',
        //       'list|20': [
        //         { code: '01', name: /(\d{1,3}\*){3}\d{1,3}/ }
        //       ]
        //     }
        //   ]
        // }
        {
          'id|1-100': 1,
          name: '大六角',
          fullName: '紧固件>高强螺栓>大六角',
          serialNumber: /[0-9]{10}/,
          measureUnit: '套', // 计量单位
          accountingUnit: '套', // 核算单位
          accountingPrecision: 0, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnit: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: matClsEnum.MATERIAL.V,
          specConfig: [
            {
              id: 1,
              name: '直径',
              list: [
                { code: '01', name: 'M24' },
                { code: '02', name: 'M26' },
                { code: '03', name: 'M27' },
                { code: '04', name: 'M28' },
                { code: '05', name: 'M29' },
                { code: '06', name: 'M30' },
                { code: '07', name: 'M31' },
                { code: '08', name: 'M32' },
                { code: '09', name: 'M33' },
                { code: '10', name: 'M34' }
              ]
            },
            {
              id: 2,
              name: '长度',
              list: [
                { code: '01', name: '60' },
                { code: '02', name: '65' },
                { code: '03', name: '66' },
                { code: '04', name: '67' },
                { code: '05', name: '68' },
                { code: '06', name: '69' },
                { code: '07', name: '70' },
                { code: '08', name: '71' },
                { code: '09', name: '72' },
                { code: '10', name: '73' },
                { code: '11', name: '74' }
              ]
            }
          ]
        }
    }
  }
}

const downloadAttachment = {
  url: RegExp('/api/common/attachment/download/' + '.*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功'
    }
  }
}

// 获取供应商（简要信息）
const getSuppliersBrief = {
  url: '/api/wms/supplier/all/brief',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        content: [
          {
            id: 1,
            name: '物产贸易',
            type: supplierTypeEnum.RAW_MATERIAL.V,
            basicClass: supplierClassEnum.STEEL_PLATE.V,
            'boolHide': supplierIsHideEnum.FALSE.V

          },
          {
            id: 2,
            name: '景风重工',
            type: supplierTypeEnum.RAW_MATERIAL.V | supplierTypeEnum.MANUFACTURED.V,
            basicClass: supplierClassEnum.STEEL_PLATE.V | supplierClassEnum.STEEL_COIL.V | supplierClassEnum.ENCL_MANUFACTURED.V,
            'boolHide': supplierIsHideEnum.FALSE.V
          },
          {
            id: 3,
            name: '杭州天马制造',
            type: supplierTypeEnum.MANUFACTURED.V,
            basicClass: supplierClassEnum.STRUC_MANUFACTURED.V,
            'boolHide|1-2': supplierIsHideEnum.FALSE.V
          },
          {
            id: 4,
            name: '山东万马物流',
            type: supplierTypeEnum.LOGISTICS.V,
            basicClass: supplierClassEnum.LOGISTICS.V,
            'boolHide|1-2': supplierIsHideEnum.FALSE.V
          }
        ]
      }
    }
  }
}

const getDeptAllSimple = {
  url: '/api/dept/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      data: [
        {
          children: [
            { children: null, id: 2, name: '合同管理部' },
            { children: null, id: 3, name: '技术部' },
            { children: null, id: 4, name: '工程部' },
            { children: null, id: 5, name: '商务管理部' },
            { children: null, id: 11, name: '计划管理部' },
            { children: null, id: 12, name: '采购部' },
            {
              children: [
                { children: null, id: 22, name: '萧山仓库' },
                { children: null, id: 23, name: '江干仓库' }
              ],
              id: 13,
              name: '仓库管理部'
            },
            { children: null, id: 14, name: '财务部' },
            {
              children: [
                { children: null, id: 20, name: '萧山质检' },
                { children: null, id: 21, name: '江干质检' }
              ],
              id: 15,
              name: '质量管理部'
            },
            { children: null, id: 16, name: '行政部' },
            {
              children: [
                { children: null, id: 18, name: '萧山工厂' },
                { children: null, id: 19, name: '江干工厂' },
                { children: null, id: 31, name: '吕冰工厂' }
              ],
              id: 17,
              name: '生产部'
            },
            {
              children: [
                { children: null, id: 25, name: '桥梁生产车间' },
                { children: null, id: 26, name: '桥梁质检' },
                { children: null, id: 27, name: '桥梁发运' },
                { children: null, id: 29, name: '桥梁管理部' },
                { children: null, id: 30, name: '桥梁项目部' }
              ],
              id: 28,
              name: '桥梁事业部'
            },
            { children: null, id: 32, name: '仓管' }
          ],
          id: 1,
          name: '初鸣建科'
        }
      ],
      message: '操作成功'
    }
  }
}

// 获取所有用户
const getUserAllSimple = {
  url: '/api/user/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        totalElements: 100,
        'content|100': [
          {
            'id|1-100': 1,
            name: '@cname',
            deptName: null,
            phone: validatorPhone,
            email: '@email'
          }
        ]
      }
    }
  }
}

// 获取所有用户,带部门
const getUserTree = {
  url: '/api/user/tree',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: [
        {
          'children': [
            {
              'id': 18,
              'isUser': true,
              'label': '王欣'
            }
          ],
          'id': 2,
          'isUser': false,
          'label': '合同管理部'
        },
        {
          'children': [
            {
              'id': 26,
              'isUser': true,
              'label': '李亮'
            },
            {
              'id': 64,
              'isUser': true,
              'label': '李巍'
            }
          ],
          'id': 3,
          'isUser': false,
          'label': '技术部'
        },
        {
          'children': [
            {
              'id': 23,
              'isUser': true,
              'label': '洪太枫'
            },
            {
              'id': 36,
              'isUser': true,
              'label': '刘文成'
            },
            {
              'id': 52,
              'isUser': true,
              'label': '齐训利'
            }
          ],
          'id': 4,
          'isUser': false,
          'label': '工程部'
        },
        {
          'children': [
            {
              'id': 20,
              'isUser': true,
              'label': '张飞'
            },
            {
              'id': 73,
              'isUser': true,
              'label': '吕冰商务'
            }
          ],
          'id': 5,
          'isUser': false,
          'label': '商务管理部'
        },
        {
          'children': [
            {
              'id': 25,
              'isUser': true,
              'label': '李勇'
            }
          ],
          'id': 11,
          'isUser': false,
          'label': '计划管理部'
        },
        {
          'children': [
            {
              'id': 33,
              'isUser': true,
              'label': '老董 '
            }
          ],
          'id': 12,
          'isUser': false,
          'label': '采购部'
        },
        {
          'children': [
            {
              'children': [
                {
                  'id': 34,
                  'isUser': true,
                  'label': '王凯'
                }
              ],
              'id': 22,
              'isUser': false,
              'label': '萧山仓库'
            },
            {
              'children': [
                {
                  'id': 35,
                  'isUser': true,
                  'label': '刘再道'
                }
              ],
              'id': 23,
              'isUser': false,
              'label': '江干仓库'
            },
            {
              'id': 37,
              'isUser': true,
              'label': '谢勇'
            },
            {
              'id': 74,
              'isUser': true,
              'label': '吕冰仓库管理'
            }
          ],
          'id': 13,
          'isUser': false,
          'label': '仓库管理部'
        },
        {
          'id': 14,
          'isUser': false,
          'label': '财务部'
        },
        {
          'children': [
            {
              'children': [
                {
                  'id': 14,
                  'isUser': true,
                  'label': '小侯'
                },
                {
                  'id': 16,
                  'isUser': true,
                  'label': '小米'
                },
                {
                  'id': 17,
                  'isUser': true,
                  'label': '小赵'
                }
              ],
              'id': 20,
              'isUser': false,
              'label': '萧山质检'
            },
            {
              'children': [
                {
                  'id': 15,
                  'isUser': true,
                  'label': '小虎'
                }
              ],
              'id': 21,
              'isUser': false,
              'label': '江干质检'
            }
          ],
          'id': 15,
          'isUser': false,
          'label': '质量管理部'
        },
        {
          'children': [
            {
              'id': 41,
              'isUser': true,
              'label': '张斌'
            }
          ],
          'id': 16,
          'isUser': false,
          'label': '行政部'
        },
        {
          'children': [
            {
              'children': [
                {
                  'id': 3,
                  'isUser': true,
                  'label': '李冰冰'
                },
                {
                  'id': 4,
                  'isUser': true,
                  'label': '王中磊'
                },
                {
                  'id': 5,
                  'isUser': true,
                  'label': '冯小刚'
                },
                {
                  'id': 11,
                  'isUser': true,
                  'label': '小黄'
                },
                {
                  'id': 12,
                  'isUser': true,
                  'label': '小周'
                },
                {
                  'id': 13,
                  'isUser': true,
                  'label': '小陈'
                },
                {
                  'id': 24,
                  'isUser': true,
                  'label': '周斌'
                },
                {
                  'id': 29,
                  'isUser': true,
                  'label': '胡军'
                },
                {
                  'id': 30,
                  'isUser': true,
                  'label': '张学友'
                },
                {
                  'id': 31,
                  'isUser': true,
                  'label': '向华强'
                },
                {
                  'id': 32,
                  'isUser': true,
                  'label': '古天乐'
                },
                {
                  'id': 39,
                  'isUser': true,
                  'label': '001'
                },
                {
                  'id': 50,
                  'isUser': true,
                  'label': '史正阳'
                }
              ],
              'id': 18,
              'isUser': false,
              'label': '萧山工厂'
            },
            {
              'children': [
                {
                  'id': 6,
                  'isUser': true,
                  'label': '川建国'
                },
                {
                  'id': 7,
                  'isUser': true,
                  'label': '小强'
                },
                {
                  'id': 8,
                  'isUser': true,
                  'label': '小张'
                },
                {
                  'id': 9,
                  'isUser': true,
                  'label': '小李'
                },
                {
                  'id': 10,
                  'isUser': true,
                  'label': '小刘'
                },
                {
                  'id': 19,
                  'isUser': true,
                  'label': '小王'
                },
                {
                  'id': 21,
                  'isUser': true,
                  'label': '张波'
                },
                {
                  'id': 22,
                  'isUser': true,
                  'label': '王元阳'
                },
                {
                  'id': 27,
                  'isUser': true,
                  'label': '刘德华'
                },
                {
                  'id': 28,
                  'isUser': true,
                  'label': '范冰冰'
                }
              ],
              'id': 19,
              'isUser': false,
              'label': '江干工厂'
            },
            {
              'children': [
                {
                  'id': 69,
                  'isUser': true,
                  'label': '吕冰工人一'
                },
                {
                  'id': 70,
                  'isUser': true,
                  'label': '吕冰工人二'
                },
                {
                  'id': 71,
                  'isUser': true,
                  'label': '吕冰质检一'
                },
                {
                  'id': 72,
                  'isUser': true,
                  'label': '吕冰车间管理'
                }
              ],
              'id': 31,
              'isUser': false,
              'label': '吕冰工厂'
            },
            {
              'id': 38,
              'isUser': true,
              'label': '陈朝阳'
            },
            {
              'id': 66,
              'isUser': true,
              'label': '测试1'
            }
          ],
          'id': 17,
          'isUser': false,
          'label': '生产部'
        },
        {
          'children': [
            {
              'children': [
                {
                  'id': 45,
                  'isUser': true,
                  'label': '李二'
                },
                {
                  'id': 48,
                  'isUser': true,
                  'label': '李四'
                },
                {
                  'id': 49,
                  'isUser': true,
                  'label': '彭继明'
                },
                {
                  'id': 65,
                  'isUser': true,
                  'label': '李工'
                }
              ],
              'id': 25,
              'isUser': false,
              'label': '桥梁生产车间'
            },
            {
              'children': [
                {
                  'id': 44,
                  'isUser': true,
                  'label': '王凯'
                }
              ],
              'id': 26,
              'isUser': false,
              'label': '桥梁质检'
            },
            {
              'children': [
                {
                  'id': 43,
                  'isUser': true,
                  'label': '彭凯'
                }
              ],
              'id': 27,
              'isUser': false,
              'label': '桥梁发运'
            },
            {
              'children': [
                {
                  'id': 47,
                  'isUser': true,
                  'label': '刘鸿云'
                }
              ],
              'id': 29,
              'isUser': false,
              'label': '桥梁管理部'
            },
            {
              'children': [
                {
                  'id': 46,
                  'isUser': true,
                  'label': '王勤'
                }
              ],
              'id': 30,
              'isUser': false,
              'label': '桥梁项目部'
            },
            {
              'id': 68,
              'isUser': true,
              'label': '张三'
            }
          ],
          'id': 28,
          'isUser': false,
          'label': '桥梁事业部'
        },
        {
          'id': 32,
          'isUser': false,
          'label': '仓管'
        },
        {
          'id': 1,
          'isUser': true,
          'label': '超级管理员'
        },
        {
          'id': 2,
          'isUser': true,
          'label': '张磊'
        },
        {
          'id': 42,
          'isUser': true,
          'label': '张晓'
        },
        {
          'id': 51,
          'isUser': true,
          'label': '张飞波'
        },
        {
          'id': 58,
          'isUser': true,
          'label': '杨娟'
        },
        {
          'id': 59,
          'isUser': true,
          'label': '王子豪'
        },
        {
          'id': 61,
          'isUser': true,
          'label': '周建桥'
        },
        {
          'id': 62,
          'isUser': true,
          'label': '董志鑫'
        },
        {
          'id': 76,
          'isUser': true,
          'label': '杜昊昊'
        }
      ]
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
      'data': [{
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

const uploadAttachment = {
  url: '/api/common/attachment',
  method: 'post',
  headers: { 'Content-Type': 'multipart/form-data' },
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      data: [{
        'createTime': 1637226476143,
        'fileName': '1637226476143_文件.png',
        'id': 1,
        'mediaType': 'image/png',
        'name': '文件.png',
        'type': 7
      }],
      message: '操作成功'
    }
  }
}

export default [
  getUserTree,
  getRegionalTree,
  uploadAttachment,
  getUserAllSimple,
  getDeptAllSimple,
  getSuppliersBrief,
  getFinalMatClsById,
  downloadAttachment
]
