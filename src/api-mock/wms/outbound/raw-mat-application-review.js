import { matClsEnum } from '@/utils/enum/modules/classification'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { materialOutboundModeEnum, measureTypeEnum } from '@/utils/enum/modules/wms'

// 原材料出库清单列表
const get = {
  url: '/api/wms/outbound/application/review/raw-materials',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|20': [
          {
            'id|+1': 1,
            'basicClass|1-31': 1,
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            applicationSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库申请编号
            applicantName: '@cname',
            userUpdateTime: '@datetime(T)',
            createTime: '@datetime(T)'
          }
        ],
        totalElements: 20
      }
    }
  }
}

// 待审核出库单id列表
const getPendingReviewIdList = {
  url: '/api/wms/outbound/application/review/raw-materials/pending/ids',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [1, 2] // 待审核出库单id列表
    }
  }
}

// 获取当前用户的出库单的详情数量
const getDetailNumberByCurrentUser = {
  url: '/api/wms/outbound/application/review/raw-materials/current-user/detail-number',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: 5
    }
  }
}

// 删除出库单单条物料
const delMaterial = {
  url: '/api/wms/outbound/application/review/raw-materials/material-del',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 审核通过
const reviewPassed = {
  url: RegExp('/api/wms/outbound/application/review/raw-materials/' + '[1-9][0-9]*' + '/passed'),
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 审核退回
const reviewReturned = {
  url: RegExp('/api/wms/outbound/application/review/raw-materials/' + '[1-9][0-9]*' + '/returned'),
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 当前用户待出库清单
const getDetailByCurrentUser = {
  url: '/api/wms/outbound/application/review/raw-materials/current-user',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 出库单id
        userUpdateTime: '@datetime(T)',
        createTime: '@datetime(T)',
        applicationSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库申请编号
        applicant: {
          name: '@cname',
          deptName: '生产部'
        },
        basicClass: 31, // 采购物料基础类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V,
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
        list: [
          {
            id: 1,
            boolPartyA: true, // 甲供材料
            classifyId: 103,
            materialOutboundMode: materialOutboundModeEnum.HALF.V, // 物料出库方式
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            boolTransfer: true,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            sourceProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 3,
            boolPartyA: true, // 甲供材料
            specification: 'Q325B * 57*21*3*9',
            classifyId: 110,
            materialOutboundMode: materialOutboundModeEnum.HALF.V, // 物料出库方式
            basicClass: matClsEnum.SECTION_STEEL.V,
            outboundUnitType: measureTypeEnum.MEASURE.V,
            boolTransfer: true,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            sourceProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          },
          {
            id: 7,
            classifyId: 204,
            specification: 'M27 * 60',
            color: '天蓝',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 9,
            classifyId: 901,
            // specification: '',
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 222,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '660号仓库'
            }
          }
        ]
      }
    }
  }
}

// 出库单详情
const detail_id1 = {
  url: '/api/wms/outbound/application/review/raw-materials/1',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 1, // 出库单id
        userUpdateTime: '@datetime(T)',
        createTime: '@datetime(T)',
        applicationSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库申请编号
        applicant: {
          name: '@cname',
          deptName: '生产部'
        },
        basicClass: 7, // 采购物料基础类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V,
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
        list: [
          {
            id: 1,
            classifyId: 103,
            basicClass: 1,
            specification: 'Q325B',
            materialOutboundMode: materialOutboundModeEnum.HALF.V, // 物料出库方式
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 2,
            basicClass: matClsEnum.STEEL_PLATE.V,
            classifyId: 103,
            specification: 'Q235B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 5,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 2355000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 3,
            specification: 'Q325B * 57*21*3*9',
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            boolTransfer: true,
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            sourceProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          }
        ]
      }
    }
  }
}

// 出库单详情
const detail_id2 = {
  url: '/api/wms/outbound/application/review/raw-materials/2',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        id: 2, // 出库单id
        userUpdateTime: '@datetime(T)',
        createTime: '@datetime(T)',
        applicationSN: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库申请编号
        applicant: {
          name: '@cname',
          deptName: '生产部'
        },
        basicClass: 31, // 采购物料基础类型
        reviewStatus: reviewStatusEnum.UNREVIEWED.V,
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 出库单号
        list: [
          {
            id: 1,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            boolTransfer: true,
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            sourceProject: {
              id: 2,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 3,
            specification: 'Q325B * 57*21*3*9',
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            outboundUnitType: measureTypeEnum.MEASURE.V,
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 4,
              name: '668号仓库'
            }
          },
          {
            id: 7,
            classifyId: 204,
            specification: 'M27 * 60',
            color: '天蓝',
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 800000,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 9,
            classifyId: 901,
            // specification: '',
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 222,
            recipient: {
              name: '@cname',
              deptName: '生产部'
            },
            project: {
              id: 1,
              name: '你脸红个泡泡茶壶666号主路',
              shortName: '泡泡茶壶',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '660号仓库'
            }
          }
        ]
      }
    }
  }
}

export default [get, detail_id1, detail_id2, getDetailByCurrentUser, delMaterial, getPendingReviewIdList, reviewPassed, reviewReturned, getDetailNumberByCurrentUser]
