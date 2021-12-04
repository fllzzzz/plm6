import { pickUpModeEnum, purchaseOrderPaymentModeEnum, orderSupplyTypeEnum, baseMaterialTypeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { patternLicensePlate } from '@/utils/validate/pattern'
import { matClsEnum } from '@/utils/enum/modules/classification'

// 入库申请列表
const get = {
  url: '/api/wms/inbound/application/record/raw-materials',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1, // 入库单id
            basicClass: 7, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州天天向上有限公司'
            },
            reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
            founderName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 入库单id
            basicClass: 8, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州艾哈有限公司'
            },
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 入库单id
            basicClass: 16, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            licensePlate: patternLicensePlate, // 车牌号
            'projects|2': [
              {
                'id|+1': 1,
                'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              }
            ], // 项目id
            supplier: {
              // 供应商
              id: 1,
              name: '杭州艾哈有限公司'
            },
            reviewStatus: reviewStatusEnum.REFUSE.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 4, // 入库单id
            basicClass: 8, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseSN: 'CG-211125-123213', // 采购单号
            licensePlate: patternLicensePlate, // 车牌号
            supplier: {
              // 供应商
              id: 1,
              name: '吖丫丫有限公司'
            },
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
            approvalComments: '@csentence',
            founderName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 钢材详情
const detail_id1 = {
  url: '/api/wms/inbound/application/record/raw-materials/1',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        purchaseOrder: {
          id: 1, // 订单id
          purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
          supplyType: orderSupplyTypeEnum.PARTY_A.V, // 供应类型
          basicClass: 7, // 采购物料基础类型
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
          'projects|2': [
            {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            }
          ], // 项目id
          pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
          purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
          weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
          supplier: {
            id: 1,
            name: '杭州天马钢材有限公司'
          }
        },
        'requisitionsList|2': [{
          'basicClass|1-16': 1,
          'serialNumber|+1': ['SG-AFTER-123456', 'SG-AFTER-133456'],
          'projectId|+1': 1
        }],
        id: 1, // 入库单id
        basicClass: 7, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
        licensePlate: patternLicensePlate, // 车牌号
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: 1,
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            mete: 800000,
            unitPrice: 0.01,
            amount: 8000,
            priceExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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
            basicClass: 1,
            classifyId: 103,
            specification: 'Q235B',
            quantity: 5,
            thickness: 20,
            length: 1500,
            width: 2000,
            brand: '哈哈',
            heatNoAndBatchNo: 'fddfd',
            mete: 2355000,
            weight: 2355000,
            unitPrice: 0.02,
            amount: 47100,
            priceExcludingVAT: 41681.42,
            inputVAT: 5418.58,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          // {
          //   id: 3,
          //   sn: '110_0',
          //   specification: '57*21*3*9',
          //   classifyId: 110,
          //   basicClass: 2,
          //   quantity: 1,
          //   length: 10000,
          //   totalLength: 10,
          //   brand: '马钢',
          //   heatNoAndBatchNo: 'ooopp',
          //   mete: 152900,
          //   weight: 152900,
          //   unitPrice: 0.03,
          //   amount: 4587,
          //   priceExcludingVAT: 4059.29,
          //   requisitionsSN: 'SG-AFTER-123456',
          //   inputVAT: 527.71,
          //   project: {
          //     'id|+1': 1,
          //     'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
          //     'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
          //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          //   },
          //   factoryId: 1,
          //   warehouse: {
          //     id: 1,
          //     name: '666号仓库'
          //   }
          // },
          // {
          //   uid: 4,
          //   // sn: '110_1',
          //   classifyId: 110,
          //   quantity: 2,
          //   length: 2500,
          //   totalLength: 5,
          //   brand: '鞍钢',
          //   heatNoAndBatchNo: 'qqwww',
          //   requisitionsSN: 'SG-AFTER-123456',
          //   mete: 150000,
          //   weight: 150000,
          //   unitPrice: 0.04,
          //   amount: 6000,
          //   priceExcludingVAT: 5000,
          //   inputVAT: 1000,
          //   project: {
          //     'id|+1': 1,
          //     'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
          //     'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
          //     serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
          //   },
          //   factoryId: 1,
          //   warehouse: {
          //     id: 1,
          //     name: '666号仓库'
          //   }
          // },
          {
            id: 5,
            classifyId: 120,
            basicClass: 4,
            specification: 'DC51D+Z',
            quantity: 1,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: '12341234fsafs1234',
            requisitionsSN: 'SG-AFTER-133456',
            thickness: 0.326,
            length: 3907.62,
            width: 1000,
            mete: 10000,
            weight: 10000,
            unitPrice: 0.05,
            amount: 500,
            priceExcludingVAT: 450,
            inputVAT: 50,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
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

// 气体详情
const detail_id2 = {
  url: '/api/wms/inbound/application/record/raw-materials/2',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        purchaseOrder: {
          id: 3, // 订单id
          purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
          supplyType: orderSupplyTypeEnum.PARTY_A.V, // 供应类型
          basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
          'projects|2': [
            {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            }
          ], // 项目id
          pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
          purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
          weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
          supplier: {
            id: 1,
            name: '杭州天马钢材有限公司'
          }
        },
        id: 2, // 入库单id
        basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
        licensePlate: patternLicensePlate, // 车牌号
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            classifyId: 204,
            specification: 'M27*60',
            color: '天蓝',
            basicClass: 1,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 800000,
            unitPrice: 0.01,
            amount: 8000,
            priceExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ]
      }
    }
  }
}

// 辅材详情
const detail_id3 = {
  url: '/api/wms/inbound/application/record/raw-materials/3',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        purchaseOrder: {
          id: 4, // 订单id
          purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 采购类型
          supplyType: orderSupplyTypeEnum.PARTY_A.V, // 供应类型
          basicClass: matClsEnum.GAS.V, // 采购物料基础类型
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 订单编号
          'projects|2': [
            {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            }
          ], // 项目id
          pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
          purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
          weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
          supplier: {
            id: 1,
            name: '杭州天马钢材有限公司'
          }
        },
        id: 3, // 入库单id
        basicClass: matClsEnum.GAS.V, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
        licensePlate: patternLicensePlate, // 车牌号
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            classifyId: 901,
            // specification: '',
            basicClass: 1,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 222,
            unitPrice: 0.01,
            amount: 8000,
            priceExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factoryId: 1,
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          }
        ]
      }
    }
  }
}

// 修改采购订单
const edit = {
  url: '/api/wms/inbound/application/record/raw-materials',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除采购订单
const del = {
  url: '/api/wms/inbound/application/record/raw-materials',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [get, edit, del, detail_id1, detail_id2, detail_id3]
