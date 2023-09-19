import {
  purchaseOrderPaymentModeEnum,
  orderSupplyTypeEnum,
  baseMaterialTypeEnum,
  receiptRejectStatusEnum,
  materialRejectStatusEnum
} from '@enum-ms/wms'
import { invoiceTypeEnum, weightMeasurementModeEnum } from '@enum-ms/finance'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { patternLicensePlate } from '@/utils/validate/pattern'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { STEEL_ENUM } from '@/settings/config'

// 入库单列表
const getReceiptList = {
  url: '/api/wms/report/raw-materials/inbound/receipt',
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
            basicClass: STEEL_ENUM, // 采购物料基础类型
            rejectStatus: receiptRejectStatusEnum.PENDING_REVIEW.V, // 退货状态
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            // 采购合同编号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            rejectAmountExcludingVAT: 10000, // 退货金额
            inboundAmountExcludingVAT: 1000000, // 入库金额
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
            editable: true, // 可修改的
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            inboundTime: '@datetime(T)', // 入库时间
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 入库单id
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            rejectStatus: receiptRejectStatusEnum.ALL.V, // 退货状态
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            rejectAmountExcludingVAT: 100000, // 退货金额
            inboundAmountExcludingVAT: 100000, // 入库金额
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
            editable: true, // 可修改的
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            inboundTime: '@datetime(T)', // 入库时间
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 入库单id
            basicClass: matClsEnum.GAS.V, // 采购物料基础类型
            rejectStatus: receiptRejectStatusEnum.PART.V, // 退货状态
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            rejectAmountExcludingVAT: 50000, // 退货金额
            inboundAmountExcludingVAT: 260000, // 入库金额
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
            editable: true, // 可修改的
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            inboundTime: '@datetime(T)', // 入库时间
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 4, // 入库单id
            boolPartyA: true, // 是否甲供
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
            licensePlate: patternLicensePlate, // 车牌号
            supplier: {
              // 供应商
              id: 1,
              name: '吖丫丫有限公司'
            },
            reviewStatus: reviewStatusEnum.PASS.V, // 审核状态
            editable: false, // 可修改的
            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            inboundTime: '@datetime(T)', // 入库时间
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 4
      }
    }
  }
}

// 钢材详情
const getReceiptDetail_id1 = {
  url: '/api/wms/report/raw-materials/inbound/receipt/1',
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
          supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
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
          // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
          logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
          requisitionsSN: ['SG-AFTER-123456', 'SG-AFTER-133456'], // 采购申请单
          purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 付款方式
          weightMeasurementMode: weightMeasurementModeEnum.OVERWEIGHT.V, // 重量计量方式
          supplier: {
            id: 1,
            name: '杭州天马钢材有限公司'
          }
        },
        id: 1, // 入库单id
        basicClass: 7, // 采购物料基础类型
        serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
        shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
        licensePlate: patternLicensePlate, // 车牌号
        loadingWeight: 2000.0, // 过磅重量
        list: [
          {
            id: 1,
            rejectStatus: materialRejectStatusEnum.PENDING_REVIEW.V,
            serialNumber: '3192520223',
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
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
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 1,
                  serialNumber: '3192520223',
                  classifyId: 103,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  specification: 'Q325B',
                  quantity: 1,
                  thickness: 10,
                  length: 1000,
                  width: 1000,
                  brand: '嘻嘻',
                  heatNoAndBatchNo: 'aaff',
                  remark: '66666',
                  mete: 80000,
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              },
              {
                reviewStatus: reviewStatusEnum.UNREVIEWED.V,
                createTime: '@datetime(T)',
                material: {
                  id: 1,
                  serialNumber: '3192520223',
                  classifyId: 103,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  specification: 'Q325B',
                  quantity: 2,
                  thickness: 10,
                  length: 1000,
                  width: 1000,
                  brand: '嘻嘻',
                  heatNoAndBatchNo: 'aaff',
                  remark: '66666',
                  mete: 160000,
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          },
          {
            id: 2,
            rejectStatus: materialRejectStatusEnum.ALL.V,
            basicClass: matClsEnum.STEEL_PLATE.V,
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
            amountExcludingVAT: 41681.42,
            inputVAT: 5418.58,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 2,
                  basicClass: matClsEnum.STEEL_PLATE.V,
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
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          },
          {
            id: 3,
            rejectStatus: materialRejectStatusEnum.PART.V,
            specification: '57*21*3*9 * Q325B',
            nationalStandard: 'GB-10', // 国家标准
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            quantity: 2,
            length: 10000,
            brand: '马钢',
            heatNoAndBatchNo: 'ooopp',
            mete: 252900,
            weight: 252900,
            unitPrice: 0.03,
            amount: 9174,
            amountExcludingVAT: 8188.58,
            requisitionsSN: 'SG-AFTER-123456',
            inputVAT: 1055.42,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 3,
                  rejectStatus: materialRejectStatusEnum.PART.V,
                  specification: '57*21*3*9 * Q325B',
                  nationalStandard: 'GB-10', // 国家标准
                  classifyId: 110,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  quantity: 1,
                  length: 10000,
                  brand: '马钢',
                  heatNoAndBatchNo: 'ooopp',
                  mete: 126450,
                  weight: 126450,
                  unitPrice: 0.03,
                  amount: 4587,
                  amountExcludingVAT: 4059.29,
                  requisitionsSN: 'SG-AFTER-123456',
                  inputVAT: 527.71,
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 200000,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: '12341234fsafs1234',
            requisitionsSN: 'SG-AFTER-133456',
            thickness: 0.326,
            length: 3907.62,
            width: 1000,
            mete: 200000,
            weight: 1000000,
            unitPrice: 0.05,
            amount: 500,
            amountExcludingVAT: 450,
            inputVAT: 50,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
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
const getReceiptDetail_id2 = {
  url: '/api/wms/report/raw-materials/inbound/receipt/2',
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
          supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
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
          // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
          logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
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
        shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
        licensePlate: patternLicensePlate, // 车牌号
        list: [
          {
            rejectStatus: materialRejectStatusEnum.ALL.V,
            id: 6,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 80,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 6,
                  classifyId: 204,
                  specification: 'M27 * 60',
                  basicClass: matClsEnum.MATERIAL.V,
                  brand: '嘻嘻',
                  mete: 80,
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          },
          {
            rejectStatus: materialRejectStatusEnum.ALL.V,
            id: 7,
            classifyId: 247,
            basicClass: matClsEnum.STEEL_PLATE.V,
            quantity: 10,
            brand: '嘻嘻',
            color: '蓝色',
            mete: 100000,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 7,
                  classifyId: 247,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  quantity: 10,
                  brand: '嘻嘻',
                  color: '蓝色',
                  mete: 100000,
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          }
        ]
      }
    }
  }
}

// 气体详情
const getReceiptDetail_id3 = {
  url: '/api/wms/report/raw-materials/inbound/receipt/3',
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
          supplyType: orderSupplyTypeEnum.SELF.V, // 供应类型
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
          // pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
          logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
          logisticsPayerType: logisticsPayerEnum.DEMAND.V, // 物流运输方式
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
        shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
        licensePlate: patternLicensePlate, // 车牌号
        list: [
          {
            rejectStatus: materialRejectStatusEnum.PART.V,
            id: 8,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 8,
                  classifyId: 901,
                  basicClass: matClsEnum.GAS.V,
                  quantity: 8,
                  brand: '嘻嘻',
                  remark: '66666',
                  mete: 160000,
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ]
          }
        ]
      }
    }
  }
}

// 入库明细
const getDetails = {
  url: '/api/wms/report/raw-materials/inbound/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            rejectStatus: materialRejectStatusEnum.PENDING_REVIEW.V,
            boolPartyA: true,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            quantity: 10,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            mete: 800000,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 1,
                  serialNumber: '3192520223',
                  classifyId: 103,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  specification: 'Q325B',
                  quantity: 1,
                  thickness: 10,
                  length: 1000,
                  width: 1000,
                  brand: '嘻嘻',
                  heatNoAndBatchNo: 'aaff',
                  remark: '66666',
                  mete: 80000,
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              },
              {
                reviewStatus: reviewStatusEnum.UNREVIEWED.V,
                createTime: '@datetime(T)',
                material: {
                  id: 1,
                  serialNumber: '3192520223',
                  classifyId: 103,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  specification: 'Q325B',
                  quantity: 2,
                  thickness: 10,
                  length: 1000,
                  width: 1000,
                  brand: '嘻嘻',
                  heatNoAndBatchNo: 'aaff',
                  remark: '66666',
                  mete: 160000,
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ],
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 2,
            rejectStatus: materialRejectStatusEnum.ALL.V,
            basicClass: matClsEnum.STEEL_PLATE.V,
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
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.02,
            amount: 47100,
            amountExcludingVAT: 41681.42,
            inputVAT: 5418.58,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 2,
                  basicClass: matClsEnum.STEEL_PLATE.V,
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
                  requisitionsSN: 'SG-AFTER-123456',
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ],
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 3,
            rejectStatus: materialRejectStatusEnum.PART.V,
            specification: '57*21*3*9 * Q325B',
            nationalStandard: 'GB-10', // 国家标准
            classifyId: 110,
            basicClass: matClsEnum.SECTION_STEEL.V,
            quantity: 2,
            length: 10000,
            brand: '马钢',
            heatNoAndBatchNo: 'ooopp',
            mete: 252900,
            weight: 252900,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.03,
            amount: 9174,
            amountExcludingVAT: 8188.58,
            requisitionsSN: 'SG-AFTER-123456',
            inputVAT: 1055.42,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            rejectList: [
              {
                reviewStatus: reviewStatusEnum.PASS.V,
                createTime: '@datetime(T)',
                material: {
                  id: 3,
                  rejectStatus: materialRejectStatusEnum.PART.V,
                  specification: '57*21*3*9 * Q325B',
                  nationalStandard: 'GB-10', // 国家标准
                  classifyId: 110,
                  basicClass: matClsEnum.STEEL_PLATE.V,
                  quantity: 1,
                  length: 10000,
                  brand: '马钢',
                  heatNoAndBatchNo: 'ooopp',
                  mete: 126450,
                  weight: 126450,
                  unitPrice: 0.03,
                  amount: 4587,
                  amountExcludingVAT: 4059.29,
                  requisitionsSN: 'SG-AFTER-123456',
                  inputVAT: 527.71,
                  project: {
                    'id|+1': 1,
                    'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
                    'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
                    serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                  },
                  factory: {
                    id: 1,
                    name: '一号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              }
            ],
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 5,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 200000,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: '12341234fsafs1234',
            requisitionsSN: 'SG-AFTER-133456',
            thickness: 0.326,
            length: 3907.62,
            width: 1000,
            mete: 200000,
            weight: 1000000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.05,
            amount: 500,
            amountExcludingVAT: 450,
            inputVAT: 50,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 4,
              name: '668号仓库'
            },
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 6,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 80,
            unitPrice: 0.01,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 7,
            classifyId: 247,
            basicClass: matClsEnum.STEEL_PLATE.V,
            quantity: 10,
            brand: '嘻嘻',
            color: '蓝色',
            mete: 100000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 8,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            requisitionsSN: 'SG-AFTER-123456',
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            factory: {
              id: 1,
              name: '一号工厂'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            inboundReceipt: {
              id: 1, // 入库单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
              shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
              licensePlate: patternLicensePlate, // 车牌号
              supplier: {
                id: 1,
                name: '天马耗材有限公司'
              },
              purchaseOrder: {
                id: 1,
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
              },
              applicantName: '@cname', // 创建人（填写入库的人）
              editorName: '@cname', // 编辑人（最后编辑的用户）
              reviewerName: '@cname', // 审核人（审核的人）
              inboundTime: '@datetime(T)', // 入库时间
              createTime: '@datetime(T)', // 创建时间
              updateTime: '@datetime(T)', // 修改时间
              userUpdateTime: '@datetime(T)', // 用户修改时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          }
        ],
        totalElements: 7
      }
    }
  }
}

// 入库明细excel导出
const exportDetailsExcel = {
  url: '/api/wms/report/raw-materials/inbound/details/excel',
  method: 'get',
  timeout: 500,
  rawResponse: async (req, res) => {
    let result = ''
    res.setHeader('Content-Type', 'application/vnd.ms-excel;charset=UTF-8')
    res.setHeader('Content-Disposition', 'attachment;filename=%E5%85%A5%E5%BA%93%E6%98%8E%E7%BB%86.xlsx')
    if (Math.random() > 0.5) {
      result = 'code=20000;message='
    } else {
      result =
        'code=40000;message=%E5%85%A5%E5%BA%93%E6%98%8E%E7%BB%86excel%E8%A1%A8%E6%A0%BC%E5%AF%BC%E5%87%BA%E6%97%B6%E9%97%B4%E8%8C%83%E5%9B%B4%E4%B8%8D%E5%8F%AF%E8%B6%85%E8%BF%87%E4%B8%80%E5%B9%B4'
    }
    res.setHeader('Result', result)
    res.statusCode = 200
    res.end(`入库明细excel导出`)
  }
}

export default [getReceiptList, getReceiptDetail_id1, getReceiptDetail_id2, getReceiptDetail_id3, getDetails, exportDetailsExcel]
