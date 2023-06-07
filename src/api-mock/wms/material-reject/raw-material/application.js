import { matClsEnum } from '@/utils/enum/modules/classification'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { baseMaterialTypeEnum, materialRejectStatusEnum, orderSupplyTypeEnum, purchaseOrderPaymentModeEnum } from '@/utils/enum/modules/wms'
import { patternLicensePlate } from '@/utils/validate/pattern'

const getInboundListForRejectable = {
  url: '/api/wms/reject/inbound-rejectable',
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
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
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
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            // reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)' // 用户修改时间
          },
          {
            id: 2, // 入库单id
            basicClass: matClsEnum.MATERIAL.V, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
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

            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          },
          {
            id: 3, // 入库单id
            basicClass: matClsEnum.STEEL_PLATE.V6, // 采购物料基础类型
            serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 入库单号
            purchaseOrder: {
              id: 1,
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/ // 采购合同编号
            },
            shipmentNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 邮递-物流单号
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

            approvalComments: '@csentence',
            applicantName: '@cname', // 创建人（填写入库的人）
            editorName: '@cname', // 编辑人（最后编辑的用户）
            reviewerName: '@cname', // 审核人（审核的人）
            createTime: '@datetime(T)', // 创建时间
            updateTime: '@datetime(T)', // 修改时间
            userUpdateTime: '@datetime(T)', // 用户修改时间
            reviewTime: '@datetime(T)' // 审核时间
          }
        ],
        totalElements: 3
      }
    }
  }
}

const getInboundDetail_1 = {
  url: '/api/wms/reject/inbound-rejectable/1',
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
            workshop: {
              id: 1,
              name: '一号车间'
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
                  workshop: {
                    id: 1,
                    name: '一号车间'
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
                  workshop: {
                    id: 1,
                    name: '一号车间'
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
            workshop: {
              id: 1,
              name: '一号车间'
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
                  workshop: {
                    id: 1,
                    name: '一号车间'
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
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
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
            workshop: {
              id: 1,
              name: '一号车间'
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
                  classifyId: 110,
                  nationalStandard: 'GB-10', // 国家标准
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
                  workshop: {
                    id: 1,
                    name: '一号车间'
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
            workshop: {
              id: 1,
              name: '一号车间'
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

// 辅材入库详情
const getInboundDetail_2 = {
  url: '/api/wms/reject/inbound-rejectable/2',
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
            workshop: {
              id: 1,
              name: '一号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
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
            workshop: {
              id: 1,
              name: '一号车间'
            },
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

// 辅材入库详情
const getInboundDetail_3 = {
  url: '/api/wms/reject/inbound-rejectable/3',
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
            workshop: {
              id: 1,
              name: '一号车间'
            },
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

const getMatchList_1 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/1',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 11,
            classifyId: 103,
            basicClass: matClsEnum.STEEL_PLATE.V,
            specification: 'Q325B',
            boolHasFrozen: true, // 有冻结
            frozenQuantity: 7, // 冻结数量
            quantity: 8, // 数量
            frozenMete: 700000, // 冻结核算量
            mete: 800000,
            thickness: 10,
            length: 1000,
            width: 1000,
            brand: '嘻嘻',
            heatNoAndBatchNo: 'aaff',
            remark: '66666',
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 1,
              name: '一号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            }
          },
          {
            id: 12,
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
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '一号仓库'
            }
          }
        ]
      }
    }
  }
}

// 匹配详情
const getMatchList_3 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/3',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 31,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
            basicClass: matClsEnum.SECTION_STEEL.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 32,
            specification: '57*21*3*9 * Q325B',
            classifyId: 110,
            nationalStandard: 'GB-10', // 国家标准
            basicClass: matClsEnum.SECTION_STEEL.V,
            quantity: 1,
            length: 10000,
            totalLength: 10,
            brand: '马钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            mete: 152900,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '一号仓库'
            }
          }
        ]
      }
    }
  }
}

// 匹配详情
const getMatchList_5 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/5',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 41,
            classifyId: 120,
            basicClass: matClsEnum.STEEL_COIL.V,
            specification: 'DC51D+Z',
            quantity: 2207,
            color: '天蓝',
            brand: '武钢',
            heatNoAndBatchNo: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{4,6}/,
            thickness: 0.326,
            length: 3907,
            width: 1000,
            mete: 10000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          }
        ]
      }
    }
  }
}

// 匹配详情
const getMatchList_6 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/6',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 41,
            classifyId: 204,
            specification: 'M27 * 60',
            basicClass: matClsEnum.MATERIAL.V,
            brand: '嘻嘻',
            mete: 1000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          }
        ]
      }
    }
  }
}

// 油漆匹配详情
const getMatchList_7 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/7',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 42,
            classifyId: 247,
            basicClass: matClsEnum.MATERIAL.V,
            quantity: 10,
            brand: '嘻嘻',
            color: '蓝色',
            mete: 100000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          }
        ]
      }
    }
  }
}

// 油漆匹配详情
const getMatchList_8 = {
  url: '/api/wms/reject/inbound-rejectable/material-match/8',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 51,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 52,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 53,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          },
          {
            id: 54,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            remark: '66666',
            mete: 200000,
            project: {
              id: 1,
              name: '长安街666666号辅路',
              shortName: '长安街',
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            workshop: {
              id: 2,
              name: '二号车间'
            },
            warehouse: {
              id: 1,
              name: '666号仓库'
            },
            outboundTime: '@datetime(T)'
          }
        ]
      }
    }
  }
}

// 退货申请
const rejectApplication = {
  url: '/api/wms/reject/inbound-rejectable',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getInboundListForRejectable,
  getInboundDetail_1,
  getInboundDetail_2,
  getInboundDetail_3,
  getMatchList_1,
  getMatchList_3,
  getMatchList_5,
  getMatchList_6,
  getMatchList_7,
  getMatchList_8,
  rejectApplication
]
