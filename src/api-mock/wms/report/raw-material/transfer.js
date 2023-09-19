import { matClsEnum } from '@/utils/enum/modules/classification'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { transferTypeEnum } from '@/utils/enum/modules/wms'

// 调拨明细
const getDetails = {
  url: '/api/wms/report/raw-materials/transfer/details',
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
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                factory: {
                  id: 1,
                  name: '彩虹3号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '62号仓库'
                }
              },
              direction: {
                project: {
                  // 项目
                  id: 3,
                  name: '长沙五一广场',
                  shortName: '五一广场',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              borrowProject: {
                // 借用项目
                id: 2,
                name: '长安街666666号辅路',
                shortName: '长安街',
                serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
              },
              boolBorrowReturnNotSelf: true, // 是否其他项目借用归还
              transferType: transferTypeEnum.BORROW_RETURN.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
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
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.02,
            amount: 47100,
            amountExcludingVAT: 41681.42,
            inputVAT: 5418.58,
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              },
              direction: {
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 3,
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
            inputVAT: 1055.42,
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                project: {
                  // 项目
                  id: 3,
                  name: '长沙五一广场',
                  shortName: '五一广场',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '彩虹3号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '62号仓库'
                }
              },
              direction: {
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
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
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: [
                {
                  factory: {
                    id: 1,
                    name: '长江1号工厂'
                  },
                  warehouse: {
                    id: 1,
                    name: '666号仓库'
                  }
                }
              ],
              direction: {
                project: {
                  // 项目
                  id: 1,
                  name: '你脸红个泡泡茶壶666号主路',
                  shortName: '你脸红个泡泡茶壶',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
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
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              },
              direction: {
                project: {
                  // 项目
                  id: 1,
                  name: '你脸红个泡泡茶壶666号主路',
                  shortName: '你脸红个泡泡茶壶',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.PROJECT_WARE.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
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
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              },
              direction: {
                project: {
                  // 项目
                  id: 1,
                  name: '你脸红个泡泡茶壶666号主路',
                  shortName: '你脸红个泡泡茶壶',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.RETURN_PARTY_A.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          },
          {
            id: 8,
            classifyId: 901,
            basicClass: matClsEnum.GAS.V,
            quantity: 10,
            brand: '嘻嘻',
            mete: 200000,
            invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
            taxRate: 3, // 税率（百分比）
            unitPrice: 0.01,
            amount: 8000,
            amountExcludingVAT: 7079.64,
            inputVAT: 920.36,
            transferReceipt: {
              id: 1, // 调拨单id
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/, // 调拨单号
              source: {
                project: {
                  // 项目
                  id: 2,
                  name: '长安街666666号辅路',
                  shortName: '长安街',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江1号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '666号仓库'
                }
              },
              direction: {
                project: {
                  // 项目
                  id: 1,
                  name: '你脸红个泡泡茶壶666号主路',
                  shortName: '你脸红个泡泡茶壶',
                  serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
                },
                factory: {
                  id: 1,
                  name: '长江2号工厂'
                },
                warehouse: {
                  id: 1,
                  name: '662号仓库'
                }
              },
              transferType: transferTypeEnum.PUBLIC_WARE.V, // 调拨类型
              applicantName: '@cname', // 创建人（填写调拨的人）
              reviewerName: '@cname', // 审核人（审核的人）
              transferTime: '@datetime(T)', // 调拨时间
              createTime: '@datetime(T)', // 创建时间
              reviewTime: '@datetime(T)' // 审核时间
            }
          }
        ],
        totalElements: 7
      }
    }
  }
}

// 调拨明细excel导出
const exportDetailsExcel = {
  url: '/api/wms/report/raw-materials/transfer/details/excel',
  method: 'get',
  timeout: 500,
  rawResponse: async (req, res) => {
    let result = ''
    res.setHeader('Content-Type', 'application/vnd.ms-excel;charset=UTF-8')
    res.setHeader('Content-Disposition', 'attachment;filename=%E8%B0%83%E6%8B%A8%E6%98%8E%E7%BB%86.xlsx')
    if (Math.random() > 0.5) {
      result = 'code=20000;message='
    } else {
      result =
        'code=40000;message=%E8%B0%83%E6%8B%A8%E6%98%8E%E7%BB%86excel%E8%A1%A8%E6%A0%BC%E5%AF%BC%E5%87%BA%E6%97%B6%E9%97%B4%E8%8C%83%E5%9B%B4%E4%B8%8D%E5%8F%AF%E8%B6%85%E8%BF%87%E4%B8%80%E5%B9%B4'
    }
    res.setHeader('Result', result)
    res.statusCode = 200
    res.end(`调拨明细excel导出`)
  }
}

export default [getDetails, exportDetailsExcel]
