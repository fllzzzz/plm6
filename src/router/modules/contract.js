// 路由：合同管理
export default {
  id: 3,
  name: '合同管理',
  children: [
    {
      path: 'contract',
      component: 'Layout',
      hidden: false,
      name: 'ContractProjectManage',
      alwaysShow: false,
      redirect: '/contract/project',
      meta: { title: '项目管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'ContractProject',
          path: 'project',
          hidden: false,
          component: '/contract/project-manage/index',
          meta: { title: '项目列表', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/contract/contract-center',
      component: 'Layout',
      hidden: false,
      name: 'ContractCenter',
      alwaysShow: false,
      redirect: '/contract/contract-center/record',
      meta: { title: '合同执行', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'ContractChange',
          path: 'contract-change',
          hidden: false,
          component: '/contract/contract-change/index',
          meta: { title: '合同变更', icon: 'contract2', noCache: true }
        },
        {
          name: 'ContractRecord',
          path: 'record',
          hidden: false,
          component: '/contract/contract-record/index',
          meta: { title: '合同档案', icon: 'contract2', noCache: true }
        },
        {
          name: 'ContractLedger',
          path: 'contract-ledger',
          hidden: false,
          component: '/contract/contract-ledger/index',
          meta: { title: '项目台账', icon: 'contract2', noCache: true }
        },
        {
          name: 'CollectionLedger',
          path: 'collection-ledger',
          hidden: false,
          component: '/contract/collection-ledger/index',
          meta: { title: '收款台账', icon: 'contract2', noCache: true }
        },
        {
          name: 'CollectionWarn',
          path: 'collection-warn',
          hidden: false,
          component: '/contract/collection-warn/index',
          meta: { title: '收款预警', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/supplier-payment-manage/material',
      component: 'Layout',
      hidden: false,
      name: 'MaterialSupplierPaymentManage',
      alwaysShow: false,
      redirect: '/supplier-payment-manage/material/supplier-material',
      meta: { title: '供应商付款', icon: 'contract2', noCache: true },
      children: [
        // {
        //   name: 'ContractReimbursementList',
        //   path: 'reimbursement',
        //   hidden: false,
        //   component: '/contract/payment-manage/reimbursement-manage/index',
        //   meta: { title: '报销列表', icon: 'contract2', noCache: true }
        // },
        {
          name: 'supplierMaterial',
          path: 'supplier-material',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/material-manage/index',
          meta: { title: '采购合同', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierProduct',
          path: 'supplier-product',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/product-manage/index',
          meta: { title: '制成品', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierLogistics',
          path: 'supplier-logistics',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/logistics-manage/index',
          meta: { title: '物流', icon: 'contract2', noCache: true }
        },
        {
          name: 'subcontractPaymentManage',
          path: 'subcontract-payment-manage',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/subcontract-manage/index',
          meta: { title: '分包订单', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPayable',
          path: 'payable',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/payable/index',
          meta: { title: '应付汇总', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPaymentLedger',
          path: 'payment-ledger',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/payment-ledger/index',
          meta: { title: '付款台账', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/contract/sales-manage',
      component: 'Layout',
      hidden: false,
      name: 'SalesManage',
      alwaysShow: false,
      redirect: '/contract/sales-manage/price-manage/index',
      meta: { title: '销售管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'SalesPriceManage',
          path: 'price-manage',
          hidden: false,
          component: '/contract/sales-manage/price-manage/index',
          meta: { title: '商务录入', icon: 'contract2', noCache: true }
        },
        {
          name: 'VisaManage',
          path: 'visa-manage',
          hidden: false,
          component: '/contract/sales-manage/visa-manage/index',
          meta: { title: '签证管理', icon: 'contract2', noCache: true }
        },
        {
          name: 'SettlementManage',
          path: 'settlement-manage',
          hidden: false,
          component: '/contract/sales-manage/settlement-manage/index',
          meta: { title: '结算管理', icon: 'contract2', noCache: true }
        },
        {
          name: 'OrderTracking',
          path: 'order-tracking',
          hidden: false,
          component: '/contract/sales-manage/order-tracking/index',
          meta: { title: '订单跟踪', icon: 'contract2', noCache: true }
        },
        {
          name: 'TransactionRecord',
          path: 'transaction-record',
          hidden: false,
          component: '/contract/sales-manage/transaction-record/index',
          meta: { title: '客户交易记录', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/contract/expense-entry',
      component: 'Layout',
      hidden: false,
      name: 'ExpenseEntry',
      alwaysShow: false,
      redirect: '/contract/expense-entry',
      meta: { title: '费用录入', icon: 'contract2', noCache: true },
      children: [
        {
          path: 'fixed-assets-depreciation',
          component: '',
          hidden: false,
          name: 'FixedAssetsDepreciation',
          alwaysShow: false,
          redirect: '/contract/expense-entry/fixed-assets-depreciation',
          meta: { title: '固定资产折旧', icon: 'contract2', noCache: true },
          children: [
            {
              name: 'PlantDepreciation',
              path: 'plant-depreciation',
              hidden: false,
              component: '/contract/expense-entry/fixed-assets-depreciation/plant-depreciation/index',
              meta: { title: '厂房折旧', icon: 'contract2', noCache: true }
            },
            {
              name: 'DeviceDepreciation',
              path: 'device-depreciation',
              hidden: false,
              component: '/contract/expense-entry/fixed-assets-depreciation/device-depreciation/index',
              meta: { title: '设备折旧', icon: 'contract2', noCache: true }
            }
          ]
        },
        {
          path: 'water-electricity-cost',
          component: '',
          hidden: false,
          name: 'WaterElectricityCost',
          alwaysShow: false,
          redirect: '/contract/expense-entry/water-electricity-cost',
          meta: { title: '水电费', icon: 'contract2', noCache: true },
          children: [
            {
              name: 'WaterElectricity',
              path: 'water-electricity',
              hidden: false,
              component: '/contract/expense-entry/water-electricity-cost/water-electricity/index',
              meta: { title: '水电费', icon: 'contract2', noCache: true }
            },
            {
              name: 'GasCost',
              path: 'gas-cost',
              hidden: false,
              component: '/contract/expense-entry/water-electricity-cost/gas-cost/index',
              meta: { title: '气体统计', icon: 'contract2', noCache: true }
            }
          ]
        },
        {
          name: 'ExpenseReporting',
          path: 'expense-reporting',
          hidden: false,
          component: '/contract/expense-entry/expense-reporting/index',
          meta: { title: '费用填报', icon: 'contract2', noCache: true }
        },
        {
          path: 'management-cost',
          component: '',
          hidden: false,
          name: 'ManagementCost',
          alwaysShow: false,
          redirect: '/contract/expense-entry/management-cost',
          meta: { title: '管理费', icon: 'contract2', noCache: true },
          children: [
            {
              name: 'Salary',
              path: 'salary',
              hidden: false,
              component: '/contract/expense-entry/management-cost/salary/index',
              meta: { title: '人员工资', icon: 'contract2', noCache: true }
            },
            {
              name: 'PropertyCost',
              path: 'property-cost',
              hidden: false,
              component: '/contract/expense-entry/management-cost/property-cost/index',
              meta: { title: '物业费', icon: 'contract2', noCache: true }
            }
          ]
        },
        {
          name: 'TestingCost',
          path: 'testing-cost',
          hidden: false,
          component: '/contract/expense-entry/testing-cost/index',
          meta: { title: '检测费', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/contract/fortune-report-manage',
      component: 'Layout',
      hidden: false,
      name: 'FortuneReport',
      alwaysShow: false,
      redirect: '/contract/fortune-report-manage/fortune-report',
      meta: { title: '业财报表', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'ContractFortuneReport',
          path: 'fortune-report',
          hidden: false,
          component: '/contract/fortune-report/index',
          meta: {
            title: '业财报表',
            icon: 'project',
            noCache: true
          }
        }
      ]
    }
  ]
}
