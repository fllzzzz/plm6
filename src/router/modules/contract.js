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
          meta: { title: '原材料', icon: 'contract2', noCache: true }
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
      redirect: '/contract/sales-manage/visa-manage/change/index',
      meta: { title: '销售管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'VisaManage',
          path: 'visa-manage',
          hidden: false,
          redirect: '/contract/sales-manage/visa-manage/change',
          meta: { title: '签证管理', icon: 'contract2', noCache: true },
          children: [{
            name: 'VisaChange',
            path: 'change',
            hidden: false,
            component: '/contract/sales-manage/visa-manage/change/index',
            meta: { title: '签证变更', icon: 'contract2', noCache: true }
          }]
        },
        {
          name: 'TransactionRecord',
          path: 'transaction-record',
          hidden: false,
          component: '/contract/sales-manage/transaction-record/index',
          meta: { title: '客户交易记录', icon: 'contract2', noCache: true }
        }
      ]
    }
  ]
}
