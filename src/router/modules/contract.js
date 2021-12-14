// 路由：计划管理
export default {
  id: 3,
  name: '合同管理',
  children: [
    {
      path: '/contract/contract-center',
      component: 'Layout',
      hidden: false,
      name: 'ContractCenter',
      alwaysShow: false,
      redirect: '/contract/contract-center/record',
      meta: { title: '合同管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'contractRecord',
          path: 'record',
          hidden: false,
          component: '/contract/contract-record/index',
          meta: { title: '合同档案', icon: 'contract2', noCache: true }
        }
      ]
    },
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
      path: '/contract/collection-manage',
      component: 'Layout',
      hidden: false,
      name: 'ContractCollectionManage',
      alwaysShow: false,
      redirect: '/contract/collection-manage/collection-and-invoice',
      meta: { title: '收款管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'ContractCollectionAndInvoice',
          path: 'collection-and-invoice',
          hidden: false,
          component: '/contract/collection-and-invoice/index',
          meta: { title: '收款与开票', icon: 'contract2', noCache: true }
        }
      ]
    },
    // {
    //   path: '/contract/project-execution',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'projectExecution',
    //   alwaysShow: false,
    //   redirect: '/contract/project-execution/ledger',
    //   meta: { title: '项目执行台帐', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'projectExecutionLedger',
    //       path: 'ledger',
    //       hidden: false,
    //       component: '/contract/project-execution-ledger/index',
    //       meta: { title: '项目执行台帐', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/contract/collection-manage',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ContractCollectionManage',
    //   alwaysShow: false,
    //   redirect: '/contract/collection-manage/collection-list',
    //   meta: { title: '收款管理', icon: 'contract2', noCache: true },
    //   children: [
    //     // {
    //     //   name: 'ContractCollectionList',
    //     //   path: 'collection-list',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/collection-list',
    //     //   meta: { title: '收款列表', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'ContractProjectExecutionLedger',
    //     //   path: 'project-execution-ledger',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/project-execution-ledger',
    //     //   meta: { title: '项目执行台账', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'ContractArrearageWarning',
    //     //   path: 'project',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/arrearage-warning',
    //     //   meta: { title: '欠款预警', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'ContractCollectionPlan',
    //     //   path: 'collection-plan',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/collection-plan',
    //     //   meta: { title: '月收款计划', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'ContractCollectionPlanExecution',
    //     //   path: 'collection-plan-execution',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/collection-plan-execution',
    //     //   meta: { title: '收款计划执行', icon: 'contract2', noCache: true }
    //     // },
    //     {
    //       name: 'ContractCollectionAndInvoice',
    //       path: 'collection-and-invoice',
    //       hidden: false,
    //       component: '/contract/collection-manage/collection-and-invoice',
    //       meta: { title: '收款与开票', icon: 'contract2', noCache: true }
    //     }
    //     // {
    //     //   name: 'ContractShipInfo',
    //     //   path: 'ship-info',
    //     //   hidden: false,
    //     //   component: '/contract/collection-manage/ship-info',
    //     //   meta: { title: '发运信息', icon: 'contract2', noCache: true }
    //     // }
    //   ]
    // },
    // {
    //   path: '/supplier-payment-manage/material',
    //   // path: 'contract',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MaterialSupplierPaymentManage',
    //   alwaysShow: false,
    //   redirect: '/supplier-payment-manage/material/supplier-list',
    //   meta: { title: '付款管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'ContractReimbursementList',
    //       path: 'reimbursement-list',
    //       hidden: false,
    //       component: '/contract/reimbursement-manage/reimbursement-list',
    //       meta: { title: '报销列表', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       path: 'payment',
    //       // path: 'contract',
    //       component: '',
    //       hidden: false,
    //       name: 'supplierPayment',
    //       alwaysShow: false,
    //       redirect: '/supplier-payment-manage/material/payment/payable',
    //       meta: { title: '供应商付款', icon: 'contract2', noCache: true },
    //       children: [
    //         {
    //           name: 'MaterialSupplierPayable',
    //           path: 'payable',
    //           hidden: false,
    //           component: '/contract/supplier-payment-manage/material/payable/index',
    //           meta: { title: '应付汇总', icon: 'contract2', noCache: true }
    //         },
    //         {
    //           name: 'MaterialPaymentInfo',
    //           path: 'paymentInfo',
    //           hidden: false,
    //           component: '/contract/supplier-payment-manage/material/payment-info/index',
    //           meta: { title: '应付看板', icon: 'contract2', noCache: true }
    //         },
    //         {
    //           name: 'MaterialSupplierPaymentAndInvoice',
    //           path: 'payment-and-invoice',
    //           hidden: false,
    //           component: '/contract/supplier-payment-manage/material/payment-and-invoice/index',
    //           meta: { title: '付款与收票', icon: 'contract2', noCache: true }
    //         },
    //         {
    //           name: 'MaterialPayList',
    //           path: 'pay-list',
    //           hidden: false,
    //           component: '/contract/supplier-payment-manage/material/pay-list/index',
    //           meta: { title: '付款台账', icon: 'contract2', noCache: true }
    //         }
    //       ]
    //     }
    //     // {
    //     //   name: 'MaterialSupplierPaymentPurchaseOrderList',
    //     //   path: 'supplier-list',
    //     //   hidden: false,
    //     //   component: '/contract/supplier-payment-manage/material/purchase-order-list',
    //     //   meta: { title: '订单列表', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'MaterialSupplierPaymentAndInvoice',
    //     //   path: 'payment-and-invoice',
    //     //   hidden: false,
    //     //   component: '/contract/supplier-payment-manage/material/payment-and-invoice/index',
    //     //   meta: { title: '付款与收票', icon: 'contract2', noCache: true }
    //     // },
    //     // {
    //     //   name: 'MaterialSupplierPayable',
    //     //   path: 'payable',
    //     //   hidden: false,
    //     //   component: '/contract/supplier-payment-manage/material/payable/index',
    //     //   meta: { title: '应付汇总', icon: 'contract2', noCache: true }
    //     // }
    //   ]
    // },
    // {
    //   path: '/contract/project-tax',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ProjectTax',
    //   alwaysShow: false,
    //   redirect: '/contract/project-tax/tax-list',
    //   meta: { title: '增值税管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'TaxList',
    //       path: 'tax-list',
    //       hidden: false,
    //       component: '/contract/project-tax/index',
    //       meta: { title: '增值税预估', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/contract/reimbursement-manage',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ContractReimbursementManage',
    //   alwaysShow: true,
    //   redirect: '/contract/reimbursement-manage/reimbursement-list',
    //   meta: { title: '报销管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'ContractReimbursementList',
    //       path: 'reimbursement-list',
    //       hidden: false,
    //       component: '/contract/reimbursement-manage/reimbursement-list',
    //       meta: { title: '报销列表', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/supplier-payment-manage/material',
    //   // path: 'contract',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MaterialSupplierPaymentManage',
    //   alwaysShow: false,
    //   redirect: '/supplier-payment-manage/material/supplier-list',
    //   meta: { title: '物料供应商付款管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'MaterialSupplierPaymentPurchaseOrderList',
    //       path: 'supplier-list',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/material/purchase-order-list',
    //       meta: { title: '订单列表', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'MaterialSupplierPaymentAndInvoice',
    //       path: 'payment-and-invoice',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/material/payment-and-invoice/index',
    //       meta: { title: '付款与收票', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'MaterialSupplierPayable',
    //       path: 'payable',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/material/payable/index',
    //       meta: { title: '应付汇总', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/supplier-payment-manage/logistics',
    //   // path: 'contract',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'LogisticsSupplierPaymentManage',
    //   alwaysShow: false,
    //   redirect: '/supplier-payment-manage/logistics/supplier-list',
    //   meta: { title: '物流供应商付款管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'LogisticsSupplierPaymentPurchaseOrderList',
    //       path: 'supplier-list',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/logistics/purchase-order-list',
    //       meta: { title: '物料运输订单', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'LogisticsSupplierPaymentAndInvoice',
    //       path: 'payment-and-invoice',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/logistics/payment-and-invoice/index',
    //       meta: { title: '物料运输付款与收票', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'LogisticsSupplierPaymentManufacturesOrderList',
    //       path: 'manufactures-order-list',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/logistics/manufactures-order-list',
    //       meta: { title: '制成品运输订单', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'LogisticsSupplierManufacturesPaymentAndInvoice',
    //       path: 'manufactures-payment-and-invoice',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/logistics/manufactures-payment-and-invoice/index',
    //       meta: { title: '制成品运输付款与收票', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'LogisticsSupplierPayable',
    //       path: 'payable',
    //       hidden: false,
    //       component: '/supplier/supplier-payment-manage/logistics/payable/index',
    //       meta: { title: '应付汇总', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/contract/cost-center',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'CostCenter',
    //   alwaysShow: false,
    //   redirect: '/contract/cost-center/financial-report',
    //   meta: { title: '成本中心', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'FinancialReport',
    //       path: 'financial-report',
    //       hidden: false,
    //       component: '/contract/cost-center/financial-report',
    //       meta: { title: '项目财报', icon: 'contract2', noCache: true }
    //     }
    //   ]
    // }
    // {
    //   path: 'contract-report',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ContractReport',
    //   alwaysShow: false,
    //   redirect: '/contract-report/report',
    //   meta: { title: '报表管理', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'Report',
    //       path: 'report',
    //       hidden: false,
    //       component: '/contract/report/index',
    //       meta: { title: 'KPI', icon: 'chart', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   // path: '/mes-project',
    //   path: '/contract',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'Contract',
    //   alwaysShow: false,
    //   redirect: '/contract/projects',
    //   meta: { title: '项目管理', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'ContractProject',
    //       path: 'projects',
    //       hidden: false,
    //       component: '/contract/projects/index',
    //       meta: { title: '项目列表', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: 'contract-ledger',
    //   // path: 'contract',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ContractLedgerManage',
    //   alwaysShow: false,
    //   redirect: '/contract/ledger',
    //   meta: { title: '合同管理', icon: 'contract2', noCache: true },
    //   children: [
    //     {
    //       name: 'ContractLedger',
    //       path: 'ledger',
    //       hidden: false,
    //       component: '/contract/ledger/index',
    //       meta: { title: '项目台账', icon: 'contract2', noCache: true }
    //     },
    //     {
    //       name: 'Invoice',
    //       path: 'invoice',
    //       hidden: false,
    //       component: '/contract/invoice/index',
    //       meta: { title: '收款及开票', icon: 'invoice', noCache: true }
    //     }
    //   ]
    // }
  ]
}