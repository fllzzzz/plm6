// 路由：计划管理
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
          name: 'contractChange',
          path: 'contract-change',
          hidden: false,
          component: '/contract/contract-change/index',
          meta: { title: '合同变更', icon: 'contract2', noCache: true }
        },
        {
          name: 'contractRecord',
          path: 'record',
          hidden: false,
          component: '/contract/contract-record/index',
          meta: { title: '合同档案', icon: 'contract2', noCache: true }
        },
        {
          name: 'contractLedger',
          path: 'contract-ledger',
          hidden: false,
          component: '/contract/contract-ledger/index',
          meta: { title: '项目台账', icon: 'contract2', noCache: true }
        }
      ]
    },
    {
      path: '/supplier-payment-manage/material',
      component: 'Layout',
      hidden: false,
      name: 'MaterialSupplierPaymentManage',
      alwaysShow: false,
      redirect: '/supplier-payment-manage/material/reimbursement',
      meta: { title: '付款管理', icon: 'contract2', noCache: true },
      children: [
        {
          name: 'ContractReimbursementList',
          path: 'reimbursement',
          hidden: false,
          component: '/contract/payment-manage/reimbursement-manage/index',
          meta: { title: '报销列表', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPayable',
          path: 'payable',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/payable/index',
          meta: { title: '供应商付款-应付汇总', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPayBoard',
          path: 'payBoard',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/payment-board/index',
          meta: { title: '供应商付款-应付看板', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPaymentInvoice',
          path: 'payment-invoice',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/pay-receive/index',
          meta: { title: '供应商付款-付款收票', icon: 'contract2', noCache: true }
        },
        {
          name: 'supplierPayList',
          path: 'pay-list',
          hidden: false,
          component: '/contract/payment-manage/supplier-manage/pay-list/index',
          meta: { title: '供应商付款-付款台账', icon: 'contract2', noCache: true }
        }
      ]
    }
  ]
}
