// 路由：供应链
export default {
  id: 7,
  name: '供应链',
  children: [
    {
      path: '/supply-chain/purchase-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainPurchaseOrder',
      alwaysShow: false,
      redirect: '/supply-chain/purchase-order',
      meta: { title: '采购合同管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/supply-chain/purchase-order/index',
          meta: { title: '原材料-采购合同', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/requisitions-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainRequisitions',
      alwaysShow: false,
      redirect: '/supply-chain/requisitions',
      meta: { title: '申购管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'Requisitions',
          path: 'requisitions',
          hidden: false,
          component: '/supply-chain/requisitions/index',
          meta: { title: '材料申购', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/purchase-reconciliation-manage',
      component: 'Layout',
      hidden: false,
      name: 'PurchaseReconciliationManage',
      alwaysShow: false,
      redirect: '/supply-chain/payment-ledger',
      meta: { title: '采购台账', icon: 'contract', noCache: true },
      children: [
        {
          name: 'InboundLog',
          path: 'inbound-log',
          hidden: false,
          component: '/supply-chain/purchase-reconciliation-manage/inbound-log/index',
          meta: { title: '入库记录', icon: 'project', noCache: true }
        },
        {
          name: 'AcceptanceLog',
          path: 'acceptance-log',
          hidden: false,
          component: '/supply-chain/purchase-reconciliation-manage/acceptance-log/index',
          meta: { title: '验收记录', icon: 'project', noCache: true }
        },
        {
          name: 'ReconciliationLog',
          path: 'reconciliation-log',
          hidden: false,
          component: '/supply-chain/purchase-reconciliation-manage/reconciliation-log/index',
          meta: { title: '供应商对账', icon: 'project', noCache: true }
        },
        {
          name: 'PurchasePaymentLedger',
          path: 'payment-ledger',
          hidden: false,
          component: '/supply-chain/purchase-reconciliation-manage/payment-ledger/index',
          meta: { title: '采购台账', icon: 'project', noCache: true }
        },
        {
          name: 'PurchaseOrderLedger',
          path: 'order-ledger',
          hidden: false,
          component: '/supply-chain/purchase-reconciliation-manage/order-ledger/index',
          meta: { title: '订单查询', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/logistics-payment-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainLogisticsPayment',
      alwaysShow: false,
      redirect: '/supply-chain/logistics-payment-manage/logistics-record',
      meta: { title: '物流台账', icon: 'contract', noCache: true },
      children: [
        {
          name: 'logisticsRecord',
          path: 'logistics-record',
          hidden: false,
          component: '/supply-chain/logistics-payment-manage/logistics-record/index',
          meta: { title: '物流记录', icon: 'project', noCache: true }
        },
        {
          name: 'logisticsPayment',
          path: 'logistics-payment',
          hidden: false,
          component: '/supply-chain/logistics-payment-manage/logistics-payment/index',
          meta: { title: '原材料物流', icon: 'project', noCache: true }
        },
        {
          name: 'productLogisticsPayment',
          path: 'product-logistics-payment',
          hidden: false,
          component: '/supply-chain/logistics-payment-manage/product-logistics-payment/index',
          meta: { title: '制成品物流', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/subcontract-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainSubcontract',
      alwaysShow: false,
      redirect: '/supply-chain/subcontract-manage/subcontract-order',
      meta: { title: '分包管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'subcontractOrder',
          path: 'subcontract-order',
          hidden: false,
          component: '/supply-chain/subcontract-order/index',
          meta: { title: '分包订单', icon: 'project', noCache: true }
        },
        {
          name: 'subcontractOrderPayment',
          path: 'subcontract-order-payment',
          hidden: false,
          component: '/supply-chain/subcontract-order-payment/index',
          meta: { title: '付款台账', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/logistics-manage',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainLogisticsOrder',
      alwaysShow: false,
      redirect: '/supply-chain/logistics-order',
      meta: { title: '物流订单管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'LogisticsOrder',
          path: 'purchase-order',
          hidden: false,
          component: '/supply-chain/logistics-order/index',
          meta: { title: '物流订单', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/supply-chain/supplier',
      component: 'Layout',
      hidden: false,
      name: 'SupplierManage',
      alwaysShow: false,
      redirect: '/supply-chain/supplier/manage',
      meta: { title: '供应商管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PurchaseSupplier',
          path: 'manage',
          hidden: false,
          component: '/supply-chain/supplier/manage/index',
          meta: { title: '供应商列表', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
