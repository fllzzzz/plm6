// 路由：运营分析
export default {
  id: 1011,
  name: '运营分析',
  children: [
    {
      path: '/operation',
      component: 'Layout',
      hidden: false,
      name: 'Operation',
      alwaysShow: false,
      redirect: '/operation/order-analysis',
      meta: { title: '首页', icon: 'config-2', noCache: true },
      children: [
        {
          path: '/operation/capacity-load-rate',
          component: '/operation/capacity-load-rate/index',
          hidden: false,
          name: 'OperationCapacityLoadRate',
          alwaysShow: false,
          meta: { title: '产能负荷率', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/order-delivery-rate',
          component: '/operation/order-delivery-rate/index',
          hidden: false,
          name: 'OperationOrderDeliveryRate',
          alwaysShow: false,
          meta: { title: '订单交付率', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/inspection-qualified-rate',
          component: '/operation/inspection-qualified-rate/index',
          hidden: false,
          name: 'OperationOrderInspectionQualifiedRate',
          alwaysShow: false,
          meta: { title: '检验合格率', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/order-analysis',
          component: '/operation/order-analysis/index',
          hidden: false,
          name: 'OperationOrderAnalysis',
          alwaysShow: false,
          meta: { title: '订单分析', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/income-expenditure-analysis',
          component: '/operation/income-expenditure-analysis/index',
          hidden: false,
          name: 'OperationIncomeExpenditureAnalysis',
          alwaysShow: false,
          meta: { title: '收支分析', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/yield-analysis',
          component: '/operation/yield-analysis/index',
          hidden: false,
          name: 'OperationYieldAnalysis',
          alwaysShow: false,
          meta: { title: '产量分析', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/difference-analysis',
          component: '/operation/difference-analysis/index',
          hidden: false,
          name: 'OperationDifferenceAnalysis',
          alwaysShow: false,
          meta: { title: '差异分析', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/aux-material-consumption',
          component: '/operation/aux-material-consumption/index',
          hidden: false,
          name: 'OperationAuxMaterialConsumption',
          alwaysShow: false,
          meta: { title: '辅材消耗', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/purchasing-index',
          component: '/operation/purchasing-index/index',
          hidden: false,
          name: 'OperationPurchasingIndex',
          alwaysShow: false,
          meta: { title: '采购指数', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/purchase-sell-stock-analysis',
          component: '/operation/purchase-sell-stock-analysis/index',
          hidden: false,
          name: 'OperationPurchaseSellStockAnalysis',
          alwaysShow: false,
          meta: { title: '进销存分析', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/receivables',
          component: '/operation/receivables/index',
          hidden: false,
          name: 'OperationReceivables',
          alwaysShow: false,
          meta: { title: '应收款', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/QHSE',
          component: '/operation/QHSE/index',
          hidden: false,
          name: 'OperationQHSE',
          alwaysShow: false,
          meta: { title: 'QHSE事件', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/project-cost',
          component: '/operation/project-cost/index',
          hidden: false,
          name: 'OperationProjectCost',
          alwaysShow: false,
          meta: { title: '项目直接成本', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/production-type-analysis',
          component: '/operation/production-type-analysis/index',
          hidden: false,
          name: 'OperationProductionTypeAnalysis',
          alwaysShow: false,
          meta: { title: '构件类型', icon: 'config-2', noCache: true }
        }
      ]
    }
  ]
}
