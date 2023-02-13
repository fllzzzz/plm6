// 路由：运营分析
export default {
  id: 10,
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
          path: '/operation/production-cost-analysis',
          component: '/operation/production-cost-analysis/index',
          hidden: false,
          name: 'OperationProductionCostAnalysis',
          alwaysShow: false,
          meta: { title: '生产成本分析', icon: 'config-2', noCache: true }
        },
        // {
        //   path: '/operation/unloading-manual-analysis',
        //   component: '/operation/unloading-manual-analysis/index',
        //   hidden: false,
        //   name: 'OperationUnloadingManualAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '下料人工分析', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/artifact-analysis',
        //   component: '/operation/artifact-analysis/index',
        //   hidden: false,
        //   name: 'OperationArtifactAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '构件分析', icon: 'config-2', noCache: true }
        // },
        {
          path: '/operation/painting-fee',
          component: '/operation/painting-fee/index',
          hidden: false,
          name: 'OperationPaintingFee',
          alwaysShow: false,
          meta: { title: '涂装费', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/auxiliary-fee',
          component: '/operation/auxiliary-fee/index',
          hidden: false,
          name: 'OperationAuxiliaryFee',
          alwaysShow: false,
          meta: { title: '辅材费', icon: 'config-2', noCache: true }
        },
        // {
        //   path: '/operation/equipment-maintenance-cost',
        //   component: '/operation/equipment-maintenance-cost/index',
        //   hidden: false,
        //   name: 'OperationEquipmentMaintenanceCost',
        //   alwaysShow: false,
        //   meta: { title: '设备维修费', icon: 'config-2', noCache: true }
        // },
        {
          path: '/operation/testing-fee',
          component: '/operation/testing-fee/index',
          hidden: false,
          name: 'OperationTestingFee',
          alwaysShow: false,
          meta: { title: '检测费', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/water-electricity-fee',
          component: '/operation/water-electricity-fee/index',
          hidden: false,
          name: 'OperationWaterElectricityFee',
          alwaysShow: false,
          meta: { title: '水电费', icon: 'config-2', noCache: true }
        },
        {
          path: '/operation/management-fee',
          component: '/operation/management-fee/index',
          hidden: false,
          name: 'OperationManagementFee',
          alwaysShow: false,
          meta: { title: '管理费', icon: 'config-2', noCache: true }
        },
        // {
        //   path: '/operation/auxiliary-material-consumption-analysis',
        //   component: '/operation/auxiliary-material-consumption-analysis/index',
        //   hidden: false,
        //   name: 'OperationAuxiliaryMaterialConsumptionAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '辅材消耗分析', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/capacity-load-rate',
        //   component: '/operation/capacity-load-rate/index',
        //   hidden: false,
        //   name: 'OperationCapacityLoadRate',
        //   alwaysShow: false,
        //   meta: { title: '产能负荷率', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/order-delivery-rate',
        //   component: '/operation/order-delivery-rate/index',
        //   hidden: false,
        //   name: 'OperationOrderDeliveryRate',
        //   alwaysShow: false,
        //   meta: { title: '订单交付率', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/inspection-qualified-rate',
        //   component: '/operation/inspection-qualified-rate/index',
        //   hidden: false,
        //   name: 'OperationOrderInspectionQualifiedRate',
        //   alwaysShow: false,
        //   meta: { title: '检验合格率', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/order-analysis',
        //   component: '/operation/order-analysis/index',
        //   hidden: false,
        //   name: 'OperationOrderAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '订单分析', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/income-expenditure-analysis',
        //   component: '/operation/income-expenditure-analysis/index',
        //   hidden: false,
        //   name: 'OperationIncomeExpenditureAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '收支分析', icon: 'config-2', noCache: true }
        // },
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
        // {
        //   path: '/operation/aux-material-consumption',
        //   component: '/operation/aux-material-consumption/index',
        //   hidden: false,
        //   name: 'OperationAuxMaterialConsumption',
        //   alwaysShow: false,
        //   meta: { title: '辅材消耗', icon: 'config-2', noCache: true }
        // },
        {
          path: '/operation/purchasing-index',
          component: '/operation/purchasing-index/index',
          hidden: false,
          name: 'OperationPurchasingIndex',
          alwaysShow: false,
          meta: { title: '采购指数', icon: 'config-2', noCache: true }
        },
        // {
        //   path: '/operation/purchase-sell-stock-analysis',
        //   component: '/operation/purchase-sell-stock-analysis/index',
        //   hidden: false,
        //   name: 'OperationPurchaseSellStockAnalysis',
        //   alwaysShow: false,
        //   meta: { title: '进销存分析', icon: 'config-2', noCache: true }
        // },
        // {
        //   path: '/operation/receivables',
        //   component: '/operation/receivables/index',
        //   hidden: false,
        //   name: 'OperationReceivables',
        //   alwaysShow: false,
        //   meta: { title: '应收款', icon: 'config-2', noCache: true }
        // },
        {
          path: '/operation/QHSE',
          component: '/operation/QHSE/index',
          hidden: false,
          name: 'OperationQHSE',
          alwaysShow: false,
          meta: { title: 'QHSE事件', icon: 'config-2', noCache: true }
        },
        // {
        //   path: '/operation/project-cost',
        //   component: '/operation/project-cost/index',
        //   hidden: false,
        //   name: 'OperationProjectCost',
        //   alwaysShow: false,
        //   meta: { title: '项目直接成本', icon: 'config-2', noCache: true }
        // },
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
