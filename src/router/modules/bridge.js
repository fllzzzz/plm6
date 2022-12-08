// 路由：计划管理
export default {
  id: 1117,
  name: '桥梁Mes',
  children: [
    {
      path: '/bridge/overall-plan',
      component: 'Layout',
      hidden: false,
      name: 'BridgePlanInfo',
      alwaysShow: false,
      redirect: '/bridge/overall-plan/bridge-monomer',
      meta: { title: '计划管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'BridgeMonomerManage',
          path: 'bridge-monomer',
          hidden: false,
          component: '/plan/overall-plan/monomer/index',
          meta: { title: '项目单体', icon: 'project', noCache: true }
        },
        {
          name: 'BridgeAreaManage',
          path: 'bridge-area',
          hidden: false,
          component: '/plan/overall-plan/area/index',
          meta: { title: '区域列表', icon: 'project', noCache: true }
        },
        {
          name: 'BridgeMakeManage',
          path: 'bridge-make',
          hidden: false,
          component: '/plan/overall-plan/plan-make/index',
          meta: { title: '工作计划', icon: 'project', noCache: true }
        },
        {
          name: 'BridgePlanSummary',
          path: 'bridge-plan-summary',
          hidden: false,
          component: '/plan/overall-plan/plan-summary/index',
          meta: { title: '工单汇总', icon: 'project', noCache: true }
        },
        {
          name: 'BridgeProgress',
          path: 'bridge-progress',
          hidden: false,
          component: '/plan/overall-plan/plan-progress/index',
          meta: { title: '计划跟踪', icon: 'project', noCache: true }
        },
        {
          name: 'BridgeConfirm',
          path: 'bridge-confirm',
          hidden: false,
          component: '/plan/overall-plan/plan-confirm/index',
          meta: { title: '工作确认', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/bridge/technical-manage',
      component: 'Layout',
      hidden: false,
      name: 'BridgeTechnicalManage',
      alwaysShow: false,
      redirect: '/bridge/technical-manage/box-list',
      meta: { title: '技术管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PlanBridgeBoxList',
          path: 'box-list',
          hidden: false,
          component: '/bridge/bridge-plan/box-list/index',
          meta: { title: '分段清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanBridgeCellPartList',
          path: 'cell-part-list',
          hidden: false,
          component: '/bridge/bridge-plan/cell-part-list/index',
          meta: { title: '单元清单', icon: 'project', noCache: true }
        },
        {
          path: 'bridge-summary',
          component: '',
          hidden: false,
          name: 'BridgeSummary',
          alwaysShow: false,
          redirect: '/bridge/technical-manage/bridge-summary/cell-summary',
          meta: { title: '清单汇总', icon: 'contract', noCache: true },
          children: [
            {
              name: 'BoxSummary',
              path: 'box-summary',
              hidden: false,
              component: '/bridge/bridge-plan/list-summary/box-summary/index',
              meta: { title: '分段清单汇总', icon: 'project', noCache: true }
            },
            {
              name: 'CellSummary',
              path: 'cell-summary',
              hidden: false,
              component: '/bridge/bridge-plan/list-summary/cell-summary/index',
              meta: { title: '单元清单汇总', icon: 'project', noCache: true }
            },
            {
              name: 'PartSummary',
              path: 'part-summary',
              hidden: false,
              component: '/bridge/bridge-plan/list-summary/part-summary/index',
              meta: { title: '零件清单汇总', icon: 'project', noCache: true }
            }
          ]
        },
        {
          path: 'bridge-technical-achievement',
          component: '',
          hidden: false,
          name: 'BridgeTechnicalAchievement',
          alwaysShow: false,
          redirect: '/bridge/technical-manage/bridge-technical-achievement/bridge-model',
          meta: { title: '技术成果', icon: 'contract', noCache: true },
          children: [
            {
              name: 'BridgeModelFile',
              path: 'bridge-model',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/model/index',
              meta: { title: '模型文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'BridgeDrawingFile',
              path: 'bridge-drawing',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/drawing/index',
              meta: { title: '图纸文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'BridgeCNCFile',
              path: 'bridge-cnc',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/cnc/index',
              meta: { title: '数控文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'BridgePlanChangeFile',
              path: 'bridge-change-file',
              hidden: false,
              component: '/plan/technical-data-manage/change-file/index',
              meta: { title: '变更文件', icon: 'project', noCache: true }
            },
            {
              name: 'BridgePlanBlueprint',
              path: 'bridge-blueprint',
              hidden: false,
              component: '/plan/technical-data-manage/blueprint/index',
              meta: { title: '施工蓝图', icon: 'project', noCache: true }
            }
          ]
        }
      ]
    },
    {
      path: '/bridge/production-order-manage',
      component: 'Layout',
      hidden: false,
      name: 'BridgeProductionOrderManage',
      alwaysShow: false,
      redirect: '/bridge/production-order-manage/production-order',
      meta: {
        title: '生产排期管理',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeProductionOrder',
          path: 'production-order',
          hidden: false,
          component: '/bridge/production-order/index',
          meta: {
            title: '生产排期',
            icon: 'project',
            noCache: true
          }
        }
      ]
    },
    {
      path: '/bridge/scheduling-manage',
      component: 'Layout',
      hidden: false,
      name: 'BridgeSchedulingManage',
      alwaysShow: true,
      redirect: '/bridge/scheduling-manage/scheduling/box',
      meta: {
        title: '生产排产',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeSchedulingBox',
          path: 'scheduling/box',
          hidden: false,
          component: '/bridge/scheduling-manage/box/index',
          meta: {
            title: '分段排产',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeSchedulingMachinePart',
          path: 'scheduling/machine-part',
          component: '',
          hidden: false,
          alwaysShow: true,
          redirect: '/bridge/scheduling-manage/scheduling/machine-part/index',
          meta: {
            title: '零件排产',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'BridgeSchedulingMachinePartIndex',
              path: 'index',
              hidden: false,
              component: '/bridge/scheduling-manage/machine-part/index',
              meta: {
                title: '零件排产',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'BridgeSchedulingMachinePartRecord',
              path: 'record',
              hidden: false,
              component: '/bridge/scheduling-manage/machine-part/record/index',
              meta: {
                title: '预览记录',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'BridgeSchedulingMachinePartNestingResult',
              path: 'nesting-result',
              hidden: false,
              component: '/bridge/scheduling-manage/machine-part/nesting-result/index',
              meta: {
                title: '套料成果',
                icon: 'project',
                noCache: true
              }
            }
          ]
        }
      ]
    },
    {
      path: '/bridge/task-tracking',
      component: 'Layout',
      hidden: false,
      name: 'BridgeTaskTracking',
      alwaysShow: false,
      redirect: '/bridge/task-tracking',
      meta: {
        title: '任务跟踪',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeWorkOrderTracking',
          path: 'work-order-tracking',
          hidden: false,
          component: '/bridge/task-tracking/work-order-tracking/index',
          meta: {
            title: '工单跟踪',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeMonthlyTaskTracking',
          path: 'monthly-task-tracking',
          hidden: false,
          component: '/bridge/task-tracking/monthly-task-tracking/index',
          meta: {
            title: '月度任务跟踪',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeProductionLineTracking',
          path: 'production-line-tracking',
          hidden: false,
          component: '/bridge/task-tracking/production-line-tracking/index',
          meta: {
            title: '产线跟踪',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeProcessSluggish',
          path: 'process-sluggish',
          hidden: false,
          component: '/bridge/task-tracking/process-sluggish/index',
          meta: {
            title: '工序呆滞',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeAssistanceOperate',
          path: 'assistance-operate',
          hidden: false,
          component: '',
          meta: {
            title: '协同操作',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'BridgeAssistanceProductionLine',
              path: 'productionLine-assistance',
              hidden: false,
              component: '/bridge/task-tracking/assistance-operate/productionLine-assistance/index',
              meta: {
                title: '产线协同',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'BridgeAssistanceProcess',
              path: 'process-assistance',
              hidden: false,
              component: '/bridge/task-tracking/assistance-operate/process-assistance/index',
              meta: {
                title: '工序协同',
                icon: 'project',
                noCache: true
              }
            }
          ]
        }
      ]
    },
    {
      name: 'BridgeProductionDashboard',
      path: '/bridge/dashboard',
      hidden: false,
      redirect: '/bridge/production-manage/dashboard/project-state',
      meta: {
        title: '项目制造',
        icon: 'project',
        noCache: true
      },
      children: [{
        name: 'BridgeProductionDashboardProjectOverview',
        path: 'project-overview',
        hidden: false,
        component: '/bridge/production-manage/dashboard/project-overview/index',
        meta: {
          title: '项目总览',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'BridgeProductionDashboardProductionDashboard',
        path: 'production-dashboard',
        hidden: false,
        component: '/bridge/production-manage/dashboard/production-dashboard/index',
        meta: {
          title: '生产看板',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'BridgeProductionDashboardAssemblyMatch',
        path: 'assembly-match',
        hidden: false,
        component: '/bridge/production-manage/dashboard/assembly-match/index',
        meta: {
          title: '项目齐套',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'BridgeProductionDashboardPainting',
        path: 'painting',
        hidden: false,
        component: '/bridge/production-manage/dashboard/painting/index',
        meta: {
          title: '涂装计算',
          icon: 'project',
          noCache: true
        }
      }
      ]
    },
    {
      path: '/bridge/QHSE-manage',
      component: 'Layout',
      hidden: false,
      name: 'BridgeQHSEManage',
      alwaysShow: false,
      redirect: '/bridge/QHSE-manage/quality-inspection-report',
      meta: {
        title: '质安管理',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeQHSEManageQualityInspectionReport',
          path: 'quality-inspection-report',
          hidden: false,
          component: '/bridge/QHSE-manage/quality-inspection-report/index',
          meta: {
            title: '质检报表',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeQHSEManageProductionLineReport',
          path: 'production-line-report',
          hidden: false,
          component: '/bridge/QHSE-manage/production-line-report/index',
          meta: {
            title: '产线报表',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeQHSEManageDisclosure',
          path: 'disclosure',
          hidden: false,
          component: '/bridge/QHSE-manage/disclosure/index',
          meta: {
            title: '问题曝光',
            icon: 'project',
            noCache: true
          }
        }
      ]
    },
    {
      path: '/bridge/label-print',
      component: 'Layout',
      hidden: false,
      name: 'BridgeLabelPrint',
      alwaysShow: false,
      redirect: '/bridge/label-print/box',
      meta: {
        title: '产品标签',
        icon: 'project',
        noCache: true
      },
      children: [{
        name: 'BridgeLabelPrintBox',
        path: 'box',
        hidden: false,
        component: '/bridge/label-print/box/index',
        meta: {
          title: '分段',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'BridgeLabelPrintSingleElement',
        path: 'single-element',
        hidden: false,
        component: '/bridge/label-print/single-element/index',
        meta: {
          title: '单元件',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'BridgeLabelPrintAuxiliaryMaterial',
        path: 'auxiliary-material',
        hidden: false,
        component: '/bridge/label-print/auxiliary-material/index',
        meta: {
          title: '配套件',
          icon: 'project',
          noCache: true
        }
      }
      ]
    },
    {
      path: '/bridge/pack-and-ship',
      component: 'Layout',
      hidden: false,
      name: 'BridgePackAndShip',
      alwaysShow: false,
      redirect: '/bridge/pack-and-ship/ship-summary',
      meta: {
        title: '发运管理',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeShipSummary',
          path: 'ship-summary',
          hidden: false,
          component: '/bridge/pack-and-ship/ship-summary/index',
          meta: {
            title: '发运管理',
            icon: 'project',
            noCache: true
          }
        },
        {
          path: 'pack-manage',
          component: '',
          hidden: false,
          name: 'BridgePackManage',
          alwaysShow: false,
          redirect: '/bridge/pack-and-ship/pack-manage/manual-pack',
          meta: {
            title: '打包管理',
            icon: 'project',
            noCache: true
          },
          children: [
            {
              name: 'BridgeManualPack',
              path: 'manual-pack',
              hidden: false,
              component: '/bridge/pack-and-ship/manual-pack/index',
              meta: {
                title: '打包操作',
                icon: 'project',
                noCache: true
              }
            },
            {
              name: 'BridgePackList',
              path: 'pack-list',
              hidden: false,
              component: '/bridge/pack-and-ship/pack-list/index',
              meta: {
                title: '打包记录',
                icon: 'project',
                noCache: true
              }
            }
          ]
        },
        {
          name: 'BridgeShipList',
          path: 'ship-list',
          hidden: false,
          component: '/bridge/pack-and-ship/ship-list/index',
          meta: {
            title: '发运记录',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeReceiptStatus',
          path: 'receipt-status',
          hidden: false,
          component: '/bridge/pack-and-ship/receipt-status/index',
          meta: {
            title: '收货状态',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeLogisticsList',
          path: 'logistics-list',
          hidden: false,
          component: '/bridge/pack-and-ship/logistics-list/index',
          meta: {
            title: '物流记录',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeShipAudit',
          path: 'ship-audit',
          hidden: false,
          component: '/bridge/pack-and-ship/ship-audit/index',
          meta: {
            title: '发运审核',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'BridgeProductSendReceiveStorage',
          path: 'product-send-receive-storage',
          hidden: false,
          component: '/bridge/pack-and-ship/product-send-receive-storage/index',
          meta: {
            title: '制品入发存',
            icon: 'project',
            noCache: true
          }
        }
      ]
    },
    {
      path: '/bridge/factory-report',
      component: 'Layout',
      hidden: false,
      name: 'BridgeFactoryReport',
      alwaysShow: false,
      redirect: '/bridge/factory-report',
      meta: {
        title: '工厂报表',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'BridgeWorkshopReport',
          path: 'workshop-report',
          hidden: false,
          component: '/bridge/factory-report/workshop-report/index',
          meta: {
            title: '车间报表',
            icon: 'project',
            noCache: true
          }
        }
      ]
    }
  ]
}
