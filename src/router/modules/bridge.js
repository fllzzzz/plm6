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
          meta: { title: '项目单体', icon: 'project', noCache: true, ownProductType: 2 }
        },
        {
          name: 'BridgeAreaManage',
          path: 'bridge-area',
          hidden: false,
          component: '/plan/overall-plan/area/index',
          meta: { title: '区域列表', icon: 'project', noCache: true, ownProductType: 2 }
        },
        {
          name: 'BridgeMakeManage',
          path: 'bridge-make',
          hidden: false,
          component: '/plan/overall-plan/plan-make/index',
          meta: { title: '工作计划', icon: 'project', noCache: true, ownProductType: 2 }
        },
        {
          name: 'BridgePlanSummary',
          path: 'bridge-plan-summary',
          hidden: false,
          component: '/plan/overall-plan/plan-summary/index',
          meta: { title: '工单汇总', icon: 'project', noCache: true, ownProductType: 2 }
        },
        {
          name: 'BridgeProgress',
          path: 'bridge-progress',
          hidden: false,
          component: '/plan/overall-plan/plan-progress/index',
          meta: { title: '计划跟踪', icon: 'project', noCache: true, ownProductType: 2 }
        },
        {
          name: 'BridgeConfirm',
          path: 'bridge-confirm',
          hidden: false,
          component: '/plan/overall-plan/plan-confirm/index',
          meta: { title: '工作确认', icon: 'project', noCache: true, ownProductType: 2 }
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
        }
      ]
    }
  ]
}
