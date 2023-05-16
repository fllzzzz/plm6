// 路由：围护
export default {
  id: 11,
  name: '围护MES',
  children: [
    {
      path: 'enclosure-manage',
      component: 'Layout',
      hidden: false,
      name: 'enclosureManage',
      alwaysShow: false,
      redirect: '/enclosure-manage/enclosure-plan',
      meta: { title: '计划管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'enclosurePlan',
          path: 'enclosure-plan',
          hidden: false,
          component: '/enclosure/enclosure-plan/area/index',
          meta: { title: '计划列表', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: 'enclosure-list',
      component: 'Layout',
      hidden: false,
      name: 'enclosureList',
      alwaysShow: false,
      redirect: '/enclosure-list/enclosure-sandwich-board',
      meta: { title: '清单管理', icon: 'user', noCache: true },
      children: [
        {
          name: 'enclosureSandwichBoard',
          path: 'enclosure-sandwich-board',
          hidden: false,
          component: '/enclosure/enclosure-list/sandwich-board/index',
          meta: { title: '夹芯板', icon: 'project', noCache: true }
        },
        {
          name: 'enclosureProfiledPlate',
          path: 'enclosure-profiled-plate',
          hidden: false,
          component: '/enclosure/enclosure-list/profiled-plate/index',
          meta: { title: '压型彩板', icon: 'project', noCache: true }
        },
        {
          name: 'enclosurePressureBearingPlate',
          path: 'enclosure-pressure-bearing-plate',
          hidden: false,
          component: '/enclosure/enclosure-list/pressure-bearing-plate/index',
          meta: { title: '压型楼承板', icon: 'project', noCache: true }
        },
        {
          name: 'enclosureTrussFloorPlate',
          path: 'enclosure-truss-floor-plate',
          hidden: false,
          component: '/enclosure/enclosure-list/truss-floor-plate/index',
          meta: { title: '桁架楼承板', icon: 'project', noCache: true }
        },
        {
          name: 'enclosureBending',
          path: 'enclosure-bending',
          hidden: false,
          component: '/enclosure/enclosure-list/bending/index',
          meta: { title: '折边件', icon: 'project', noCache: true }
        },
        {
          name: 'enclosureStandardPart',
          path: 'enclosure-standard-part',
          hidden: false,
          component: '/enclosure/enclosure-list/standard-part/index',
          meta: { title: '配套件', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: 'enclosure/production-manage',
      component: 'Layout',
      hidden: false,
      name: 'EnclosureManage',
      alwaysShow: false,
      redirect: '/enclosure/production-manage/scheduling-manage',
      meta: { title: '生产管理', icon: 'project', noCache: true },
      children: [
        {
          name: 'EnclosureSchedulingManage',
          path: 'scheduling-manage',
          hidden: false,
          component: '/enclosure/production-manage/scheduling-manage/index',
          meta: { title: '排产管理', icon: 'project', noCache: true }
        },
        {
          name: 'EnclosureSchedulingWorkOrder',
          path: 'scheduling-work-order',
          hidden: false,
          component: '/enclosure/production-manage/scheduling-work-order/index',
          meta: { title: '排产工单', icon: 'project', noCache: true }
        },
        {
          name: 'EnclosureProductionTracking',
          path: 'production-tracking',
          hidden: false,
          meta: { title: '生产跟踪', icon: 'project', noCache: true },
          children: [
            {
              name: 'EnclosureTaskTracking',
              path: 'task-tracking',
              hidden: false,
              component: '/enclosure/production-manage/production-tracking/task-tracking/index',
              meta: { title: '任务跟踪', icon: 'project', noCache: true }
            },
            {
              name: 'EnclosureProjectOverview',
              path: 'project-overview',
              hidden: false,
              component: '/enclosure/production-manage/production-tracking/project-overview/index',
              meta: { title: '项目全貌', icon: 'project', noCache: true }
            }
          ]
        }
      ]
    },
    {
      path: 'enclosure/production-report',
      component: 'Layout',
      hidden: false,
      name: 'EnclosureProductionReport',
      alwaysShow: false,
      redirect: '/enclosure/production-report/production-statistics',
      meta: { title: '生产报表', icon: 'project', noCache: true },
      children: [
        {
          name: 'EnclosureProductionStatistics',
          path: 'production-statistics',
          hidden: false,
          component: '/enclosure/production-report/production-statistics/index',
          meta: { title: '生产统计', icon: 'project', noCache: true }
        },
        {
          name: 'EnclosureTeamProduction',
          path: 'team-production',
          hidden: false,
          component: '/enclosure/production-report/team-production/index',
          meta: { title: '班组产量', icon: 'project', noCache: true }
        },
        {
          name: 'EnclosureTypeAnalysis',
          path: 'type-analysis',
          hidden: false,
          component: '/enclosure/production-report/type-analysis/index',
          meta: { title: '类型分析', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/enclosure/label-print',
      component: 'Layout',
      hidden: false,
      name: 'EnclosureLabelPrint',
      alwaysShow: false,
      redirect: '/enclosure/label-print/artifact',
      meta: {
        title: '产品标签',
        icon: 'project',
        noCache: true
      },
      children: [
        // {
        //   name: 'EnclosureLabelPrintArtifact',
        //   path: 'artifact',
        //   hidden: false,
        //   component: '/enclosure/label-print/artifact/index',
        //   meta: {
        //     title: '构件',
        //     icon: 'project',
        //     noCache: true
        //   }
        // },
        // {
        //   name: 'EnclosureLabelPrintPart',
        //   path: 'part',
        //   hidden: false,
        //   component: '/mes/label-print/part/index',
        //   meta: {
        //     title: '直发件',
        //     icon: 'project',
        //     noCache: true
        //   }
        // },
        {
          name: 'EnclosureLabelPrintEnclosure',
          path: 'enclosure',
          hidden: false,
          component: '/enclosure/label-print/enclosure/index',
          meta: {
            title: '围护-产品标签',
            icon: 'project',
            noCache: true
          }
        }
        // {
        //   name: 'EnclosureLabelPrintAuxiliaryMaterial',
        //   path: 'auxiliary-material',
        //   hidden: false,
        //   component: '/enclosure/label-print/auxiliary-material/index',
        //   meta: {
        //     title: '配套件',
        //     icon: 'project',
        //     noCache: true
        //   }
        // }
        // {
        //   name: 'EnclosureLabelPrintFoldingPiece',
        //   path: 'enclosure',
        //   hidden: false,
        //   component: '/enclosure/label-print/folding-piece/index',
        //   meta: {
        //     title: '折边件',
        //     icon: 'project',
        //     noCache: true
        //   }
        // }
        //     {
        //       name: 'EnclosureLabelPrintingAuxiliaryMaterial',
        //       path: 'auxiliary-material',
        //       hidden: false,
        //       component: '/enclosure/label-printing/auxiliary-material/index',
        //       meta: { title: '辅材', icon: 'project', noCache: true }
        //     }
      ]
    }
  ]
}
