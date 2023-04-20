// 路由：围护
export default {
  id: 800,
  name: '围护MES',
  children: [
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
    }
  ]
}
