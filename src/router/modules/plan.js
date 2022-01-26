// 路由：计划管理
export default {
  id: 6,
  name: '计划管理',
  children: [
    // {
    //   path: '/plan-project',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'Plan',
    //   alwaysShow: false,
    //   redirect: '/plan-project/projects',
    //   meta: { title: '项目列表', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'PlanProject',
    //       path: 'projects',
    //       hidden: false,
    //       component: '/plan/projects/index',
    //       meta: { title: '我的项目', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    {
      path: '/plan/overall-plan',
      component: 'Layout',
      hidden: false,
      name: 'OverallPlanInfo',
      alwaysShow: false,
      redirect: '/plan/overall-plan/monomer',
      meta: { title: '计划管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PlanMonomerManage',
          path: 'monomer',
          hidden: false,
          component: '/plan/overall-plan/monomer/index',
          meta: { title: '项目单体', icon: 'project', noCache: true }
        },
        {
          name: 'PlanAreaManage',
          path: 'area',
          hidden: false,
          component: '/plan/overall-plan/area/index',
          meta: { title: '区域列表', icon: 'project', noCache: true }
        },
        {
          name: 'PlanMakeManage',
          path: 'plan-make',
          hidden: false,
          component: '/plan/overall-plan/plan-make/index',
          meta: { title: '工作计划', icon: 'project', noCache: true }
        },
        {
          name: 'PlanSummary',
          path: 'plan-summary',
          hidden: false,
          component: '/plan/overall-plan/plan-summary/index',
          meta: { title: '工单汇总', icon: 'project', noCache: true }
        },
        {
          name: 'PlanProgress',
          path: 'plan-progress',
          hidden: false,
          component: '/plan/overall-plan/plan-progress/index',
          meta: { title: '计划跟踪', icon: 'project', noCache: true }
        },
        {
          name: 'PlanConfirm',
          path: 'plan-confirm',
          hidden: false,
          component: '/plan/overall-plan/plan-confirm/index',
          meta: { title: '工作确认', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/plan/technical-manage/',
      component: 'Layout',
      hidden: false,
      name: 'PlanTechnicalManage',
      alwaysShow: false,
      redirect: '/plan/technical-manage/artifact',
      meta: { title: '技术管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PlanArtifactTreeList',
          path: 'artifact-tree',
          hidden: false,
          component: '/plan/technical-manage/artifact-tree/index',
          meta: { title: '构件-零构件清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanArtifactList',
          path: 'artifact',
          hidden: false,
          component: '/plan/technical-manage/artifact/index',
          meta: { title: '构件-构件清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanMachinePartList',
          path: 'machine-part',
          hidden: false,
          component: '/plan/technical-manage/machine-part/index',
          meta: { title: '构件-零件清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanAssemblyList',
          path: 'assembly',
          hidden: false,
          component: '/plan/technical-manage/assembly/index',
          meta: { title: '构件-组立清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanEnclosureList',
          path: 'enclosureList',
          hidden: false,
          component: '/plan/technical-manage/enclosure-list/index',
          meta: { title: '围护清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanDeepen',
          path: 'deepen',
          hidden: false,
          component: '/plan/technical-data-manage/deepen/index',
          meta: { title: '技术资料-深化图纸', icon: 'project', noCache: true }
        },
        {
          name: 'PlanBlueprint',
          path: 'blueprint',
          hidden: false,
          component: '/plan/technical-data-manage/blueprint/index',
          meta: { title: '技术资料-蓝图', icon: 'project', noCache: true }
        },
        {
          name: 'PlanChangeFile',
          path: 'change-file',
          hidden: false,
          component: '/plan/technical-data-manage/change-file/index',
          meta: { title: '技术资料-变更文件', icon: 'project', noCache: true }
        },
        {
          name: 'PlanModel',
          path: 'model',
          hidden: false,
          component: '/plan/technical-data-manage/model/index',
          meta: { title: '技术资料-模型', icon: 'project', noCache: true }
        },
        {
          name: 'PlanOtherFile',
          path: 'other-file',
          hidden: false,
          component: '/plan/technical-data-manage/other-file/index',
          meta: { title: '技术资料-其他文件', icon: 'project', noCache: true }
        },
        {
          name: 'SummaryList',
          path: 'summary-list',
          hidden: false,
          component: '/plan/technical-manage/summary-list/index',
          meta: { title: '清单合计', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/plan/dosage-statistical',
      component: 'Layout',
      hidden: false,
      name: 'DosageStatistical',
      alwaysShow: false,
      redirect: '/plan/dosage-statistical/steel-statistical',
      meta: { title: '标准用量统计', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SteelStatistical',
          path: 'steel-statistical',
          hidden: false,
          component: '/plan/technical-manage/steel-statistical/index',
          meta: { title: '钢材使用用量对比', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
