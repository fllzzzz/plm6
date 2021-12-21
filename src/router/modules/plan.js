// 路由：计划管理
export default {
  id: 1377,
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
          name: 'PlanOverallPlanManage',
          path: 'plan-make',
          hidden: false,
          component: '/plan/overall-plan/plan-make/index',
          meta: { title: '工作计划', icon: 'project', noCache: true }
        },
        {
          name: 'PlanOverallPlanProgress',
          path: 'plan-progress',
          hidden: false,
          component: '/plan/overall-plan/plan-progress/index',
          meta: { title: '计划跟踪', icon: 'project', noCache: true }
        },
        {
          name: 'PlanOverallPlanDetail',
          path: 'plan-detail',
          hidden: true,
          component: '/plan/overall-plan/plan-detail/index',
          meta: { title: '计划确认', icon: 'project', noCache: true }
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
          name: 'PlanTrussSupportList',
          path: 'trussSupport',
          hidden: false,
          component: '/plan/technical-manage/enclosure/trussSupport/index',
          meta: { title: '围护-桁架楼层板清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanSandwichList',
          path: 'sandwich',
          hidden: false,
          component: '/plan/technical-manage/enclosure/sandwich/index',
          meta: { title: '围护-夹芯板清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanPressedSupportList',
          path: 'pressedSupport',
          hidden: false,
          component: '/plan/technical-manage/enclosure/pressedSupport/index',
          meta: { title: '围护-压型楼承板清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanPressedColorList',
          path: 'pressedColor',
          hidden: false,
          component: '/plan/technical-manage/enclosure/pressedColor/index',
          meta: { title: '围护-压型板清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanBendingList',
          path: 'bending',
          hidden: false,
          component: '/plan/technical-manage/enclosure/bending/index',
          meta: { title: '围护-折边件清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanTechnicalDataManageDeepen',
          path: 'deepen',
          hidden: false,
          component: '/plan/technical-data-manage/deepen/index',
          meta: { title: '技术资料-深化图纸', icon: 'project', noCache: true }
        },
        {
          name: 'PlanTechnicalDataManageBlueprint',
          path: 'blueprint',
          hidden: false,
          component: '/plan/technical-data-manage/blueprint/index',
          meta: { title: '技术资料-蓝图', icon: 'project', noCache: true }
        },
        {
          name: 'PlanTechnicalDataManageChangeFile',
          path: 'change-file',
          hidden: false,
          component: '/plan/technical-data-manage/change-file/index',
          meta: { title: '技术资料-变更文件', icon: 'project', noCache: true }
        },
        {
          name: 'PlanTechnicalDataManageModel',
          path: 'model',
          hidden: false,
          component: '/plan/technical-data-manage/model/index',
          meta: { title: '技术资料-模型', icon: 'project', noCache: true }
        },
        {
          name: 'PlanTechnicalDataManageOtherFile',
          path: 'other-file',
          hidden: false,
          component: '/plan/technical-data-manage/other-file/index',
          meta: { title: '技术资料-其他文件', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}