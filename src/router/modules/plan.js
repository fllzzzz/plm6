// 路由：计划管理
export default {
  id: 6,
  name: '计划管理',
  children: [
    {
      path: '/plan-project',
      component: 'Layout',
      hidden: false,
      name: 'Plan',
      alwaysShow: false,
      redirect: '/plan-project/projects',
      meta: { title: '项目列表', icon: 'contract', noCache: true },
      children: [
        {
          name: 'PlanProject',
          path: 'plan-projects',
          hidden: false,
          component: '/plan/project-list/index',
          meta: { title: '我的项目', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/plan-kpi',
      component: 'Layout',
      hidden: false,
      name: 'PlanKpi',
      alwaysShow: false,
      redirect: '/plan-kpi/technical-kpi',
      meta: {
        title: 'KPI',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'MesTechnicalKpi',
          path: 'technical-kpi',
          hidden: false,
          component: '/plan/technical-kpi/index',
          meta: {
            title: '技术KPI',
            icon: 'project',
            noCache: true
          }
        }
      ]
    },
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
      redirect: '/plan/technical-manage/artifact-tree',
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
          meta: { title: '构件-部件清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanEnclosureList',
          path: 'enclosureList',
          hidden: false,
          component: '/plan/technical-manage/enclosure-list/index',
          meta: { title: '围护清单', icon: 'project', noCache: true }
        },
        {
          name: 'PlanStandardPart',
          path: 'planStandardPart',
          hidden: false,
          component: '/plan/technical-manage/standard-part/index',
          meta: { title: '项目配套', icon: 'project', noCache: true }
        },
        // {
        //   name: 'PlanAuxiliaryMaterialList',
        //   path: 'planAuxiliaryMaterialList',
        //   hidden: false,
        //   component: '/plan/technical-manage/auxiliary-material/index',
        //   meta: { title: '配套件清单', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'PlanAuxiliaryMaterialSummary',
        //   path: 'auxiliary-material-summary',
        //   hidden: false,
        //   component: '/plan/technical-manage/auxiliary-material-summary/index',
        //   meta: { title: '配套件汇总', icon: 'project', noCache: true }
        // },
        {
          name: 'SummaryList',
          path: 'summary-list',
          hidden: false,
          component: '/plan/technical-manage/summary-list/index',
          meta: { title: '清单合计', icon: 'project', noCache: true }
        },
        {
          path: 'technical-achievement',
          component: '',
          hidden: false,
          name: 'TechnicalAchievement',
          alwaysShow: false,
          redirect: '/plan/technical-manage/technical-achievement/model',
          meta: { title: '技术成果', icon: 'contract', noCache: true },
          children: [
            {
              name: 'ModelFile',
              path: 'model',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/model/index',
              meta: { title: '模型文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'DrawingFile',
              path: 'drawing',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/drawing/index',
              meta: { title: '图纸文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'CNCFile',
              path: 'cnc',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/cnc/index',
              meta: { title: '数控文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'XMLFile',
              path: 'xml',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/xml/index',
              meta: { title: 'XML文件管理', icon: 'project', noCache: true }
            },
            {
              name: 'PlanChangeFile',
              path: 'change-file',
              hidden: false,
              component: '/plan/technical-data-manage/change-file/index',
              meta: { title: '变更文件', icon: 'project', noCache: true }
            },
            {
              name: 'PlanBlueprint',
              path: 'blueprint',
              hidden: false,
              component: '/plan/technical-data-manage/blueprint/index',
              meta: { title: '施工蓝图', icon: 'project', noCache: true }
            },
            {
              name: 'PlanProcess',
              path: 'plan-process',
              hidden: false,
              component: '/plan/technical-data-manage/technical-achievement/process/index',
              meta: { title: '工艺文件', icon: 'project', noCache: true }
            }
          ]
        }
      ]
    }
    // {
    //   path: '/plan/material-preparation',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'PlanMaterialPreparation',
    //   alwaysShow: false,
    //   redirect: '/plan/material-preparation/project-preparation',
    //   meta: { title: '备料管理', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'MaterialProjectPreparation',
    //       path: 'project-preparation',
    //       hidden: false,
    //       component: '/plan/material-preparation/project-preparation/index',
    //       meta: { title: '项目备料', icon: 'project', noCache: true }
    //     }
    //   ]
    // }
    // {
    //   path: '/plan/dosage-statistical',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'DosageStatistical',
    //   alwaysShow: false,
    //   redirect: '/plan/dosage-statistical/steel-statistical',
    //   meta: { title: '标准用量统计', icon: 'contract', noCache: true },
    //   children: [
    //     {
    //       name: 'SteelStatistical',
    //       path: 'steel-statistical',
    //       hidden: false,
    //       component: '/plan/technical-manage/steel-statistical/index',
    //       meta: { title: '钢材使用用量对比', icon: 'project', noCache: true }
    //     }
    //   ]
    // }
  ]
}
