export default {
  id: 2,
  name: '建钢MES',
  children: [{
    path: '/mes-project',
    component: 'Layout',
    hidden: false,
    name: 'Mes',
    alwaysShow: false,
    redirect: '/mes-project/projects',
    meta: {
      title: '项目列表',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesProject',
      path: 'projects',
      hidden: false,
      component: '/mes/projects/index',
      meta: {
        title: '我的项目',
        icon: 'project',
        noCache: true
      }
    }]
  },
  {
    path: '/mes-kpi',
    component: 'Layout',
    hidden: false,
    name: 'MesKpi',
    alwaysShow: false,
    redirect: '/mes-kpi/production-kpi',
    meta: {
      title: 'KPI',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesProductionKpi',
        path: 'production-kpi',
        hidden: false,
        component: '/mes/production-kpi/index',
        meta: {
          title: '生产KPI',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesShipKpi',
        path: 'ship-kpi',
        hidden: false,
        component: '/mes/ship-kpi/index',
        meta: {
          title: '发运KPI',
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
        name: 'PlanAuxiliaryMaterialList',
        path: 'planAuxiliaryMaterialList',
        hidden: false,
        component: '/plan/technical-manage/auxiliary-material/index',
        meta: { title: '配套件清单', icon: 'project', noCache: true }
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
          }
        ]
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
    path: '/plan/material-preparation',
    component: 'Layout',
    hidden: false,
    name: 'PlanMaterialPreparation',
    alwaysShow: false,
    redirect: '/plan/material-preparation/project-preparation',
    meta: { title: '备料管理', icon: 'contract', noCache: true },
    children: [
      {
        name: 'MaterialProjectPreparation',
        path: 'project-preparation',
        hidden: false,
        component: '/plan/material-preparation/project-preparation/index',
        meta: { title: '项目备料', icon: 'project', noCache: true }
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
  },
  { path: '/mes/craft-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesCraftManage',
    alwaysShow: false,
    redirect: '/mes/craft-manage/section-steel',
    meta: {
      title: '工艺管理',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesArtifactSpecificationManage',
        path: 'artifact-specification-manage',
        hidden: false,
        redirect: '/mes/craft-manage/artifact-specification-manage/artifact-specification-revise',
        meta: {
          title: '构件规格管理',
          icon: 'project',
          noCache: true
        },
        children: [
          {
            name: 'MesArtifactSpecificationRevise',
            path: 'artifact-specification-revise',
            hidden: false,
            component: '/mes/craft-manage/artifact-specification-revise/index',
            meta: {
              title: '构件规格修正',
              icon: 'project',
              noCache: true
            }
          }
        ]
      },
      {
        name: 'MesCraftSectionSteel',
        path: 'section-steel',
        hidden: false,
        redirect: '/mes/craft-manage/section-steel/nesting',
        meta: {
          title: '型材套料',
          icon: 'project',
          noCache: true
        },
        children: [
          {
            name: 'MesCraftSectionSteelNesting',
            path: 'nesting',
            hidden: false,
            component: '/mes/craft-manage/section-steel/nesting/index',
            meta: {
              title: '型材套料',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesCraftSectionSteelNestingSetting',
            path: 'nesting-setting',
            hidden: false,
            component: '/mes/craft-manage/section-steel/nesting-setting/index',
            meta: {
              title: '套料设置',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesCraftSectionSteelNestingResult',
            path: 'nesting-result',
            hidden: false,
            component: '/mes/craft-manage/section-steel/nesting-result/index',
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
  // {
  //   path: '/mes/changed-manage',
  //   component: 'Layout',
  //   hidden: false,
  //   name: 'MesChangedManage',
  //   alwaysShow: false,
  //   redirect: '/mes/changed-manage/artifact',
  //   meta: {
  //     title: '变更管理',
  //     icon: 'project',
  //     noCache: true
  //   },
  //   children: [
  //     {
  //       name: 'MesCommonChanged',
  //       path: 'common-change',
  //       hidden: false,
  //       component: '/mes/changed-manage/common-change/index',
  //       meta: {
  //         title: '变更列表',
  //         icon: 'project',
  //         noCache: true
  //       }
  //     },
  //     {
  //       name: 'MesSurplusList',
  //       path: 'surplus-list',
  //       hidden: false,
  //       component: '/mes/changed-manage/surplus-list/index',
  //       meta: {
  //         title: '多余列表',
  //         icon: 'project',
  //         noCache: true
  //       }
  //     }
  //   ]
  // },
  {
    path: '/mes/production-order-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesProductionOrderManage',
    alwaysShow: false,
    redirect: '/mes/production-order-manage/production-order',
    meta: {
      title: '生产排期管理',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'ProductionOrder',
        path: 'production-order',
        hidden: false,
        component: '/mes/production-order/index',
        meta: {
          title: '生产排期',
          icon: 'project',
          noCache: true
        }
      }
    ]
  },
  {
    path: '/mes/scheduling-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesSchedulingManage',
    alwaysShow: true,
    redirect: '/mes/scheduling-manage/scheduling/artifact',
    meta: {
      title: '生产排产',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesSchedulingArtifact',
        path: 'scheduling/artifact',
        hidden: false,
        component: '/mes/scheduling-manage/artifact/index',
        meta: {
          title: '构件排产',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSchedulingMachinePart',
        path: 'scheduling/machine-part',
        component: '',
        hidden: false,
        alwaysShow: true,
        redirect: '/mes/scheduling-manage/scheduling/machine-part/index',
        meta: {
          title: '零件排产',
          icon: 'project',
          noCache: true
        },
        children: [
          {
            name: 'MesSchedulingMachinePartIndex',
            path: 'index',
            hidden: false,
            component: '/mes/scheduling-manage/machine-part/index',
            meta: {
              title: '零件排产',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesSchedulingMachinePartRecord',
            path: 'record',
            hidden: false,
            component: '/mes/scheduling-manage/machine-part/record/index',
            meta: {
              title: '预览记录',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesSchedulingMachinePartNestingResult',
            path: 'nesting-result',
            hidden: false,
            component: '/mes/scheduling-manage/machine-part/nesting-result/index',
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
    path: '/mes/work-order-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesWorkOrderManage',
    alwaysShow: true,
    redirect: '/mes/work-order-manage/artifact',
    meta: {
      title: '工单管理',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesWorkOrderArtifact',
        path: 'artifact',
        hidden: false,
        component: '/mes/work-order-manage/artifact/index',
        meta: {
          title: '结构工单',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesMachinePartOrder',
        path: 'machinePart',
        hidden: false,
        component: '/mes/work-order-manage/machine-part/index',
        meta: {
          title: '零件工单',
          icon: 'project',
          noCache: true
        }
      }
      // {
      //   name: 'MesSchedulingMachinePart',
      //   path: 'scheduling/machine-part',
      //   hidden: false,
      //   component: '/mes/scheduling-manage/machine-part/index',
      //   meta: {
      //     title: '零件排产',
      //     icon: 'project',
      //     noCache: true
      //   }
      // }
    ]
  },
  {
    path: '/mes/task-tracking',
    component: 'Layout',
    hidden: false,
    name: 'MesTaskTracking',
    alwaysShow: false,
    redirect: '/mes/task-tracking',
    meta: {
      title: '任务跟踪',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesWorkOrderTracking',
        path: 'work-order-tracking',
        hidden: false,
        component: '/mes/task-tracking/work-order-tracking/index',
        meta: {
          title: '工单跟踪',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesMonthlyTaskTracking',
        path: 'monthly-task-tracking',
        hidden: false,
        component: '/mes/task-tracking/monthly-task-tracking/index',
        meta: {
          title: '月度任务跟踪',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesProductionLineTracking',
        path: 'production-line-tracking',
        hidden: false,
        component: '/mes/task-tracking/production-line-tracking/index',
        meta: {
          title: '产线跟踪',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesProcessSluggish',
        path: 'process-sluggish',
        hidden: false,
        component: '/mes/task-tracking/process-sluggish/index',
        meta: {
          title: '工序呆滞',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesAssistanceOperate',
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
            name: 'MesAssistanceProductionLine',
            path: 'productionLine-assistance',
            hidden: false,
            component: '/mes/task-tracking/assistance-operate/productionLine-assistance/index',
            meta: {
              title: '产线协同',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesAssistanceProcess',
            path: 'process-assistance',
            hidden: false,
            component: '/mes/task-tracking/assistance-operate/process-assistance/index',
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
    path: '/mes/production-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesProductionManage',
    alwaysShow: false,
    redirect: '/mes/production-manage/report',
    meta: {
      title: '生产管理',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesProductionReport',
      path: 'report',
      hidden: false,
      redirect: '/mes/production-manage/report/artifact',
      meta: {
        title: '生产报表',
        icon: 'project',
        noCache: false
      },
      children: [
        {
          name: 'MesProductionReportMachinePart',
          path: 'machine-part',
          hidden: false,
          component: '/mes/production-manage/report/machine-part/index',
          meta: {
            title: '零件报表',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'MesProductionReportAssemble',
          path: 'assemble',
          hidden: false,
          component: '/mes/production-manage/report/assemble/index',
          meta: {
            title: '部件报表',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'MesProductionReportArtifact',
          path: 'artifact',
          hidden: false,
          component: '/mes/production-manage/report/artifact/index',
          meta: {
            title: '结构报表',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'MesProductionReportEnclosure',
          path: 'enclosure',
          hidden: false,
          component: '/mes/production-manage/report/enclosure/index',
          meta: {
            title: '围护报表',
            icon: 'project',
            noCache: true
          }
        }
        // {
        //   name: 'MesProductionReportPressedPlate',
        //   path: 'pressed-plate',
        //   hidden: false,
        //   component: '/mes/production-manage/report/pressed-plate/index',
        //   meta: { title: '压型板报表', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'MesProductionReportSandwichBoard',
        //   path: 'sandwich-board',
        //   hidden: false,
        //   component: '/mes/production-manage/report/sandwich-board/index',
        //   meta: { title: '夹芯板报表', icon: 'project', noCache: true }
        // },
        // // {
        // //   name: 'MesProductionReportFloorPlate',
        // //   path: 'floor-plate',
        // //   hidden: false,
        // //   component: '/mes/production-manage/report/floor-plate/index',
        // //   meta: { title: '楼承板报表', icon: 'project', noCache: true }
        // // },
        // {
        //   name: 'MesProductionReportTrussFloorPlate',
        //   path: 'truss-floor-plate',
        //   hidden: false,
        //   component: '/mes/production-manage/report/truss-floor-plate/index',
        //   meta: { title: '桁架楼承板报表', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'MesProductionReportPressedFloorPlate',
        //   path: 'pressed-floor-plate',
        //   hidden: false,
        //   component: '/mes/production-manage/report/pressed-floor-plate/index',
        //   meta: { title: '压型楼承板报表', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'MesProductionReportFoldingPiece',
        //   path: 'folding-piece',
        //   hidden: false,
        //   component: '/mes/production-manage/report/folding-piece/index',
        //   meta: { title: '折边件报表', icon: 'project', noCache: true }
        // }
      ]
    },

    {
      name: 'MesProductionAnalysis',
      path: 'analysis',
      hidden: false,
      redirect: '/mes/production-manage/analysis/production-statistics',
      meta: {
        title: '生产分析',
        icon: 'project',
        noCache: true
      },
      children: [{
        name: 'MesProductionAnalysisStatistics',
        path: 'production-statistics',
        hidden: false,
        component: '/mes/production-manage/analysis/production-statistics/index',
        meta: {
          title: '在制品统计',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesProductionDelayReport',
        path: 'delay-report',
        hidden: false,
        component: '/mes/production-manage/analysis/delay-report/index',
        meta: {
          title: '迟滞报表',
          icon: 'project',
          noCache: true
        }
      }
      ]
    }
    ]
  },
  {
    name: 'MesProductionDashboard',
    path: 'dashboard',
    hidden: false,
    redirect: '/mes/production-manage/dashboard/project-state',
    meta: {
      title: '项目制造',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesProductionDashboardProjectOverview',
      path: 'project-overview',
      hidden: false,
      component: '/mes/production-manage/dashboard/project-overview/index',
      meta: {
        title: '项目总览',
        icon: 'project',
        noCache: true
      }
    },
    // {
    //   name: 'MesProductionDashboardProjectDashboard',
    //   path: 'project-dashboard',
    //   hidden: false,
    //   component: '/mes/production-manage/dashboard/project-dashboard/index',
    //   meta: {
    //     title: '项目看板',
    //     icon: 'project',
    //     noCache: true
    //   }
    // },
    // {
    //   name: 'MesProductionDashboardMainMaterialTrack',
    //   path: 'main-material-track',
    //   hidden: false,
    //   component: '/mes/production-manage/dashboard/main-material-track/index',
    //   meta: {
    //     title: '主材跟踪',
    //     icon: 'project',
    //     noCache: true
    //   }
    // },
    // {
    //   name: 'MesProductionDashboardProjectState',
    //   path: 'project-state',
    //   hidden: false,
    //   component: '/mes/production-manage/dashboard/project-state/index',
    //   meta: {
    //     title: '项目状态',
    //     icon: 'project',
    //     noCache: true
    //   }
    // },
    {
      name: 'MesProductionDashboardArtifactDashboard',
      path: 'artifact-dashboard',
      hidden: false,
      component: '/mes/production-manage/dashboard/artifact-dashboard/index',
      meta: {
        title: '生产看板',
        icon: 'project',
        noCache: true
      }
    },
    // {
    //   name: 'MesProductionDashboardEnclosureDashboard',
    //   path: 'enclosure-dashboard',
    //   hidden: false,
    //   component: '/mes/production-manage/dashboard/enclosure-dashboard/index',
    //   meta: {
    //     title: '围护看板',
    //     icon: 'project',
    //     noCache: true
    //   }
    // },
    // {
    //   name: 'MesProductionDashboardProjectReport',
    //   path: 'project-report',
    //   hidden: false,
    //   component: '/mes/production-manage/dashboard/project-report/index',
    //   meta: {
    //     title: '项目报表',
    //     icon: 'project',
    //     noCache: true
    //   }
    // },
    {
      name: 'MesProductionDashboardAssemblyMatch',
      path: 'assembly-match',
      hidden: false,
      component: '/mes/production-manage/dashboard/assembly-match/index',
      meta: {
        title: '项目齐套',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesProductionDashboardPainting',
      path: 'painting',
      hidden: false,
      component: '/mes/production-manage/dashboard/painting/index',
      meta: {
        title: '涂装计算',
        icon: 'project',
        noCache: true
      }
    }
    ]
  },
  // {
  //   path: '/mes/business-manage',
  //   component: 'Layout',
  //   hidden: false,
  //   name: 'MesBusinessManage',
  //   alwaysShow: false,
  //   redirect: '/mes/business-manage/business-entry',
  //   meta: { title: '商务管理', icon: 'project', noCache: true },
  //   children: [
  //     {
  //       name: 'MesBusinessEntry',
  //       path: 'business-entry',
  //       hidden: false,
  //       component: '/mes/business-manage/business-entry/index',
  //       meta: { title: '商务录入', icon: 'project', noCache: true }
  //     },
  //     {
  //       name: 'MesBusinessOutboundTracking',
  //       path: 'outbound-tracking',
  //       hidden: false,
  //       component: '/mes/business-manage/outbound-tracking/index',
  //       meta: { title: '出库跟踪', icon: 'project', noCache: true }
  //     },
  //     {
  //       name: 'MesBusinessInstallationTracking',
  //       path: 'installation-tracking',
  //       hidden: false,
  //       component: '/mes/business-manage/installation-tracking/index',
  //       meta: { title: '工地跟踪', icon: 'project', noCache: true }
  //     },
  //     {
  //       name: 'MesBusinessTripTracking',
  //       path: 'trip-tracking',
  //       hidden: false,
  //       component: '/mes/business-manage/trip-tracking/index',
  //       meta: { title: '车次跟踪', icon: 'project', noCache: true }
  //     }
  //   ]
  // },
  // {
  //   path: '/mes/component-abnormal',
  //   component: 'Layout',
  //   hidden: false,
  //   name: 'MesAbnormalManage',
  //   alwaysShow: false,
  //   redirect: '/mes/component-abnormal',
  //   meta: { title: '异常清单管理', icon: 'project', noCache: true },
  //   children: [
  //     {
  //       name: 'MesAbnormalList',
  //       path: 'component-abnormal',
  //       hidden: false,
  //       component: '/mes/component-abnormal/index',
  //       meta: { title: '异常清单列表', icon: 'project', noCache: true }
  //     }
  //   ]
  // },
  {
    path: '/mes/team-report',
    component: 'Layout',
    hidden: false,
    name: 'MesTeamReport',
    alwaysShow: false,
    redirect: '/mes/team-report/artifact-team',
    meta: {
      title: '班组报表',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesTeamReportArtifact',
        path: 'artifact-team',
        hidden: false,
        component: '/mes/team-report/artifact-team/index',
        meta: {
          title: '结构班组进度',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesTeamReportEnclosure',
        path: 'enclosure-team',
        hidden: false,
        component: '/mes/team-report/enclosure-team/index',
        meta: {
          title: '围护班组进度',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesTeamReportArtifactWage',
        path: 'artifact-team-wage',
        hidden: false,
        component: '/mes/team-report/artifact-team-wage/index',
        meta: {
          title: '结构班组工资',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesTeamReportEnclosureWage',
        path: 'enclosure-team-wage',
        hidden: false,
        component: '/mes/team-report/enclosure-team-wage/index',
        meta: {
          title: '围护班组工资',
          icon: 'project',
          noCache: true
        }
      },
      // {
      //   name: 'MesTeamReportInStaffPiecework',
      //   path: 'in-staff/piecework-system',
      //   hidden: false,
      //   component: '/mes/team-report/in-staff/piecework-system/index',
      //   meta: {
      //     title: '编内-计件制',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      // {
      //   name: 'MesTeamReportInStaffAllocation',
      //   path: 'in-staff/allocation-system',
      //   hidden: false,
      //   component: '/mes/team-report/in-staff/allocation-system/index',
      //   meta: {
      //     title: '编内-分配制',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },

      // {
      //   name: 'MesTeamReportOffStaffSettlement',
      //   path: 'off-staff/settlement',
      //   hidden: false,
      //   component: '/mes/team-report/off-staff/settlement/index',
      //   meta: {
      //     title: '编外-工资结算',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      // {
      //   name: 'MesTeamReportOffStaffPiecework',
      //   path: 'off-staff/piecework-system',
      //   hidden: false,
      //   component: '/mes/team-report/off-staff/piecework-system/index',
      //   meta: {
      //     title: '编外-计件制',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      {
        name: 'MesTeamReportWagesAdjust',
        path: 'wages-adjust',
        hidden: false,
        component: '/mes/team-report/wages-adjust/index',
        meta: { title: '编内-工价调整', icon: 'project', noCache: true }
      },
      // {
      //   name: 'MesTeamReportInStaffAllocation',
      //   path: 'in-staff/allocation-system',
      //   hidden: false,
      //   component: '/mes/team-report/in-staff/allocation-system/index',
      //   meta: {
      //     title: '编外-分配制',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      {
        name: 'MesTeamReportOffStaffWagesConfig',
        path: 'off-staff/wages-config',
        hidden: false,
        component: '/mes/team-report/off-staff/wages-config/index',
        meta: {
          title: '编外-工价调整',
          icon: 'project',
          noCache: true
        }
      }
    ]
  },
  {
    path: '/mes/QHSE-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesQHSEManage',
    alwaysShow: false,
    redirect: '/mes/QHSE-manage/quality-inspection-report',
    meta: {
      title: '质安管理',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesQHSEManageQualityInspectionReport',
        path: 'quality-inspection-report',
        hidden: false,
        component: '/mes/QHSE-manage/quality-inspection-report/index',
        meta: {
          title: '质检报表',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesQHSEManageProductionLineReport',
        path: 'production-line-report',
        hidden: false,
        component: '/mes/QHSE-manage/production-line-report/index',
        meta: {
          title: '产线报表',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesQHSEManageDisclosure',
        path: 'disclosure',
        hidden: false,
        component: '/mes/QHSE-manage/disclosure/index',
        meta: {
          title: '问题曝光',
          icon: 'project',
          noCache: true
        }
      }
      // {
      //   name: 'MesQHSEManageExFactoryData',
      //   path: 'ex-factory-data',
      //   hidden: false,
      //   component: '/mes/QHSE-manage/ex-factory-data/index',
      //   meta: { title: '出厂资料', icon: 'project', noCache: true }
      // }
    ]
  },
  {
    path: '/mes/label-print',
    component: 'Layout',
    hidden: false,
    name: 'MesLabelPrint',
    alwaysShow: false,
    redirect: '/mes/label-print/artifact',
    meta: {
      title: '产品标签',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesLabelPrintArtifact',
      path: 'artifact',
      hidden: false,
      component: '/mes/label-print/artifact/index',
      meta: {
        title: '构件',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesLabelPrintEnclosure',
      path: 'enclosure',
      hidden: false,
      component: '/mes/label-print/enclosure/index',
      meta: {
        title: '围护',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesLabelPrintAuxiliaryMaterial',
      path: 'auxiliary-material',
      hidden: false,
      component: '/mes/label-print/auxiliary-material/index',
      meta: {
        title: '配套件',
        icon: 'project',
        noCache: true
      }
    }
    // {
    //   name: 'MesLabelPrintFoldingPiece',
    //   path: 'enclosure',
    //   hidden: false,
    //   component: '/mes/label-print/folding-piece/index',
    //   meta: {
    //     title: '折边件',
    //     icon: 'project',
    //     noCache: true
    //   }
    // }
      //     {
      //       name: 'MesLabelPrintingAuxiliaryMaterial',
      //       path: 'auxiliary-material',
      //       hidden: false,
      //       component: '/mes/label-printing/auxiliary-material/index',
      //       meta: { title: '辅材', icon: 'project', noCache: true }
      //     }
    ]
  },
  {
    path: '/mes/manufactures-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesManufacturesManage',
    alwaysShow: false,
    redirect: '/mes/manufactures-manage/inbound-state/artifact-dashboard',
    meta: {
      title: '制成品管理',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesInboundStateArtifactDashboard',
      path: 'inbound-state/artifact-dashboard',
      hidden: false,
      component: '/mes/manufactures-manage/inbound-state/artifact-dashboard/index',
      meta: {
        title: '入库看板-构件',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesInboundStateEnclosureDashboard',
      path: 'inbound-state/enclosure-dashboard',
      hidden: false,
      component: '/mes/manufactures-manage/inbound-state/enclosure-dashboard/index',
      meta: {
        title: '入库看板-围护',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesOutboundStateArtifactDashboard',
      path: 'outbound-state/artifact-dashboard',
      hidden: false,
      component: '/mes/manufactures-manage/outbound-state/artifact-dashboard/index',
      meta: {
        title: '出库看板-构件',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesOutboundStateEnclosureDashboard',
      path: 'outbound-state/enclosure-dashboard',
      hidden: false,
      component: '/mes/manufactures-manage/outbound-state/enclosure-dashboard/index',
      meta: {
        title: '出库看板-围护',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesWarehouseStateArtifact',
      path: 'warehouse-state/artifact',
      hidden: false,
      component: '/mes/manufactures-manage/warehouse-state/artifact/index',
      meta: {
        title: '出入库状态-构件',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesWarehouseStateEnclosure',
      path: 'warehouse-state/enclosure',
      hidden: false,
      component: '/mes/manufactures-manage/warehouse-state/enclosure/index',
      meta: {
        title: '出入库状态-围护',
        icon: 'project',
        noCache: true
      }
    },
    //     {
    //       name: 'MesWarehouseStateAuxiliaryMaterial',
    //       path: 'auxiliary-material',
    //       hidden: false,
    //       component: '/mes/manufactures-manage/warehouse-state/auxiliary-material/index',
    //       meta: { title: '辅材', icon: 'project', noCache: true }
    //     }

    {
      name: 'MesManufacturesReport',
      path: 'report',
      hidden: false,
      component: '/mes/manufactures-manage/report/index',
      meta: { title: '入发存报表', icon: 'project', noCache: true }
    }
    ]
  },
  {
    path: '/mes/pack-and-ship',
    component: 'Layout',
    hidden: false,
    name: 'MesPackAndShip',
    alwaysShow: false,
    redirect: '/mes/pack-and-ship/ship-summary',
    meta: {
      title: '发运管理',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesShipSummary',
        path: 'ship-summary',
        hidden: false,
        component: '/mes/pack-and-ship/ship-summary/index',
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
        name: 'MesPackManage',
        alwaysShow: false,
        redirect: '/mes/pack-and-ship/pack-manage/manual-pack',
        meta: {
          title: '打包管理',
          icon: 'project',
          noCache: true
        },
        children: [
          {
            name: 'MesManualPack',
            path: 'manual-pack',
            hidden: false,
            component: '/mes/pack-and-ship/manual-pack/index',
            meta: {
              title: '打包操作',
              icon: 'project',
              noCache: true
            }
          },
          {
            name: 'MesPackList',
            path: 'pack-list',
            hidden: false,
            component: '/mes/pack-and-ship/pack-list/index',
            meta: {
              title: '打包记录',
              icon: 'project',
              noCache: true
            }
          }
        ]
      },
      {
        name: 'MesShipList',
        path: 'ship-list',
        hidden: false,
        component: '/mes/pack-and-ship/ship-list/index',
        meta: {
          title: '发运记录',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesReceiptStatus',
        path: 'receipt-status',
        hidden: false,
        component: '/mes/pack-and-ship/receipt-status/index',
        meta: {
          title: '收货状态',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesLogisticsList',
        path: 'logistics-list',
        hidden: false,
        component: '/mes/pack-and-ship/logistics-list/index',
        meta: {
          title: '物流记录',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesShipAudit',
        path: 'ship-audit',
        hidden: false,
        component: '/mes/pack-and-ship/ship-audit/index',
        meta: {
          title: '发运审核',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesProductSendReceiveStorage',
        path: 'product-send-receive-storage',
        hidden: false,
        component: '/mes/pack-and-ship/product-send-receive-storage/index',
        meta: {
          title: '制品入发存',
          icon: 'project',
          noCache: true
        }
      }
      // {
      //   name: 'MesLimitList',
      //   path: 'limit-list',
      //   hidden: false,
      //   component: '/mes/pack-and-ship/limit-list/index',
      //   meta: { title: '发运限制', icon: 'project', noCache: true }
      // }
    ]
  },
  {
    path: '/mes/factory-report',
    component: 'Layout',
    hidden: false,
    name: 'MesFactoryReport',
    alwaysShow: false,
    redirect: '/mes/factory-report',
    meta: {
      title: '工厂报表',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesWorkshopReport',
        path: 'workshop-report',
        hidden: false,
        component: '/mes/factory-report/workshop-report/index',
        meta: {
          title: '车间报表',
          icon: 'project',
          noCache: true
        }
      }
    ]
  },
  {
    path: '/mes/production-line-wage-statistics',
    component: 'Layout',
    hidden: false,
    name: 'MesProductionLineWageStatistics',
    alwaysShow: false,
    redirect: '/mes/production-line-wage-statistics',
    meta: {
      title: '产线工资统计',
      icon: 'project',
      noCache: true
    },
    children: [
      {
        name: 'MesProductionStatistics',
        path: 'production-statistics',
        hidden: false,
        component: '/mes/production-line-wage-statistics/production-statistics/index',
        meta: {
          title: '产量统计',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesStudSleeveStatistics',
        path: 'stud-sleeve-statistics',
        hidden: false,
        component: '/mes/production-line-wage-statistics/stud-sleeve-statistics/index',
        meta: {
          title: '栓钉套筒统计',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesWageAdjust',
        path: 'wage-adjust',
        hidden: false,
        component: '/mes/production-line-wage-statistics/wage-adjust/index',
        meta: {
          title: '工价调整',
          icon: 'project',
          noCache: true
        }
      }
    ]
  }
    // {
    //   path: '/mes/production-state',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesProductionState',
    //   alwaysShow: false,
    //   redirect: '/mes/production-state/artifact-process',
    //   meta: { title: '生产看板', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesProductionStateArtifactProcess',
    //       path: 'artifact-process',
    //       hidden: false,
    //       component: '/mes/production-state/artifact-process/index',
    //       meta: { title: '工序状态-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateMachinePartProcess',
    //       path: 'machine-part-process',
    //       hidden: false,
    //       component: '/mes/production-state/machine-part-process/index',
    //       meta: { title: '工序状态-零件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateEnclosureProcess',
    //       path: 'enclosure-process',
    //       hidden: false,
    //       component: '/mes/production-state/enclosure-process/index',
    //       meta: { title: '工序状态-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateArtifactProcessPass',
    //       path: 'artifact-process-pass',
    //       hidden: false,
    //       component: '/mes/production-state/artifact-process-pass/index',
    //       meta: { title: '工序完成表-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateMachinePartProcessPass',
    //       path: 'machine-part-process-pass',
    //       hidden: false,
    //       component: '/mes/production-state/machine-part-process-pass/index',
    //       meta: { title: '工序完成表-零件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateEnclosureProcessPass',
    //       path: 'enclosure-process-pass',
    //       hidden: false,
    //       component: '/mes/production-state/enclosure-process-pass/index',
    //       meta: { title: '工序完成表-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateArtifactDashboard',
    //       path: 'artifact-dashboard',
    //       hidden: false,
    //       component: '/mes/production-state/artifact-dashboard/index',
    //       meta: { title: '生产看板-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateMachinePartDashboard',
    //       path: 'machine-part-dashboard',
    //       hidden: false,
    //       component: '/mes/production-state/machine-part-dashboard/index',
    //       meta: { title: '生产看板-零件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateEnclosureDashboard',
    //       path: 'enclosure-dashboard',
    //       hidden: false,
    //       component: '/mes/production-state/enclosure-dashboard/index',
    //       meta: { title: '生产看板-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionStateProcessCompletedState',
    //       path: 'process-completed-state',
    //       hidden: false,
    //       component: '/mes/production-state/process-completed-state/index',
    //       meta: { title: '工序完成表', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/receipt-state',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesReceiptState',
    //   alwaysShow: false,
    //   redirect: '/mes/receipt-state/artifact-dashboard',
    //   meta: { title: '收货看板', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesReceiptStateArtifactDashboard',
    //       path: 'artifact-dashboard',
    //       hidden: false,
    //       component: '/mes/receipt-state/artifact-dashboard/index',
    //       meta: { title: '收货看板-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesReceiptStateEnclosureDashboard',
    //       path: 'enclosure-dashboard',
    //       hidden: false,
    //       component: '/mes/receipt-state/enclosure-dashboard/index',
    //       meta: { title: '收货看板-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesReceiptStateAuxiliaryMaterialDashboard',
    //       path: 'auxiliary-material-dashboard',
    //       hidden: false,
    //       component: '/mes/receipt-state/auxiliary-material-dashboard/index',
    //       meta: { title: '收货看板-辅材', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/report-manage',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesReportManage',
    //   alwaysShow: false,
    //   redirect: '/mes/report-manage/installation-report',
    //   meta: { title: '填报管理', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesInstallationReport',
    //       path: 'installation-report',
    //       hidden: false,
    //       component: '/mes/report-manage/installation-report/index',
    //       meta: { title: '安装填报', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/installation-state',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesInstallationState',
    //   alwaysShow: false,
    //   redirect: '/mes/installation-state/artifact-dashboard',
    //   meta: { title: '安装看板', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesInstallationStateArtifactDashboard',
    //       path: 'artifact-dashboard',
    //       hidden: false,
    //       component: '/mes/installation-state/artifact-dashboard/index',
    //       meta: { title: '安装看板-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesInstallationStateEnclosureDashboard',
    //       path: 'enclosure-dashboard',
    //       hidden: false,
    //       component: '/mes/installation-state/enclosure-dashboard/index',
    //       meta: { title: '安装看板-围护', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/site-warehouse-state',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesSiteWarehouseState',
    //   alwaysShow: false,
    //   redirect: '/mes/site-warehouse-state/artifact',
    //   meta: { title: '收安综合看板', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesArtifactSiteWarehouseState',
    //       path: 'artifact',
    //       hidden: false,
    //       component: '/mes/site-warehouse-state/artifact/index',
    //       meta: { title: '收安状态-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesEnclosureSiteWarehouseState',
    //       path: 'enclosure',
    //       hidden: false,
    //       component: '/mes/site-warehouse-state/enclosure/index',
    //       meta: { title: '收安状态-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesAuxiliaryMaterialSiteWarehouseState',
    //       path: 'auxiliary-material',
    //       hidden: false,
    //       component: '/mes/site-warehouse-state/auxiliary-material/index',
    //       meta: { title: '收安状态-辅材', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/complex-state',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesComplexState',
    //   alwaysShow: false,
    //   redirect: '/mes/complex-state/artifact',
    //   meta: { title: '总看板', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesArtifactComplexState',
    //       path: 'artifact',
    //       hidden: false,
    //       component: '/mes/complex-state/artifact/index',
    //       meta: { title: '汇总状态-构件', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesEnclosureComplexState',
    //       path: 'enclosure',
    //       hidden: false,
    //       component: '/mes/complex-state/enclosure/index',
    //       meta: { title: '汇总状态-围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesAuxiliaryMaterialComplexState',
    //       path: 'auxiliary-material',
    //       hidden: false,
    //       component: '/mes/complex-state/auxiliary-material/index',
    //       meta: { title: '汇总状态-辅材', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/dosage-statistical',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesDosageStatistical',
    //   alwaysShow: false,
    //   redirect: '/mes/dosage-statistical/steel',
    //   meta: { title: '标准用量统计', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesSteelDosage',
    //       path: 'steel',
    //       hidden: false,
    //       component: '/mes/dosage-statistical/steel/index',
    //       meta: { title: '钢材用量对比', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    // {
    //   path: '/mes/report',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'MesReport',
    //   alwaysShow: false,
    //   redirect: '/mes/report/all/base-statistics',
    //   meta: { title: '报表', icon: 'project', noCache: true },
    //   children: [
    //     {
    //       name: 'MesAllBaseStatistics',
    //       path: 'all/base-statistics',
    //       hidden: false,
    //       component: '/mes/report/all/base-statistics/index',
    //       meta: { title: '数据统计', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionComponentReport',
    //       path: 'production/component',
    //       hidden: false,
    //       component: '/mes/report/production/component/index',
    //       meta: { title: '生产报表-构件围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesProductionTeamReport',
    //       path: 'production/team',
    //       hidden: false,
    //       component: '/mes/report/production/team/index',
    //       meta: { title: '生产报表-班组', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesOutboundComponentReport',
    //       path: 'outbound/component',
    //       hidden: false,
    //       component: '/mes/report/outbound/component/index',
    //       meta: { title: '出库报表-构件围护', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesAuxiliaryMaterialOutboundReport',
    //       path: 'outbound/auxiliary-material',
    //       hidden: false,
    //       component: '/mes/report/outbound/auxiliary-material/index',
    //       meta: { title: '出库报表-辅材', icon: 'project', noCache: true }
    //     },
    //     {
    //       name: 'MesInstallationComponentReport',
    //       path: 'installation/component',
    //       hidden: false,
    //       component: '/mes/report/installation/component/index',
    //       meta: { title: '安装报表-构件围护', icon: 'project', noCache: true }
    //     }
    //     // {
    //     //   name: 'MesAuxiliaryMaterialInstallationReport',
    //     //   path: 'installation/auxiliary-material',
    //     //   hidden: false,
    //     //   component: '/mes/report/installation/auxiliary-material/index',
    //     //   meta: { title: '安装报表-辅材', icon: 'project', noCache: true }
    //     // }
    //   ]
    // }

  ]
}
