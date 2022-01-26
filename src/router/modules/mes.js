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
    path: '/mes/changed-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesChangedManage',
    alwaysShow: false,
    redirect: '/mes/changed-manage/artifact',
    meta: {
      title: '变更管理',
      icon: 'project',
      noCache: true
    },
    children: [
      // {
      //   name: 'MesArtifactChanged',
      //   path: 'artifact',
      //   hidden: false,
      //   component: '/mes/changed-manage/artifact/index',
      //   meta: {
      //     title: '构件变更',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      // {
      //   name: 'MesMachinePartChanged',
      //   path: 'machine-part',
      //   hidden: false,
      //   component: '/mes/changed-manage/machine-part/index',
      //   meta: {
      //     title: '零件变更',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      // {
      //   name: 'MesAssembleChanged',
      //   path: 'assemble',
      //   hidden: false,
      //   component: '/mes/changed-manage/assemble/index',
      //   meta: {
      //     title: '组立变更',
      //     icon: 'project',
      //     noCache: true
      //   }
      // },
      {
        name: 'MesCommonChanged',
        path: 'common-change',
        hidden: false,
        component: '/mes/changed-manage/common-change/index',
        meta: {
          title: '变更列表',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSurplusList',
        path: 'surplus-list',
        hidden: false,
        component: '/mes/changed-manage/surplus-list/index',
        meta: {
          title: '多余列表',
          icon: 'project',
          noCache: true
        }
      }
      //     {
      //       name: 'MesChangedListManage',
      //       path: 'changed-list',
      //       hidden: false,
      //       component: '/mes/changed-manage/changed-list/index',
      //       meta: { title: '变更清单列表', icon: 'project', noCache: true }
      //     },
      //     {
      //       name: 'MesScrappedListManage',
      //       path: 'scrapped-list',
      //       hidden: false,
      //       component: '/mes/changed-manage/scrapped-list/index',
      //       meta: { title: '报废清单列表', icon: 'project', noCache: true }
      //     },
      //     {
      //       name: 'MesReusedListManage',
      //       path: 'reused-list',
      //       hidden: false,
      //       component: '/mes/changed-manage/reused-list/index',
      //       meta: { title: '二次利用清单列表', icon: 'project', noCache: true }
      //     }
    ]
  },
  {
    path: '/mes/scheduling-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesSchedulingManage',
    alwaysShow: false,
    redirect: '/mes/scheduling-manage/scheduling/artifact',
    meta: {
      title: '工单管理',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesSchedulingArtifact',
      path: 'scheduling/artifact',
      hidden: false,
      redirect: '/mes/task-manage/scheduling/artifact/assemble',
      meta: {
        title: '构件工单',
        icon: 'project',
        noCache: true
      },
      children: [
        {
          name: 'MesSchedulingArtifactMachinePart',
          path: 'machine-part',
          hidden: false,
          component: '/mes/scheduling-manage/scheduling/machine-part-summary/index',
          meta: {
            title: '零件工单',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'MesSchedulingArtifactAssemble',
          path: 'assemble',
          hidden: false,
          component: '/mes/scheduling-manage/scheduling/assemble/index',
          meta: {
            title: '一次工单',
            icon: 'project',
            noCache: true
          }
        },
        {
          name: 'MesSchedulingArtifactArtifact',
          path: 'artifact',
          hidden: false,
          component: '/mes/scheduling-manage/scheduling/artifact/index',
          meta: {
            title: '二次工单',
            icon: 'project',
            noCache: true
          }
        }

      ]
    },
    {
      name: 'MesSchedulingEnclosure',
      path: 'scheduling/enclosure',
      hidden: false,
      redirect: '/mes/task-manage/scheduling/enclosure/contour-plate',
      meta: {
        title: '围护工单',
        icon: 'project',
        noCache: true
      },
      children: [{
        name: 'MesSchedulingPressedPlate',
        path: 'pressed-plate',
        hidden: false,
        component: '/mes/scheduling-manage/scheduling/enclosure/pressed-plate/index',
        meta: {
          title: '压型板工单',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSchedulingFloorPlate',
        path: 'floor-plate',
        hidden: false,
        component: '/mes/scheduling-manage/scheduling/enclosure/floor-plate/index',
        meta: {
          title: '压型楼承板工单',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSchedulingTrussFloorPlate',
        path: 'truss-floor-plate',
        hidden: false,
        component: '/mes/scheduling-manage/scheduling/enclosure/truss-floor-plate/index',
        meta: {
          title: '桁架式楼承板工单',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSchedulingSandwichBoard',
        path: 'sandwich-board',
        hidden: false,
        component: '/mes/scheduling-manage/scheduling/enclosure/sandwich-board/index',
        meta: {
          title: '夹芯板工单',
          icon: 'project',
          noCache: true
        }
      },
      {
        name: 'MesSchedulingFoldingPiece',
        path: 'folding-piece',
        hidden: false,
        component: '/mes/scheduling-manage/scheduling/enclosure/folding-piece/index',
        meta: {
          title: '折边件工单',
          icon: 'project',
          noCache: true
        }
      }
      ]
    },
    {
      name: 'MesTaskArtifact',
      path: 'task/artifact',
      hidden: false,
      component: '/mes/scheduling-manage/task/artifact/index',
      meta: {
        title: '构件排产',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTaskMachinePart',
      path: 'task/machine-part',
      hidden: false,
      component: '/mes/scheduling-manage/task/machine-part/index',
      meta: {
        title: '零件排产',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTaskEnclosure',
      path: 'task/enclosure',
      hidden: false,
      component: '/mes/scheduling-manage/task/enclosure/index',
      meta: {
        title: '围护排产',
        icon: 'project',
        noCache: true
      }
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
      children: [{
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
        //   meta: { title: '桁架式楼承板报表', icon: 'project', noCache: true }
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
          title: '生产统计',
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
      title: '项目看板',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesProductionDashboardProjectDashboard',
      path: 'project-dashboard',
      hidden: false,
      component: '/mes/production-manage/dashboard/project-dashboard/index',
      meta: {
        title: '项目看板',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesProductionDashboardMainMaterialTrack',
      path: 'main-material-track',
      hidden: false,
      component: '/mes/production-manage/dashboard/main-material-track/index',
      meta: {
        title: '主材跟踪',
        icon: 'project',
        noCache: true
      }
    },
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
        title: '结构看板',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesProductionDashboardEnclosureDashboard',
      path: 'enclosure-dashboard',
      hidden: false,
      component: '/mes/production-manage/dashboard/enclosure-dashboard/index',
      meta: {
        title: '围护看板',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesProductionDashboardProjectReport',
      path: 'project-report',
      hidden: false,
      component: '/mes/production-manage/dashboard/project-report/index',
      meta: {
        title: '项目报表',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesProductionDashboardAssemblyMatch',
      path: 'assembly-match',
      hidden: false,
      component: '/mes/production-manage/dashboard/assembly-match/index',
      meta: {
        title: '零件齐套',
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
    children: [{
      name: 'MesTeamReportArtifact',
      path: 'artifact-team',
      hidden: false,
      component: '/mes/team-report/artifact-team/index',
      meta: {
        title: '结构班组',
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
        title: '围护班组',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTeamReportInStaffPiecework',
      path: 'in-staff/piecework-system',
      hidden: false,
      component: '/mes/team-report/in-staff/piecework-system/index',
      meta: {
        title: '编内-计件制',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTeamReportInStaffAllocation',
      path: 'in-staff/allocation-system',
      hidden: false,
      component: '/mes/team-report/in-staff/allocation-system/index',
      meta: {
        title: '编内-分配制',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTeamReportOffStaffWagesConfig',
      path: 'off-staff/wages-config',
      hidden: false,
      component: '/mes/team-report/off-staff/wages-config/index',
      meta: {
        title: '编外-工价',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTeamReportOffStaffSettlement',
      path: 'off-staff/settlement',
      hidden: false,
      component: '/mes/team-report/off-staff/settlement/index',
      meta: {
        title: '编外-工资结算',
        icon: 'project',
        noCache: true
      }
    },
    {
      name: 'MesTeamReportWagesAdjust',
      path: 'wages-adjust',
      hidden: false,
      component: '/mes/team-report/wages-adjust/index',
      meta: { title: '工价调整', icon: 'project', noCache: true }
    }
      //     {
      //       name: 'MesTeamReportInStaffPayroll',
      //       path: 'in-staff-payroll',
      //       hidden: false,
      //       redirect: '/mes/team-report/in-staff-payroll/payroll',
      //       meta: { title: '班组工资表-编内', icon: 'project', noCache: true },
      //       children: [
      //         {
      //           name: 'MesTeamReportInStaffPayrollPayroll',
      //           path: 'payroll',
      //           hidden: false,
      //           component: '/mes/team-report/in-staff-payroll/payroll/index',
      //           meta: { title: '工资结算', icon: 'project', noCache: true }
      //         },
      //         {
      //           name: 'MesTeamReportInStaffPayrollWageAdjust',
      //           path: 'wage-adjust',
      //           hidden: false,
      //           component: '/mes/team-report/in-staff-payroll/wage-adjust/index',
      //           meta: { title: '工价调整', icon: 'project', noCache: true }
      //         }
      //       ]
      //     },
      //     {
      //       name: 'MesTeamReportOffStaffPayroll',
      //       path: 'off-staff-payroll',
      //       hidden: false,
      //       redirect: '/mes/team-report/off-staff-payroll/payroll',
      //       meta: { title: '班组工资表-编外', icon: 'project', noCache: true },
      //       children: [
      //         {
      //           name: 'MesTeamReportWageAdjust',
      //           path: 'wage-adjust',
      //           hidden: false,
      //           component: '/mes/team-report/off-staff-payroll/wage-adjust/index',
      //           meta: { title: '工价定额', icon: 'project', noCache: true }
      //         },
      //         {
      //           name: 'MesTeamReportWageAdjust',
      //           path: 'payroll',
      //           hidden: false,
      //           component: '/mes/team-report/off-staff-payroll/payroll/index',
      //           meta: { title: '工资结算', icon: 'project', noCache: true }
      //         }
      //       ]
      //     }
    ]
  },
  {
    path: '/mes/QHSE-manage',
    component: 'Layout',
    hidden: false,
    name: 'MesQHSEManage',
    alwaysShow: false,
    redirect: '/mes/QHSE-manage/disclosure',
    meta: {
      title: 'QHSE管理',
      icon: 'project',
      noCache: true
    },
    children: [{
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
    redirect: '/mes/pack-and-ship/pack-list',
    meta: {
      title: '打包与发运',
      icon: 'project',
      noCache: true
    },
    children: [{
      name: 'MesManualPack',
      path: 'manual-pack',
      hidden: false,
      component: '/mes/pack-and-ship/manual-pack/index',
      meta: {
        title: '手工打包',
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
    }
      // {
      //   name: 'MesLimitList',
      //   path: 'limit-list',
      //   hidden: false,
      //   component: '/mes/pack-and-ship/limit-list/index',
      //   meta: { title: '发运限制', icon: 'project', noCache: true }
      // }
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
