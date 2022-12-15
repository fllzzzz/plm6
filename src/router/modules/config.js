// 路由：配置管理
export default {
  id: 1,
  name: '配置管理',
  children: [
    {
      path: '/base-config',
      component: 'Layout',
      hidden: false,
      name: 'BaseConfig',
      alwaysShow: false,
      redirect: '/config-manage/main/unit-config',
      meta: { title: '基础配置', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'SystemConfig',
          path: 'system-config',
          hidden: false,
          component: '/config-manage/main/system-config/index',
          meta: { title: '公司配置', icon: 'project', noCache: true, permissions: ['systemConfigPM'] }
        },
        {
          name: 'UnitConfig',
          path: 'unit-config',
          hidden: false,
          component: '/config-manage/main/unit-config/index',
          meta: { title: '单位配置', icon: 'project', noCache: true, permissions: ['configUnitPM'] }
        },
        {
          name: 'factoryAndWorkshopConfig',
          path: 'factory-and-workshop',
          hidden: false,
          component: '/mes/production-config/factory-and-workshop/index',
          meta: { title: '工厂管理', icon: 'project', noCache: true, permissions: ['configFactoryPM'] }
        },
        {
          name: 'NumberConfig',
          path: 'number-config',
          hidden: false,
          component: '/config-manage/main/number-config/index',
          meta: { title: '编号配置', icon: 'project', noCache: true, permissions: ['numberConfigPM'] }
        },
        {
          name: 'CommonTaxRate',
          path: 'common-tax-rate',
          hidden: false,
          component: '/config-manage/main/common-tax-rate/index',
          meta: { title: '常用税率', icon: 'project', noCache: true, permissions: ['configCommonTaxRatePM'] }
        },
        {
          name: 'ExpenseManagement',
          path: 'expense-management',
          hidden: false,
          component: '/config-manage/system/expense-management/index',
          meta: { title: '费用归类', icon: 'project', noCache: true, permissions: ['expenseManagementPM'] }
        },
        {
          name: 'BranchCompany',
          path: 'branch-company',
          hidden: false,
          component: '/config-manage/system/branch-company/index',
          meta: { title: '分支机构', icon: 'project', noCache: true, permissions: ['branchCompanyPM'] }
        },
        {
          name: 'ProjectMode',
          path: 'project-mode',
          hidden: false,
          component: '/config-manage/system/project-mode/index',
          meta: { title: '项目模式', icon: 'project', noCache: true, permissions: ['projectModePM'] }
        },
        {
          name: 'TablePrinting',
          path: 'table-printing',
          hidden: false,
          component: '/config-manage/system/table-print-template/index',
          meta: { title: '表格模板', icon: 'project', noCache: true, permissions: ['tablePrintTemplatePM'] }
        }
      ]
    },
    // {
    //   path: '/project-config',
    //   component: 'Layout',
    //   hidden: false,
    //   name: 'ProjectConfig',
    //   alwaysShow: false,
    //   redirect: '/config-manage/project-config/number-config',
    //   meta: { title: '工程配置', icon: 'config-2', noCache: true },
    //   children: [
    //     {
    //       name: 'NumberConfig',
    //       path: 'number-config',
    //       hidden: false,
    //       component: '/config-manage/project-config/number-config/index',
    //       meta: { title: '编号配置', icon: 'project', noCache: true }
    //     }
    //   ]
    // },
    {
      path: '/classification-manage',
      component: 'Layout',
      hidden: false,
      name: 'ClassificationManage',
      alwaysShow: false,
      redirect: '/config-manage/classification-manage/classification-config',
      meta: { title: '科目管理', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'ClassificationConfig',
          path: 'classification-config',
          hidden: false,
          component: '/config-manage/classification-manage/classification-config/index',
          meta: { title: '科目配置', icon: 'project', noCache: true, permissions: ['classConfigPM'] }
        },
        {
          name: 'SpecificationConfig',
          path: 'specification-config',
          hidden: false,
          component: '/config-manage/classification-manage/specification-config/index',
          meta: { title: '规格配置', icon: 'project', noCache: true, permissions: ['specConfigPM'] }
        },
        {
          name: 'ClassificationUnitConfig',
          path: 'measure-config',
          hidden: false,
          component: '/config-manage/classification-manage/measure-config/index',
          meta: { title: '计量配置', icon: 'project', noCache: true, permissions: ['measureConfigPM'] }
        },
        {
          name: 'ConfigSectionSteelLibrary',
          path: 'section-steel-specification-config',
          hidden: false,
          component: '/config-manage/classification-manage/section-steel-specification-config/index',
          meta: { title: '型材库', icon: 'project', noCache: true, permissions: ['sectionSteelLibraryPM'] }
        }
        // {
        //   name: 'OutboundMethod',
        //   path: 'outbound-method',
        //   hidden: false,
        //   component: '/classification-manage/outbound-method/index',
        //   meta: { title: '出库方式', icon: 'project', noCache: true }
        // }
        // {
        //   name: 'InstallationMethod',
        //   path: 'installation-method',
        //   hidden: false,
        //   component: '/classification-manage/installation-method/index',
        //   meta: { title: '安装方式', icon: 'project', noCache: true }
        // }
      ]
    },
    {
      path: '/wms-config',
      component: 'Layout',
      hidden: false,
      name: 'WMSConfig',
      alwaysShow: false,
      redirect: '/config-manage/wms/base',
      meta: { title: 'WMS-配置管理', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'WmsBaseConfig',
          path: 'base',
          hidden: false,
          component: '/config-manage/wms/base/index',
          meta: { title: '基础配置', icon: 'project', noCache: true, permissions: ['configWmsBasicForPM'] }
        },
        {
          name: 'ConfigWarehouse',
          path: 'warehouse',
          hidden: false,
          component: '/config-manage/wms/warehouse/index',
          meta: { title: '仓库设置', icon: 'project', noCache: true, permissions: ['configWmsFactoryWarehousePM'] }
        },
        {
          name: 'ScrapDefinition',
          path: 'scrap-definition',
          hidden: false,
          component: '/config-manage/wms/scrap-definition/index',
          meta: { title: '废料定义', icon: 'project', noCache: true, permissions: ['configWmsScrapDefinitionPM'] }
        },
        {
          name: 'ConfigInspectionRawMaterial',
          path: 'inspection-raw-material',
          hidden: false,
          component: '/config-manage/wms/inspection-raw-material/index',
          meta: { title: '入库质检物料', icon: 'project', noCache: true, permissions: ['configWmsInspectionRawMaterialPM'] }
        }
      ]
    },
    {
      path: '/mes-config',
      component: 'Layout',
      hidden: false,
      name: 'MesConfigManage',
      alwaysShow: true,
      redirect: '/mes-config/base',
      meta: { title: 'MES-公共配置', icon: 'project', noCache: true },
      children: [
        {
          name: 'MesBaseConfig',
          path: 'base',
          hidden: false,
          component: '/config-manage/mes/base/index',
          meta: { title: '基础配置', icon: 'project', noCache: true, permissions: ['configMesBasePM'] }
        },
        {
          name: 'ChangeReason',
          path: 'change-reason',
          hidden: false,
          component: '/config-manage/mes/change-reason-config/index',
          meta: { title: '变更原因配置', icon: 'project', noCache: true, permissions: ['changeReasonPM'] }
        }
      ]
    },
    {
      path: '/mes/production-config',
      component: 'Layout',
      hidden: false,
      name: 'MesProductionConfig',
      alwaysShow: false,
      redirect: '/mes/production-config/steel-workshop',
      meta: { title: '建钢-生产配置', icon: 'project', noCache: true },
      children: [
        {
          name: 'SteelWorkshop',
          path: 'steel-workshop',
          hidden: false,
          component: '/mes/production-config/workshop/index',
          meta: { title: '建钢-车间', icon: 'project', noCache: true, permissions: ['configWorkshopPM'] }
        },
        {
          path: 'characteristics-config',
          component: '',
          hidden: false,
          name: 'CharacteristicsConfig',
          alwaysShow: true,
          redirect: '/mes/production-config/characteristics-config/artifact-config',
          meta: { title: '构零件定义', icon: 'project', noCache: true },
          children: [
            {
              name: 'ArtifactConfig',
              path: 'artifact-config',
              hidden: false,
              component: '/config-manage/mes/artifact-config/index',
              meta: { title: '构件特征定义', icon: 'project', noCache: true, permissions: ['artifactConfigPM'] }
            },
            {
              name: 'MachinePartConfig',
              path: 'machine-part-config',
              hidden: false,
              component: '/config-manage/mes/machine-part/index',
              meta: { title: '部件特征定义', icon: 'project', noCache: true, permissions: ['machinePartConfigPM'] }
            },
            {
              name: 'SteelClassic',
              path: 'steel-classic',
              hidden: false,
              component: '/config-manage/mes/steel-classic/index',
              meta: { title: '零件特征定义', icon: 'project', noCache: true, permissions: ['steelClassicPM'] }
            },
            {
              name: 'AuxiliaryMaterialConfig',
              path: 'auxiliary-material-config',
              hidden: false,
              component: '/config-manage/mes/auxiliary-material-config/index',
              meta: { title: '配套件特征定义', icon: 'project', noCache: true, permissions: ['auxiliaryMaterialConfigPM'] }
            }
          ]
        },
        {
          path: 'process-config',
          component: '',
          hidden: false,
          name: 'ProcessConfig',
          alwaysShow: true,
          redirect: '/mes/production-config/process-config/process',
          meta: { title: '生产工序定义', icon: 'project', noCache: true },
          children: [
            {
              name: 'MesConfigProcess',
              path: 'process',
              hidden: false,
              component: '/mes/production-config/process/index',
              meta: { title: '工序配置', icon: 'project', noCache: true, permissions: ['configProcessPM'] }
            },
            {
              name: 'MesConfigArtifactProductProcess',
              path: 'artifact-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/artifact/index',
              meta: { title: '构件工序定义', icon: 'project', noCache: true, permissions: ['configProductProcessArtifactPM'] }
            },
            {
              name: 'MesConfigAssembleProductProcess',
              path: 'assemble-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/assemble/index',
              meta: { title: '部件工序定义', icon: 'project', noCache: true, permissions: ['configProductProcessAssemblePM'] }
            },
            {
              name: 'MesConfigMachinePartProductProcess',
              path: 'machine-part-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/machine-part/index',
              meta: { title: '零件工序定义', icon: 'project', noCache: true, permissions: ['configProductProcessMachinePartPM'] }
            }
          ]
        },
        {
          path: 'production-line-config',
          component: '',
          hidden: false,
          name: 'ProductionLineConfig',
          alwaysShow: true,
          redirect: '/mes/production-config/production-line-config/production-team',
          meta: { title: '生产线配置', icon: 'project', noCache: true },
          children: [
            {
              name: 'MesConfigProductionTeam',
              path: 'production-team',
              hidden: false,
              component: '/mes/production-config/production-team/index',
              meta: { title: '班组管理', icon: 'project', noCache: true, permissions: ['configProductionLineTeamPM', 'configProductionLineInspectPM'] }
            },
            {
              name: 'MesConfigProductionLine',
              path: 'production-line',
              hidden: false,
              component: '/mes/production-config/production-line/index',
              meta: { title: '生产线管理', icon: 'project', noCache: true, permissions: ['configProductionLinePM', 'configProductionLineGroupPM'] }
            },
            {
              name: 'MesConfigInspectionMode',
              path: 'inspection-mode',
              hidden: false,
              component: '/mes/production-config/inspection-mode/index',
              meta: { title: '报检方式', icon: 'project', noCache: true, permissions: ['configInspectionModePM'] }
            }
          ]
        },
        {
          path: 'statistical-config',
          component: '',
          hidden: false,
          name: 'StatisticalConfig',
          alwaysShow: true,
          redirect: '/mes/production-config/statistical-config/artifact/rivet-weld-config',
          meta: { title: '统计配置', icon: 'project', noCache: true },
          children: [
            {
              name: 'MesConfigArtifactTypeConfig',
              path: 'artifact/artifact-type-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/artifact-type-config/index',
              meta: { title: '构件-清单种类', icon: 'project', noCache: true, permissions: ['configArtifactTypeConfigPM'] }
            },
            {
              name: 'MesConfigArtifactRivetWeldConfig',
              path: 'artifact/rivet-weld-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/artifact-rivet-weld-config/index',
              meta: { title: '构件-组铆焊价格', icon: 'project', noCache: true, permissions: ['configArtifactRivetWeldConfigPM'] }
            },
            {
              name: 'MesConfigCoatingConfig',
              path: 'artifact/coating-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/coating-config/index',
              meta: { title: '构件-涂装', icon: 'project', noCache: true, permissions: ['configStatisticalCoatingPM'] }
            },
            {
              name: 'MesConfigAssembleRivetWeldConfig',
              path: 'assemble/rivet-weld-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/assemble-rivet-weld-config/index',
              meta: { title: '部件-组铆焊价格', icon: 'project', noCache: true, permissions: ['configAssembleRivetWeldConfigPM'] }
            },
            {
              name: 'MesConfigFabricatedConfig',
              path: 'artifact/fabricated-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/fabricated-config/index',
              meta: { title: '配套件-栓钉套筒', icon: 'project', noCache: true, permissions: ['configStatisticalFabricatedPM'] }
            },
            {
              name: 'MesConfigDrillHoleConfig',
              path: 'parts/drill-hole-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/drill-hole-config/index',
              meta: { title: '零件-钻孔', icon: 'project', noCache: true, permissions: ['configStatisticalDrillHolePM'] }
            },
            {
              name: 'MesConfigPartsLayingConfig',
              path: 'parts/parts-laying-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/parts-laying-config/index',
              meta: { title: '零件-下料', icon: 'project', noCache: true, permissions: ['configStatisticalPartsLayingPM'] }
            }
          ]
        },
        // {
        //   name: 'MesConfigWageQuota',
        //   path: 'wage-quota',
        //   hidden: false,
        //   component: '/mes/production-config/wage-quota/index',
        //   meta: { title: '工价定额', icon: 'project', noCache: true }
        // },
        {
          name: 'MesConfigCutting',
          path: 'cutting-config',
          hidden: false,
          component: '/mes/production-config/cutting-config/index',
          meta: { title: '零件下料配置', icon: 'project', noCache: true, permissions: ['configMachinePartLayingPM'] }
        }
      ]
    },
    {
      path: '/bridge/production-config',
      component: 'Layout',
      hidden: false,
      name: 'BridgeProductionConfig',
      alwaysShow: false,
      redirect: '/bridge/production-config/factory-and-workshop',
      meta: { title: '桥梁-生产配置', icon: 'project', noCache: true },
      children: [
        {
          name: 'BridgeWorkShop',
          path: 'bridge-workshop',
          hidden: false,
          component: '/bridge/bridge-production-config/workshop/index',
          meta: { title: '桥梁-车间', icon: 'project', noCache: true, permissions: ['bridgeWorkshopPM'] }
        },
        {
          path: 'bridge-characteristics-config',
          component: '',
          hidden: false,
          name: 'BridgeCharacteristicsConfig',
          alwaysShow: true,
          redirect: '/bridge/production-config/bridge-characteristics-config/artifact-config',
          meta: { title: '分段单元定义', icon: 'project', noCache: true },
          children: [
            {
              name: 'BoxConfig',
              path: 'box-config',
              hidden: false,
              component: '/bridge/bridge-production-config/bridge-characteristics-config/box-config/index',
              meta: { title: '分段特征定义', icon: 'project', noCache: true, permissions: ['boxConfigPM'] }
            },
            {
              name: 'CellConfig',
              path: 'cell-config',
              hidden: false,
              component: '/bridge/bridge-production-config/bridge-characteristics-config/cell-config/index',
              meta: { title: '单元特征定义', icon: 'project', noCache: true, permissions: ['cellConfigPM'] }
            },
            {
              name: 'PartConfig',
              path: 'part-config',
              hidden: false,
              component: '/bridge/bridge-production-config/bridge-characteristics-config/part-config/index',
              meta: { title: '零件特征定义', icon: 'project', noCache: true, permissions: ['partConfigPM'] }
            },
            {
              name: 'BridgeAuxiliaryMaterialConfig',
              path: 'bridge-auxiliary-material-config',
              hidden: false,
              component: '/bridge/bridge-production-config/bridge-characteristics-config/auxiliary-material-config/index',
              meta: { title: '配套件特征定义', icon: 'project', noCache: true, permissions: ['bridgeAuxiliaryMaterialConfigPM'] }
            }
          ]
        },
        {
          path: 'bridge-process-config',
          component: '',
          hidden: false,
          name: 'BridgeProcessConfig',
          alwaysShow: true,
          redirect: '/bridge/production-config/bridge-process-config/bridge-process',
          meta: { title: '生产工序定义', icon: 'project', noCache: true },
          children: [
            {
              name: 'BridgeConfigProcess',
              path: 'bridge-process',
              hidden: false,
              component: '/bridge/bridge-production-config/process/index',
              meta: { title: '工序配置', icon: 'project', noCache: true, permissions: ['bridgeConfigProcessPM'] }
            },
            {
              name: 'BridgeConfigBoxProductProcess',
              path: 'box-product-process',
              hidden: false,
              component: '/bridge/bridge-production-config/product-process/box/index',
              meta: { title: '分段工序定义', icon: 'project', noCache: true, permissions: ['configProductProcessBoxPM'] }
            },
            {
              name: 'BridgeConfigCellProductProcess',
              path: 'cell-product-process',
              hidden: false,
              component: '/bridge/bridge-production-config/product-process/cell/index',
              meta: { title: '单元工序定义', icon: 'project', noCache: true, permissions: ['configProductProcessCellPM'] }
            },
            {
              name: 'BridgeConfigPartProductProcess',
              path: 'bridge-part-product-process',
              hidden: false,
              component: '/bridge/bridge-production-config/product-process/machine-part/index',
              meta: { title: '零件工序定义', icon: 'project', noCache: true, permissions: ['bridgeConfigProductProcessMachinePartPM'] }
            }
          ]
        },
        {
          path: 'bridge-production-line-config',
          component: '',
          hidden: false,
          name: 'BridgeProductionLineConfig',
          alwaysShow: true,
          redirect: '/bridge/production-config/bridge-production-line-config/bridge-production-line',
          meta: { title: '生产线配置', icon: 'project', noCache: true },
          children: [
            {
              name: 'BridgeConfigProductionTeam',
              path: 'bridge-production-team',
              hidden: false,
              component: '/bridge/bridge-production-config/production-team/index',
              meta: { title: '班组管理', icon: 'project', noCache: true, permissions: ['bridgeConfigProductionLineTeamPM', 'bridgeConfigProductionLineInspectPM'] }
            },
            {
              name: 'BridgeConfigProductionLine',
              path: 'bridge-production-line',
              hidden: false,
              component: '/bridge/bridge-production-config/production-line/index',
              meta: { title: '生产线管理', icon: 'project', noCache: true, permissions: ['bridgeConfigProductionLinePM', 'bridgeConfigProductionLineGroupPM'] }
            },
            {
              name: 'BridgeConfigInspectionMode',
              path: 'bridge-inspection-mode',
              hidden: false,
              component: '/bridge/bridge-production-config/inspection-mode/index',
              meta: { title: '报检方式', icon: 'project', noCache: true, permissions: ['bridgeConfigWageQuotaPM'] }
            }
          ]
        },
        {
          path: 'bridge-statistical-config',
          component: '',
          hidden: false,
          name: 'BridgeStatisticalConfig',
          alwaysShow: true,
          redirect: '/bridge/production-config/bridge-statistical-config/box/box-type-config',
          meta: { title: '统计配置', icon: 'project', noCache: true },
          children: [
            {
              name: 'BridgeConfigBoxTypeConfig',
              path: 'box/box-type-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/box-type-config/index',
              meta: { title: '分段-清单种类', icon: 'project', noCache: true, permissions: ['configBoxTypeConfigPM'] }
            },
            {
              name: 'BridgeConfigBoxRivetWeldConfig',
              path: 'box/rivet-weld-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/box-rivet-weld-config/index',
              meta: { title: '分段-组铆焊价格', icon: 'project', noCache: true, permissions: ['configBoxRivetWeldConfigPM'] }
            },
            {
              name: 'BridgeConfigCoatingConfig',
              path: 'box/coating-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/coating-config/index',
              meta: { title: '分段-涂装', icon: 'project', noCache: true, permissions: ['bridgeConfigStatisticalCoatingPM'] }
            },
            {
              name: 'BridgeConfigCellRivetWeldConfig',
              path: 'cell/rivet-weld-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/cell-rivet-weld-config/index',
              meta: { title: '单元件-组铆焊价格', icon: 'project', noCache: true, permissions: ['configCellRivetWeldConfigPM'] }
            },
            {
              name: 'BridgeConfigFabricatedConfig',
              path: 'box/fabricated-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/fabricated-config/index',
              meta: { title: '配套件-栓钉套筒', icon: 'project', noCache: true, permissions: ['bridgeConfigStatisticalFabricatedPM'] }
            },
            {
              name: 'BridgeConfigDrillHoleConfig',
              path: 'bridge-parts/drill-hole-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/drill-hole-config/index',
              meta: { title: '零件-钻孔', icon: 'project', noCache: true, permissions: ['bridgeConfigStatisticalDrillHolePM'] }
            },
            {
              name: 'BridgeConfigPartsLayingConfig',
              path: 'bridge-parts/parts-laying-config',
              hidden: false,
              component: '/bridge/bridge-production-config/statistical-config/parts-laying-config/index',
              meta: { title: '零件-下料', icon: 'project', noCache: true, permissions: ['bridgeConfigStatisticalPartsLayingPM'] }
            }
          ]
        },
        {
          name: 'BridgeConfigCutting',
          path: 'bridge-cutting-config',
          hidden: false,
          component: '/bridge/bridge-production-config/cutting-config/index',
          meta: { title: '零件下料配置', icon: 'project', noCache: true, permissions: ['bridgeConfigMachinePartLayingPM'] }
        }
      ]
    },
    {
      path: '/contract/enclosure-config',
      component: 'Layout',
      hidden: false,
      name: 'ContractConfig',
      alwaysShow: true,
      redirect: '/contract/enclosure-config/info-manage',
      meta: { title: '项目配置', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'EnclosureInfoConfig',
          path: 'info-manage',
          hidden: false,
          component: '/config-manage/contract/enclosure-content/index',
          meta: { title: '围护信息配置', icon: 'project', noCache: true, permissions: ['enclosureInfoConfigPM'] }
        },
        {
          name: 'MemberConfig',
          path: 'member-config',
          hidden: false,
          component: '/config-manage/contract/setting-config/info-manage/index',
          meta: { title: '项目成员配置', icon: 'project', noCache: true, permissions: ['memberConfigPM'] }
        }
      ]
    },
    {
      path: '/approval-manage',
      component: 'Layout',
      hidden: false,
      name: 'ApprovalManage',
      alwaysShow: true,
      redirect: '/config-manage/approval-manage/company-process',
      meta: { title: '审批管理', icon: 'project', noCache: true },
      children: [
        {
          name: 'CompanyProcess',
          path: 'company-process',
          hidden: false,
          component: '/config-manage/approval-manage/company-process/index',
          meta: { title: '公司审批流程', icon: 'project', noCache: true, permissions: ['companyProcessConfigPM'] }
        }
      ]
    },
    {
      path: '/supply-chain-config',
      component: 'Layout',
      hidden: false,
      name: 'SupplyChainConfig',
      alwaysShow: true,
      redirect: '/supply-chain-config/subcontract-config',
      meta: { title: '供应链配置管理', icon: 'project', noCache: true },
      children: [
        {
          name: 'SubcontractConfig',
          path: 'subcontract-config',
          hidden: false,
          component: '/config-manage/supply-chain/base/index',
          meta: { title: '分包配置', icon: 'project', noCache: true, permissions: ['subcontractConfigPM'] }
        }
      ]
    },
    {
      path: '/project-config',
      component: 'Layout',
      hidden: false,
      name: 'ProjectConfigManage',
      alwaysShow: true,
      redirect: '/project-config/subcontract-config',
      meta: { title: '项目管理配置', icon: 'project', noCache: true },
      children: [
        {
          name: 'QualityProblemConfig',
          path: 'quality-problem-config',
          hidden: false,
          component: '/config-manage/project/quality-problem-config/index',
          meta: { title: '问题分类', icon: 'project', noCache: true, permissions: ['projectProblemConfigPM'] }
        },
        {
          name: 'VisaReasonConfig',
          path: 'visa-reason-config',
          hidden: false,
          component: '/config-manage/project/visa-reason-config/index',
          meta: { title: '分包签证原因', icon: 'project', noCache: true, permissions: ['projectVisaReasonConfigPM'] }
        }
      ]
    }
  ]
}
