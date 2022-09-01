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
          meta: { title: '公司配置', icon: 'project', noCache: true }
        },
        {
          name: 'UnitConfig',
          path: 'unit-config',
          hidden: false,
          component: '/config-manage/main/unit-config/index',
          meta: { title: '单位配置', icon: 'project', noCache: true }
        },
        {
          name: 'factoryAndWorkshopConfig',
          path: 'factory-and-workshop',
          hidden: false,
          component: '/mes/production-config/factory-and-workshop/index',
          meta: { title: '工厂管理', icon: 'project', noCache: true }
        },
        {
          name: 'NumberConfig',
          path: 'number-config',
          hidden: false,
          component: '/config-manage/main/number-config/index',
          meta: { title: '编号配置', icon: 'project', noCache: true }
        },
        {
          name: 'CommonTaxRate',
          path: 'common-tax-rate',
          hidden: false,
          component: '/config-manage/main/common-tax-rate/index',
          meta: { title: '常用税率', icon: 'project', noCache: true }
        },
        {
          name: 'ExpenseManagement',
          path: 'expense-management',
          hidden: false,
          component: '/config-manage/system/expense-management/index',
          meta: { title: '费用归类', icon: 'project', noCache: true }
        },
        {
          name: 'BranchCompany',
          path: 'branch-company',
          hidden: false,
          component: '/config-manage/system/branch-company/index',
          meta: { title: '分支机构', icon: 'project', noCache: true }
        },
        {
          name: 'ProjectMode',
          path: 'project-mode',
          hidden: false,
          component: '/config-manage/system/project-mode/index',
          meta: { title: '项目模式', icon: 'project', noCache: true }
        },
        {
          name: 'TablePrinting',
          path: 'table-printing',
          hidden: false,
          component: '/config-manage/system/table-print-template/index',
          meta: { title: '表格模板', icon: 'project', noCache: true }
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
          meta: { title: '科目配置', icon: 'project', noCache: true }
        },
        {
          name: 'SpecificationConfig',
          path: 'specification-config',
          hidden: false,
          component: '/config-manage/classification-manage/specification-config/index',
          meta: { title: '规格配置', icon: 'project', noCache: true }
        },
        {
          name: 'ClassificationUnitConfig',
          path: 'measure-config',
          hidden: false,
          component: '/config-manage/classification-manage/measure-config/index',
          meta: { title: '计量配置', icon: 'project', noCache: true }
        },
        {
          name: 'ConfigSectionSteelLibrary',
          path: 'section-steel-specification-config',
          hidden: false,
          component: '/config-manage/classification-manage/section-steel-specification-config/index',
          meta: { title: '型材库', icon: 'project', noCache: true }
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
          meta: { title: '基础配置', icon: 'project', noCache: true }
        },
        {
          name: 'ConfigWarehouse',
          path: 'warehouse',
          hidden: false,
          component: '/config-manage/wms/warehouse/index',
          meta: { title: '仓库设置', icon: 'project', noCache: true }
        },
        {
          name: 'ScrapDefinition',
          path: 'scrap-definition',
          hidden: false,
          component: '/config-manage/wms/scrap-definition/index',
          meta: { title: '废料定义', icon: 'project', noCache: true }
        },
        {
          name: 'ConfigInspectionRawMaterial',
          path: 'inspection-raw-material',
          hidden: false,
          component: '/config-manage/wms/inspection-raw-material/index',
          meta: { title: '入库质检物料', icon: 'project', noCache: true }
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
          meta: { title: '基础配置', icon: 'project', noCache: true }
        },
        {
          name: 'ChangeReason',
          path: 'change-reason',
          hidden: false,
          component: '/config-manage/mes/change-reason-config/index',
          meta: { title: '变更原因配置', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/mes/production-config',
      component: 'Layout',
      hidden: false,
      name: 'MesProductionConfig',
      alwaysShow: false,
      redirect: '/mes/production-config/factory-and-workshop',
      meta: { title: '建钢-生产配置', icon: 'project', noCache: true },
      children: [
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
              meta: { title: '构件特征定义', icon: 'project', noCache: true }
            },
            {
              name: 'MachinePartConfig',
              path: 'machine-part-config',
              hidden: false,
              component: '/config-manage/mes/machine-part/index',
              meta: { title: '部件特征定义', icon: 'project', noCache: true }
            },
            {
              name: 'SteelClassic',
              path: 'steel-classic',
              hidden: false,
              component: '/config-manage/mes/steel-classic/index',
              meta: { title: '零件特征定义', icon: 'project', noCache: true }
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
              meta: { title: '工序配置', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigArtifactProductProcess',
              path: 'artifact-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/artifact/index',
              meta: { title: '构件工序定义', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigAssembleProductProcess',
              path: 'assemble-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/assemble/index',
              meta: { title: '部件工序定义', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigMachinePartProductProcess',
              path: 'machine-part-product-process',
              hidden: false,
              component: '/mes/production-config/product-process/machine-part/index',
              meta: { title: '零件工序定义', icon: 'project', noCache: true }
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
              meta: { title: '班组管理', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigProductionLine',
              path: 'production-line',
              hidden: false,
              component: '/mes/production-config/production-line/index',
              meta: { title: '生产线管理', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigInspectionMode',
              path: 'inspection-mode',
              hidden: false,
              component: '/mes/production-config/inspection-mode/index',
              meta: { title: '报检方式', icon: 'project', noCache: true }
            }
          ]
        },
        {
          path: 'statistical-config',
          component: '',
          hidden: false,
          name: 'StatisticalConfig',
          alwaysShow: true,
          redirect: '/mes/production-config/statistical-config/coating-config',
          meta: { title: '统计配置', icon: 'project', noCache: true },
          children: [
            {
              name: 'MesConfigCoatingConfig',
              path: 'coating-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/coating-config/index',
              meta: { title: '涂装配置', icon: 'project', noCache: true }
            },
            {
              name: 'MesConfigFabricatedConfig',
              path: 'fabricated-config',
              hidden: false,
              component: '/mes/production-config/statistical-config/fabricated-config/index',
              meta: { title: '栓钉套筒', icon: 'project', noCache: true }
            }
          ]
        },
        {
          name: 'MesConfigWageQuota',
          path: 'wage-quota',
          hidden: false,
          component: '/mes/production-config/wage-quota/index',
          meta: { title: '工价定额', icon: 'project', noCache: true }
        },
        {
          name: 'MesConfigCutting',
          path: 'cutting-config',
          hidden: false,
          component: '/mes/production-config/cutting-config/index',
          meta: { title: '切割配置', icon: 'project', noCache: true }
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
          meta: { title: '围护信息配置', icon: 'project', noCache: true }
        },
        {
          name: 'MemberConfig',
          path: 'member-config',
          hidden: false,
          component: '/config-manage/contract/setting-config/info-manage/index',
          meta: { title: '项目成员配置', icon: 'project', noCache: true }
        }
      ]
    }
  ]
}
