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
      redirect: '/config-manage/base-config/unit-config',
      meta: { title: '基础配置', icon: 'config-2', noCache: true },
      children: [
        {
          name: 'UnitConfig',
          path: 'unit-config',
          hidden: false,
          component: '/config-manage/main/unit-config/index',
          meta: { title: '单位配置', icon: 'project', noCache: true }
        }
      ]
    },
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
          meta: { title: '型材库', icon: 'warehouse', noCache: true }
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
      meta: { title: 'WMS配置管理', icon: 'config-2', noCache: true },
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
          meta: { title: '仓库设置', icon: 'Steve-Jobs', noCache: true }
        },
        {
          name: 'CommonTaxRate',
          path: 'common-tax-rate',
          hidden: false,
          component: '/config-manage/wms/common-tax-rate/index',
          meta: { title: '常用税率', icon: 'Steve-Jobs', noCache: true }
        }
        // {
        //   name: 'InventoryWarning',
        //   path: 'inventory-warning',
        //   hidden: false,
        //   component: '/wms-config/inventory-warning/index',
        //   meta: { title: '库存预警', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'UnitConfig',
        //   path: 'unit-config',
        //   hidden: false,
        //   component: '/wms-config/unit-config/index',
        //   meta: { title: '单位配置', icon: 'project', noCache: true }
        // },
        // {
        //   name: 'MesConfigFactory',
        //   path: 'factory',
        //   hidden: false,
        //   component: '/wms-config/factory/index',
        //   meta: { title: '工厂管理', icon: 'factory', noCache: true }
        // },

        // {
        //   name: 'ConfigSectionSteelLibrary',
        //   path: 'profile-spec',
        //   hidden: false,
        //   component: '/wms-config/profile-spec/index',
        //   meta: { title: '型材库', icon: 'warehouse', noCache: true }
        // }
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
          name: 'MesConfigFactoryAndWorkshop',
          path: 'factory-and-workshop',
          hidden: false,
          component: '/mes/production-config/factory-and-workshop/index',
          meta: { title: '工厂管理', icon: 'project', noCache: true }
        },
        {
          name: 'MesConfigProductionLine',
          path: 'production-line',
          hidden: false,
          component: '/mes/production-config/production-line/index',
          meta: { title: '生产线管理', icon: 'project', noCache: true }
        },
        {
          name: 'MesConfigProcess',
          path: 'process',
          hidden: false,
          component: '/mes/production-config/process/index',
          meta: { title: '工序配置', icon: 'project', noCache: true }
        },
        {
          name: 'MesConfigProductProcess',
          path: 'product-process',
          hidden: false,
          component: '/mes/production-config/product-process/index',
          meta: { title: '工序管理', icon: 'project', noCache: true }
        },
        {
          name: 'MesConfigWageQuota',
          path: 'wage-quota',
          hidden: false,
          component: '/mes/production-config/wage-quota/index',
          meta: { title: '工价定额', icon: 'project', noCache: true }
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
        }
      ]
    }
  ]
}
