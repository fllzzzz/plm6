// 路由：项目管理
export default {
  id: 956,
  name: '项目管理',
  children: [
    {
      path: '/project-manage/progress-manage',
      component: 'Layout',
      hidden: false,
      name: 'ProgressManage',
      alwaysShow: false,
      redirect: '/project-manage/progress-manage/project-progress',
      meta: { title: '进度管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'ProjectProgress',
          path: 'project-progress',
          hidden: false,
          component: '/project-manage/progress-manage/project-progress/index',
          meta: { title: '进度状态', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/project-manage/subcontract-manage',
      component: 'Layout',
      hidden: false,
      name: 'SubcontractManage',
      alwaysShow: false,
      redirect: '/project-manage/subcontract-manage/subcontract-plan',
      meta: { title: '分包管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'SubcontractPlan',
          path: 'subcontract-plan',
          hidden: false,
          component: '/project-manage/subcontract-manage/subcontract-plan/index',
          meta: { title: '工期制定', icon: 'project', noCache: true }
        },
        {
          name: 'SubcontractProgress',
          path: 'subcontract-progress',
          hidden: false,
          component: '/project-manage/subcontract-manage/subcontract-progress/index',
          meta: { title: '计划跟踪', icon: 'project', noCache: true }
        },
        {
          name: 'SubcontractPayment',
          path: 'subcontract-payment',
          hidden: false,
          component: '/project-manage/subcontract-manage/subcontract-payment/index',
          meta: { title: '付款申请', icon: 'project', noCache: true }
        },
        {
          name: 'SubcontractVisaManage',
          path: 'subcontract-visa-manage',
          hidden: false,
          component: '/project-manage/subcontract-manage/subcontract-visa-manage/index',
          meta: { title: '签证管理', icon: 'project', noCache: true }
        },
        {
          name: 'SafetyQualityManage',
          path: 'safety-quality-manage',
          hidden: false,
          component: '/project-manage/subcontract-manage/safety-quality-manage/index',
          meta: { title: '质安管理', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/project-manage/visa-settle-manage',
      component: 'Layout',
      hidden: false,
      name: 'VisaSettleManage',
      alwaysShow: false,
      redirect: '/project-manage/visa-settle-manage/visa-settle',
      meta: { title: '签证结算', icon: 'contract', noCache: true },
      children: [
        {
          name: 'VisaSettle',
          path: 'visa-settle',
          hidden: false,
          component: '/project-manage/visa-settle-manage/index',
          meta: { title: '签证结算', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/project-manage/delivery-manage',
      component: 'Layout',
      hidden: false,
      name: 'DeliveryManage',
      alwaysShow: false,
      redirect: '/project-manage/delivery-manage/homemade-delivery',
      meta: { title: '收货管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'HomemadeDelivery',
          path: 'homemade-delivery',
          hidden: false,
          component: '/project-manage/delivery-manage/homemade-delivery/index',
          meta: { title: '自制收货', icon: 'project', noCache: true }
        },
        {
          name: 'OutsourceDelivery',
          path: 'outsource-delivery',
          hidden: false,
          component: '/project-manage/delivery-manage/outsource-delivery/index',
          meta: { title: '外购收货', icon: 'project', noCache: true }
        },
        {
          path: 'delivery-report',
          component: '',
          hidden: false,
          name: 'DeliveryReport',
          alwaysShow: false,
          redirect: '/project-manage/delivery-manage/delivery-report/delivery-report-list',
          meta: { title: '报表管理', icon: 'contract', noCache: true },
          children: [
            {
              name: 'DeliveryReportList',
              path: 'delivery-report-list',
              hidden: false,
              component: '/project-manage/delivery-manage/delivery-report/report-list/index',
              meta: { title: '收货报表', icon: 'project', noCache: true }
            },
            {
              name: 'DeliveryDashboard',
              path: 'delivery-dashboard',
              hidden: false,
              component: '/project-manage/delivery-manage/delivery-report/report-dashboard/index',
              meta: { title: '收货看板', icon: 'project', noCache: true }
            }
          ]
        },
        {
          name: 'DeliveryInstall',
          path: 'delivery-install',
          hidden: false,
          component: '/project-manage/delivery-manage/delivery-install-list/index',
          meta: { title: '收安报表', icon: 'project', noCache: true }
        }
      ]
    },
    {
      path: '/project-manage/install-manage',
      component: 'Layout',
      hidden: false,
      name: 'InstallManage',
      alwaysShow: false,
      redirect: '/project-manage/install-manage/handle-install',
      meta: { title: '安装管理', icon: 'contract', noCache: true },
      children: [
        {
          name: 'HandleInstall',
          path: 'handle-install',
          hidden: false,
          component: '/project-manage/install-manage/handle-install/index',
          meta: { title: '安装填报', icon: 'project', noCache: true }
        },
        {
          name: 'InstallAudit',
          path: 'install-audit',
          hidden: false,
          component: '/project-manage/install-manage/install-audit/index',
          meta: { title: '安装审核', icon: 'project', noCache: true }
        },
        {
          path: 'install-report',
          component: '',
          hidden: false,
          name: 'InstallReport',
          alwaysShow: false,
          redirect: '/project-manage/install-manage/install-report/install-report-list',
          meta: { title: '报表管理', icon: 'contract', noCache: true },
          children: [
            {
              name: 'InstallReportList',
              path: 'install-report-list',
              hidden: false,
              component: '/project-manage/install-manage/install-report/report-list/index',
              meta: { title: '安装报表', icon: 'project', noCache: true }
            },
            {
              name: 'InstallDashboard',
              path: 'install-dashboard',
              hidden: false,
              component: '/project-manage/install-manage/install-report/install-dashboard/index',
              meta: { title: '安装看板', icon: 'project', noCache: true }
            }
          ]
        }
      ]
    },
    {
      path: 'project-manage/data-manage',
      component: 'Layout',
      hidden: false,
      name: 'DataManage',
      alwaysShow: false,
      redirect: '/project-manage/data-manage/construction-log',
      meta: { title: '资料管理', icon: 'list', noCache: true },
      children: [
        {
          name: 'ImageProgress',
          path: 'image-progress',
          hidden: false,
          component: '/project-manage/data-manage/image-progress/index',
          meta: { title: '形象进度', icon: 'list', noCache: true }
        },
        {
          name: 'ConstructionLog',
          path: 'construction-log',
          hidden: false,
          component: '/project-manage/data-manage/construction-log/index',
          meta: { title: '施工日志', icon: 'list', noCache: true }
        },
        {
          name: 'ConstructionData',
          path: 'construction-data',
          hidden: false,
          component: '/project-manage/data-manage/construction-data/index',
          meta: { title: '施工资料', icon: 'list', noCache: true }
        }
      ]
    },
    {
      path: 'project-manage/install-config-manage',
      component: 'Layout',
      hidden: false,
      name: 'InstallConfigManage',
      alwaysShow: false,
      redirect: '/project-manage/install-config-manage/install-config',
      meta: { title: '安装设置管理', icon: 'list', noCache: true },
      children: [
        {
          name: 'InstallConfig',
          path: 'install-config',
          hidden: false,
          component: '/project-manage/install-config/index',
          meta: { title: '安装设置', icon: 'list', noCache: true }
        }
      ]
    }
  ]
}
